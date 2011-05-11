AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'active_support'
require 'systemu'
require 'sexpistol'
require "#{AGENT_ROOT}/../cm/logger.rb"
require "#{AGENT_ROOT}/../cm/agent.rb"
require "#{AGENT_ROOT}/../cm/kb.rb"
require "#{AGENT_ROOT}/../cm/cirrocumulus.rb"

# load corresponding backend
require "#{AGENT_ROOT}/storage_config.rb"
require "#{AGENT_ROOT}/#{Cirrocumulus::platform}/#{STORAGE_BACKEND}/storage_node.rb"

require "#{AGENT_ROOT}/storage_db.rb"

class StorageAgent < Agent
  def initialize(cm)
    super(cm)
    @default_ontology = 'cirrocumulus-storage'
  end
  
  def restore_state()
    StorageNode.list_disks().each do |disk_number|
      export_is_up = StorageNode.is_exported?(disk_number)
      export_should_be_up = AgentState.export_should_be_up?(disk_number)
      
      if export_should_be_up && !export_is_up
        Log4r::Logger['agent'].info "bringing up export #{disk_number}"
        StorageNode.add_export(disk_number, storage_number)
      elsif !export_should_be_up && export_is_up
        Log4r::Logger['agent'].info "shutting down export #{disk_number}"
        StorageNode.remove_export(disk_number)
      end
    end
    
    Log4r::Logger['agent'].info "restored agent state"
  end

  def handle(message, kb)
    s = Sexpistol.new
    case message.act
      when 'query-ref'
        msg = query(message.content)
        msg.receiver = message.sender
        msg.ontology = @default_ontology
        msg.in_reply_to = message.reply_with
        @cm.send(msg)

      when 'query-if'
        msg = query_if(message.content)
        msg.receiver = message.sender
        msg.ontology = @default_ontology
        msg.in_reply_to = message.reply_with
        @cm.send(msg)

      when 'request'
        handle_request(message)
    end
  end

  private
  
  def storage_number
    hostname = `hostname`
    if hostname =~ /(\d+)$/
      return $1.to_i
    end
    
    0
  end

  def query(obj)
    msg = Cirrocumulus::Message.new(nil, 'inform', nil)
    if obj.first == :free_space
      msg.content = Sexpistol.new.to_sexp([:'=', obj, [StorageNode.free_space]])
    elsif obj.first == :used_space
      msg.content = Sexpistol.new.to_sexp([:'=', obj, [StorageNode.used_space]])
    end

    msg
  end

  def query_if(obj)
    msg = Cirrocumulus::Message.new(nil, 'inform', nil)
    if obj.first == :exported
      disk_number = nil
      
      obj.each do |param|
        next if !param.is_a?(Array)
        if param.first == :disk_number
          disk_number = param.second.to_i
        end
      end

      msg.content = StorageNode.is_exported?(disk_number) ? Sexpistol.new.to_sexp(obj) : Sexpistol.new.to_sexp([:not, obj])
    elsif obj.first == :exists
      obj.each do |param|
        next if !param.is_a?(Array)
        if param.first == :volume
          disk_number = nil
          
          volume = param
          volume.each do |vparam|
            next if !vparam.is_a?(Array)
            if vparam.first == :disk_number
              disk_number = vparam.second.to_i
            end
          end
          
          if StorageNode::volume_exists?(disk_number)
            msg.content = obj
          else
            msg.content = [:not, obj]
          end
        elsif param.first == :export
          disk_number = nil
          export = param
          export.each do |eparam|
            next if !eparam.is_a?(Array)
            if eparam.first == :disk_number
              disk_number = eparam.second.to_i
            end
          end
          
          if StorageNode::is_exported?(disk_number)
            msg.content = obj
          else
            msg.content = [:not, obj]
          end
        end
      end
    end

    msg
  end

  def handle_request(message)
    action = message.content.first
    disk_number = nil

    if action == :delete
      obj = message.content.second

      if obj.first == :export
        obj.each do |param|
          next if !param.is_a? Array
          if param.first == :disk_number
            disk_number = param.second.to_i
          end
        end
        
        if StorageNode::remove_export(disk_number)
          msg = Cirrocumulus::Message.new(nil, 'inform', [message.content, [:finished]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        else
          msg = Cirrocumulus::Message.new(nil, 'failure', [message.content, [:unknown_reason]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        end
      elsif obj.first == :volume
        obj.each do |param|
          next if !param.is_a? Array
          if param.first == :disk_number
            disk_number = param.second.to_i
          end
        end
        
        if !StorageNode::volume_exists?(disk_number)
          msg = Cirrocumulus::Message.new(nil, 'refuse', [message.content, [:not_exists]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
          return
        end
        
        if StorageNode::delete_volume(disk_number)
          msg = Cirrocumulus::Message.new(nil, 'inform', [message.content, [:finished]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        else
          msg = Cirrocumulus::Message.new(nil, 'failure', [message.content, [:unknown_reason]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        end
      end
    elsif action == :create
      obj = message.content.second
      disk_number = disk_size = nil

      if obj.first == :volume
        obj.each do |param|
          next if !param.is_a? Array
          if param.first == :disk_number
            disk_number = param.second.to_i
          elsif param.first == :size
            disk_size = param.second.to_i
          end
        end

        if StorageNode::volume_exists?(disk_number)
          msg = Cirrocumulus::Message.new(nil, 'refuse', [message.content, [:already_exists]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
          return
        end

        if StorageNode::create_volume(disk_number, disk_size)
          msg = Cirrocumulus::Message.new(nil, 'inform', [message.content, [:finished]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        else
          msg = Cirrocumulus::Message.new(nil, 'failure', [message.content, [:unknown_reason]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        end
      elsif obj.first == :export
        disk_slot = disk_number = nil
        obj.each do |param|
          next if !param.is_a? Array
          if param.first == :disk_number
            disk_number = param.second.to_i
          elsif param.first == :slot
            disk_slot = param.second.to_i
          end
        end
        
        if StorageNode::add_export(disk_number, disk_slot)
          msg = Cirrocumulus::Message.new(nil, 'inform', [message.content, [:finished]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        else
          msg = Cirrocumulus::Message.new(nil, 'failure', [message.content, [:unknown_reason]])
          msg.ontology = @default_ontology
          msg.receiver = message.sender
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        end
      end
    end
  end
end

Log4r::Logger['agent'].info "storage backend = #{STORAGE_BACKEND}"
cm = Cirrocumulus.new('storage')
cm.run(StorageAgent.new(cm), Kb.new)
