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
backend_platform = 'linux'
backend_platform = 'freebsd' if PLATFORM =~ /freebsd/
require "#{AGENT_ROOT}/#{backend_platform}/#{STORAGE_BACKEND}/storage_node.rb"

class StorageAgent < Agent
  def initialize(cm)
    super()

    @cm = cm
    @default_ontology = 'cirrocumulus-storage'
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
      disk_number = obj.second.to_i
      msg.content = StorageNode.is_exported?(disk_number) ? Sexpistol.new.to_sexp(obj) : Sexpistol.new.to_sexp([:not, obj])
    end

    msg
  end

  def handle_request(message)
    action = message.content.first
    if action == :create
      obj = message.content.second
      disk_number = disk_size = nil

      if obj.first == :raid
        obj.each do |param|
          next if !param.is_a? Array
          if param.first == :disk_number
            disk_number = param.second.to_i
          elsif param.first == :size
            disk_size = param.second.to_i
          end
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
      end
    end
  end
  
end

# <fipa-message act="query-if" ontology="/cirrocumulus-xen"><content>(running (domu "03731b0100d9680173f75076e603d15d"))</content></fipa-message>
# <fipa-message act="request" ontology="/cirrocumulus-xen"><content>start (domu (id 153) (name "03731b0100d9680173f75076e603d15d") (mem 256) (vcpus 1) (disks (xvda 153)) (cpu_weight 256) (cpu_cap 0))</content></fipa-message>

Log4r::Logger['agent'].info "current platform = #{PLATFORM}"
Log4r::Logger['agent'].info "backend = #{STORAGE_BACKEND}"
cm = Cirrocumulus.new('storage')
cm.run(StorageAgent.new(cm), Kb.new)
