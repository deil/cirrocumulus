AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'active_support'
require 'systemu'
require 'sexpistol'
require "#{AGENT_ROOT}/../cm/logger.rb"
require "#{AGENT_ROOT}/../cm/agent.rb"
require "#{AGENT_ROOT}/../cm/kb.rb"
require "#{AGENT_ROOT}/../cm/cirrocumulus.rb"
require "#{AGENT_ROOT}/storage_node.rb"

class StorageAgent < Agent
  def initialize(cm)
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
        request(message.content)
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

  def request(obj)
    
  end
  
end

# <fipa-message act="query-if" ontology="/cirrocumulus-xen"><content>(running (domu "03731b0100d9680173f75076e603d15d"))</content></fipa-message>
# <fipa-message act="request" ontology="/cirrocumulus-xen"><content>start (domu (id 153) (name "03731b0100d9680173f75076e603d15d") (mem 256) (vcpus 1) (disks (xvda 153)) (cpu_weight 256) (cpu_cap 0))</content></fipa-message>

cm = Cirrocumulus.new('storage')
cm.run(StorageAgent.new(cm), Kb.new)
