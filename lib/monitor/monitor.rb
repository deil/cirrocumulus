AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'active_support'
require 'sexpistol'
require 'systemu'
require "#{AGENT_ROOT}/../cm/logger.rb"
require "#{AGENT_ROOT}/../cm/agent.rb"
require "#{AGENT_ROOT}/../cm/kb.rb"
require "#{AGENT_ROOT}/../cm/cirrocumulus.rb"

class MonitorAgent < Agent
  def initialize(cm)
    @cm = cm
  end
  
  def handles_ontology?(ontology)
    true
  end

  def handle(message, kb)
    #p message
    @kb = kb # :-)
    agent = message.sender
    key = [:agent, agent]
    key_str = Sexpistol.new.to_sexp(key)
    last_seen_at = kb.query_fact(key_str)
    last_seen = Time.now.to_i - last_seen_at.to_i
    if last_seen_at.nil? || last_seen > 3600
      Log4r::Logger['agent'].info "agent #{message.sender} goes online"
    else
      kb.remove_fact(key_str)
    end
    
    kb.add_fact(key, Time.now.to_i.to_s)
    
    msg = message.sender
    if message.receiver
      msg += " => " + message.receiver
    end
    
    if !message.in_reply_to.blank?
      msg += " (#{message.in_reply_to})"
    end
    
    msg += ": " + Sexpistol.new.to_sexp(message.content)
    puts msg
  end
  
  def tick()
    return unless @kb
    
    @kb.keys.each do |key|
      last_seen_at = @kb.query_fact(key)
      last_seen = Time.now.to_i - last_seen_at.to_i
      if last_seen > 3600
        Log4r::Logger['agent'].warn "agent #{key} goes offline"
      end
    end
  end
end

# <fipa-message act="query-if" ontology="/cirrocumulus-xen"><content>(running (domu "03731b0100d9680173f75076e603d15d"))</content></fipa-message>
# <fipa-message act="request" ontology="/cirrocumulus-xen"><content>start (domu (id 153) (name "03731b0100d9680173f75076e603d15d") (mem 256) (vcpus 1) (disks (xvda 153)) (cpu_weight 256) (cpu_cap 0))</content></fipa-message>

cm = Cirrocumulus.new('monitor')
cm.run(MonitorAgent.new(cm), Kb.new, true)
