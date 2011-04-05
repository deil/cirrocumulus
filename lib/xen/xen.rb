AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'active_support'
require 'systemu'
require 'sexpistol'
require "#{AGENT_ROOT}/../cm/logger.rb"
require "#{AGENT_ROOT}/../cm/agent.rb"
require "#{AGENT_ROOT}/../cm/saga.rb"
require "#{AGENT_ROOT}/../cm/kb.rb"
require "#{AGENT_ROOT}/../cm/cirrocumulus.rb"
require "#{AGENT_ROOT}/xen_node.rb"
require "#{AGENT_ROOT}/raid.rb"
require "#{AGENT_ROOT}/raid_assemble_saga.rb"
require "#{AGENT_ROOT}/raid_stop_saga.rb"
require "#{AGENT_ROOT}/raid_create_saga.rb"

class DomUKb < Kb
  def collect_knowledge()
    @knowledge = []
    domUs = XenNode::list_running_domUs()
    domUs.each do |domU|
      add_fact([:running, [:domu, domU]], 'yes')
    end

    add_fact([:free_memory], XenNode::free_memory)
    add_fact([:domus_running], XenNode::list_running_domUs().size)
  end
end

class XenAgent < Agent
  def initialize(cm)
    super()
    @cm = cm
    @default_ontology = 'cirrocumulus-xen'
    XenNode::set_cpu(0, 10000, 0)
    systemu 'aoe-discover'
  end

  def handle(message, kb)
    #p message
    s = Sexpistol.new

    case message.act
      when 'query-if' then
        fact = [s.to_sexp(message.content)]
        #p fact
        query = kb.query_fact(fact.first)
        reply = message.content
        if query.nil?
          reply = [:not, message.content]
        end
        
        msg = Cirrocumulus::Message.new('', 'inform', s.to_sexp(reply))
        msg.receiver = message.sender
        msg.ontology = @default_ontology
        msg.in_reply_to = message.reply_with

        @cm.send(msg)
      when 'query-ref' then
        fact = s.to_sexp(message.content)
        Log4r::Logger['agent'].info "query fact: #{fact}"
        if message.content.first == :state
          obj = message.content.second
          if obj.first == :raid
            disk_id = obj.second
            raid_state = Raid::check_raid(disk_id)
            msg = Cirrocumulus::Message.new(nil, 'inform', s.to_sexp([:'=', message.content, [raid_state]]))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          elsif obj.first == :aoe
            disk_id = obj.second
            visible_exports = Raid::check_aoe(disk_id)
            msg = Cirrocumulus::Message.new(nil, 'inform', s.to_sexp([:'=', message.content, [visible_exports].flatten]))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          end
        else
          query = kb.query_fact(fact)
          reply = [:'=', fact, [query]]
          msg = Cirrocumulus::Message.new('', 'inform', s.to_sexp(reply))
          msg.receiver = message.sender
          msg.ontology = @default_ontology
          msg.in_reply_to = message.reply_with
          @cm.send(msg)
        end
      when 'request' then
        action = message.content.first
        if action == :attach_disk
          domU_name = nil
          disk_number = nil
          block_device = nil
          params = message.content.second
          params.each do |par|
            next unless par.is_a? Array
            domU_name = par.second if par.first == :domu
            disk_number = par.second.to_i if par.first == :disk_number
            block_device = par.second if par.first == :block_device
          end

          if XenNode::attach_disk(domU_name, disk_number, block_device)
            msg = Cirrocumulus::Message.new(nil, 'inform', s.to_sexp([message.content, [:finished]]))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          else
            msg = Cirrocumulus::Message.new(nil, 'failure', s.to_sexp(message.content))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          end
        elsif action == :detach_disk
          params = message.content.second
          params.each do |par|
            next unless par.is_a? Array
            domU_name = par.second if par.first == :domu
            block_device = par.second if par.first == :block_device
          end
          
          if XenNode::detach_disk(domU_name, block_device)
            msg = Cirrocumulus::Message.new(nil, 'inform', s.to_sexp([message.content, [:finished]]))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          else
            msg = Cirrocumulus::Message.new(nil, 'failure', s.to_sexp(message.content))
            msg.receiver = message.sender
            msg.ontology = @default_ontology
            msg.in_reply_to = message.reply_with
            @cm.send(msg)
          end
        elsif action == :restart
          domU_config = message.content.second
          domU_name = domU_config.second.second
          XenNode::restart(domU_name)
        elsif action == :stop
          obj = message.content.second
          if obj.first == :domu
            domU_config = message.content.second
            domU_name = domU_config.second.second
            if XenNode::stop(domU_name)
              msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([message.content, [:finished]]))
              msg.ontology = @default_ontology
              msg.receiver = message.sender
              msg.in_reply_to = message.reply_with
              @cm.send(msg)
            else
              msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([message.content, [:unknown_reason]]))
              msg.ontology = @default_ontology
              msg.receiver = message.sender
              msg.in_reply_to = message.reply_with
              @cm.send(msg)
            end
          elsif obj.first == :raid
            start_raid_stop_saga(obj.second.to_i, message)
          end
        elsif action == :start
          obj = message.content.second
          if obj.first == :domu
            domU_id = 0
            domU_name = nil
            domU_mem = 0
            domU_vcpus = 1
            domU_cpu_weight = 0
            domU_cpu_cap = 0
            domU_disks = []

            domU_config = message.content.second
            p domU_config
            domU_config.each do |cfg|
              next if !cfg.is_a? Array

              if cfg.first == :id
                domU_id = cfg.second
              elsif cfg.first == :name
                domU_name = cfg.second
              elsif cfg.first == :mem
                domU_mem = cfg.second
              elsif cfg.first == :vcpus
                domU_vcpus = cfg.second
              elsif cfg.first == :disks
                cfg_disks = cfg.second
                cfg_disks.each do |disk|
                  next if !disk.is_a? Array
                  domU_disks << {:name => disk.first, :number => disk.second}
                end
              elsif cfg.first == :cpu_weight
                domU_cpu_weight = cfg.second
              elsif cfg.first == :cpu_cap
                domU_cpu_cap = cfg.second
              end
            end

            if domU_mem > XenNode::free_memory
              @cm.refuse(message.sender, s.to_sexp(message.content), s.to_sexp([:not_enough_memory, XenNode::free_memory]))
            else
              domU = DomU.new(domU_id, domU_name, domU_mem, domU_vcpus, domU_disks, domU_cpu_weight, domU_cpu_cap)
              xml_config = File.join(AGENT_ROOT, "domu_#{domU_name}.xml")
              xml = File.open(xml_config, "w")
              xml.write(domU.to_xml)
              xml.close

              if XenNode::start(xml_config) && XenNode::list_running_domUs().include?(domU_name)
                XenNode::set_cpu(domU_name, domU_cpu_weight, domU_cpu_cap)

                msg = Cirrocumulus::Message.new(nil, 'inform', Sexpistol.new.to_sexp([message.content, [:finished]]))
                msg.ontology = @default_ontology
                msg.receiver = message.sender
                msg.in_reply_to = message.reply_with
                @cm.send(msg)
              else
                msg = Cirrocumulus::Message.new(nil, 'failure', Sexpistol.new.to_sexp([message.content, [:unknown_reason]]))
                msg.ontology = @default_ontology
                msg.receiver = message.sender
                msg.in_reply_to = message.reply_with
                @cm.send(msg)
              end
            end
          elsif obj.first == :raid
            start_raid_assemble_saga(obj.second.to_i, message)
          end
        end
    end

  end
  
  private
  
  def start_raid_assemble_saga(disk_number, message)
    @saga_idx += 1
    saga_id = "xen-assemble-raid-#{@saga_idx}"
    saga = RaidAssembleSaga.new(saga_id, @cm, self)
    @sagas << saga
    saga.start(disk_number, message.context)
  end
  
  def start_raid_stop_saga(disk_number, message)
    @saga_idx += 1
    saga_id = "xen-stop-raid-#{@saga_idx}"
    saga = RaidStopSaga.new(saga_id, @cm, self)
    @sagas << saga
    saga.start(disk_number, message.context)
  end
end

# <fipa-message act="query-if" ontology="/cirrocumulus-xen"><content>(running (domu "03731b0100d9680173f75076e603d15d"))</content></fipa-message>
# <fipa-message act="request" ontology="/cirrocumulus-xen"><content>start (domu (id 153) (name "03731b0100d9680173f75076e603d15d") (mem 256) (vcpus 1) (disks (xvda 153)) (cpu_weight 256) (cpu_cap 0))</content></fipa-message>

cm = Cirrocumulus.new('xen')
cm.run(XenAgent.new(cm), DomUKb.new)
