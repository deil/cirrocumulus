AGENT_ROOT = File.dirname(__FILE__)

require 'rubygems'
require 'active_support'
require 'active_record'
require 'systemu'
require "#{AGENT_ROOT}/../cm/logger.rb"
require "#{AGENT_ROOT}/../cm/saga.rb"
require "#{AGENT_ROOT}/../cm/agent.rb"
require "#{AGENT_ROOT}/../cm/kb.rb"
require "#{AGENT_ROOT}/../cm/cirrocumulus.rb"
require "#{AGENT_ROOT}/vps_start_saga.rb"
require "#{AGENT_ROOT}/vps_stop_saga.rb"
require "#{AGENT_ROOT}/vps_restart_saga.rb"
require "#{AGENT_ROOT}/query_vps_status_saga.rb"
require "#{AGENT_ROOT}/disk_attach_saga.rb"
require "#{AGENT_ROOT}/disk_detach_saga.rb"
require "#{AGENT_ROOT}/storage_disk_history.rb"
require "#{AGENT_ROOT}/storage_disk.rb"
require "#{AGENT_ROOT}/vps_configuration_history.rb"
require "#{AGENT_ROOT}/vps_configuration.rb"
require "#{AGENT_ROOT}/vps_state.rb"
require "#{AGENT_ROOT}/storage_disks_vps_configuration.rb"

#
# VPS agent.
# actions:
#  start_vps, stop_vps, vps_status
#

class VpsAgent < Agent
  def initialize(cm)
    ActiveRecord::Base.establish_connection(
      :adapter => 'mysql',
      :host => '172.16.11.5',
      :username => 'o1host',
      :password => 'o1h0st',
      :database => 'o1_panel',
      :encoding => 'utf8'
    )

    @cm = cm
    @sagas = []
    @saga_idx = 0
    @default_ontology = 'cirrocumulus-vps'
    Log4r::Logger['agent'].info 'initialized VpsAgent'
  end
  
  def handles_ontology?(ontology)
    super(ontology) || ontology == 'cirrocumulus-xen'
  end
  
  def tick()
    @sagas.each do |saga|
      next if saga.is_finished?
      saga.timeout -= 1 if saga.timeout > 0
      saga.handle(nil) if saga.timeout == 0
    end
  end

  def handle(message, kb)
    #p message
    
    was_processed = false

    @sagas.each do |saga|
      next if saga.is_finished?
      
      if saga.id == message.in_reply_to
        was_processed = true
        saga.handle(message)
      end
    end

    return if was_processed

    case message.act
      when 'request' then
        action = message.content.first
        vps_id = message.content.second.second
        
        if action == :start_vps
          start_saga(vps_id, message.context)
        elsif action == :stop_vps
          start_stop_saga(vps_id, message.context)
        elsif action == :restart_vps
          start_restart_saga(vps_id, message.context)
        elsif action == :attach_disk
          vps_id = disk_number = block_device = nil
          params = message.content
          params.each do |par|
            next unless par.is_a? Array
            vps_id = par.second.to_i if par.first == :vps_id
            disk_number = par.second.to_i if par.first == :disk_number
            block_device = par.second if par.first == :block_device
          end
          
          start_disk_attach_saga(vps_id, disk_number, block_device, message.context)
        elsif action == :detach_disk
          vps_id = block_device = nil
          params = message.content
          params.each do |par|
            next unless par.is_a? Array
            vps_id = par.second.to_i if par.first == :vps_id
            block_device = par.second if par.first == :block_device
          end

          start_disk_detach_saga(vps_id, block_device, message.context)
        end

      when 'query-ref' then
        query = message.content
        if query.first == :vps_state
          vps_id = query.second.second
          start_query_status_saga(vps_id, message.context)
        end
    end
  end

  private
  
  def start_disk_attach_saga(vps_id, disk_number, block_device, context)
    @saga_idx += 1
    id = "disk-attach-#{@saga_idx}"
    saga = DiskAttachSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, disk_number, block_device, context)
  end

  def start_disk_detach_saga(vps_id, block_device, context)
    @saga_idx += 1
    id = "disk-detach-#{@saga_idx}"
    saga = DiskDetachSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, block_device, context)
  end

  def start_saga(vps_id, context)
    @saga_idx += 1
    id = "vps-start-#{@saga_idx}"
    saga = VpsStartSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, context)
  end

  def start_stop_saga(vps_id, context)
    @saga_idx += 1
    id = "vps-stop-#{@saga_idx}"
    saga = VpsStopSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, context)
  end
  
  def start_restart_saga(vps_id, context)
    @saga_idx += 1
    id = "vps-restart-#{@saga_idx}"
    saga = VpsRestartSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, context)
  end
  
  def start_query_status_saga(vps_id, context)
    @saga_idx += 1
    id = "vps-query-state-#{@saga_idx}"
    saga = QueryVpsStatusSaga.new(id, @cm, self)
    @sagas << saga
    saga.start(vps_id, context)
  end
end

# <fipa-message act="request" ontology="cirrocumulus-vps"><content>start_vps (id 153)</content></fipa-message>

cm = Cirrocumulus.new('vps')
cm.run(VpsAgent.new(cm), Kb.new)