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
require "#{AGENT_ROOT}/api_request.rb"

#
# VPS agent.
# actions:
#  start_vps, stop_vps, vps_status
#

class ApiAgent < Agent
  def initialize(cm)
    Time.zone = "UTC"
    ActiveRecord::Base.time_zone_aware_attributes = true
    ActiveRecord::Base.default_timezone = "UTC"

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
    @default_ontology = 'cirrocumulus-api'
    Log4r::Logger['agent'].info 'initialized ApiAgent'
  end
  
  def handles_ontology?(ontology)
    super(ontology) || ontology == 'cirrocumulus-xen' || ontology == 'cirrocumulus-vps'
  end
  
  def tick()
    ActiveRecord::Base.transaction do
      requests = ApiRequest.all(:conditions => {:is_finished => false, :is_sent => false})
      requests.each do |request|
        p request
        msg = Cirrocumulus::Message.new(@jid, request.action, request.content)
        msg.ontology = request.ontology
        msg.reply_with = "api-request-" + request.id.to_s
        @cm.send(msg)
        request.is_sent = true
        request.save
      end
    end
  end

  def handle(message, kb)
    if message && message.in_reply_to
      request_id = message.in_reply_to.gsub('api-request-', '').to_i
      if ApiRequest.exists? request_id
        ActiveRecord::Base.transaction do
          request = ApiRequest.find(request_id)
          request.is_finished = true
          request.reply = Sexpistol.new.to_sexp(message.content)
          request.reply_action = message.act
          request.reply_agent = message.sender
          request.updated_at = Time.now
          request.save
        end
      end
    end
  end

  private

end

# <fipa-message act="request" ontology="cirrocumulus-vps"><content>start_vps (id 153)</content></fipa-message>

cm = Cirrocumulus.new('web-api', false)
cm.run(ApiAgent.new(cm), Kb.new)
