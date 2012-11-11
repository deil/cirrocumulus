require 'xmpp4r'
require 'xmpp4r-simple'
require 'guid'
require 'sexpistol'
require_relative 'message_bus.rb'

module Cirrocumulus
	class JabberBus < MessageBus
	  attr_reader :jid

	  def initialize
	    @send_queue = Queue.new
	    Log4r::Logger['bus'].info 'will use Jabber for agent communications'
	  end

	  def connected?
	    @jabber && @jabber.connected?
	  end

	  def connect(identifier)
		  @jid = identifier
	    Log4r::Logger['bus'].info "logging to #{JABBER_SERVER} as #{jid}"

	    begin
	      full_jid = jid + "@" + JABBER_SERVER
	      @jabber = Jabber::Simple.new(full_jid, JABBER_DEFAULT_PASSWORD)
	    rescue Errno::ECONNREFUSED => ex
	      Log4r::Logger['bus'].warn 'connection refused' and return false
	    rescue Jabber::ClientAuthenticationFailure => ex
	      Log4r::Logger['bus'].warn 'got Jabber::ClientAuthenticationFailure'
	      Log4r::Logger['bus'].info 'using default password to register new account'

	      client = Jabber::Client.new(full_jid)
	      client.connect()
	      client.register(JABBER_DEFAULT_PASSWORD) #, {'username' => full_jid, 'password' => JABBER_DEFAULT_PASSWORD})
	      client.close()
	      @jabber = Jabber::Simple.new(full_jid, JABBER_DEFAULT_PASSWORD)
	    end

	    if connected?
	      Log4r::Logger['bus'].info 'joining ' + JABBER_CONFERENCE
	      @jabber.send!("<presence to='#{JABBER_CONFERENCE}/#{@jid}' />")
	      true
	    else
	      false
	    end
	  rescue
	    false
	  end

	  def start(agent)
	    thrd = Thread.new do
	      s = Sexpistol.new
	      while true do
	        begin
	          if connected?
	            @jabber.received_messages do |message|
	              # <fipa-message ontology="cirrocumulus-cloud" act="inform" receiver="sapco-cloud"><content>q</content></fipa-message>
	              next unless message.x('jabber:x:delay').nil?

	              begin
	                xml = Hash.from_xml(message.body)['fipa_message']
	                content_raw = xml['content']
	                content = s.parse_string(content_raw)
	                acl = Cirrocumulus::Message.new(message.from.resource, xml['act'], content)
	                acl.receiver = xml['receiver']
	                acl.ontology = xml['ontology']
	                acl.reply_with = xml['reply_with']
	                acl.in_reply_to = xml['in_reply_to']
	                acl.conversation_id = xml['conversation_id']
	                flatten_message_content(acl)

	                #Log4r::Logger['bus'].debug(acl.inspect)
	                agent.process_incoming_message(acl)
	              rescue Exception => ex
	                p ex
	                p ex.backtrace
	              end
	            end

	            while true do
	              message = @send_queue.pop(true) rescue nil
	              break if message.nil?

	              send_message_actual(message)
	            end
	          else
	            sleep 5 unless connect
	          end

	          sleep 0.01
	        rescue Exception => ex
	          puts ex.to_s
	          puts ex.backtrace.to_s
	        end
	      end
	    end
	  end

	  def send_message(message)
	    @send_queue << message
	  end

	  private

	  def flatten_message_content(message)
	    if !message.content.is_a?(Array)
	      message.content = [message.content]
	    else
	      while message.content.is_a?(Array) && message.content.size == 1 && message.content.first.is_a?(Array)
	        message.content = message.content.first
	      end
	    end
	  end

	  def send_message_actual(message)
	    message_content = message.content if message.content.is_a?(String)
	    message_content = Sexpistol.new.to_sexp(message.content) if message.content.is_a?(Array)

	    text = '<fipa-message ontology="%s"' % message.ontology
	    text += ' receiver="%s"' % message.receiver if message.receiver
	    text += ' act="%s"' % message.act
	    text += ' reply-with="%s"' % message.reply_with if message.reply_with
	    text += ' in-reply-to="%s"' % message.in_reply_to if message.in_reply_to
	    text += ' conversation-id="%s"' % message.conversation_id if message.conversation_id
	    text += '><content>%s</content></fipa-message>' % message_content
	    @jabber.send!('<message type="groupchat" to="%s" id="%s"><body>%s</body></message>' % [
	        JABBER_CONFERENCE,
	        Guid.new.to_s.gsub('-', ''),
	        text.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;').gsub('"', '&quot;')
	    ])

	    true
	  rescue Exception => ex
	    puts ex.to_s
	    puts ex.backtrace.to_s
	    false
	  end
	end
end
