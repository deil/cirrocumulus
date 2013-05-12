require 'xmpp4r'
require 'guid'
require 'thread'

if RUBY_VERSION < "1.9"
# ...
else
    # Encoding patch
    require 'socket'
    class TCPSocket
        def external_encoding
            Encoding::BINARY
        end
    end

    require 'rexml/source'
    class REXML::IOSource
        alias_method :encoding_assign, :encoding=
        def encoding=(value)
            encoding_assign(value) if value
        end
    end

    begin
        # OpenSSL is optional and can be missing
        require 'openssl'
        class OpenSSL::SSL::SSLSocket
            def external_encoding
                Encoding::BINARY
            end
        end
    rescue
    end
end

class JabberIdentifier < RemoteIdentifier
  def initialize(jid)
    @jid = "#{Cirrocumulus::Environment.current.name}-#{jid}"
    @channel = JabberChannel.new()
    @channel.connect(@jid)
    @thrd = Thread.new do
      parser = Sexpistol.new
      while true do
        @channel.tick()
        
        instance = Ontology.query_ontology_instance(self)
        if !instance.nil?
          while true
            msg = @channel.received_messages.pop(true) rescue nil
            break if msg.nil?

            begin
              fipa_message = parser.parse_string(msg.body)
              id = RemoteIdentifier.new(msg.from.resource)
              
              next if fipa_message.size < 1
              
              fipa_message = fipa_message.first
              
              next if fipa_message.size < 2
              
              act = fipa_message[0]
              fipa_message.delete_at(0)
              content = fipa_message
              
              receiver = nil
              action_content = nil
              options = {}
              content.each do |parameter|
                next if !parameter.is_a?(Array) || parameter.size < 1

                if parameter[0] == :receiver
                  receiver = parameter[1][2]
                elsif parameter[0] == :content
                  action_content = parameter[1]
                elsif [:ontology, :reply_with, :in_reply_to, :conversation_id].include?(parameter[0])
                  options[parameter[0]] = parameter[1]
                elsif parameter[0] == :reply_to

                end
              end

              next if options.has_key?(:ontology) && options[:ontology] != instance.name
              next if !options.has_key?(:ontology) && (receiver.nil? || receiver != @jid)
              
              case act
                when :query
                  instance.handle_query(id, action_content, options)
                when :query_if
                  instance.handle_query_if(id, action_content, options)
                when :inform
                  instance.handle_inform(id, action_content, options)
                when :request
                  instance.handle_request(id, action_content, options)
                when :agree
                  instance.handle_agree(id, action_content, options)
                when :refuse
                  instance.handle_refuse(id, action_content[0], action_content[1], options)
                when :failure
                  instance.handle_failure(id, action_content[0], action_content[1], options)
              end
            rescue Exception => ex
              Log4r::Logger['channels'].warn('Failed to process incoming message')
              Log4r::Logger['channels'].debug("Message body: #{msg.body}")
              Log4r::Logger['channels'].debug("Exception: #{ex.message}")
              Log4r::Logger['channels'].debug("Backtrace: #{ex.backtrace.join("\n")}")
            end
          end
        end
        sleep 0.1
      end
    end
  end
  
  def join
    @thrd.join
  end

  def to_s
    @jid
  end
end

class JabberChannel
  class << self
    @@jabber_clients = []
    
    def query_client(jid)
      @@jabber_clients.find {|c| c.jid == jid}
    end

    def server(server)
      @@server = server
    end

    def password(pass)
      @@password = pass
    end

    def conference(conf)
      @@conference = conf
    end
  end

  attr_reader :jid
  attr_reader :conference

  def initialize(server = nil, conference = nil)
    logger.info 'initializing new channel'

    @jabber = nil
    @server = server || @@server
    @conference = conference || @@conference
    @send_q = Queue.new
    @recv_q = Queue.new

    logger.info "server = #{@server}"
    logger.info "conference = #{@conference}"

    @@jabber_clients << self
  end
  
  def connected?
    @jabber
  end
  
  def connect(jid)
    Jabber::debug = true

    @should_be_connected = true
    @full_jid = "%s@%s" % [jid, @server]
    @jid = jid

    logger.info "connecting as #{@jid}"

    begin
      @jabber = Jabber::Client.new(@full_jid + '/cirrocumulus')
      @jabber.connect
      @jabber.auth(@@password)
    rescue Jabber::ClientAuthenticationFailure => ex
      @jabber.close

      logger.debug 'received Jabber::ClientAuthenticationFailure, attempting to register new account'
      client = Jabber::Client.new(@full_jid)
      client.connect()
      client.register(@@password)
      client.close()

      @jabber = Jabber::Client.new(@full_jid + '/cirrocumulus')
      @jabber.connect
      @jabber.auth(@@password)
    rescue Exception => ex
      Log4r::Logger['channels::jabber'].fatal('Failed to register new account or connect.')
      Log4r::Logger['channels::jabber'].fatal("Received exception: #{ex.to_s}")
      @jabber = nil

      return false
    end

    @jabber.send(Jabber::Presence.new.set_type(:available))

    @jabber.add_message_callback do |message|
      if message.first_element('delay').nil? && !message.body.nil?
        logger.debug(message.to_s)
        @recv_q << message
      end
    end

    @jabber.add_iq_callback do |iq_received|
      p iq_received
      if iq_received.type == :get
        if iq_received.queryns.to_s != 'http://jabber.org/protocol/disco#info'
          iq = Jabber::Iq.new(:result, @jabber.jid.node)
          iq.id = iq_received.id
          iq.from = iq_received.to
          iq.to = iq_received.from
          p iq
          @jabber.send(iq)
        end
      end
    end

    join_conference(@conference) if connected?
    connected?
  end

  def reconnect
    return unless @should_be_connected

    @jabber.reconnect
    #@jabber = Jabber::Simple.new(@full_jid, @@password)
    join_conference(@conference)

    true
  rescue Exception => ex
    logger.warn 'got exception during reconnect'
    logger.warn ex.to_s
    logger.warn ex.backtrace.to_s

    false
  end

  def disconnect
    @should_be_connected = false
    logger.info 'disconnecting'
    @jabber.close
  end
  
  def queue(msg)
    @send_q << msg
  end
  
  def received_messages
    @recv_q
  end
  
  def tick()
    return if !@should_be_connected

    if !connected?
      reconnect
      return
    end

    while true do
      to_send = @send_q.pop(true) rescue nil
      break if to_send.nil?

      msg = '<message type="groupchat" to="%s" id="%s"><body>%s</body></message>' % [
        "%s@conference.%s" % [@conference, @server], Guid.new.to_s.gsub('-', ''),
        to_send.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;').gsub('"', '&quot;')
      ]
      logger.debug(msg)

      @jabber.send(msg)
    end
  rescue Exception => ex
    logger.warn 'got exception in tick():'
    logger.warn ex.to_s
    logger.warn ex.backtrace.to_s
  end
  
  protected
  
  def join_conference(conference)
    logger.info "joining conference #{conference}"
    msg = "<presence to='#{conference}@conference.#{@server}/#{@jid}' />"
    logger.debug(msg)
    @jabber.send(msg)
  end

  def logger
    Log4r::Logger['channels::jabber']
  end
end
