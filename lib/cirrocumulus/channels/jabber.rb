require 'xmpp4r'
require 'xmpp4r-simple'
require 'guid'
require 'thread'

class JabberIdentifier < RemoteIdentifier
  def initialize(jid)
    @jid = "#{Cirrocumulus::Environment.current.name}-#{jid}"
    @channel = JabberChannel.new('172.16.11.4', 'cirrocumulus')
    @channel.connect(@jid, 'q1w2e3r4')
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
                elsif [:reply_with, :in_reply_to, :conversation_id].include?(parameter[0])
                  options[parameter[0]] = parameter[1]
                end
              end
              
              next if receiver.nil? || receiver != @jid
              
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
                  instance.handle_refuse(id, action[0], action[1], options)
                when :failure
                  instance.handle_failure(id, action[0], action[1], options)
              end
            rescue Exception => ex
              puts ex.message
              puts ex.backtrace.to_s
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
  end

  attr_reader :jid
  attr_reader :conference

  def initialize(server, conference)
    @jabber = nil
    @server = server
    @conference = conference
    @send_q = Queue.new
    @recv_q = Queue.new
    
    @@jabber_clients << self
  end
  
  def connected?
    @jabber && @jabber.connected?
  end
  
  def connect(jid, password)
    @full_jid = "%s@%s" % [jid, @server]
    @jid = jid

    puts "Using jid #{@jid}"
    
    begin
      @jabber = Jabber::Simple.new(@full_jid, password)
    rescue Jabber::ClientAuthenticationFailure => ex
      puts ex.class.name
      client = Jabber::Client.new(@full_jid)
      client.connect()
      client.register(password)
      client.close()
      @jabber = Jabber::Simple.new(@full_jid, password)
    rescue Exception => ex
      puts ex.to_s
    end
    
    join_conference(@conference) if connected?
    connected?
  end
  
  def disconnect()
    @jabber.disconnect()
  end
  
  def queue(msg)
    @send_q << msg
  end
  
  def received_messages
    @recv_q
  end
  
  def tick()
    return if !connected?
    
    @jabber.received_messages do |msg|
     next unless msg.x('jabber:x:delay').nil?
     
     @recv_q << msg
    end
    
    while true do
      to_send = @send_q.pop(true) rescue nil
      break if to_send.nil?
      
      @jabber.send!('<message type="groupchat" to="%s" id="%s"><body>%s</body></message>' % [                                 
        "%s@conference.%s" % [@conference, @server], Guid.new.to_s.gsub('-', ''),                                                                                         
        to_send.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;').gsub('"', '&quot;')                                     
      ]) 
    end
  end
  
  protected
  
  def join_conference(conference)
    @jabber.send!("<presence to='#{conference}@conference.#{@server}/#{@jid}' />")
  end
end
