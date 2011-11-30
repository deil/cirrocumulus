require 'systemu'
require 'thread'

class Cirrocumulus
  # Message. Used by agents to communicate
  class Message
    attr_accessor :sender, :act, :content
    attr_accessor :receiver, :reply_with, :in_reply_to, :conversation_id
    attr_accessor :ontology

    def initialize(sender, act, content)
      @sender = sender
      @act = act
      @content = content
    end

    def failed?
      act == 'failure' || act == 'refuse'
    end

    def context
      Context.new(@sender, @reply_with)
    end

    def self.parse_params(content, subroutine = false)                                                                            
      return parse_params(content.size == 1 ? content[0] : content, true)  if !subroutine                                         

      return [] if content.nil?                                                                                                   
      return content if !content.is_a?(Array)                                                                                     
      return [] if content.size == 0                                                                                              
      return {content[0] => []} if content.size == 1                                                                              
      return {content[0] => parse_params(content[1], true)} if content.size == 2                                                  
                       
      res = {content[0] => []}                                                                                                    

      if content.all? {|item| !item.is_a?(Array)}                                                                                 
        content.each_with_index do |item,i|                                                                                       
          if i == 0                                                                                                               
            res[content[0]] = []                                                                                                  
          else                                                                                                                    
            res[content[0]] << item                                                                                               
          end                                                                                                                     
        end                                                                                                                       
      else                                                                                                                        
        content.each_with_index do |item,i|                                                                                       
          if i == 0                                                                                                               
            res[content[0]] = {}                                                                                                  
          else                                                                                                                    
            res[content[0]].merge!(parse_params(item, true))                                                                      
          end                                                                                                        
        end
      end                                                                                                            

      res                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    end                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
  end

  # Message context. Includes sender, reply-with and conversation-id
  class Context
    attr_reader :sender
    attr_reader :reply_with
    attr_reader :conversation_id

    def initialize(sender, reply_with, conversation_id = nil)
      @sender = sender
      @reply_with = reply_with
      @conversation_id = conversation_id
    end
  end

  # Gets current platform (linux, freebsd, etc)
  def self.platform
    if RUBY_PLATFORM =~ /freebsd/
      return 'freebsd'
    elsif RUBY_PLATFORM =~ /linux/
      return 'linux'
    end

    return 'unknown'
  end

  attr_reader :jid

  def initialize(suffix, generate_jid = true)
    Log4r::Logger['cirrocumulus'].info 'platform: ' + Cirrocumulus::platform
    @suffix = suffix
    @generate_jid = generate_jid
    @send_queue = Queue.new
  end

  # Sends message to other agents
  def send(message)
    @send_queue << message
    msg = "<fipa-message ontology=\"#{message.ontology}\""
    msg += " receiver=\"#{message.receiver}\"" if message.receiver
    msg += " act=\"#{message.act}\""
    msg += " reply-with=\"#{message.reply_with}\"" if message.reply_with
    msg += " in-reply-to=\"#{message.in_reply_to}\"" if message.in_reply_to
    msg += " conversation-id=\"#{message.conversation_id}\"" if message.conversation_id
    message_content = message.content if message.content.is_a?(String)
    message_content = Sexpistol.new.to_sexp(message.content) if message.content.is_a? Array
    msg += "><content>#{message_content}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end

=begin
  def inform(receiver, answer, ontology = nil)
    msg = "<fipa-message ontology=\"#{ontology || @ontology}\" receiver=\"#{receiver}\" act=\"inform\"><content>#{answer}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end

  def refuse(receiver, action, reason, ontology = nil)
    msg = "<fipa-message ontology=\"#{ontology || @ontology}\" receiver=\"#{receiver}\" act=\"refuse\"><content>(#{action} #{reason})</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end

  def failure(receiver, action, ontology = nil)
    msg = "<fipa-message ontology=\"#{ontology || @ontology}\" receiver=\"#{receiver}\" act=\"failure\"><content>#{action}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end

  def request(receiver, action, ontology = nil)
    msg = "<fipa-message ontology=\"#{ontology || @ontology}\" receiver=\"#{receiver}\" act=\"request\"><content>#{action}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end
=end

  # Main loop. Connects to other agents and processes all incoming messages
  def run(agent, kb, sniff = false)
    connect(generate_jid(agent))

    Log4r::Logger['cirrocumulus'].info("entering main loop")
    agent.restore_state()

    s = Sexpistol.new
    should_be_running = true

    while should_be_running do
      kb.collect_knowledge()

      begin
        connect(generate_jid(agent)) if !connected?
      rescue Exception => e
        puts e.to_s
        sleep 5
      end

      next if !connected?

      # process incoming messages from queue
      @im.received_messages do |message|
        process_incoming_message(agent, kb, message, s, sniff)
      end

      # pass execution to agent instance
      agent.tick()
      sleep 0.5
    end

    @im.disconnect
  end

  private

  # Flatterns incoming message if is bounded into multiple arrays
  def flatten_message_content(message)
    if !message.content.is_a?(Array)
      message.content = [message.content]
    else
      while message.content.is_a?(Array) && message.content.size == 1 && message.content.first.is_a?(Array)
        message.content = message.content.first
      end
    end
  end

  # Generates JID for self identification
  def generate_jid(agent)
    suffix = agent.default_ontology ? agent.default_ontology.gsub('cirrocumulus-', '') : @suffix
    if @generate_jid
      _, hostname = systemu('hostname')
      hostname.strip!
      "%s-%s" % [hostname, suffix]
    else
      suffix
    end
  end

  # Checks if agent is connected to Jabber conference
  def connected?
    @im && @im.connected?
  end

  # Connects to Jabber server and joins the conference
  def connect(jid)
    @jid = jid
    Log4r::Logger['cirrocumulus'].info "logging as " + @jid

    begin
      @im.disconnect if @im && @im.connected?
      full_jid = @jid + "@" + JABBER_SERVER
      @im = Jabber::Simple.new(full_jid, JABBER_DEFAULT_PASSWORD)
    rescue Jabber::ClientAuthenticationFailure => ex
      Log4r::Logger['cirrocumulus'].warn ex.to_s
      Log4r::Logger['cirrocumulus'].info "registering new account with default password"

      client = Jabber::Client.new(full_jid)
      client.connect()
      client.register(JABBER_DEFAULT_PASSWORD) #, {'username' => full_jid, 'password' => JABBER_DEFAULT_PASSWORD})
      client.close()
      @im = Jabber::Simple.new(full_jid, JABBER_DEFAULT_PASSWORD)
    rescue Exception => ex
      puts ex
      #puts ex.backtrace.to_s
    end

    if !@im.nil? && @im.connected?
      Log4r::Logger['cirrocumulus'].info 'joining ' + JABBER_CONFERENCE
      @im.send!("<presence to='#{JABBER_CONFERENCE}/#{@jid}' />")
    end
  end

  def process_incoming_message(agent, kb, message, s, sniff)
    return if !message.x('jabber:x:delay').nil?

    begin
      xml = Hash.from_xml(message.body)['fipa_message']

      ontology = xml['ontology']
      sender = message.from.resource
      receiver = xml['receiver']
      supported_ontology = agent.handles_ontology?(ontology)

      if (supported_ontology && (receiver == @jid || receiver.blank?)) || receiver == @jid || sniff
        content_raw = xml['content']
        content = s.parse_string(content_raw)
        msg = Cirrocumulus::Message.new(sender, xml['act'], content)
        msg.receiver = receiver
        msg.ontology = ontology
        msg.reply_with = xml['reply_with']
        msg.in_reply_to = xml['in_reply_to']
        msg.conversation_id = xml['conversation_id']

        flatten_message_content(msg)
        Log4r::Logger['cirrocumulus'].debug(msg.inspect)
        agent.handle_message(msg, kb)
      else # ignore message
        #Log4r::Logger['cirrocumulus'].debug("unhandled ontology: %s" % [ontology])
      end
    rescue Exception => e # exception while parsing; possibly it is not XML (humans speaking!)
      Log4r::Logger['cirrocumulus'].error("%s\n%s" % e.to_s, e.backtrace)
    end
  end

  # Processes send queue and sends all pending messages
  def process_send_queue()
    while !@send_queue.empty? do
      message = @send_queue.pop
      !actual_send(message)
    end
  end

  # Sends message to Jabber conference
  def actual_send(message)
    msg = "<fipa-message ontology=\"#{message.ontology}\""
    msg += " receiver=\"#{message.receiver}\"" if message.receiver
    msg += " act=\"#{message.act}\""
    msg += " reply-with=\"#{message.reply_with}\"" if message.reply_with
    msg += " in-reply-to=\"#{message.in_reply_to}\"" if message.in_reply_to
    msg += " conversation-id=\"#{message.conversation_id}\"" if message.conversation_id
    message_content = message.content if message.content.is_a?(String)
    message_content = Sexpistol.new.to_sexp(message.content) if message.content.is_a? Array
    msg += "><content>#{message_content}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
    true
  rescue
    false
  end

end
