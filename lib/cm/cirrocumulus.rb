require 'xmpp4r'
require 'xmpp4r-simple'
require 'sexpistol'
require File.join(AGENT_ROOT, "../cm/jabber_config.rb")

class Cirrocumulus
  class Message
    attr_accessor :sender, :act, :content
    attr_accessor :receiver, :reply_with, :in_reply_to
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
  end

  class Context
    attr_reader :sender
    attr_reader :reply_with

    def initialize(sender, reply_with)
      @sender = sender
      @reply_with = reply_with
    end
  end

  def self.platform
    if PLATFORM =~ /freebsd/
      return 'freebsd'
    elsif PLATFORM =~ /linux/
      return 'linux'
    end

    return 'unknown'
  end

  def initialize(suffix, generate_jid = true)
    Log4r::Logger['cirrocumulus'].info 'platform: ' + Cirrocumulus::platform
    _, hostname = systemu 'hostname'
    hostname.strip!
    @jid = generate_jid ? "#{hostname}-#{suffix}" : suffix
    Log4r::Logger['cirrocumulus'].info "logging as " + @jid
    @im = Jabber::Simple.new("#{@jid}@#{JABBER_SERVER}", JABBER_DEFAULT_PASSWORD)
    Log4r::Logger['cirrocumulus'].info 'joining ' + JABBER_CONFERENCE
    @im.send!("<presence to='#{JABBER_CONFERENCE}/#{@jid}' />")
  end
  
  def send(message)
    msg = "<fipa-message ontology=\"#{message.ontology}\""
    msg += " receiver=\"#{message.receiver}\"" if message.receiver
    msg += " act=\"#{message.act}\""
    msg += " reply-with=\"#{message.reply_with}\"" if message.reply_with
    msg += " in-reply-to=\"#{message.in_reply_to}\"" if message.in_reply_to
    message_content = message.content if message.content.is_a? String
    message_content = Sexpistol.new.to_sexp(message.content) if message.content.is_a? Array
    msg += "><content>#{message_content}</content></fipa-message>"
    @im.send!("<message type=\"groupchat\" to=\"#{JABBER_CONFERENCE}\" id=\"aaefa\"><body>#{msg.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
  end

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

  def run(agent, kb, sniff = false)
    s = Sexpistol.new

    loop do
      kb.collect_knowledge()

      @im.received_messages do |message|
        next if !message.x('jabber:x:delay').nil?

        begin
          xml = Hash.from_xml(message.body)['fipa_message']
 
          ontology = xml['ontology']
          sender = message.from.resource
          receiver = xml['receiver']
          if (agent.handles_ontology? ontology)
            if sniff || receiver.nil? || receiver == '' || receiver == @jid
              act = xml['act']
              content_raw = xml['content']
              content = s.parse_string(content_raw)
              content = content.first if content.size() == 1
              msg = Cirrocumulus::Message.new(sender, act, content)
              msg.receiver = receiver
              msg.reply_with = xml['reply_with']
              msg.in_reply_to = xml['in_reply_to']
              agent.handle(msg, kb)
            end
          elsif receiver == @jid
            Log4r::Logger['cirrocumulus'].warn "received message with unknown ontology=#{ontology}"
          end
        rescue Exception => e
          #puts e.to_s
          #puts e.backtrace
        end
      end

      agent.tick()
      sleep 0.5
    end

    @im.disconnect
  end
end
