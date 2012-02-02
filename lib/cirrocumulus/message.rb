module Cirrocumulus
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
end
