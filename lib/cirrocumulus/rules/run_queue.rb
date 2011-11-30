require 'thread'

module RuleEngine
  class QueueEntry
    attr_reader :rule
    attr_reader :params
    attr_reader :run_at

    def initialize(rule, params, run_at = nil)
      @rule = rule
      @params = params
      @run_at = run_at
    end
  end

  class RunQueue
    def initialize(engine)
      @engine = engine
      @queue = Queue.new
      @queue_tmp = []
    end

    def enqueue(rule, params)
      if !already_queued?(rule, params)
        if !rule.options.blank? && rule.options.include?(:for)
          delay = rule.options[:for]
          run_at = Time.now + delay
          debug "Will run this rule after #{delay.to_s} sec (at #{run_at.strftime("%H:%M:%S %d.%m.%Y")})"
          @queue.push QueueEntry.new(rule, params, run_at)
          @queue_tmp << QueueEntry.new(rule, params, run_at)
        else
          @queue.push QueueEntry.new(rule, params)
          @queue_tmp << QueueEntry.new(rule, params)
        end
      end
    end

    def run_queued_rules()
      if @queue.size > 0
        debug "There are #{@queue.size} rules waiting, running.." if @queue.size > 1

        @queue_tmp.clear()
        idx = @queue.size

        while idx > 0
          entry = @queue.pop

          if entry.run_at.blank? || (entry.run_at <= Time.now)
            rule = entry.rule
            params = entry.params

            begin
              debug "Executing #{rule.name}#{params.inspect}"
              rule.code.call(@engine, params)
            rescue; end
          else
            @queue << entry
            @queue_tmp << entry
          end

          idx -= 1
        end

        @queue_tmp.clear()
      end
    end

    private

    def already_queued?(rule, params)
      return false if @queue.empty?

      @queue_tmp.each do |queued_rule|
        next if queued_rule.rule.name != rule.name
        return true if params === queued_rule.params
      end

      false
    end

    def log(msg)
      Log4r::Logger['kb'].info(msg)
    rescue
      puts "[INFO] %s" % msg
    end

    def debug(msg)
      Log4r::Logger['kb'].debug(msg)
    rescue
      puts "[DEBUG] %s" % msg
    end

  end
end
