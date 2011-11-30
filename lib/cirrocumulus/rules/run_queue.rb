require 'thread'

module RuleEngine
  class QueueEntry
    attr_reader :rule
    attr_reader :params
    attr_reader :run_at
    attr_accessor :state

    def initialize(rule, params, run_at = nil)
      @rule = rule
      @params = params
      @run_at = run_at
      @state = :queued
    end
  end

  class RunQueue
    def initialize(engine)
      @engine = engine
      @queue = []
      @running_rules = []
      @mutex = Mutex.new
    end

    def enqueue(rule, params)
      @mutex.synchronize do
        if !already_queued?(rule, params)
          debug "Enqueueing rule #{rule.name}#{params.inspect}"

          if !rule.options.blank? && rule.options.include?(:for)
            delay = rule.options[:for]
            run_at = Time.now + delay
            debug "Will run this rule after #{delay.to_s} sec (at #{run_at.strftime("%H:%M:%S %d.%m.%Y")})"
            @queue << QueueEntry.new(rule, params, run_at)
          else
            @queue << QueueEntry.new(rule, params)
          end
        end
      end
    end

    def run_queued_rules()
      rules_for_running = Queue.new
      @mutex.synchronize do
        count = @queue.count {|entry| entry.state == :queued}

        if count > 0
          debug "There are #{count} rules waiting, running.." if count > 1

          idx = 0
          while count > 0
            entry = @queue[idx]

            if entry.state != :queued
              idx += 1
              next
            end

            if entry.run_at.blank? || (entry.run_at <= Time.now)
              rules_for_running << entry
              entry.state = :running
            end

            count -= 1
          end
        end
      end

      while !rules_for_running.empty? do
        entry = rules_for_running.pop
        begin
          debug "Executing #{entry.rule.name}#{entry.params.inspect}"
          entry.rule.code.call(@engine, entry.params)
          entry.state = :finished # TODO: cleanup @queue from this entries
        rescue Exception => e
          Log4r::Logger['kb'].warn "Exception while executing rule: %s\n%s" % [e.to_s, e.backtrace.to_s]
        end
      end
    end

    private

    def already_queued?(rule, params)
      return false if @queue.empty?

      @queue.each do |queued_rule|
        next if queued_rule.rule.name != rule.name
        return true if params === queued_rule.params && queued_rule.state != :finished
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
