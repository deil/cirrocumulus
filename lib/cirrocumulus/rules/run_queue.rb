require 'thread'

module RuleEngine
  class QueueEntry
    attr_reader :run_data
    attr_reader :rule
    attr_reader :params
    attr_reader :run_at
    attr_accessor :state

    def initialize(run_data, run_at = nil)
      @run_data = run_data
      @rule = run_data.rule
      @params = run_data.parameters
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

    def enqueue(match_data)
      @mutex.synchronize do
        if !already_queued?(match_data.rule, match_data.parameters)
          debug "Enqueueing rule #{match_data.rule.name}#{match_data.parameters.inspect}"

          if !match_data.rule.options.blank? && match_data.rule.options.include?(:for)
            delay = match_data.rule.options[:for]
            run_at = Time.now + delay
            debug "Will run this rule after #{delay.to_s} sec (at #{run_at.strftime("%H:%M:%S %d.%m.%Y")})"
            @queue << QueueEntry.new(match_data, run_at)
          else
            @queue << QueueEntry.new(match_data)
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

            idx += 1
            count -= 1
          end
        end
      end

      while !rules_for_running.empty? do
        entry = rules_for_running.pop
        if entry.run_data.matched_facts.all? {|fact| !fact.is_deleted} # TODO: there should be smth like WeakReference
          begin
            debug "Executing #{entry.rule.name}#{entry.params.inspect}"
            entry.rule.code.call(@engine, entry.params)
            entry.state = :finished # TODO: cleanup @queue from this entries

          rescue Exception => e
            Log4r::Logger['kb'].warn "Exception while executing rule: %s\n%s" % [e.to_s, e.backtrace.to_s]
          end
        else
          entry.state = :finished
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
