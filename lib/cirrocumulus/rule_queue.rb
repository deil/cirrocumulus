require 'thread'

class RuleQueue
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

  def initialize
    @mutex = Mutex.new
    @queue = []
  end

  def push(entry)
    @mutex.synchronize do
      @queue.push(QueueEntry.new(entry)) unless @queue.find {|e| e.state != :finished && e.run_data == entry}
    end
  end

  def pop
    @mutex.synchronize do
      @queue.empty? ? nil : @queue.shift
    end
  end

  def size
    @mutex.synchronize do
      @queue.size
    end
  end

  def run_queued_rules
    while (entry = pop) != nil do
      next if entry.state == :finished
      if entry.run_at && (entry.run_at < Time.now)
        push(entry)
        next
      end

      if entry.run_data.matched_facts.all? {|fact| fact.is_deleted}
        begin
          debug "Executing #{entry.rule.name}#{entry.params.inspect}"
          entry.rule.code.call(self, entry.params)
          entry.state
        rescue Exception => e
          puts "[WARN] Exception while executing rule: %s\n%s" % [e.to_s, e.backtrace.to_s]
        end
      end
    end
  end
end
