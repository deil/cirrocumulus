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

  def initialize(ontology_instance)
    @mutex = Mutex.new
    @queue = []
    @ontology_instance = ontology_instance
  end

  def push(entry)
    @mutex.synchronize do
      return if @queue.find {|e| e != :marker && e.state != :finished && e.run_data == entry}

      if entry.rule.options.has_key?(:for)
        delay = entry.rule.options[:for]
        run_at = Time.now + delay
        debug("Enqueued rule #{entry.rule.name}#{entry.parameters.inspect}")
        debug("And will execute this rule after #{delay} sec (at #{run_at.strftime("%H:%M:%S %d.%m.%Y")})")
        @queue.push(QueueEntry.new(entry, run_at))
      else
        @queue.push(QueueEntry.new(entry))
      end
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
    @queue.push :marker

    while (entry = pop) != :marker do
      next if entry.state == :finished

      if !entry.run_at.nil? && (entry.run_at > Time.now)
        @queue.push(entry)
        next
      end

      if entry.run_data.matched_facts.all? {|fact| !fact.is_deleted}
        begin
          debug "Executing #{entry.rule.name}#{entry.params.inspect}"
          entry.rule.code.call(@ontology_instance, entry.params)
          entry.state
        rescue Exception => e
          warn "Exception while executing rule: %s\n%s" % [e.to_s, e.backtrace.to_s]
        end
      end
    end
  end

  private

  def debug(msg)
    Log4r::Logger['ontology::run_queue'].debug(msg)
  end

  def warn(msg)
    Log4r::Logger['ontology::run_queue'].warn(msg)
  end

end
