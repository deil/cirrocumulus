module RuleEngine
  class Base
    def self.rule(name, args, &block)
      @@loaded_rules << {:name => name, :args => args, :body => block}
    end

    def assert(fact)
      puts "assert: #{fact.inspect}"
      @facts = [] if @facts.nil?
      @facts << fact
      process()
    end

    def retract(fact)
      @facts = [] if @facts.nil?
      if @facts.delete(fact)
        process()
      else
        puts "fact #{fact.inspect} not found"
      end
    end

    protected

    @@loaded_rules = []

    def matches?(rule)
      rule[:args].each do |arg|
        if @facts.include?(arg)
          return true
        end
      end

      false
    end

    def execute(rule)
      puts "executing rule: #{rule[:name]}"
      rule[:body].call(self)
    end

    def process()
      @@loaded_rules.each do |rule|
        execute(rule) if matches?(rule)
      end
    end

  end
end
