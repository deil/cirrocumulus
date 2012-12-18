#
# Fact (piece of knowledge) representation.
# It holds time when it was observed (added to facts database) and expire time in seconds.
#
class Fact
	def initialize(data, time, options)
		@data = data
    @time = time
    @options = options
		@is_deleted = false
  end

	attr_reader :data
	attr_accessor :is_deleted

	def timed_out?
		return false if @options[:expires] == nil
		@time + @options[:expires] < Time.now
	end
end

#
# Adapter for facts database.
#
class FactsDatabase
	def initialize
		@storage = []
	end

	def add(fact, options = {})
		@storage << Fact.new(fact, Time.now, options)
	end

	def remove(fact)
		@storage.delete_if {|f| f.data == fact}
	end

	def enumerate
		@storage.dup
	end
end

class KnowledgeClass
  class KnowledgeClassDescription
    attr_accessor :name
    attr_accessor :primary_key
    attr_reader :properties

    def initialize(klass)
      @name = klass.name
      @properties = []
    end
  end

  class << self
    @@classes = {}

    def klass(class_name)
      @@classes[self.name] = KnowledgeClassDescription.new(self) if @@classes[self.name].nil?
      @@classes[self.name].name = class_name.to_s
    end

    def id(id_name)
      @@classes[self.name] = KnowledgeClassDescription.new(self) if @@classes[self.name].nil?
      @@classes[self.name].primary_key = id_name.to_s
      @@classes[self.name].properties << id_name.to_s
    end

    def property(property_name)
      @@classes[self.name] = KnowledgeClassDescription.new(self) if @@classes[self.name].nil?
      @@classes[self.name].properties << property_name.to_s
    end

    def to_template
      description = @@classes[self.name]
      fact = [description.name.to_sym]
      fact << description.primary_key.upcase.to_sym
      description.properties.each do |k|
        next if k == description.primary_key

        fact << k.to_sym
        fact << k.upcase.to_sym
      end

      fact
    end

    def from_fact(fact)
      description = @@classes[self.name]
      return nil if fact[0] != description.name.to_sym

      instance = self.new
      instance.values[description.primary_key] = fact[1]
      description.properties.each_with_index do |k, idx|
        next if idx == 0
        instance.values[k] = fact[1 + idx*2]
      end

      instance
    end
  end

  attr_reader :values

  def initialize
    @values = {}
  end

  def to_template
    description = @@classes[self.class.name]
    fact = [description.name.to_sym]
    fact << values[description.primary_key]
    description.properties.each do |k|
      next if k == description.primary_key

      fact << k.to_sym
      fact << values[k]
    end

    fact
  end

  def to_fact
    description = @@classes[self.class.name]
    fact = [description.name.to_sym]
    fact << values[description.primary_key]
    description.properties.each do |k|
      next if k == description.primary_key

      fact << k.to_sym
      fact << values[k]
    end

    fact
  end

  def method_missing(meth, *args, &block)
    description = @@classes[self.class.name]
    description.properties.each do |prop|
      if meth.to_s == "#{prop}="
        return values[prop] = args[0]
      elsif meth.to_s == prop
        return values[prop]
      end
    end

    super
  end
end
