class Kb
  def initialize
    @knowledge = []
  end

  def add_fact(key, value)
    s = Sexpistol.new
    key_str = s.to_sexp(key)
    #puts "add: #{key_str}"
    @knowledge << {:key => key_str, :value => value}
  end

  def query_fact(key)
    #puts "query: #{key}"
    @knowledge.each do |h|
      if h[:key] == key
        #puts "found: #{h[:value]}"
        return h[:value]
      end
    end

    nil
  end
  
  def remove_fact(key)
    #puts "remove fact: #{key}"
    @knowledge.delete(key)
  end
  
  def keys
    res = []
    @knowledge.each do |h|
      res << h[:key]
    end
    
    res
  end

  def collect_knowledge()
  end
end
