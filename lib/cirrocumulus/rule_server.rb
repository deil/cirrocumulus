require 'eventmachine'
require 'sexpistol'
require 'log4r'

module RuleEngine
  class Server < EventMachine::Connection
    def self.run
      l = Log4r::Logger.new('kb')
      o = Log4r::StdoutOutputter.new('console')
      l.outputters << o
      
      EventMachine.run do
        Signal.trap("INT") { EventMachine.stop }
        Signal.trap("TERM") { EventMachine.stop }
        
        EventMachine.start_server('127.0.0.1', 2812, self)
      end
    end
    
    attr_reader :engine
    
    def initialize()
      @engine = Test.new
    end

    def receive_data(data)
      s = Sexpistol.new
      sexp = s.parse_string(data)
      if sexp.first == :assert
        engine.assert get_fact(sexp)
      elsif sexp.first == :retract
        engine.retract get_fact(sexp)
      elsif sexp.first == :query
        send_data engine.query(get_fact(sexp))
      else
        puts sexp.inspect
      end
    rescue Exception => ex
      puts ex.to_s
    end
    
    protected
    
    def get_fact(sexp)
      sexp[1]
    end
    
  end
end
