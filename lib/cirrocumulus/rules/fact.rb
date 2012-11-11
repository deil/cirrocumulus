module Cirrocumulus
  module Ruler
    # Fact.
    # Piece of information, collected and operated in Cirrocumulus.
    # We also remember the time, when this fact was created (observed). Optionally can have expiration time
    class Fact
      attr_reader :data
      attr_accessor :is_deleted

      def initialize(data, time, options)
        @data = data
        @time = time
        @options = options
      end

      def expired?
        return false if @options[:expires] == nil
        @time + @options[:expires] < Time.now
      end
    end
  end
end
