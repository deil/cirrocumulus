require_relative 'message.rb'

module Cirrocumulus
	class MessageBus
		def connected?; end

		def connect(identifier); end

		def start(agent); end

		def send_message(message); end
	end
end
