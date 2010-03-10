require 'rubygems'
require 'erlectricity'

receive do |f|
    f.when([:echo, String]) do |text|
	f.send!([:result, "Yoo said: #{text}"])
	f.receive_loop
    end
end
