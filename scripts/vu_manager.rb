require 'rubygems'
require 'erlectricity'

receive do |f|
  f.when([:has_vu, String]) do |vps_uid|
    f.send!([:has_vu_reply, vps_uid, true])
    f.receive_loop
  end

  f.when([:echo, String]) do |text|
    f.send!([:result, "Yoo said: #{text}"])
    f.receive_loop
  end
end
