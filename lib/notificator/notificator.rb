require 'rubygems'
require 'xmpp4r'
require 'xmpp4r-simple'
require 'active_support'
require 'systemu'

im = Jabber::Simple.new("h2-notificator@o1host.net", "h2-notificator")
im.send!("<presence to='cirrocumulus@conference.o1host.net/h2-notificator' />")

# <fipa-message act="propose" ontology="/cirrocumulus-notifications"><content>vu_state c60871c47d9811de9fdf173756d89593</content></fipa-message>

loop do
  im.received_messages do |message|
		begin
			xml = Hash.from_xml(message.body)['fipa_message']
			ontology = xml['ontology']
			act = xml['act']
			if (ontology =~ /cirrocumulus-notifications/) && act == 'propose'
				content = xml['content'].split(' ')
				delivery_method = content.first
				if delivery_method == 'sms'
					xml['content'].gsub("&quot;", "\"") =~ /sms (\+\d+) "(.+)"/
					phone = $1
					text = $2
					systemu "echo \"#{text}\" | gnokii --sendsms #{phone}"
				end
			end
		rescue Exception => e
			puts e.to_s
		end
  end

	sleep 1
end

im.disconnect