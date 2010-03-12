#!/usr/bin/ruby

require 'rubygems'
require 'xmpp4r-simple'

jabber = Jabber::Simple.new('zabbix@o1host.net', "zabbix")
jabber.send!("<presence to='cirrocumulus@conference.o1host.net/zabbix' />")
jabber.send!("<message to='cirrocumulus@conference.o1host.net' type='groupchat'><body>#{ARGV[2]}</body></message>")
jabber.disconnect


