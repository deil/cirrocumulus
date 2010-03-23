#!/usr/bin/ruby

require 'rubygems'
require 'xmpp4r-simple'

body = "<fipa-message act=\"inform\" ontology=\"http://api.o1host.net/ontology/cirrocumulus/zabbix\"><sender name=\"zabbix@o1host.net\"/><content>(inform (msg \"#{ARGV[2]}\")</content></fipa-message>"
jabber = Jabber::Simple.new('zabbix@o1host.net', "zabbix")
jabber.send!("<presence to='cirrocumulus@conference.o1host.net/zabbix' />")
jabber.send!("<message to='cirrocumulus@conference.o1host.net' type='groupchat'><body>#{body.gsub('"', '&quot;').gsub('<', '&lt;').gsub('>', '&gt;')}</body></message>")
jabber.disconnect


