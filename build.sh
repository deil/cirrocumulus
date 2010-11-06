rm -rf ebin/*
cd src
/opt/erlang/bin/erlc -I../include -I/opt/exmpp/exmpp-0.9.5-6-g6f5a404/include -o ../ebin *
/usr/local/bin/erlc -I../include -I/opt/exmpp/exmpp-0.9.5-6-g6f5a404/include -o ../ebin *
cd ..
