.SUFFIXES: .erl .beam

.erl.beam:
	erlc -I../../exmpp/include -o ../ebin -W $<

ERL = erl -boot start_clean

MODS = logger script_server fipa_message message_bus cirrocumulus account_monitor

all: compile
#	${ERL} -pa '/home/joe/.../this/dir' -s module1 start
    
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump