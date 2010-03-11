.SUFFIXES: .erl .beam

.erl.beam:
	erlc -o ../ebin -W $<

ERL = erl -boot start_clean

MODS = script_server cirrocumulus

all: compile
#	${ERL} -pa '/home/joe/.../this/dir' -s module1 start
    
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump