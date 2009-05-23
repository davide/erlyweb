all: code

code: clean
	erl -noshell -s make all load -s init stop

run:	code
	erl -yaws debug -run yaws -yaws id musician.webserver conf yaws.conf

clean:
	rm -fv ebin/*.beam musician.rel musician.script musician.boot erl_crash.dump *.log *.access

cleandb:
	rm -rfv *.mnesia Mnesia*

cleandocs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
	rm -fv doc/*.png
