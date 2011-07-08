
.PHONY: deps rel

compile:deps
	rebar compile

rel: compile
	rebar generate

deps:
	rebar get-deps;	rebar update-deps

clean:
	rebar clean
	rebar delete-deps
	rm -rf deps/distel

distel:
	git clone "https://github.com/massemanet/distel.git" deps/distel
	make -C deps/distel
