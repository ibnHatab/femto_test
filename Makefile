
REBAR=./rebar

.PHONY: deps rel

compile:deps
	$(REBAR) compile

rel: compile
	$(REBAR) generate

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	$(REBAR) delete-deps
	rm -rf deps/distel

distel:
	git clone "https://github.com/massemanet/distel.git" deps/distel
	make -C deps/distel
