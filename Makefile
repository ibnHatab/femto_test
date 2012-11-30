
REBAR=./rebar

.PHONY: deps rel

compile:deps distel eunit_viz
	$(REBAR) compile

rel: compile
	$(REBAR) generate

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	$(REBAR) delete-deps

distclean: clean
	rm -rf deps/distel
	rm -rf deps/eunitv_viz

distel:
	-git clone "https://github.com/massemanet/distel.git" deps/distel
	make -C deps/distel

eunit_viz:
	-git clone "https://github.com/ThomasArts/Visualizing-EUnit-tests.git" deps/eunitv_viz
	cd deps/eunitv_viz/ebin; erl -make

