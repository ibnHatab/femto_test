%% Code testing frequency.erl which is itself from

-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1]).

eunit_visualize() ->
    Filter = [{frequency,init}],                % Cutout not relevant stuff
    Dict=[{1, started}, {3, error}, {0, init}], % Translate state names
						% Generate PTNT
    {Pos, Neg} = eunit_to_fsm:dynamic("frequency_tests.erl", Filter),
    FA = bluefringe:qsm({Pos, Neg}),            % BlueFringe algorithm
    FSM = qsm:rename_fsm(FA, Dict),             % Rename FA states
    S = qsm:gen_fsm(FSM),                       % Generate states for Eqc FSM
    bluefringe_dot:visualize(FSM).              % Show picture in firefox


%% A single positive test.

% Hard Coded
-define(BAND, [1,2]).

startstop_test_() -> 
    {setup,
     fun ()  -> start(?BAND) end,       
     fun (_) -> stop() end,
     fun () ->
	     ?assertMatch(ok	,stop()),
	     ?assertMatch(true	,start(?BAND))
     end

    }.

stopFirst_test() ->
    ?assertError(badarg ,stop()).

startstart_test_() -> 
    {setup,
     fun ()  -> start(?BAND) end,       
     fun (_) -> stop() end,
     fun () ->
	     ?assertError(badarg, start(?BAND))
     end
    }.

running_test_() ->
    {setup,
     fun () -> start(?BAND) end,
     fun (_) -> stop() end,
     fun () ->
	      ?assertMatch({ok,1}, allocate()),
 	      ?assertMatch(ok, deallocate(1)),
 	      ?assertMatch({ok,1}, allocate())
      end
    }.

outOfFreq_test_() ->
    {setup,
     fun () -> start(?BAND) end,
     fun (_) -> stop() end,
     fun () ->
	      ?assertMatch({ok,1}, allocate()),
	      ?assertMatch({ok,2}, allocate()),
	      ?assertMatch({error, no_frequency}, allocate())
      end
    }.

allocateNotStarted_test() ->
    ?assertError(_, allocate()).

deallocateNotStarted_test() ->
    ?assertError(_, deallocate(1)).

