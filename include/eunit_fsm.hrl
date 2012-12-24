%%% File    : eunit_fsm.hrl
%%% Description : Define macro for eunit FSM test

%% Usage example
%% fsm_state_test_() ->
%%     {foreach,
%%      fun ()  -> {ok, P} = start(?SECRET), P end,
%%      fun (_) -> stop() end,
%%      [
%%       ?fsm_test(whereis(locker),"Started Properly Test",
%% 		[{state,is,unlocked},		 
%% 		 {loopdata,is,[?SECRET]}
%% 		]),
%%       ?fsm_state(whereis(locker), unlocked),
%%       ?fsm_data(whereis(locker), [?SECRET])
%%      ]  
%%     }.


-define(_fsm_test(Id, Title, CmdList),
       {Title, fun() -> [eunit_fsm:translateCmd(Id, Cmd)
			 || Cmd <- CmdList]
	       end}).

-define(fsm_state(Id, StateName),
	eunit_fsm:translateCmd(Id, {state,is,StateName})).

-define(fsm_data(Id, Data),
	eunit_fsm:translateCmd(Id, {loopdata,is,Data})).

-define(_fsm_state(Id, StateName),
	{??StateName, fun() -> ?fsm_state(Id, StateName) end}).

-define(_fsm_data(Id, Data), fun() -> ?fsm_data(Id, Data) end).

-define(exit_on_error(F, E), case F of E -> exit(error); Other -> Other end).
