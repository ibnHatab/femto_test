%%% File    : eunit_fsm.erl
%%% Description : Tester for FSM


-module(eunit_fsm).

-export([translateCmd/2, get_status/2]).

-define(Expr(E),??E).


get_status(Pid, Which) ->
    {status, Pid, _Mod, List} = sys:get_status(Pid),
    AllData = lists:flatten([ X || {data, X} <- lists:last(List)]),
    proplists:get_value(Which, AllData).

translateCmd(Id, {state, is, X}) ->
    case get_status(Id, "StateName") of
	X -> true;
	V -> erlang:error({state_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expected, X},
			     {value, V}]})
    end;
translateCmd(_Id, {call, M, F, A, X}) ->
%%    io:format("~p:~p~n", [M, F]),
    case apply(M, F, A) of
	X -> ok;
	V -> erlang:error({function_call_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expression, ?Expr(apply(M, F, A))},
			     {expected, X},
			     {value, V}]})
    end;
translateCmd(Id, {loopdata, is, X}) ->
    case tl(tuple_to_list(get_status(Id, "StateData"))) of
	X -> true;
	V -> erlang:error({loopdata_match_failed,
			    [{module, ?MODULE},
			     {line, ?LINE},
			     {expected, X},
			     {value, V}]})
    end.
