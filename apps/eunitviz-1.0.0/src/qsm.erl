%#!/usr/bin/env escript
%% -*- erlang -*-
%%! debug verbose
-module(qsm).
-export([rename_fsm/2, gen_fsm/1]).

-include("../include/automata.hrl").

translate(Names, Dict) ->
    lists:map(fun (Name) ->
		      case lists:keysearch(Name, 1, Dict) of
			  {value, {_Old, New}} -> New;
			  false -> Name
		      end
	      end, Names).

rename_fsm({fa,States,Tokens,InitState,Transitions,FailingStates}, Dict) ->
    NewTransitions = [{hd(translate([O], Dict)), L, hd(translate([D], Dict))}
		      || {O, L, D} <- Transitions],
    {fa,
     translate(States, Dict),
     Tokens,
     hd(translate([InitState], Dict)),
     NewTransitions,
     translate(FailingStates, Dict)
    }.

gen_fsm({fa,States,_Tokens,InitState,Transitions,FailingStates}) ->
    OrdStates = [InitState | (States -- [InitState | FailingStates])] ++ FailingStates,
    OrdTransitions = lists:keysort(3, Transitions),
    StateAndTrans = lists:map(fun(S) ->
				      {S,
				       [{O, L, D}
					|| {O, L, D} <- OrdTransitions, S =:= O]}
			      end, OrdStates),
    Fsm = lists:flatten(
	    lists:map(fun translate_state/1, StateAndTrans)
	   ),
    io:format("~s", [Fsm]).

translate_transition({_Ori, Label, Dest}) ->
    lists:flatten(io_lib:format("{~p, ~p},~n", [Dest, Label])).

translate_state({St, Tr}) ->
    lists:flatten([ io_lib:format("~p(State) ->\n", [St]),
		    "[\n",
		    lists:map(fun translate_transition/1, Tr),
		    "].\n\n"
		  ]).

