%%% @author Vladyslav Kinzerskiy <lib.aca55a@gmail.com>
%%% @copyright (C) 2011, Vladyslav Kinzerskiy
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2011 by Vladyslav Kinzerskiy <lib.aca55a@gmail.com>

-module(prop_frequency).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).


init(State) ->
    [
%%      {error, {frequency,deallocate,[nat()]}},
%%      {error, {frequency,allocate,[]}},
     {error,   {?MODULE,stop,[]}},
     {started, {?MODULE,start,[nat()]}}
    ].

started(State) ->
    [{error,   {?MODULE,start,[nat()]}},
     {init,    {?MODULE,stop,[]}},
     {started, {frequency,deallocate,[nat()]}},
     {started, {frequency,allocate,[]}}
    ].

error(State) ->
    [].


start(Freq) ->
    frequency:start(lists:seq(1, Freq)).

stop() ->
    frequency:stop().


postcondition(init,started,_,{call,_,start,_},R) ->
    R == true;
postcondition(started,init,_,{call,_,stop,_},R) ->
    R == ok;
postcondition(_From,error,_,{call,_,_,_},R) ->
    case R of
	{'EXIT',_} ->
	    true;
	_ -> false
    end.


