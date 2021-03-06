%%%-------------------------------------------------------------------
%%% File    : bluefringe.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Implements the blue-fringe algorithm
%%%
%%% Created : 10 Nov 2010
%%%-------------------------------------------------------------------

-module(bluefringe).
-include("../include/automata.hrl").

%% API
-export([dot/1,qsm/1]).
-import(bluefringe_merge,[merge/3,number_of_merges/3]).

dot({Positive,Negative}) ->
  bluefringe_dot:visualize(qsm({Positive,Negative})).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: qsm(PositiveTraces, NegativeTraces)
%% Description: Executes QSM algorithm with blue-finge on the lists
%%--------------------------------------------------------------------

qsm({PT, NT}) ->
    Aut = bluefringe_apta:generateApta({PT, NT}),
    remove_floating_states(iterate_all({Aut, [Aut#fa.iSt]}, {PT, NT})).

%%--------------------------------------------------------------------
%% Function: iterate({{Automata, OptimizedExtraInfo}, Red}, Traces)
%% Description: Executes an iteration of blue-fringe over the pair
%%   updates Red. Doesn't clean the mixed states. Returns
%%   {minimal, Automata, OptimizedExtraInfo} if the automata is min.
%%--------------------------------------------------------------------

iterate({Aut, NotStrippedRed}, Traces) ->
%    Debug line
%    io:format("Red: ~p~nAuto: ~p~n====~n", [NotStrippedRed, Aut]),
    Red = NotStrippedRed -- (NotStrippedRed -- Aut#fa.st),
    Blue = calculate_blue(Aut, Red),
    case Blue of
	[] -> {minimal, Aut};
	_ -> Scores = compute_scores(Aut, Red, Blue),
	     case checkNewReds(Blue, Scores) of
 		 [] -> iterate({Aut, Red}, Blue, Scores, Traces);
		 [Head|_] -> iterate({Aut, [Head|Red]}, Traces)
	     end
    end.

iterate({Aut, Red}, [HeadBlue|_], Scores, Traces) ->
    case getNexts(Scores) of
	{[_|_] = List} -> CheckedList = lists:map(make_check_if_compatible(Aut, Traces), List),
			StrippedList = lists:filter(fun (X) -> X =/= incompatible end, CheckedList),
			case StrippedList of
			    [] -> {Aut, [HeadBlue|Red]};
% Replace this lines for the nextone to check all possibilities (too slow)
% also uncomment: make_iterate_all and get_best_of
%			    [#fa{} = Result] -> {Result, Red};
%			    ListOfResults -> IteratedList = lists:map(make_iterate_all(Red, Traces), ListOfResults),
%					     {minimal, get_best_of(IteratedList)}
                            [#fa{} = Result|_] -> {Result, Red}
			end;
        _ -> {Aut, [HeadBlue|Red]}
    end.

checkAutomata(Aut, {PT, NT}) ->
    Initialize = fun (X) -> {Aut#fa.iSt, X} end,
    PTraces = lists:map(Initialize, PT),
    NTraces = lists:map(Initialize, NT),
    Advancer = fun (X) -> advanceTrace(Aut, X) end,
    EndPStates = lists:map(Advancer, PTraces),
    EndNStates = lists:map(Advancer, NTraces),
    PositiveChecker = fun (X) -> (X =:= outOfAutomata) or lists:member(X, Aut#fa.fSt) end,
    NegativeChecker = fun (X) -> (X =:= outOfAutomata) or (not lists:member(X, Aut#fa.fSt)) end,
    case {lists:filter(PositiveChecker, EndPStates), lists:filter(NegativeChecker, EndNStates)} of
        {[], []} -> Aut;
        _ -> incompatible
    end.

advanceTrace(_Aut, {State, []}) -> State;
advanceTrace(Aut, {State, [Head|Tail]}) ->
    case [To || {StateN, HeadN, To} <- Aut#fa.tr, StateN =:= State, HeadN =:= Head] of
        [NewState] -> advanceTrace(Aut, {NewState, Tail});
        [] -> outOfAutomata
    end.


make_check_if_compatible(Aut, Traces) ->
    fun ({St1, St2}) ->
        case (catch merge(Aut, St1, St2)) of
            incompatible -> incompatible;
            NewAut -> checkAutomata(NewAut, Traces)
        end
    end.

%make_iterate_all(Red, Traces) -> fun (Result) -> iterate_all({Result, Red}, Traces) end.

%get_best_of(ListOfResults) -> [Best|_] =
%				  lists:sort(fun (#fa{} = Fa1, #fa{} = Fa2) ->
%						     length(Fa1#fa.st) - length(Fa1#fa.fSt) < length(Fa2#fa.st) - length(Fa2#fa.fSt) end,
%					     ListOfResults),
%			      Best.

%%--------------------------------------------------------------------
%% Function: iterate_all({{Automata, OptimizedExtraInfo}, Red}, Traces)
%% Description: Iterates until the automata is minimal
%%--------------------------------------------------------------------

iterate_all(A, Traces) -> case iterate(A, Traces) of
		      {minimal, Auto} ->
			  mixSink(remove_floating_states(Auto));
		      NextA -> iterate_all(NextA, Traces)
		  end.


%%====================================================================
%% Internal functions
%%====================================================================

checkNewReds(Blue, Scores) ->
    NotNewReds = [S || {Y, {_, S}} <- Scores, Y =/= -1],
    Blue -- NotNewReds.

calculateNotFloating(Auto) ->
    calculateNotFloating([Auto#fa.iSt], Auto).

calculateNotFloating(States, Auto) ->
    case calculate_blue(Auto, States) of
	[] -> gb_sets:from_list(States);
	NewStates -> calculateNotFloating(NewStates++States, Auto)
    end.

remove_floating_states(Auto) ->
    NotFloatingStates = calculateNotFloating(Auto),
    Filter = fun (X) -> gb_sets:is_member(X, NotFloatingStates) end,
    FilterTrans = fun ({X, _, Y}) ->
			  gb_sets:is_member(X, NotFloatingStates) and
			      gb_sets:is_member(Y, NotFloatingStates) end,
    Auto#fa{st = lists:filter(Filter, Auto#fa.st),
	    fSt = lists:filter(Filter, Auto#fa.fSt),
	    tr = lists:filter(FilterTrans, Auto#fa.tr)}.

calculate_blue(Automata, RedStates) ->
    calculate_blue(Automata#fa.tr,
		   sets:from_list(RedStates),
		   sets:new()).

calculate_blue([], RedStates, Blue) ->
    sets:to_list(sets:subtract(Blue, RedStates));
calculate_blue([{Ori, _, Dest}|TT], RedStates, Blue) ->
    case sets:is_element(Ori, RedStates) of
	    true -> calculate_blue(TT, RedStates, sets:add_element(Dest, Blue));
	    false -> calculate_blue(TT, RedStates, Blue)
    end.

compare_scores({X, _St1}, {Y, _St2}) when X > Y -> true;
compare_scores({X, St1}, {X, St2}) when St1 < St2 -> true;
compare_scores(_, _) -> false.

compute_scores(Automata, Red, Blue) ->
    lists:sort(fun compare_scores/2,
        lists:map(fun ({St1, St2}) ->
	    {number_of_merges(Automata, St1, St2), {St1, St2}} end,
	      [{X, Y} || X <- Red, Y <- Blue])).

getNext([]) -> clean;
getNext([{-1, _}|_]) -> all_red;
getNext([{Score, {Ori, Dest}}|Others]) -> {Score, {Ori, Dest}, Others}.

getNexts(List) ->
    case getNext(List) of
	{Score, {_, _} = First, Others} -> getNexts(Score, Others, [First]);
	Result -> Result
    end.

getNexts(Score, [{Score, {_, _} = First}|Others], List) ->
    getNexts(Score, Others, [First|List]);
getNexts(_, _Others, List) -> {List}.
    

mixSink(Auto) ->
    case Auto#fa.fSt of
	[A, B|Tail] ->
	    NA = merge(Auto, A, B),
	    mixSink(NA#fa{fSt = [A|Tail]});
	_ -> Auto
    end.

