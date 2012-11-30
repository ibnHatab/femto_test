-ifdef(McErlang).
-compile([{parse_transform,eqc_mce_prop_transform}]).
-include("mce_opts.hrl").
-endif.

-define(RUN_VER(Sim,F),
		case Sim of 
			{simulation,Seed} ->
				mce_app:simulate_then_verify(F,Seed);
			simulation ->
				mce_app:simulate_then_verify(F);
			{simulation_only,Seed} ->
				mce_app:simulate(F,Seed);
			_ ->
				mce_app:verify(F)
		end).

-ifdef(McErlang).

-define(MCERLANG(InsMods,Var,Expr,Prop),
		?MCERLANG_INT(no_simulation,InsMods,Var,Expr,Prop)).

-define(MCERLANG_SIM(InsMods,Var,Expr,Prop),
		eqc:forall(
		  eqc_mcerlang:seed(),
		  fun(Seed) ->
				  ?MCERLANG_INT({simulation,Seed},InsMods,Var,Expr,Prop)
		  end)
	   ).

-define(MCERLANG_SIM_ONLY(InsMods,Var,Expr,Prop),
		eqc:forall(
		  eqc_mcerlang:seed(),
		  fun(Seed) ->
				  ?MCERLANG_INT({simulation_only,Seed},InsMods,Var,Expr,Prop)
		  end)
	   ).


-define(MCERLANG_INT(Sim,InsMods,Var,Expr,Prop),
  case eqc_mcerlang:check_scheduler(mcerlang,InsMods) of
	  {error,not_consistently_instrumented} ->
		  ?WHENFAIL(io:format(
					  "Modules not consistently instrumented with mcerlang:\n(~p)\n",
					  [lists:zip(InsMods,
						 lists:map(fun eqc_mce:which_scheduler/1,
								   InsMods))]),
					false);
	  ok ->	case erlang:whereis(mcerlang) of
			undefined ->
				?WHENFAIL(io:format("McErlang-application is not running~n"),
						  false);
			_ ->
				?RUN_VER(Sim,(fun() ->
									 Var = Expr,
									 case  mcerlang__property__(Prop) of
										 true  -> no_res;
										 false -> throw({failed,Var})
									 end
							 end)),
				case mce_result:is_correct(mce_app:get_res()) of
					false ->
						%% Ugly piece of code, but not using any potentially 
						%% already bound variables...
						Var = case mce_result:error(mce_app:get_res()) of
								  {value,deadlock} ->
									  deadlock;
								  {exception,_} ->
									  element(2,mce_result:exception_reason(
												  mce_result:error(
													mce_app:get_res()))
									   )
							  end,
						?WHENFAIL(
						   io:format("McErlang generated a Counter-example, it can " ++
									 "be accessed via mce_app:get_res().\n"),
						   mcerlang__false__(Prop));
					true ->
						classify(mce_result:is_inconclusive(mce_app:get_res()),
								 inconclusive,
								 measure(elapsed_time
								 		 ,case mce_result:wallclock(mce_app:get_res()) of
								 			  N when is_integer(N) -> N/1000;
								 			  _ -> 0
								 		  end,
										 mcerlang__true__(Prop)
										)
								)
						
				end
		end
  end).

-else.
-define(MCERLANG(InsMods,Var,Expr,Prop),
	?WHENFAIL(io:format("ERROR: Running McErlang-property without " 
                            "using McErlang-compiler!\n"), 
	          false)).

-define(MCERLANG_SIM(InsMods,Var,Expr,Prop),
	?WHENFAIL(io:format("ERROR: Running McErlang-property without " 
                            "using McErlang-compiler!\n"), 
	          false)).

-define(MCERLANG_SIM_ONLY(InsMods,Var,Expr,Prop),
	?WHENFAIL(io:format("ERROR: Running McErlang-property without " 
                            "using McErlang-compiler!\n"), 
	          false)).
-endif.

