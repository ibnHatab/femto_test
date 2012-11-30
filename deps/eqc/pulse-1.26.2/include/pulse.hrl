%% Expr may not contain self(), need to do parse transform on it!
%% Therefore, ?PULSE should evaluate to pulse:try_to_run or so and
%% by including pulse.hrl a parse transform is applied to the code.

-define(PULSE(Res,Expr,Prop),
  eqc:forall(
       pulse:seed(),
       fun(Seed) ->
          Res = 
            case whereis(pulse_event) of
                 undefined ->
                   %% We assume user wants to run un-instrumented code
                   %% In case code is instrumented, it will generate an
                   %% exception to warn the user.
                   Expr;
                 _ ->
                   pulse:run_with_seed(fun() -> Expr end,Seed)
            end,
          Prop
       end)).


