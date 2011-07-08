%%% File    : eunit_fsm.hrl
%%% Description : Define macro for eunit FSM test


-define(fsm_test(Id, Title, CmdList),
       {Title, fun() -> [eunit_fsm:translateCmd(Id, Cmd) || Cmd <- CmdList] end}).



