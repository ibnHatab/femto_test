

> http://hg.basho.com/rebar/downloads/rebar

$ mkdir gpt && cd gpt

$ rebar create-app appid=gpt

$ ls -1 src
gpt_app.erl
gpt.app.src
gpt_sup.erl

src/gpt.app.src:
{description, "GProc tutorial"},
  ...
  {applications,
   [
    kernel,
    stdlib,
    gproc     %

$ cd ../../
$ mkdir rel && cd rel

$ rebar create-node nodeid=gptnode

reltool.config:
-sname gptnode@localhost

$ cd ../
rebar.config:
{deps_dir, ["deps"]}.

{sub_dirs, ["rel", "apps/gpt"]}.

{erl_opts, [debug_info, fail_on_warning]}.

{deps,
 [
  {gproc, ".*", {git, "http://github.com/esl/gproc.git", "master"}}
 ]}.

$ rebar get-deps
$ rebar compile
$ rebar generate

(cd rel/gptnode && sh bin/gptnode console)

(gptnode@localhost)1> application:which_applications().

#Erlang
export ERL_LIBS=$HOME/libs/femto_test/deps:$HOME/libs/femto_test/apps
~/.erlang
code:load_abs("/home/vkinzers/libs/femto_test/apps/eunit_tools/ebin/user_default").

