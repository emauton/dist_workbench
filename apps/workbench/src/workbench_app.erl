%%% @doc The {@link //workbench} application callback module.
%%% This is the entry point for the Workbench
%%% <a href="http://www.erlang.org/doc/design_principles/applications.html">
%%% Erlang/OTP application</a>; see that document for details of the callbacks
%%% defined here.
%%%
%%% Starts the top-level application supervisor.
%%% Cf. `workbench.app.src'.
-module(workbench_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @private
-spec start(Type :: term(), Args :: term()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()}.
start(_, _) ->
    workbench_sup:start_link().

%% @private
-spec stop(State :: term()) ->
    ok.
stop(_) ->
    ok.
