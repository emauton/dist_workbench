%%% @doc The {@link //workbench_metrics} supervisor callback module.
%%% This is the top-level
%%% <a href="http://www.erlang.org/doc/design_principles/sup_princ.html">
%%% Erlang/OTP supervisor</a> for the Workbench Metrics application.
%%% See the supervisor document for details of the callbacks defined here.
%%%
%%% Supervises monitoring processes for workbench.
-module(workbench_metrics_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @doc Start the workbench_metrics top-level supervisor.
%% Note that we pass empty `Args' to the init/1 callback.
%% @private
-spec start_link() ->
    {ok, pid()} |
    ignore |
    {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize the workbench_metrics top-level supervisor.
%% @private
-spec init(Args :: list()) ->
    {ok, {{RestartStrategy :: supervisor:strategy(),
           MaxR :: non_neg_integer(),
           MaxT :: non_neg_integer()},
          [supervisor:child_spec()]}} |
    ignore.
init(_Args) ->
    Periodic = {workbench_metrics_periodic,
                {workbench_metrics_periodic, start_link, []},
                permanent, infinity, worker, [workbench_metrics_periodic]},
    {ok, {{one_for_one, 5, 10}, [Periodic]}}.
