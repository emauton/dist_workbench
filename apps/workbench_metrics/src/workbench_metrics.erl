%%% @doc Workbench metrics.
%%% Simple wrapper for the `exometer' metrics application to export useful
%%% metrics from this distribution workbench.
%%%
%%% Handles setting up Graphite reporting and dynamic addition, subscription
%%% and updates for several exometer entry types.
%%%
%%% Following
%%% <a href="https://github.com/emauton/basic_metrics">basic_metrics</a>, we
%%% do everything dynamically instead of using exometer's static configuration
%%% options.
-module(workbench_metrics).
-export([init/0, counter/2, gauge/2, histogram/2,
         riak_core/1, vm/0]).
-ignore_xref([init/0, counter/2, gauge/2, histogram/2,
              riak_core/1, vm/0]).

-define(INTERVAL, 5000).

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with `exometer_report_graphite'.
-spec counter(Name :: exometer:name(), Value :: number()) ->
    ok.
counter(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, counter, []),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with `exometer_report_graphite'.
-spec gauge(Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, gauge, []),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with exometer_report_graphite.
-spec histogram(Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, histogram,
                                  [{module, exometer_histogram}]),
            exometer_report:subscribe(exometer_report_graphite,
                                      Name, [mean, 50, 75, 95, 99],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Initialize exometer with Graphite reporting.
-spec init() ->
    ok.
init() ->
    {ok, Name} = inet:gethostname(),
    Host = application:get_env(workbench_metrics, host,
                               "carbon.hostedgraphite.com"),
    Port = application:get_env(workbench_metrics, port, 2003),
    Key = application:get_env(workbench_metrics, key, ""),
    ReportOptions = [{connect_timeout, 5000},
                     {prefix, "workbench." ++ Name},
                     {host, Host},
                     {port, Port},
                     {api_key, Key}],
    ok = exometer_report:add_reporter(exometer_report_graphite, ReportOptions).

%% @doc Initialize basic VM metrics.
-spec vm() ->
    ok.
vm() ->
    % VM memory.
    % total = processes + system.
    % processes = used by Erlang processes, their stacks and heaps.
    % system = used but not directly related to any Erlang process.
    % atom = allocated for atoms (included in system).
    % binary = allocated for binaries (included in system).
    % ets = allocated for ETS tables (included in system).
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, memory],
                                   [total, processes, system, atom, binary,
                                    ets], ?INTERVAL, [], true),

    % Memory actively used by the VM, allocated (should ~match OS allocation),
    % unused (i.e. allocated - used), and usage (used / allocated).
    ok = exometer:new([recon, alloc],
                      {function, recon_alloc, memory, ['$dp'], value,
                       [used, allocated, unused, usage]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, alloc],
                                   [used, allocated, unused, usage], ?INTERVAL,
                                   [], true),

    % Memory reserved by the VM, grouped into different utility allocators.
    ok = exometer:new([recon, alloc, types],
                      {function, recon_alloc, memory,
                       [allocated_types], proplist,
                       [binary_alloc, driver_alloc, eheap_alloc,
                        ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                        std_alloc, temp_alloc]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [recon, alloc, types],
                                   [binary_alloc, driver_alloc, eheap_alloc,
                                    ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                                    std_alloc, temp_alloc], ?INTERVAL,
                                   [], true),

    % process_count = current number of processes.
    % port_count = current number of ports.
    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, system],
                                   [process_count, port_count], ?INTERVAL,
                                   [], true),

    % The number of processes that are ready to run on all available run queues.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [erlang, statistics],
                                   [run_queue], ?INTERVAL, [], true).

%% @doc Initialize riak_core metrics, including those for `VNodeNames'.
%% These are inserted into exometer by riak_core; although workbench_metrics
%% doesn't depend on riak_core, we setup the exports via graphite here and
%% allow dependent riak_core applications to call this "plugin" function to
%% get interesting stats, e.g.
%%   `workbench_metrics:riak_core([workbench_vnode]).'
-spec riak_core(VNodeNames :: [atom(), ...]) ->
    ok.
riak_core(VNodeNames) ->
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, ignored_gossip_total],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rings_reconciled],
                                   [count],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, gossip_received],
                                   [count],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, handoff_timeouts],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rejected_handoffs],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core,
                                    dropped_vnode_requests_total],
                                   [value],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, converge_delay],
                                   [mean, 50, 75, 95, 99],
                                   ?INTERVAL, [], true),
    ok = exometer_report:subscribe(exometer_report_graphite,
                                   [riak, riak_core, rebalance_delay],
                                   [mean, 50, 75, 95, 99],
                                   ?INTERVAL, [], true),
    lists:foreach(fun(Name) ->
                    ok = exometer_report:subscribe(exometer_report_graphite,
                                                   [riak, riak_core,
                                                    vnodeq, Name],
                                                   [mean, median, min,
                                                    max, total],
                                                   ?INTERVAL, [], true),
                    ok = exometer_report:subscribe(exometer_report_graphite,
                                                   [riak, riak_core,
                                                    vnodes_running, Name],
                                                   [value],
                                                   ?INTERVAL, [], true)
                  end,
                  VNodeNames),
    ok.
