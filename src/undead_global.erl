%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <free>
%%% @doc
%%%
%%% @end
%%% Created : 2017年07月05日19:11:34
%%%-------------------------------------------------------------------
-module(undead_global).

-export([start/0, stop/0]).

-export([start_link/0]).

-export([reg/2, get/0, nodes/0]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%------------------------------------------------------------------------------
-behaviour(gen_server).

-define(TIMEOUT, 1000).

-record(state, {list = []}).

%%------------------------------------------------------------------------------
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reg(Name, MFA) ->
    gen_server:call(?MODULE, {reg, Name, MFA}).

get() ->
    gen_server:call(?MODULE, get).

nodes() ->
    application:get_env(?MODULE, nodes, []).

%%------------------------------------------------------------------------------
init([]) ->
    [net_adm:ping(Node) || Node <- undead_global:nodes()],
    process_flag(trap_exit, true),
    {ok, #state{}, 0}.

handle_call({reg, Name, MFA}, _From, State) ->
     case handle_reg(Name, MFA, State) of
         {ok, State1} -> {reply, ok, State1};
         {error, Reason} -> {reply, {error, Reason}, State}
     end;
handle_call(get, _From, State) ->
    {reply, State#state.list, State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(timeout, State) ->
    State1 = handle_timeout(State),
    erlang:send_after(?TIMEOUT, self(), timeout),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
handle_reg(Name, MFA, #state{list = List} = State) ->
    case eusure_reg(Name, MFA) of
        ok -> {ok, State#state{list = lists:keystore(Name, 1, List, {Name, MFA})}};
        {error, reg_fail} -> {ok, State#state{list = lists:keystore(Name, 1, List, {Name, MFA})}};
        {error, Reason} -> {error, Reason}
    end.

%% @private
eusure_reg(Name, {M, F, A}) ->
    case catch global:whereis_name(Name) of
        undefined ->
            case catch apply(M, F, A) of
                {ok, PID} ->
                    case catch global:register_name(Name, PID) of
                        yes -> ok;
                        _ -> exit(PID, shutdown), {error, reg_fail}
                    end;
                Reason ->
                    {error, Reason}
            end;
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
handle_timeout(#state{list = List} = State) ->
    catch [net_adm:ping(Node) || Node <- undead_global:nodes()],
    Fun = fun({Name, MFA}) -> eusure_reg(Name, MFA) end,
    lists:foreach(Fun, List), State.

