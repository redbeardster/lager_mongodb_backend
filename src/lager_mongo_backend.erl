-module(lager_mongo_backend).

-behaviour(gen_event).

-include_lib("lager/include/lager.hrl").

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {level = lager_util:config_to_mask(info) :: {mask, non_neg_integer()},
                host = "localhost" :: string(),
                port = 2017 :: pos_integer(),
                database = log :: atom()|binary(),
                collection :: atom()|binary(),
                connection :: pid(),
                formatter = lager_mongo_default_formatter :: module()}).

-spec init(proplists:proplist()) -> {ok, #state{}} | {error, atom()}.
init(Params) ->
    {ok, _} = application:ensure_all_started(mongodb),
    Host = proplists:get_value(host, Params, "localhost"),
    Port = proplists:get_value(port, Params, 27017),
    DB = proplists:get_value(database, Params, log),
    {ok, C} = mc_worker_api:connect([{host, Host}, {port, Port}, {database, DB}, {w_mode, unsafe}]),
    {ok, #state{level = lager_util:config_to_mask(proplists:get_value(level, Params, info)),
                host = Host,
                port = Port,
                database = DB,
                collection = to_binary(proplists:get_value(collection, Params)),
                connection = C,
                formatter = proplists:get_value(formatter, Params, lager_mongo_default_formatter)}}.

handle_event({log, Message}, #state{level = L} = State) ->
    lager_util:is_loggable(Message, L, ?MODULE) andalso
        begin
        #state{collection = Collection, connection = C, formatter = Formatter} = State,
        mc_worker_api:insert(C, Collection, Formatter:format(Message))
        end,
    {ok, State};
handle_event(_Event, State) -> {ok, State}.

-spec(handle_call(any(), State::#state{}) -> {ok, any(), #state{}}).
handle_call(get_loglevel, #state{level = L} = State) -> {ok, L, State};
handle_call({set_loglevel, L}, State) ->
    case lists:member(L, ?LEVELS) of
        true -> {ok, ok, State#state{level = lager_util:config_to_mask(L)}};
        _ -> {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) -> {ok, ok, State}.

-spec(handle_info(any(), State::#state{}) -> {ok, #state{}}).
handle_info(_, State) -> {ok, State}.

-spec(terminate(any(), #state{}) -> {ok, list()}).
terminate(_Reason, #state{connection = C}) -> mc_worker_api:disconnect(C).

-spec(code_change(any(), State::#state{}, any()) -> {ok, #state{}}).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

to_binary(B) when is_binary(B) -> B;
to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(_) -> <<>>.
