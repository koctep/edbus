%%%-------------------------------------------------------------------
%%% @author koctep
%%% @copyright (C) 2013, koctep
%%% @doc
%%%
%%% @end
%%% Created : 2013-10-10 00:05:35.044270
%%%-------------------------------------------------------------------
-module(edbus_worker).

-behaviour(gen_server).

%% API
-export([connect/2]).
-export([connect/3]).
-export([send/2]).

%% gen_server callbacks
-export([init/1,
				 handle_call/3,
				 handle_cast/2,
				 handle_info/2,
				 terminate/2,
				 code_change/3]).

-import(edbus_data, [pack/1, unpack/1]).
-import(typextfun, [to_hex/1]).

-include_lib("messages.hrl").

-record(state, {socket, name, serial = 2}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
connect(Host, Port) ->
	connect(Host, Port, undefined).

connect(Host, Port, NonceFile) ->
	gen_server:start_link(?MODULE, [Host, Port, NonceFile], []).

send(C, Data) ->
	C ! {send, Data}.

%call(C, Path, Member, Headers, Body) ->
%	Data = pack(#method_call{path = Path, member = Member, body = Body, headers = Headers, serial = 1}),
%	C ! {send, Data}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port, NonceFile]) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}, binary, {buffer, 65535}]),
	case NonceFile of
		undefined -> ok;
		_ -> {ok, 16} = file:sendfile(NonceFile, Socket)
	end,
	gen_tcp:send(Socket, <<0>>),
	{ok, _AuthData} = edbus_auth:init(Socket),
	lager:info("auth by ~p", [_AuthData]),
	send_msg(#dbus_call{
							serial = 1,
							headers = [{interface,<<"org.freedesktop.DBus">>},
												 {destination,<<"org.freedesktop.DBus">>}],
							path = <<"/org/freedesktop/DBus">>,
							member = <<"Hello">>
						 }, #state{socket = Socket}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({send, Data}, State) ->
	{ok, NewState} = send_msg(Data, State),
	{noreply, NewState};
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
	lager:debug("unpacking ~p (~p)", [to_hex(Data), byte_size(Data)]),
	Msg = unpack(Data),
	lager:debug("recieved ~p", [Msg]),
	{noreply, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
	{stop, tcp_closed, State#state{socket = undefined}};
handle_info(Info, State) ->
	lager:warning("info msg ~p when ~p", [Info, State]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = undefined}) ->
	ok;
terminate(Reason, #state{socket = Socket} = State) ->
	gen_tcp:close(Socket),
	terminate(Reason, State#state{socket = undefined}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_msg(Msg, State)
	when
		element(#dbus_message.serial, Msg) =:= undefined ->
	Serial = State#state.serial + 1,
	NewMsg = edbus_data:set(Msg, serial, Serial),
	send_msg(NewMsg, State#state{serial = Serial});
send_msg(Data, #state{socket = Socket} = State) when is_binary(Data) ->
	gen_tcp:send(Socket, Data),
	{ok, State};
send_msg(Data, State) -> send_msg(pack(Data), State).
