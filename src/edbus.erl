%%%-------------------------------------------------------------------
%%% @author koctep
%%% @copyright (C) 2013, koctep
%%% @doc
%%%
%%% @end
%%% Created : 2013-10-07 13:04:37.923546
%%%-------------------------------------------------------------------
-module(edbus).

-export([session/0]).
-export([session/1]).

-record(opts, {host="localhost", port, nonce_file}).

-include_lib("messages.hrl").

session() ->
	session(os:getenv("DBUS_SESSION_BUS_ADDRESS")).

session(Opts) ->
	lager:debug("methods: ~p", [Opts]),
	Methods = re:split(Opts, <<";">>, [trim]),
	session1(Methods).

session1([<<"tcp:", Rest/binary>> | _]) ->
	OptsList = re:split(Rest, <<",">>, [trim]),
	Opts = parse_opts(OptsList, #opts{}),
	edbus_worker:connect(Opts#opts.host, Opts#opts.port, Opts#opts.nonce_file);
session1([<<"nonce-tcp:", Rest/binary>> | _]) ->
	session1([<<"tcp:", Rest/binary>>]);
session1([]) -> {error, no_supported_connect_method};
session1([_ | T]) -> session1(T).

parse_opts([<<"host=", Host/binary>> | T], Opts) ->
	parse_opts(T, Opts#opts{host = binary_to_list(Host)});
parse_opts([<<"port=", Port/binary>> | T], Opts) ->
	parse_opts(T, Opts#opts{port = list_to_integer(binary_to_list(Port))});
parse_opts([<<"noncefile=", NonceFile/binary>> | T], Opts) ->
	parse_opts(T, Opts#opts{nonce_file = NonceFile});
parse_opts([], Opts) -> Opts;
parse_opts([_ | T], Opts) -> parse_opts(T, Opts).
