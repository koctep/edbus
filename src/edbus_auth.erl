%%%-------------------------------------------------------------------
%%% @author koctep
%%% @copyright (C) 2013, koctep
%%% @doc
%%%
%%% @end
%%% Created : 2013-10-07 13:04:37.923546
%%%-------------------------------------------------------------------

-module(edbus_auth).

-export([init/1]).
-export([anonymous/3]).
-export([dbus_cookie_sha1/3]).

-import(typextfun, [to_hex/1, from_hex/1, trim/3]).

-define(methods, [
									<<"ANONYMOUS">>,
									<<"DBUS_COOKIE_SHA1">>
								 ]).

init(Socket) ->
	send(Socket, <<"AUTH">>),
	handle_msg(Socket, ?methods, undefined, undefined).

handle_msg(Socket, ProvidedMethods, Handler, State) ->
	{NewProvidedMethods, NewHandler, Msg, NewState} =
	receive
		{tcp, Socket, <<"REJECTED ", Rest/binary>>} ->
			MethodsBin = remove_crlf(Rest),
			AllowedMethods = re:split(MethodsBin, <<" ">>, [trim]),
			case filter_methods(AllowedMethods, ProvidedMethods) of
				[] -> exit({error, no_supported_auth_method});
				Method ->
					{lists:delete(Method, ProvidedMethods), method_handler(Method), init, undefined}
			end;
		{tcp, Socket, TcpMsg1} ->
			TcpMsg = remove_crlf(TcpMsg1),
			{ProvidedMethods, Handler, TcpMsg, State};
		{tcp_closed, Socket} ->
			exit({error, connection_closed});
		timeout ->
			exit({error, timeout})
	end,
	case NewHandler of
		undefined -> exit({error, no_supported_auth_method});
		_ -> ok
	end,
	case catch ?MODULE:NewHandler(Socket, Msg, NewState) of
		{authorized, AuthData} ->
			send(Socket, <<"BEGIN">>),
			{ok, {NewHandler, AuthData}};
		{new_handler, NewHandler1, NewState1} ->
			handle_msg(Socket, NewProvidedMethods, NewHandler1, NewState1);
		{next_state, NewState1} ->
			handle_msg(Socket, NewProvidedMethods, NewHandler, NewState1);
		{'EXIT', _Reason} ->
			send(Socket, <<"CANCEL">>),
			handle_msg(Socket, NewProvidedMethods, undefined, undefined)
	end.

filter_methods([Method | Other], Provided)
	when Provided =/= []
			 ->
	case lists:member(Method, Provided) of
		true -> Method;
		false -> filter_methods(Other, Provided)
	end;
filter_methods(Allowed, Provided)
	when Allowed =:= []; Provided =:= []
			 ->
	[].

method_handler(<<"ANONYMOUS">>) -> anonymous;
method_handler(<<"DBUS_COOKIE_SHA1">>) -> dbus_cookie_sha1;
method_handler(_) -> undefined.

anonymous(Socket, init, State) ->
	send(Socket, <<"AUTH ANONYMOUS">>),
	{next_state, State};
anonymous(_Socket, <<"OK ", Rest/binary>>, _) ->
	{authorized, from_hex(Rest)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The client sends the username it would like to authenticate as, hex-encoded.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dbus_cookie_sha1(Socket, init, State) ->
%	Id = remove_crlf(list_to_binary(os:cmd("id -u"))),
	Id = list_to_binary(os:getenv("USER")),
	IdHex = to_hex(Id),
	send(Socket, <<"AUTH DBUS_COOKIE_SHA1 ", IdHex/binary>>),
	{next_state, State};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The server sends the name of its "cookie context" (see below);
%% a space character;
%% the integer ID of the secret cookie the client must demonstrate knowledge of;
%% a space character;
%% then a randomly-generated challenge string,
%% all of this hex-encoded into one, single string.
%%
%% The client locates the cookie
%% and generates its own randomly-generated challenge string.
%% The client then concatenates the server's decoded challenge,
%% a ":" character,
%% its own challenge,
%% another ":" character,
%% and the cookie.
%% It computes the SHA-1 hash of this composite string as a hex digest.
%% It concatenates the client's challenge string,
%% a space character,
%% and the SHA-1 hex digest,
%% hex-encodes the result and sends it back to the server. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dbus_cookie_sha1(Socket, <<"DATA ", CookieHex/binary>>, State) ->
	CookieStr = from_hex(CookieHex),
	[CookieFile, CookieVal, ServerChallenge] = re:split(CookieStr, <<" ">>),
	{ok, File} = file:open(iolist_to_binary([os:getenv("HOME"), "/.dbus-keyrings/", CookieFile]), [read, raw, binary]),
	{ok, FileCookieStr} = get_cookie(File, CookieVal),
	file:close(File),
	[_Timestamp, Cookie] = re:split(FileCookieStr, <<" ">>),
	ClientChallenge = crypto:strong_rand_bytes(3),
	ShaDigest = crypto:hash(sha, iolist_to_binary([ServerChallenge, <<":">>, ClientChallenge, <<":">>, Cookie])),
	Str = to_hex(iolist_to_binary([ClientChallenge, <<" ">>, to_hex(ShaDigest)])),
	send(Socket, <<"DATA ", Str/binary>>),
	{next_state, State};
dbus_cookie_sha1(_, <<"OK ", Rest/binary>>, _) ->
	{authorized, from_hex(Rest)}.

get_cookie(File, CookieVal) ->
	L = byte_size(CookieVal),
	case file:read_line(File) of
		{ok, <<CookieVal:L/binary, " ", Rest/binary>>} ->
			{ok, remove_crlf(Rest)};
		{ok, _} ->
			get_cookie(File, CookieVal);
		eof ->
			exit({error, dbus_cookie_not_found});
		{error, Reason} ->
			exit({error, Reason})
	end.

send(Socket, Data) ->
	gen_tcp:send(Socket, <<Data/binary, "\r\n">>).

remove_crlf(Str) ->
	trim(right, Str, "\r\n").
