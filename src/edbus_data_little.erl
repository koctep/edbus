-module(edbus_data_little).

-define(endian, little).

-include("types.hrl").
-include("messages.hrl").

%-compile(export_all).

-export([unpack/1]).
-export([pack/1]).

-import(edbus_data, [extend/1, restrict/1, msg_fill/3]).
-import(edbus_data, [msg_type/1, header_type/1, header_value_type/1, header_len/1, align_type_len/2]).

-import(edbus_data, [to_hex/1, discharge/3]).

-include("data.hrl").

unpack(<<MsgType:?byte,
				 Flags:?byte,
				 1:?byte,
				 BodyLen:?uint32,
				 Serial:?uint32,
				 HeaderLen1:?uint32,
				 Rest/binary>> = _Data) ->
	Msg = #dbus_message{
					 endian = edbus_data:endian(?endian),
					 flags = Flags,
					 serial = Serial,
					 msg_type = msg_type(MsgType)
					},
	HeaderLen = header_len(HeaderLen1),
	<<Headers:HeaderLen/binary, Body:BodyLen/binary, NextMsg/binary>> = Rest,
	{msg(extend(Msg), Headers, Body), NextMsg}.

msg(Msg, HeadersBin, Body) ->
	Headers = headers(HeadersBin),
	Sign = proplists:get_value(signature, Headers, <<>>),
	ParsedBody = unpack_body(Sign, Body),
	msg_fill(Msg, Headers, ParsedBody).

headers(Bin) ->
	headers(Bin, []).

headers(<<HeaderType, Rest/binary>> = Data, Headers) ->
	{Val, L} = unpack_value($v, Rest),
	HeaderLen = header_len(L + 1),
	<<_Parsed:HeaderLen/binary, Rest1/binary>> = Data,
	headers(Rest1, [{header_type(HeaderType), Val} | Headers]);
headers(<<HeaderFieldType, 1, ValType, 0, Rest/binary>> = Data, Headers) ->
	{Val, L} = unpack_value(ValType, Rest),
	Len = L + 4,
	HeaderLen = header_len(Len),
	<<_Parsed:HeaderLen/binary, Rest1/binary>> = Data,
	Header = {header_type(HeaderFieldType), Val},
	headers(Rest1, [Header | Headers]);
headers(<<>>, Headers) -> Headers;
headers(Data, Headers) ->
	lager:warning("unparsed ~p", [to_hex(Data)]),
	lager:warning("headers ~p", [Headers]),
	Headers.

unpack_body(Sign, Body) ->
	unpack_body(Sign, Body, []).

unpack_body(<<>>, <<>>, R) -> lists:reverse(R);
unpack_body(Sign, Body, R) when Sign =:= <<>>; Body =:= <<>> ->
	lager:warning("unparsed ~p ~p", [Sign, to_hex(Body)]),
	lists:reverse(R);
unpack_body(Sign, Body, R) ->
	{Type, RestSign} = get_type(Sign),
	{Var, Len} = case catch unpack_type(Type, Body) of
								 {'EXIT', _Reason} ->
									 {{unparsed, Type, Body}, byte_size(Body)};
								 {V, L} ->
									 Align = case align_type_len(L, Type) of
														 AlignLen when AlignLen > byte_size(Body) -> byte_size(Body);
														 AlignLen -> AlignLen
													 end,
									 {V, Align}
							 end,
	<<_Parsed:Len/binary, Rest/binary>> = Body,
	unpack_body(RestSign, Rest, [Var | R]).

get_type(<<$a, Rest1/binary>>) ->
	{Type, Rest} = get_type(Rest1),
	{{array, Type}, Rest};
get_type(<<T, Rest/binary>>) when T =:= $(; T =:= ${ ->
	{Type, Rest1} = get_struct_type(Rest, []),
	{{struct, Type}, Rest1};
get_type(<<T, Rest/binary>>) when T =:= $); T =:= $} ->
	{end_struct, Rest};
get_type(<<T, RestSign/binary>>) ->
	{{value, T}, RestSign}.

get_struct_type(Sign, T) ->
	case get_type(Sign) of
		{end_struct, Rest} -> {lists:reverse(T), Rest};
		{Type, Rest} -> get_struct_type(Rest, [Type | T])
	end.

unpack_type({value, T}, Body) ->
	{Val, L} = unpack_value(T, Body),
	{Val, L};
unpack_type({array, _}, <<0:?uint32, _/binary>>) ->
	{{array, []}, 4};
unpack_type({array, Type}, <<L:?uint32, 0:32, ArrayBody:L/binary, _Rest/binary>>) when L > 0 ->
	Val = unpack_array(Type, ArrayBody, []),
	{{array, Val}, L + 8};
unpack_type({array, Type}, <<L:?uint32, ArrayBody:L/binary, _Rest/binary>>) when L > 0 ->
	Val = unpack_array(Type, ArrayBody, []),
	{{array, Val}, L + 4};
unpack_type({array, _}, <<L:?uint32, Rest/binary>>) ->
	lager:warning("array size ~p bigger then available data ~p", [L, byte_size(Rest)]),
	{{array, big}, byte_size(Rest) + 4};
unpack_type({struct, Types}, Body) ->
	{Val, L} = unpack_struct(Types, Body, [], 0),
	{{struct, Val}, L}.

unpack_array(_, <<>>, R) -> lists:reverse(R);
unpack_array(Type, Body, R) ->
	{Val, L} = unpack_type(Type, Body),
	<<_Parsed:L/binary, Rest/binary>> = Body,
	unpack_array(Type, Rest, [Val | R]).

unpack_struct([Type | T], Body, R, L) ->
	{Val, Len} = unpack_type(Type, Body),
	<<_Parsed:Len/binary, Rest/binary>> = Body,
	unpack_struct(T, Rest, [Val | R], L + Len);
unpack_struct([], _Body, R, Len) ->
	AlignLen = header_len(Len),
	{lists:reverse(R), AlignLen}.

unpack_value($s, <<L:?uint32, V:L/binary, 0, _/binary>>)	-> {<<V/binary>>, L + 5};
unpack_value($g, <<L:?byte, V:L/binary, 0, _/binary>>)		-> {<<V/binary>>, L + 2};
unpack_value($y, <<V:?byte, _/binary>>)										-> {V, 1};
unpack_value($n, <<V:?int16, _/binary>>)									-> {V, 2};
unpack_value($q, <<V:?uint16, _/binary>>)									-> {V, 2};
unpack_value($i, <<V:?int32, _/binary>>)									-> {V, 4};
unpack_value($u, <<V:?uint32, _/binary>>)									-> {V, 4};
unpack_value($x, <<V:?int64, _/binary>>)									-> {V, 8};
unpack_value($t, <<V:?uint64, _/binary>>)									-> {V, 8};
unpack_value($d, <<V:?double, _/binary>>)									-> {V, 8};
unpack_value($h, <<V:?unix_fd, _/binary>>)								-> {V, 8};
unpack_value($b, <<V:?boolean, _/binary>>) when V =:= 1; V =:= 0 -> {V, 4};
unpack_value($v, Data) ->
	{Sign, Len1} = unpack_value($g, Data),
	{Type, <<>>} = get_type(Sign),
	<<_Sign:Len1/binary, Rest/binary>> = Data,
	{V, L} = unpack_type(Type, Rest),
	{V, L + Len1};
unpack_value($o, Data) -> unpack_value($s, Data).

pack(#dbus_message{
				body = Body,
				headers = Headers} = Msg) ->
	NewHeaders = case Body of
		<<>> -> proplists:delete(signature, Headers);
		Data when is_binary (Data) ->
			case proplists:get_value(signature, Headers) of
				undefined -> [{signature, <<"s">>} | Headers];
				_ -> Headers
			end
	end,
	pack1(Msg#dbus_message{headers = NewHeaders});
pack(Msg) ->
	pack(restrict(Msg)).

pack1(#dbus_message{
				msg_type = Type,
				flags = Flags,
				serial = Serial,
				headers = Headers,
				body = Body1}) ->
	MsgType = msg_type(Type),
	HeadersBin = iolist_to_binary(lists:map(fun(X) -> {_, H} = pack_header(X), H end, Headers)),
	{L, LH} = pack_header(lists:last(Headers)),
	HeaderLen = byte_size(HeadersBin) - byte_size(LH) + L,
	Body = pack_body(Body1),
	BodyLen = byte_size(Body),
	Head = <<MsgType:?byte,
					 Flags:?byte,
					 1:?byte,
					 BodyLen:?uint32,
					 Serial:?uint32,
					 HeaderLen:?uint32
				 >>,
	<<Head/binary, HeadersBin/binary, Body/binary>>.

pack_body(<<>>) -> <<>>;
pack_body(Data) when is_binary(Data) ->
	L = byte_size(Data),
	<<L:?uint32, Data/binary, 0>>;
pack_body([Data]) ->
	pack_body(Data).

pack_header({Key, Val}) ->
	Type = header_type(Key),
	ValType = header_value_type(Key),
	ValBin = pack_value(ValType, Val),
	Len = byte_size(ValBin) + 4,
	Suffix = iolist_to_binary(lists:duplicate(header_len(Len) - Len, 0)),
	H = <<Type:8, 1:8, ValType:8, 0, ValBin/binary, Suffix/binary>>,
	{Len, H}.

pack_value($s, V) -> L = byte_size(V), <<L:?uint32, V/binary, 0>>;
pack_value($g, V) -> L = byte_size(V), <<L:?byte, V/binary, 0>>;
pack_value($y, V) -> <<V:?byte>>;
pack_value($n, V) -> <<V:?int16>>;
pack_value($q, V) -> <<V:?uint16>>;
pack_value($i, V) -> <<V:?int32>>;
pack_value($u, V) -> <<V:?uint32>>;
pack_value($x, V) -> <<V:?int64>>;
pack_value($t, V) -> <<V:?uint64>>;
pack_value($d, V) -> <<V:?double>>;
pack_value($h, V) -> <<V:?unix_fd>>;
pack_value($o, V) -> pack_value($s, V).
