-module(edbus_data).

-export([pack/1]).
-export([unpack/1]).
-export([record_fields/1]).
-export([extend/1]).
-export([restrict/1]).
-export([msg_fill/3]).
-export([to_hex/1]).
-export([from_hex/1]).
-export([discharge/3]).
-export([endian/1]).
-export([msg_type/1]).
-export([header_type/1]).
-export([header_value_type/1]).
-export([set/3]).
-export([align_type_len/2]).
-export([header_len/1]).
-export([binary_reverse/1]).
-export([remove_trailing/2]).
-export([remove_char/2]).


-define(message_types, {dbus_call, dbus_reply, dbus_error, dbus_signal}).
-define(header_types, {path, interface, member, error_name, reply_serial, destination, sender, signature, unix_fds}).

-include("messages.hrl").

pack(Data) when is_tuple(Data) ->
	Endian = element(2, Data),
	Module = endian_module(Endian),
	PackedData = Module:pack(Data),
	<<Endian, PackedData/binary>>;
pack(Data) when is_list(Data) ->
	pack(Data, []).

pack([H | T], R) ->
	pack(T, [pack(H) | R]);
pack([], R) ->
	iolist_to_binary(lists:reverse(R)).

unpack(Msg) ->
	case unpack(Msg, []) of
		[Elem] -> Elem;
		Else -> Else
	end.

unpack(<<Endian:8, Data/binary>>, R) ->
	Module = endian_module(Endian),
	{Msg, Rest} = Module:unpack(Data),
	unpack(Rest, [Msg | R]);
unpack(<<>>, R) ->
	lists:reverse(R).

endian_module($l) -> edbus_data_little;
endian_module($B) -> edbus_data_big.

endian(little)	-> $l;
endian(big)			-> $B.

record_fields(dbus_message)	-> record_info(fields, dbus_message);
record_fields(dbus_call)		-> record_info(fields, dbus_call);
record_fields(dbus_reply)		-> record_info(fields, dbus_reply);
record_fields(dbus_error)		-> record_info(fields, dbus_error);
record_fields(dbus_signal)	-> record_info(fields, dbus_signal).

extend(#dbus_message{msg_type = MsgType} = Msg) ->
	[dbus_message | T] = tuple_to_list(Msg),
	[MsgType | T1] = lists:reverse(T),
	list_to_tuple([MsgType | extend(lists:reverse(T1), record_fields(MsgType), [])]).
extend([H1 | T1], [_Key | T2], R) ->
	extend(T1, T2, [H1 | R]);
extend([], [_Key | T], R) ->
	extend([], T, [undefined | R]);
extend([], [], R) ->
	lists:reverse(R).

restrict(Msg) when element(1, Msg) =/= dbus_message ->
	MsgType = element(1, Msg),
	{[MsgType | Head], [Headers, Body | Vals]} = lists:split(5, tuple_to_list(Msg)),
	{_, Keys} = lists:split(6, record_fields(MsgType)),
	list_to_tuple([dbus_message | Head] ++ [restrict(Keys, Vals, []) ++ Headers, Body, MsgType | []]);
restrict(#dbus_message{} = Msg) -> Msg.
restrict([_ | Keys], [undefined | Vals], R) ->
	restrict(Keys, Vals, R);
restrict([Key | Keys], [Val | Vals], R) ->
	restrict(Keys, Vals, [{Key, Val} | R]);
restrict([], [], R) ->
	R.

set(#dbus_message{} = Msg, serial, Value) -> Msg#dbus_message{serial = Value};
set(Msg, Field, Value) ->
	extend(set(restrict(Msg), Field, Value)).

msg_fill(#dbus_call{path = undefined} = Msg, [{path, Val} | T], Body) ->
	msg_fill(Msg#dbus_call{path = Val}, T, Body);
msg_fill(#dbus_call{member = undefined} = Msg, [{member, Val} | T], Body) ->
	msg_fill(Msg#dbus_call{member = Val}, T, Body);

msg_fill(#dbus_reply{reply_serial = undefined} = Msg, [{reply_serial, Val} | T], Body) ->
	msg_fill(Msg#dbus_reply{reply_serial = Val}, T, Body);

msg_fill(#dbus_error{error_name = undefined} = Msg, [{error_name, Val} | T], Body) ->
	msg_fill(Msg#dbus_error{error_name = Val}, T, Body);
msg_fill(#dbus_error{reply_serial = undefined} = Msg, [{reply_serial, Val} | T], Body) ->
	msg_fill(Msg#dbus_error{reply_serial = Val}, T, Body);

msg_fill(#dbus_signal{path = undefined} = Msg, [{path, Val} | T], Body) ->
	msg_fill(Msg#dbus_signal{path = Val}, T, Body);
msg_fill(#dbus_signal{interface = undefined} = Msg, [{interface, Val} | T], Body) ->
	msg_fill(Msg#dbus_signal{interface = Val}, T, Body);
msg_fill(#dbus_signal{member = undefined} = Msg, [{member, Val} | T], Body) ->
	msg_fill(Msg#dbus_signal{member = Val}, T, Body);

msg_fill(#dbus_call{headers = Headers} = Msg, [{Key, Val} | T], Body)
when Key =:= path; Key =:= member
			 ->
	lager:warning("~p is set in ~p", [Key, Msg]),
	msg_fill(Msg#dbus_call{headers = [{Key, Val} | Headers]}, T, Body);

msg_fill(Msg, [H | T], Body) ->
	{Head, [Headers | MsgTail]} = lists:split(5, tuple_to_list(Msg)),
	NewMsg = list_to_tuple(Head ++ [[H | Headers] | MsgTail]),
	msg_fill(NewMsg, T, Body);
msg_fill(Msg, [], Body) ->
	{Head, [<<>> | T]} = lists:split(6, tuple_to_list(Msg)),
	list_to_tuple(Head ++ [Body | T]).

to_hex(D) when is_binary(D) ->
	<< <<(to16(X))/integer>> || <<X:4>> <= D >>.
to16(B) when B >= 0, B =< 9 -> $0 + B;
to16(B) when B < 16 -> B rem 10 + $a.

from_hex(D) when is_binary(D) ->
	<< <<(from16(X, Y))/integer>> || <<X:8, Y:8>> <= D >>.
from16(A, B) -> 16 * from16(A) + from16(B).
from16(A) when A >= $0, A =< $9 -> A - $0;
from16(A) when A >= $a, A =< $f -> A - $a + 10;
from16(A) when A >= $A, A =< $F -> A - $A + 10.

discharge(Len, Separator, Data) when Len > 0 -> discharge(Len, Separator, Data, <<>>).
discharge(Len, Separator, Data, Result) when Len > 0, byte_size(Data) > Len ->
	<<D:Len/binary, Rest/binary>> = Data,
	discharge(Len, Separator, Rest, <<Result/binary, D/binary, Separator/binary>>);
discharge(_, Separator, Data, Result) -> <<Result/binary, Separator/binary, Data/binary>>.

list_position(E, L) -> list_position(E, L, 1).

list_position(E, [E | _], N) -> N;
list_position(E, [_ | T], N) -> list_position(E, T, N + 1);
list_position(_, [], _) -> 0.

msg_type(I) when is_integer(I) -> element(I, ?message_types);
msg_type(T) when is_atom(T)	-> list_position(T, tuple_to_list(?message_types)).

header_type(I) when is_integer(I) -> element(I, ?header_types);
header_type(T) when is_atom(T) -> list_position(T, tuple_to_list(?header_types)).

header_value_type(T)
	when
		T =:= path
		-> $o;
header_value_type(T)
	when
		T =:= reply_serial;
		T =:= unix_fds
		-> $u;
header_value_type(T)
	when
		T =:= signature
		-> $g;
header_value_type(_)
		-> $s.

alignment($s) -> 4;
alignment($y) -> 1;
alignment($n) -> 2;
alignment($q) -> 2;
alignment($i) -> 4;
alignment($u) -> 4;
alignment($x) -> 8;
alignment($t) -> 8;
alignment($d) -> 8;
alignment($o) -> alignment($s);
alignment($v) -> 1;
alignment($h) -> 4.

align_type_len(L, {value, T}) ->
	align_len(L, alignment(T));
align_type_len(L, _) ->
	L.

align_len(L, Align) when L rem Align =:= 0	-> L;
align_len(L, Align)													-> L + (Align - L rem Align).

header_len(L) -> align_len(L, 8).

remove_trailing(D, Data) -> binary_reverse(remove_char(D, binary_reverse(Data))).

remove_char(D, <<D, Data/binary>>) ->
	remove_trailing(D, Data);
remove_char(_, Data) -> binary_reverse(Data).

binary_reverse(Data) ->
	L = bit_size(Data),
	<<I:L/big-integer>> = Data,
	<<I:L/little-integer>>.
