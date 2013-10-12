-define(common_fields, endian = $l, flags = 0, protocol = 1, serial, headers = [], body = <<>>).

-record(dbus_call,	{
					?common_fields,
					path,
					member}).
-record(dbus_reply,{
					?common_fields,
					reply_serial}).
-record(dbus_error,				{
					?common_fields,
					error_name,
					reply_serial}).
-record(dbus_signal,				{
					?common_fields,
					path,
					interface,
					member}).
-record(dbus_message, {
					?common_fields,
					msg_type = 0}).
