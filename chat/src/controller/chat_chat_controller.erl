-module(chat_chat_controller, [Req]).
-compile(export_all).

-record(msg,{name, content}).

list('GET', [Iid]) ->
    {ok, [{iid, Iid}]}.

%room('GET', []) ->
%    {ok, [{user_name, <<"Anonymous">>}]}.

room('GET', [RoomName]) ->
    UserName = Req:query_param("name"),
    {ok, [{user_name, UserName}, {room_name, RoomName}, {timestamp, boss_mq:now(RoomName)}]}.

pull('GET', [RoomName, Tt]) ->
    io:fwrite("4444:~s,~s\n", [RoomName, Tt]),
    {ok, Timestamp, Messages} = boss_mq:pull(RoomName, list_to_integer(Tt)),
    [Msg|_] = Messages,
    io:fwrite("3333333:~s,~s,~s\n", [RoomName, Tt, Msg]),
    {json, [{message,Msg}, {timestamp,Timestamp}]}.

message('POST', []) ->
    RoomName = Req:post_param("room_name"),
    UserName = Req:post_param("user_name"),
    Message = Req:post_param("msg"),
    io:fwrite("1111111:~s,~s,~s\n", [RoomName,UserName,Message]),
    io:fwrite("2222222:~s\n", [lists:concat([UserName,":",Message])]),
    {ok, Timestamp} = boss_mq:push(RoomName, lists:concat([UserName,":",Message])),
    {output, <<"success">>}.

