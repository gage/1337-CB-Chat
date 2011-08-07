-module(chat_chat_controller, [Req]).
-compile(export_all).

-record(msg,{name, content}).

list('GET', [Iid]) ->
    {ok, [{iid, Iid}]}.

%room('GET', []) ->
%    {ok, [{user_name, <<"Anonymous">>}]}.

room('GET', [RoomName]) ->
    UserName = Req:query_param("name"),
    %{ok, Timestamp, Messages} = boss_mq:pull(RoomName, now),
    {ok, [{user_name, UserName}, {room_name, RoomName}]}.

pull('GET', [RoomName]) ->
    {ok, Timestamp, Messages} = boss_mq:pull(RoomName, now),
    {output, Messages}.

message('POST', []) ->
    RoomName = Req:post_param("room_name"),
    UserName = Req:post_param("user_name"),
    Message = Req:post_param("msg"),
    io:fwrite("1111111:~s,~s,~s\n", [RoomName,UserName,Message]),
    {ok, Timestamp} = boss_mq:push(RoomName, lists:concat([UserName,":",Message])),
    {output, <<"success">>}.

