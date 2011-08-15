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

get_time('GET', [RoomName]) ->
    {json, [{timestamp, boss_mq:now(RoomName)}]}.

pull('GET', [RoomName, Tt]) ->
    io:fwrite("4444:~s,~s\n", [RoomName, Tt]),
    case boss_mq:pull(RoomName, list_to_integer(Tt)) of
        {ok, Timestamp, Messages} ->
            io:fwrite("gggggggggg:~62p\n", [Messages]),
            {json, [{messages,Messages}, {timestamp,Timestamp}]};
        Other ->
            io:fwrite("eeeeeeeee:~62p\n", [Other]),
            {json, [{error, "error"}]}
    end.
    %{ok, Timestamp, Messages} = boss_mq:pull(RoomName, list_to_integer(Tt)),
    %{ok, Timestamp, Messages} = boss_mq:pull(RoomName),
    %io:fwrite("gggggggggg:~62p\n", [Messages]),
    %{json, [{messages,Messages}, {timestamp,Timestamp}]}.

    %case Messages of
    %    [] -> {json, [{message,""}, {timestamp,Timestamp}]};
    %    [Msg|_] -> {json, [{message,Msg}, {timestamp,Timestamp}]}
    %end.

    %[Msg|_] = Messages,
    %io:fwrite("3333333:~s,~s,~s\n", [RoomName, Tt, Msg]),
    %{json, [{message,Messages}, {timestamp,Timestamp}]}.

message('POST', []) ->
    RoomName = Req:post_param("room_name"),
    UserName = Req:post_param("user_name"),
    Message = Req:post_param("msg"),
    io:fwrite("1111111:~s,~s,~s\n", [RoomName,UserName,Message]),
    io:fwrite("2222222:~s\n", [lists:concat([UserName,":",Message])]),
    {ok, Timestamp} = boss_mq:push(RoomName, [{username, UserName}, {msg, Message}]),
    {output, <<"success">>}.

say('GET', []) ->
    {ok, Timestamp, Messages} = boss_mq:pull("mtc", 'now'),
    {output, Messages}.
