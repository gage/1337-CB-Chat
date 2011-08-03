-module(chat_chat_controller, [Req]).
-compile(export_all).

list('GET', [Iid]) ->
    {ok, [{iid, Iid}]}.
