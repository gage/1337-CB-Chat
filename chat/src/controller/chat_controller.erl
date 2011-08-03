-module(chat_controller, [Req]).
-compile(export_all).

list('GET', []) ->
    {ok, [{}]}.
