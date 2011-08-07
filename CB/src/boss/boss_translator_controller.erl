-module(boss_translator_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {strings, blocks, application}).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    BossApp = proplists:get_value(application, Options),
    StringDictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_strings(BossApp, Lang))}
        end, boss_files:language_list(BossApp)),
    BlockDictionaryList = lists:map(fun(Lang) ->
                {Lang, dict:from_list(boss_lang:extract_po_blocks(BossApp, Lang, comment))}
        end, boss_files:language_list(BossApp)),
    {ok, #state{
            strings = dict:from_list(StringDictionaryList), 
            blocks = dict:from_list(BlockDictionaryList),
            application = BossApp }}.

handle_call({lookup, Key, Locale}, _From, State) when is_atom(Key) ->
    Return = case dict:find(Locale, State#state.blocks) of
        {ok, Dict} ->
            case dict:find(atom_to_list(Key), Dict) of
                {ok, Trans} -> Trans;
                error -> undefined
            end;
        _ ->
            undefined
    end,
    {reply, Return, State};
handle_call({lookup, Key, Locale}, _From, State) ->
    Return = case dict:find(Locale, State#state.strings) of
        {ok, Dict} ->
            case dict:find(Key, Dict) of
                {ok, Trans} -> Trans;
                error -> undefined
            end;
        _ ->
            undefined
    end,
    {reply, Return, State};

handle_call({is_loaded, Locale}, _From, State) ->
    {reply, dict:is_key(Locale, State#state.strings), State};

handle_call({reload, Locale}, _From, State) ->
    StringDict = dict:from_list(boss_lang:extract_po_strings(State#state.application, Locale)),
    BlockDict = dict:from_list(boss_lang:extract_po_blocks(State#state.application, Locale, comment)),
    NewState = State#state{
        strings = dict:store(Locale, StringDict, State#state.strings),
        blocks = dict:store(Locale, BlockDict, State#state.blocks)
    },
    {reply, ok, NewState};
handle_call(reload_all, From, State) ->
    NewState = lists:foldr(fun(X, StateAcc) -> 
                {reply, ok, State1} = handle_call({reload, X}, From, StateAcc),
                State1
        end, State, boss_files:language_list(State#state.application)),
    {reply, ok, NewState}.


handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.