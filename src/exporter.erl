%%%-------------------------------------------------------------------
%%% @doc
%%% export module local function
%%% @end
%%%-------------------------------------------------------------------
-module(exporter).
-export([export/2, export/3, export/4, export/5]).
-export([list_locals/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @dod export all matched Module:Function
-spec export(Module :: module(), Function :: atom()) -> {ok, module(), binary()} | {error, term()}.
export(Module, Function) ->
    export(Module, Function, 0).

%% @dod export all matched Module:Function
-spec export(Module :: module(), Function :: atom(), Arity :: non_neg_integer()) -> {ok, module(), binary()} | {error, term()}.
export(Module, Function, Arity) ->
    {_, LoadedBinary, File} = code:get_object_code(Module),
    export(Module, Function, Arity, LoadedBinary, File).

%% @dod export all matched Module:Function
-spec export(Module :: module(), Function :: atom(), Arity :: non_neg_integer(), Binary :: binary()) -> {ok, module(), binary()} | {error, term()}.
export(Module, Function, Arity, Binary) ->
    File = code:which(Module),
    export(Module, Function, Arity, Binary, File).

%% @dod export Module:Function/Arity
-spec export(Module :: module(), Function :: atom(), Arity :: arity(), LoadedBinary :: binary(), File :: file:filename()) -> {ok, module(), binary()} | {error, term()}.
export(Module, Function, Arity, LoadedBinary, File) ->
    {ok, _, Chunks} = beam_lib:all_chunks(LoadedBinary),
    {ok, {_, [{atoms, Atoms}, {labeled_exports, Exports}, {labeled_locals, Locals}]}} = beam_lib:chunks(LoadedBinary, [atoms, labeled_exports, labeled_locals]),
    %% build and replace atom table into beam dict
    Dict = setelement(2, beam_dict:new(), lists:foldl(fun({I, A}, M) -> maps:put(A, I, M) end, maps:new(), Atoms)),
    %% supply previous exports
    LoadedExports = Module:module_info(exports),
    %% split export and local
    {NewExport, NewLocal} = lists:partition(fun({F, A, _}) -> (F == Function andalso A == Arity) orelse (F == Function andalso Arity == 0) orelse lists:member({F, A}, LoadedExports) orelse (lists:keymember(F, 1, LoadedExports) andalso Arity == 0) end, Locals),
    %% export
    ExportDict = lists:foldl(fun({F, A, E}, D) -> beam_dict:export(F, A, E, D) end, Dict, NewExport ++ Exports),
    {NumberExports, ExportTable} = beam_dict:export_table(ExportDict),
    ExportChunk = <<NumberExports:32, (<<<<F:32, A:32, L:32>> || {F, A, L} <- ExportTable>>)/binary>>,
    %% local
    LocalDict = lists:foldl(fun({F, A, E}, D) -> beam_dict:local(F, A, E, D) end, Dict, NewLocal),
    {NumberLocals, LocalTable} = beam_dict:local_table(LocalDict),
    LocalChunk = <<NumberLocals:32, (<<<<F:32, A:32, L:32>> || {F, A, L} <- LocalTable>>)/binary>>,
    %% replace
    FinalChunks = lists:keyreplace("ExpT", 1, lists:keyreplace("LocT", 1, Chunks, {"LocT", LocalChunk}), {"ExpT", ExportChunk}),
    %% rebuild module binary
    {ok, Binary} = beam_lib:build_module(FinalChunks),
    %% stick module supported
    code:unstick_mod(Module),
    %% reload module
    case code:load_binary(Module, File, Binary) of
        {module, Module} ->
            {ok, Module, Binary};
        Error ->
            Error
    end.

%% @dod list locals
-spec list_locals(Module :: module()) -> [{atom(), non_neg_integer(), non_neg_integer()}].
list_locals(Module) ->
    File = code:which(Module),
    %% exporter
    {ok, {_, [{labeled_locals, Local}]}} = beam_lib:chunks(File, [labeled_locals]),
    Local.

%%%===================================================================
%%% Internal functions
%%%===================================================================
