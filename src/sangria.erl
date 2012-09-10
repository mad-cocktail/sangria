-module(sangria).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    F = fun search_and_transform_function_call_with_any/1,
    X = [erl_syntax_lib:map(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.

search_and_transform_function_call_with_any(Node) ->
    case is_local_function(Node, with_any) of
        true ->
            {RecNames, Action, AltAction}
                 = decode_arguments(erl_syntax:application_arguments(Node)),
            F = search_and_transform_record_access_any(RecNames, AltAction),
            erl_syntax_lib:map(F, Action);
        false -> Node
    end.

decode_arguments([RecNames, Action | Args]) ->
    RecNameValues = decode_arguments(RecNames),
    AltAction = maybe_optional_argument(Args),
    {RecNameValues, Action, AltAction}.

search_and_transform_record_access_any(RecNameValues, AltAction) ->
    fun(Node) ->
            case node_type(Node) of
                _OtherType -> io:format(user, "~nOtherType: ~p~n", [_OtherType])
            end
        end.

maybe_optional_argument([]) -> undefined;
maybe_optional_argument([X]) -> X.


decode_record_names(RecNames) ->
    %% Read a list of atoms.
    [erl_syntax:atom_value(Node) || Node <- erl_syntax:list_elements(RecNames)].


-spec is_local_function(Node, FunName) -> boolean() when
    Node :: erl_syntax:syntaxTree(),
    FunName :: atom().

is_local_function(Node, FunName) ->
    node_type(Node) =:= application
        andalso always(Op = erl_syntax:application_operator(Node))
        andalso node_type(Op) =:= atom
        andalso erl_syntax:atom_value(Op) =:= FunName.


always(_) -> true.

node_type(Node) -> erl_syntax:type(Node).
