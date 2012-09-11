-module(sangria).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    F = fun search_and_transform_function_call_with_any/1,
    X = [erl_syntax:revert(erl_syntax_lib:map(F, Tree)) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.

search_and_transform_function_call_with_any(Node) ->
    case is_local_function(Node, with_any) of
        true ->
            {RecNames, Action, AltAction}
            = decode_arguments(to_list(erl_syntax:application_arguments(Node))),
            F = search_and_transform_record_action_any(RecNames, AltAction),
            erl_syntax_lib:map(F, Action);
        false -> Node
    end.

to_list(X) when is_list(X) -> X;
to_list(X) -> erl_syntax:list_elements(X).

decode_arguments([RecNames, Action | Args]) ->
    RecNameValues = erl_syntax:list_elements(RecNames),
    AltAction = maybe_optional_argument(Args),
    {RecNameValues, Action, AltAction}.

maybe_optional_argument([]) -> undefined;
maybe_optional_argument([X]) -> X.


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


%% ----------------------------------------------------------------------
%% Search and transform the record actions with `#any{}'.
%% ----------------------------------------------------------------------

%% Result of this fun used with erl_syntax_lib:map/2.
-spec search_and_transform_record_action_any(RecNames, AltAction) -> GenFun when
    RecNames :: [Tree],
    AltAction :: Tree,
    GenFun :: fun((Tree) -> Tree),
    Tree :: erl_syntax:syntaxTree().
    
search_and_transform_record_action_any(RecNames, AltAction) ->
    fun(Node) ->
            case node_type(Node) of
                record_expr -> %% X#node{f1 = Y}
                    handle_record_expr_any(RecNames, AltAction, Node);
                record_access -> %% X#node.f1
                    handle_record_access_any(RecNames, AltAction, Node);
                _OtherType -> Node
            end
        end.


%% ----------------------------------------------------------------------
%% Handle the record expression with `#any{}'.
%% ----------------------------------------------------------------------

handle_record_expr_any(RecNames, AltAction, Node) ->
    RecType = erl_syntax:atom_value(erl_syntax:record_expr_type(Node)),
    case RecType of
        any -> %% #any.field
            Arg = erl_syntax:record_expr_argument(Node),
            F = generate_case_body_for_record_expr_any(Node),
            Clauses = generate_record_match(RecNames, AltAction, F),
            copy_pos(Node, erl_syntax:case_expr(Arg, Clauses));
        _OtherRecType ->
            Node
    end.


generate_case_body_for_record_expr_any(OldNode) ->
    %% Arg#any{Fields}
    Arg = erl_syntax:record_expr_argument(OldNode),
    Fields = erl_syntax:record_expr_fields(OldNode),
    fun(Name) ->
            [copy_pos(Name, erl_syntax:record_expr(Arg, Name, Fields))]
        end.


%% ----------------------------------------------------------------------
%% Handle the record access with `#any{}'.
%% ----------------------------------------------------------------------

handle_record_access_any(RecNames, AltAction, Node) ->
    RecType = erl_syntax:atom_value(erl_syntax:record_access_type(Node)),
    case RecType of
        any -> %% #any.field
            Arg = erl_syntax:record_access_argument(Node),
            F = generate_case_body_for_record_access_any(Node),
            Clauses = generate_record_match(RecNames, AltAction, F),
            copy_pos(Node, erl_syntax:case_expr(Arg, Clauses));
        _OtherRecType ->
            Node
    end.


generate_case_body_for_record_access_any(OldNode) ->
    %% Arg#any{Fields}
    Arg = erl_syntax:record_access_argument(OldNode),
    Field = erl_syntax:record_access_field(OldNode),
    fun(Name) ->
            [copy_pos(Name, erl_syntax:record_access(Arg, Name, Field))]
        end.


-spec generate_record_match(RecNames, AltAction, BodyGen) -> Clauses when
    RecNames :: [RecName],
    AltAction :: Tree | undefined,
    BodyGen :: fun((RecName) -> Tree),
    RecName :: Tree,
    Tree :: erl_syntax:syntaxTree(),
    Clauses :: [Tree].

generate_record_match(RecNames, AltAction, BodyGen) ->
    CF = fun erl_syntax:clause/3,
    Clauses = [CF([rec(Name)], none, BodyGen(Name)) || Name <- RecNames],
    case AltAction of
        undefined -> Clauses;
        _ -> Clauses ++ [CF([erl_syntax:underscore()], none, [AltAction])]
    end.


%% ``#Name{}``.
rec(Name) -> copy_pos(Name, erl_syntax:record_expr(none, Name, [])).




%% @doc Copy positional information from `From' node to `To' node.
-spec copy_pos(From, To) -> To when
    From :: erl_syntax:syntaxTree(),
    To :: From.

copy_pos(From, To) ->
    Pos = erl_syntax:get_pos(From),
    erl_syntax:set_pos(To, Pos).

