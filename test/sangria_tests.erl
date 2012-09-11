-module(sangria_tests).

-compile({parse_transform, sangria}).
-compile(export_all).


-record(node, {id, label}).
-record(edge, {id, label, source, target}).


set_label_before(L, X=#node{}) -> X#node{label = L};
set_label_before(L, X=#edge{}) -> X#edge{label = L}.

get_label_before(#node{label = L}) -> L;
get_label_before(#edge{label = L}) -> L.

set_label_after(L, X) -> with_any([node, edge], X#any{label = L}).

get_label_after(X) -> with_any([node, edge], X#any.label).


set_label_or_ignore(L, X) ->
    with_any([node, edge], X#any{label = L}, X).

set_label_or_error(L, X) ->
    with_any([node, edge], X#any{label = L}, error(bad_record)).

set_label_or_undefined(L, X) ->
    with_any([node, edge], X#any{label = L}, undefined).



-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

set_label_after_test_() ->
    [ ?_assertEqual(set_label_after(mike, #node{}), #node{label = mike})
    , ?_assertEqual(set_label_after(mike_joe, #edge{}), #edge{label = mike_joe})
    , ?_assertError({case_clause, node}, set_label_after(mike, node))
    ].

get_label_after_test_() ->
    [ ?_assertEqual(get_label_after(#node{label = mike}), mike)
    , ?_assertEqual(get_label_after(#edge{label = mike_joe}), mike_joe)
    , ?_assertError({case_clause, node}, get_label_after(node))
    ].

set_label_or_ignore_test_() ->
    [ ?_assertEqual(set_label_or_ignore(mike, #node{}), #node{label = mike})
    , ?_assertEqual(set_label_or_ignore(mike_joe, #edge{}),
                    #edge{label = mike_joe})
    , ?_assertEqual(set_label_or_ignore(mike, node), node)
    ].

set_label_or_error_test_() ->
    [ ?_assertEqual(set_label_or_error(mike, #node{}), #node{label = mike})
    , ?_assertEqual(set_label_or_error(mike_joe, #edge{}), #edge{label = mike_joe})
    , ?_assertError(bad_record, set_label_or_error(mike, node))
    ].

set_label_or_undefined_test_() ->
    [ ?_assertEqual(set_label_or_undefined(mike, #node{}), #node{label = mike})
    , ?_assertEqual(set_label_or_undefined(mike_joe, #edge{}),
                    #edge{label = mike_joe})
    , ?_assertEqual(set_label_or_undefined(mike, node), undefined)
    ].

-endif.

