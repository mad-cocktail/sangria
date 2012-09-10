-module(sangria_tests).

-compile({parse_transform, sangria_tests}).
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

