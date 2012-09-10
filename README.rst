Sangria: generalized record access
==================================

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/sangria.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/sangria


Why?
----

If you need to set same field for different records, you can write something
like this::

    set_label(L, X=#node{}) -> X#node{label = L};
    set_label(L, X=#edge{}) -> X#edge{label = L}.

    get_label(#node{label = L}) -> L;
    get_label(#edge{label = L}) -> L.

The idea of this parse transform is to let the compiper to do this work for us::

    set_label(L, X) -> with_any([node, edge], X#any{label = L}).

    get_label(X) -> with_any([node, edge], X#any.label).

The first argument is a list of the record names and the second one is an
generalized action with ``X#any``, where ``X`` is one of the records.

If nothing matched, then the ``badmatch`` error will occured.

If you want to handle unmathed values, then use the 3rd argument::

    set_label_or_ignore(L, X) -> 
        with_any([node, edge], X#any{label = L}, X).

    set_label_or_error(L, X) -> 
        with_any([node, edge], X#any{label = L}, error(bad_record)).

    set_label_or_undefined(L, X) ->
        with_any([node, edge], X#any{label = L}, undefined).

The third argument will be evaluated, if nothing matched. It is disallowed to
use ``#any`` record inside this argument.

This parse transform must be used before Rum.


Why were this application and Rum separated?
--------------------------------------------

It was made for flexibility. You can include some other parse transform
beetween them or to use only one of them.


Yet another parse transform?
----------------------------

I am sorry, but I really wanted to avoid this.

First, I wrote the same code for 10 different records, then I tried to use
macroses. Finally, I thought, that the PT is a good tool for this job.

Don't use it, when you have only few records and nothing generalized beetween
them.
