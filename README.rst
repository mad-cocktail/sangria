Sangria: generalized record access
==================================

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/sangria.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/sangria


Installation
------------

Add this repository as dependence of your project::

    {deps, [                                                                       
       {sangria,  ".*", {git, "git://github.com/mad-cocktail/sangria.git", "HEAD"}}  
       ]}.                                                                            

Include this command into each file, where you want to use this parse
transform:

.. code-block:: erlang

    -compile({parse_transform, sangria}).

Idea
----

If you need to set same field for different records, you can write something
like this:

.. code-block:: erlang

    set_label(L, X=#node{}) -> X#node{label = L};
    set_label(L, X=#edge{}) -> X#edge{label = L}.

    get_label(#node{label = L}) -> L;
    get_label(#edge{label = L}) -> L.

The idea of this parse transform is to let the compiper to do this work for us:

.. code-block:: erlang

    set_label(L, X) -> with_any([node, edge], X#any{label = L}).

    get_label(X) -> with_any([node, edge], X#any.label).

The first argument is a list of the record names and the second one is an
generalized action with ``X#any``, where ``X`` is one of the records.

If nothing matched, then the ``badmatch`` error will occured.

If you want to handle unmathed values, then use the 3rd argument:

.. code-block:: erlang

    set_label_or_ignore(L, X) -> 
        with_any([node, edge], X#any{label = L}, X).

    set_label_or_error(L, X) -> 
        with_any([node, edge], X#any{label = L}, error(bad_record)).

    set_label_or_undefined(L, X) ->
        with_any([node, edge], X#any{label = L}, undefined).

The third argument will be evaluated, if nothing matched. It is disallowed to
use ``#any`` record inside this argument.


Mix Rum and Sangria
-------------------

If you want to use Rum, than this parse transform must be executed before Rum:

.. code-block:: erlang

    -compile({parse_transform, sangria}).
    -compile({parse_transform, rum}).

