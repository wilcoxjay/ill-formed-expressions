# Learning to love ill-formed expressions

Consider the problem of indexing into a list.
So, a function of type

    list a -> int -> a

What should be done about out-of-bounds indices?

* throw an exception
* return an option
* take a default value to return
* require proof that the index is in bounds

In a language like Coq, exceptions are not primitively available. The other
three solutions are possible and used in practice, but they all have downsides.

First, they all change the type of the index function, either to return an option,
or take an additional argument (either a default value or a proof).

Returning an option or taking a default value is cumbersome when one
"knows" the index is bounds, because one is still forced to consider
the case that it might not be.

Returning an option is especially cumbersome, because callers are forced to handle
a return type that is not the same as list elements. They may have to introduce
pattern matches to "extract" the value from the option, and then do something
reasonable in the case when none is returned.

When taking a default value, one also typically
needs a lemma that says "if the index is in bounds, then the default value doesn't matter",
to allow reasoning about applications of the index function with different default values.

Requiring a proof that the index is in bounds in Coq is also cumbersome because it
typically requires explicit manipulation of proof terms. Furthermore, the function
typically needs to be shown to be proof irrelevant, so that it is easy to manipulate
it as a function of just the non-proof arguments.

In Dafny, there is another way to implement the fourth solution, by
using a partial function (a function with a nontrivial precondition).
Callers of the function are obliged to prove the precondition at all callsites.

A major advantage of the Dafny version of this solution is that *it
does not change the type of the operation*.

Dafny implements these precondition checks as part of a so-called
"well-formedness" check on all programs, which is conceptually
separate and prior to verification. Like verification conditions, the
well-formedness check is dispatched to Z3. Since most well-formedness
checks are obvious, this works well.

A Dafny expression is well formed if (roughly speaking) the preconditions
of all partial functions called by the expression are true.

In practice, expressions with well-formedness checks are convenient to work with, because:

* They allow functions to pass the obligation of handling some edge
  case to their caller.

  For example, by giving the list indexing operator the appropriate precondition,
  the implementation can avoid saying what to do if the index is out of bounds.

* They allow modeling only *some* of the behavior of an external system.

  For example, we can expose a low-level, unsafe array indexing operator by
  saying that it is an external function with a precondition requiring the index
  to be in bounds.

* The types of the operations don't change, allowing natural-looking programs
  that compose well.


## Deliverables

* Syntax and operational semantics of ILLPL, a core calculus based on Dafny that
  supports verification and well-formedness checks.

* A formalization of the well-formedness check and VC generation for ILLPL.

* A soundness theorem saying that verified programs don't go wrong.

* A translation of ILLPL into WELLPL, a core calculus based on Boogie, in which
  well-formedness is syntactic.

* A proof that the translation correctly translates ILLPL well-formedness into
  WELLPL verification conditions. Thus, the translation gives a way to implement
  ILLPL using traditional FOL techniques without any notion of ill-formed
  expressions.

## Motivating Example: Insertion Sort on Lists of Integers

    fun insert(a, xs) =
        if xs == [] then
            [a]
        else if a <= xs[0] then  // notice that xs[0] is allowed here because
                                 // we are in the else branch of xs == []
            [a] + xs
        else
            [xs[0]] + insert(a, xs[1..])

    fun sort(xs) =
        if xs == [] then
            []
        else
            insert(xs[0], sort(xs[1..]))

    fun sorted(xs) =
        // note xs[i] and xs[i+1] wf due to range hypothesis
        forall i. 0 <= i < |xs|-1 ==> xs[i] <= xs[i+1]

    fun insert_sorted(a, xs)
        requires sorted(xs)
        ensures _. sorted(insert(a, xs))
    =
        if xs == [] then
            ()
        else if a <= xs[0] then
            ()
        else
            insert_sorted(a, xs[1..])

    fun sorted_sorted(xs)
        ensures sorted(sort(xs))
    =
        if xs == [] then
            ()
        else
            let _ = sort_sorted(xs[1..])  // establishes insert_sorted precondition
            in insert_sorted(xs[0], sort(xs[1..]))

## Syntax and semantics of ILLPL

    x  \in  variable name
    f  \in  function name
    n  \in  Z

    op \in  total binary operators
    op ::=  + | == | <= | <

    e  \in  expression
    e  ::=  x                       // variable
         |  n                       // integer literal
         |  ()                      // unit
         |  [e, ..., e]             // list literal (0 or more elements)
         |  e op e                  // (total) binary operators
         |  e && e                  // short-circuiting conjunction
         |  e ==> e                 // short-circuiting implication
         |  e[e]                    // list indexing
         |  e[e..]                  // list slicing ('..' are part of syntax)
         |  f(e, ..., e)            // function call (0 or more arguments)
         |  if e then e else e      // conditional expression
         |  let x = e in e          // let expression (x bound in second expr)
         |  forall x. e             // universal quantifier (binds x in e)
         |  exists x. e             // existential quantifier (binds x in e)

    b  \in  binding
    b  ::=  fun f(x, ..., x)        // function binding (poss. recursive; 0 or more params)
                requires e          //     function precondition
                ensures r. e        //     function postcondition (binds r to result)
            = e                     //     function body
         |  val x = e               // top-level value binding (think: main expression)

