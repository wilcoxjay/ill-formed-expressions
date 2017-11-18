// RUN: /nologo /rlimit:100000

module InsertionSort {

    function insert(a: int, xs: seq<int>): seq<int>
    {
        if xs == [] then
            [a]
        else if a <= xs[0] then
            [a] + xs
        else
            [xs[0]] + insert(a, xs[1..])
    }

    function sort(xs: seq<int>): seq<int>
    {
        if xs == [] then
            []
        else
            insert(xs[0], sort(xs[1..]))
    }

    predicate sorted(xs: seq<int>)
    {
        forall i | 0 <= i < |xs|-1 :: xs[i] <= xs[i+1]
    }

    // Note: This is not idiomatic Dafny; typically one would use lemmas,
    // but using functions instead keeps things more expression-oriented.

    function insert_sorted(a: int, xs: seq<int>): ()
        requires sorted(xs)
        ensures sorted(insert(a, xs))
    {
        if xs == [] then
            ()
        else if a <= xs[0] then
            ()
        else
            insert_sorted(a, xs[1..])

    }

    function sort_sorted(xs: seq<int>): ()
        ensures sorted(sort(xs))
    {
        if xs == [] then
            ()
        else
            var _ := sort_sorted(xs[1..]);
            insert_sorted(xs[0], sort(xs[1..]))
    }

}
