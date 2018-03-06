# haskellyfizzbuzz

A Haksell version of the common fizzbuzz exercise, expanded to allow a custom
set of words and multiples to be used. This also includes a full suite of hspec
tests.

The original fizzbuzz function was unceremoniously stolen from a slide
I saw from a talk by Kevlin Henney. Also, the tests were adapted from another
slide from the same talk. So much kudos to him.

As an exercise, I expanded it to include "Hiss" and "Howl", another common
adaptation. In doing so, I noticed it would be possible to fully abstract
out the words and multiples to be used, in both the function and the tests.

The code can be compiled via `stack build`, executed via
`stack exec haskellyfizzbuzz` and the tests can be run via `stack test`
