# Introducing `pipes` package

The package `mmpipe` had similar features but was based on hacks modifying
`magrittr`'s namespace. The package `pipes` takes a much cleaner approach.

The most important differences are :
* The pipe operators contain the info about what they do, it's not burried into
the code through symbol recognition
* pipe can be easily created and removed, as regular objects
* pipes gain a class `pipe` and a printing method
* some pipes were renamed, and some others were added or combined