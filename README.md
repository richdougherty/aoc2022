I'm learning Racket while I do the Advent exercises. These are my notes as I go.

# Diary

## [Day 1: Calorie Counting](https://adventofcode.com/2022/day/1)

[Code](01/calorie-counting.rkt)

* My first day - I had to look up how to define functions, bind variables and do conditionals - pretty much everything!
* I used `rackunit` to check funciton outputs as I went so I could manage the problem.
* [An example](https://www.reddit.com/r/adventofcode/comments/z9ezjb/comment/iyjhev8/) using [threading macros](https://docs.racket-lang.org/threading/index.html).
* I also saw an example using `for/fold` which looks much nicer than using `foldl`.

## [Day 2: Rock Paper Scissors](https://adventofcode.com/2022/day/2)

[Code](02/rock-paper-scissors.rkt)

* Learned about hash tables today.

## [Day 3: Rucksack Reorganization](https://adventofcode.com/2022/day/3)

[Code](03/rucksack-reorganization.rkt)

* I made fewer, larger functions this time. Used `for`, which made code a lot more readable.
* Learned about multiple value returns and `let-value`.
* I'm going to try [contracts](https://docs.racket-lang.org/guide/contracts.html) next time to 

## [Day 4: Camp Cleanup](https://adventofcode.com/2022/day/4)

[Code](03/rucksack-reorganization.rkt)

* I used [structs](https://docs.racket-lang.org/guide/define-struct.html) for the first time.
* I used [pattern matching](https://docs.racket-lang.org/reference/match.html) to destructure the results of string splitting - and verify the length at the same time.

## [Day 5: Supply Stacks](https://adventofcode.com/2022/day/5)

* Using curry instead of custom lambdas

[Code](05/supply-stacks.rkt)

# Racket links

* Racket docs
  * [Define](https://docs.racket-lang.org/reference/define.html)
  * [Let](https://docs.racket-lang.org/reference/let.html)
  * [Pairs & Lists](https://docs.racket-lang.org/reference/pairs.html)
  * [Pattern Matching](https://docs.racket-lang.org/reference/match.html) [(Guide)](https://docs.racket-lang.org/guide/match.html)
  * [Strings](https://docs.racket-lang.org/reference/strings.html)
  * [File](https://docs.racket-lang.org/reference/Filesystem.html)
  * [For](https://docs.racket-lang.org/reference/for.html) [(Guide)](https://docs.racket-lang.org/guide/for.html)
  * [Hash Tables](https://docs.racket-lang.org/reference/hashtables.html) [(Guide)](https://docs.racket-lang.org/guide/hash-tables.html)
  * Functional
    * [Interfaces](https://docs.racket-lang.org/functional/interfaces.html)
    * [Maybe](https://docs.racket-lang.org/functional/maybe.html)
* Beautiful Racket
  * [Booleans and Conditionals](https://beautifulracket.com/explainer/booleans-and-conditionals.html)