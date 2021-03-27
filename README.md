# tic-tac-toe-hs

Simple command line tic-tac-toe in Haskell.

```text
>> :main

   |   |
---+---+---
   |   |
---+---+---
   |   |

Enter number 1-9

```

To play, clone the repo and then run:

```shell
stack build
stack run
```

Or just run ghci and `:main`:

```shell
stack ghci
:main
```

To exit, just... ctrl + c 😬

**TODO**:

* detect win/tie state and exit
* don't allow cells to be overwritten
* UI?
* "AI" / 1 player mode?
