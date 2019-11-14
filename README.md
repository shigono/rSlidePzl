
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rSlidePzl

<!-- badges: start -->

<!-- badges: end -->

`rSlidePzl` package provides some funtions to analyze [sliding
puzzle](https://en.wikipedia.org/wiki/Sliding_puzzle).

A main workhouse of this package is `makeGraph` function, by which you
can make a network graph representing possible ‘states’ (i.e. snapshots
of the board) and transition relationships between them.

You can examine the property of the graph with [`igraph`
package](https://igraph.org/r/), or any other tools for network analysis
like [Gephi](https://gephi.org/).

## Installation

``` r
# install.packages("devtools")
devtools::install_github("shigono/rSlidePzl")
```

## Example

Suppose you have a simple sliding puzzle with 8 pieces. The size of
board is 3 x 3. The size of all pieces is 1 x 1. The initial state is:

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    | C    |      |
| row2 | D    | B    | E    |
| row3 | F    | G    | F    |

Your goal is:

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    | B    | C    |
| row2 | D    | E    | F    |
| row3 | G    | H    |      |

Let’s try to analyse this puzzle with `rSlidePzl` package.

### make a network graph

``` r
library(rSlidePzl)
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
#> Registered S3 methods overwritten by 'huge':
#>   method    from   
#>   plot.sim  BDgraph
#>   print.sim BDgraph

# make setting of a sliding puzzle
oSetting <- makeSetting(
  boardsize = c(3,3),
  piecesize = list(
    A = c(1, 1),
    B = c(1, 1),
    C = c(1, 1),
    D = c(1, 1),
    E = c(1, 1),
    F = c(1, 1),
    G = c(1, 1),
    H = c(1, 1)
  )
)
# make an initial state
oStart <- makeState(
  list(
    makePiece(type = "A", loc = c(1,1)),
    makePiece(type = "B", loc = c(2,2)),
    makePiece(type = "C", loc = c(1,2)),
    makePiece(type = "D", loc = c(2,1)),
    makePiece(type = "E", loc = c(2,3)),
    makePiece(type = "F", loc = c(3,3)),
    makePiece(type = "G", loc = c(3,1)),
    makePiece(type = "H", loc = c(3,2))
  )
)
stopifnot(isValidState(oStart, oSetting))

# define conditions of goal
oGoalCondition <- makeState(
  list(
    makePiece(type = "A", loc = c(1,1)),
    makePiece(type = "B", loc = c(1,2)),
    makePiece(type = "C", loc = c(1,3)),
    makePiece(type = "D", loc = c(2,1)),
    makePiece(type = "E", loc = c(2,2)),
    makePiece(type = "F", loc = c(2,3)),
    makePiece(type = "G", loc = c(3,1)),
    makePiece(type = "H", loc = c(3,2))
  )
)
stopifnot(isValidState(oGoalCondition, oSetting))

# find all states within 5 moves and make a network graph of them
oGraph <- makeGraph(oSetting, oStart, oGoalCondition, max_depth = 5, verbose = 0)
```

By default, `makeGraph` searchs all states which are reachable from the
initial state without passing through the goal states. This exhaustive
search process can take very long time, and the returned graph can be
huge. You can limit your search space by `max_depth` argument or
`max_num_states` argument.

### plot the graph

`rSlidePzl` package also provides `plotGraph` function to plot a small
network graph. Consider to use Gephi or some other software for large
graphs.

``` r
set.seed(1)
plotGraph(oGraph, method = "GGally")
```

<img src="man/figures/README-example2-1.png" width="100%" />

The blue node (circle) in the plot represents the initial state. Other
nodes are possible states you can generate by moving pieces within 5
times. The red node represents the goal state.

The edges between nodes represent transition relationships between
nodes. For example, the blue node has two edges labeled as “12R” and
“23U”. It says that at the inital state you have two choice: move a
piece at (1,2) (i.e. “C”) right, or move a piece at (2,3) (“E”) upper.

### find best solutions

Though the main focus of `rSlidePzl` package is not to get solutions of
given puzzle but to generate a network graph, this package have some
helper functions to extract solutions from the graph.

In this example, you have only one path which is shortest. Your best
moves are:

``` r
# # show shortest pathes
lSolution <- getShortestPath(oGraph)
print(lSolution$transition)
#> [1] "12R" "22U" "23L" "33U"
```

It says: move a piece at (1,2) right,

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    |      | C    |
| row2 | D    | B    | E    |
| row3 | G    | H    | F    |

move a piece at (2,2) upper,

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    | B    | C    |
| row2 | D    |      | E    |
| row3 | G    | H    | F    |

move a piece at (2,3) left,

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    | B    | C    |
| row2 | D    | E    |      |
| row3 | G    | H    | F    |

and move a piece at (3,3) upper.

|      | col1 | col2 | col3 |
| ---- | :--- | :--- | :--- |
| row1 | A    | B    | C    |
| row2 | D    | E    | F    |
| row3 | G    | H    |      |

I hope you enjoy.
