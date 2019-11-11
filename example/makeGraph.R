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

# analyse the puzzle and make a network graph of states
oGraph <- makeGraph(oSetting, oStart, oGoalCondition, max_depth = 5, verbose = 1)

# plot
set.seed(123)
plotGraph(oGraph, method = "GGally")

# show shortest pathes
lSolution <- getAllShortestPaths(oGraph)
print(lSolution$state)
print(lSolution$transition)
