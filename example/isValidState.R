mysetting <- makeSetting(
  boardsize = c(3,3),
  piecesize = list(
    "A" = c(1,1),
    "B" = c(1,1),
    "C" = c(1,1),
    "D" = c(1,1),
    "E" = c(1,1),
    "F" = c(1,1),
    "G" = c(1,1)
  )
)
# a valid case
state1 <- makeState(
  list(
    makePiece(type = "A", loc = c(1,1)),
    makePiece(type = "B", loc = c(1,2))
  )
)
isValidState(state1, mysetting) # TRUE
# a invalid case
state2 <- makeState(
  list(
    makePiece(type = "A", loc = c(1,1)),
    makePiece(type = "B", loc = c(1,1))  # the location is duplicated with piece 1
  )
)
isValidState(state2, mysetting) # FALSE
