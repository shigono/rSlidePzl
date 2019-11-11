makeSetting <- function(boardsize, piecesize){
  #' make setting of a sliding puzzle
  #'
  #' @export
  #' @param boardsize a numeric vector of length 2.
  #'                  (number of rows, number of columns) of the board.
  #'                  Should be >= 1 and <= 9.
  #'
  #' @param piecesize a named list.
  #'                  The name of each element should be a alphabetic character.
  #'                  Case sensitive.
  #'                  Duplicated names are not allowed.
  #'                  Each element should be a numeric vector of length 2,
  #'                  giving (vertical size, horizontal size) of the type of piece.
  #'
  #' @return an object of class 'slidepzl_setting',
  #'         which is a list with following elements:
  #'         \describe{
  #'           \item{boardsize}{
  #'             same as \code{boardsize} argument
  #'           }
  #'           \item{piecearea}{
  #'             a named list.
  #'             The name of each element is a alphabetic character,
  #'             giving the 'type' of piece.
  #'             Each element is a numeric matrix of size (k,2),
  #'             giving relative locations of cells which that type of piece occupies.
  #'           }
  #'         }
  #'
  #' @example example/makeSetting.R

  # trap: boardsize
  stopifnot(is.numeric(boardsize))
  stopifnot(is.atomic(boardsize))
  stopifnot(length(boardsize) == 2)
  stopifnot(boardsize >= 1)
  stopifnot(boardsize <= 9)

  # trap: piecesize
  stopifnot(is.list(piecesize))
  stopifnot(names(piecesize) %in% c(LETTERS, letters))
  stopifnot(anyDuplicated(names(piecesize)) == 0)
  stopifnot(sapply(piecesize, is.numeric))
  stopifnot(sapply(piecesize, is.atomic))
  stopifnot(sapply(piecesize, function(x) length(x) == 2))
  stopifnot(sapply(piecesize, function(x) x >= 1))
  stopifnot(sapply(piecesize, function(x) x <= 9))

  # loop for each piece
  lPieceArea <- lapply(
    piecesize,
    function(anPieceSize){
      # if anPieceSize == c(2,2) then return a matrix
      #    0, 0
      #    0, 1
      #    1, 0
      #    1, 1
      as.matrix(expand.grid(
        nRow = seq_len(anPieceSize[1]) - 1,
        nCol = seq_len(anPieceSize[2]) - 1
      ))
    }
  )

  # output
  out <- list(boardsize = boardsize, piecearea = lPieceArea)
  class(out) <- append(class(out), "slidepzl_setting")
  return(out)
}
makePiece <- function(type, loc){
  #' make a piece
  #'
  #' @export
  #' @param type a alphabetic character giving 'type' of piece. Case sensitive.
  #' @param loc  a numeric vector of length 2,
  #'             giving the location (row, column) of the upper left corner
  #'             of the piece.
  #' @return an object of class "slidepzl_piece", which is a list with following elements:
  #'         \describe{
  #'           \item{type}{'type' of piece}
  #'           \item{loc}{location of piece}
  #'         }
  #' @examples
  #' # a piece of type 'A' is on the board,
  #' # whose upper left corner is located at (1, 1)
  #' makePiece('A', c(1,1))

  # trap: type
  stopifnot(type %in% c(LETTERS, letters))

  # trap: loc
  stopifnot(is.numeric(loc))
  stopifnot(is.atomic(loc))
  stopifnot(length(loc) == 2)
  stopifnot(loc >= 1)
  stopifnot(loc <= 9)

  # output
  out <- list(type = type, loc = loc)
  class(out) <- append(class(out), "slidepzl_piece")
  return(out)
}
as.character.slidepzl_piece <- function(piece){
  #' Internal. Convert the location of a piece into a string.
  #'
  #' @param piece a object of 'slidepzl_piece' class.
  #' @return a character string of length 3

  # trap: piece
  stopifnot('slidepzl_piece' %in% class(piece))

  # output
  paste0(c(as.character(piece$loc), piece$type), collapse = "")
}
makeState <- function(pieces){
  #' make a state of a puzzle, or goal conditions of a puzzle
  #'
  #' @export
  #' @param pieces a list of objects of 'slidepzl_piece' class.
  #' @return an object of class "slidepzl_state", which is a list with following elements:
  #'         \describe{
  #'           \item{piecetypes}{
  #'             a character vector, whose ith element gives the 'type' of ith piece.
  #'           }
  #'           \item{piecelocs}{
  #'           a numeric matrix of size (k, 2),
  #'           where k is the number of pieces on the board.
  #'           The ith row gives (row, column) of the upper left corner
  #'           of the ith piece.
  #'           }
  #'         }
  #'         Validity of the state is not guaranteed.
  #' @example example/makeState.R

  # trap: pieces
  stopifnot(sapply(pieces, function(x) 'slidepzl_piece' %in% class(x)))

  # piecetype
  asPieceType <- sapply(pieces, function(x) x$type)

  # piecelocs
  mnPieceLocs <- matrix(
    unlist(lapply(pieces, function(x) x$loc)), ncol = 2, byrow = T
  )

  # output
  out <- list(piecetype = asPieceType, piecelocs = mnPieceLocs)
  class(out) <- append(class(out), "slidepzl_state")
  return(out)
}
isValidState <- function(state, setting){
  #' check the validity of state
  #'
  #' @export
  #' @param state an object of 'slidepzl_state' class.
  #' @param setting an object of 'slidepzl_setting' class.
  #' @return logical. is the state valid?
  #' @example example/isValidState.R

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))
  # trap: setting
  stopifnot("slidepzl_setting" %in% class(setting))

  lOccupied <- lapply(
    seq_along(state$piecetype),
    function(nRow){
      mnArea <- setting$piecearea[[state$piecetype[nRow]]]
      mnArea[,1] <- mnArea[,1] + state$piecelocs[nRow,1]
      mnArea[,2] <- mnArea[,2] + state$piecelocs[nRow,2]
      return(mnArea)
    }
  )
  mnOccupied <- do.call(rbind, lOccupied)

  ifelse(
    all(mnOccupied[,1] %in% seq_len(setting$boardsize[1]))
    && all(mnOccupied[,2] %in% seq_len(setting$boardsize[2]))
    && (nrow(unique(mnOccupied)) == nrow(mnOccupied)) ,
    TRUE, FALSE
  )
}
as.matrix.slidepzl_state <- function(state, setting, error_invalid = TRUE){
  #' Internal. Convert a state into a character matrix.
  #' Used to show verbose messages in \code{makeGraph} function.
  #'
  #' @param state a object of 'slidepzl_state' class.
  #' @param setting a object of 'slidepzl_setting' class.
  #' @param error_invalid logical. Raise error if the state is invalid?
  #' @return a character matrix.

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))
  if (error_invalid){
    stopifnot(isValidState(state, setting))
  }

  # trap: setting
  stopifnot('slidepzl_setting' %in% class(setting))

  # loop for each piece
  lOccupied <- lapply(
    seq_along(state$piecetype),
    function(nRow){
      # get relative area
      mnArea <- setting$piecearea[[state$piecetype[nRow]]]

      # characters of each cell
      asPiece <- rep(NA, nrow(mnArea))
      # upper left corner
      asPiece[mnArea[,1] == 0 & mnArea[,2] == 0] <- state$piecetype[nRow]
      # cells in the first row
      asPiece[mnArea[,1] == 0 & mnArea[,2] > 0]  <- '-'
      # cells in the first column
      asPiece[mnArea[,1] > 0 & mnArea[,2] == 0]  <- '|'
      # other cells
      asPiece[mnArea[,1] > 0 & mnArea[,2] > 0]   <- '+'

      # convert to absolute area
      mnArea[,1] <- mnArea[,1] + state$piecelocs[nRow,1]
      mnArea[,2] <- mnArea[,2] + state$piecelocs[nRow,2]
      # add characters as rownames
      rownames(mnArea) <- asPiece
      return(mnArea)
    }
  )
  mnOccupied <- do.call(rbind, lOccupied)

  # output
  out <- matrix(' ', nrow = setting$boardsize[1], ncol = setting$boardsize[2])
  out[mnOccupied] <- rownames(mnOccupied)
  return(out)
}
as.character.slidepzl_state <- function(state){
  #' Internal. Convert a state into a vector of character string.
  #'
  #' @param state an object of 'slidepzl_state' class.
  #' @return a character vector

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))

  # e.g. state has pieces A at (1,1), B at (2,2) and B at (3,3)

  # asPiece: c("11", "22", "33")
  asPiece <- apply(
    state$piecelocs,
    1,
    function(x) paste0(as.character(x), collapse="")
  )
  # c("11A", "22B", "33C")
  sort(paste0(asPiece, state$piecetype))
}
modifyState <- function(state, pieceid, diff){
  #' Internal. Generate new state by moving a piece in a state.
  #'
  #' @param state an object of 'slidepzl_state' object.
  #' @param pieceid integer. the piece which is moved.
  #' @param diff a integer vector of length 2. direction of moving.
  #' @return an object of 'slidepzl_state' class.
  #'         Its validity is not guaranteed.

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))

  # trap: pieceid
  stopifnot(is.numeric(pieceid))
  stopifnot(is.atomic(pieceid))
  stopifnot(length(pieceid) == 1)
  stopifnot(pieceid %in% seq_along(state$piecetype))

  # trap: diff
  stopifnot(is.numeric(diff))
  stopifnot(is.atomic(diff))
  stopifnot(length(diff) == 2)

  # move a piece
  out <- state
  out$piecelocs[pieceid,] <- out$piecelocs[pieceid,] + diff

  return(out)
}
makeNextStates <- function(state, setting){
  #' Internal. Return all states which can be generated by moving a piece of given state.
  #'
  #' @param state an object of 'slidepzl_state' class. Original state.
  #' @param setting an object of 'slidepzl_setting' class.
  #' @return a list of objects of 'slidepzl_state' class, giving
  #'         all states which can be generated by moving a piece of
  #'         the original state.
  #'         Validity is guaranteed.
  #'
  #'         The name of elements give how they are generated.
  #'         e.g. '11D' means a piece at (1,1) in the original state is moved down.

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))
  # trap: setting
  stopifnot("slidepzl_setting" %in% class(setting))

  # e.g. state has pieces A at (1,1), B at (2,2) and B at (3,3)

  # loop for each piece
  lCandidate <- lapply(
    seq_along(state$piecetype),
    function(nPieceID){
      # return four states
      # labels are "U", "D", "L", "R"
      list(
        U = modifyState(state, nPieceID, c(-1, 0)),
        D = modifyState(state, nPieceID, c(1, 0)),
        L = modifyState(state, nPieceID, c(0, -1)),
        R = modifyState(state, nPieceID, c(0, 1))
      )
    }
  )
  # labels are "11", "22", "33"
  names(lCandidate) <- apply(
    state$piecelocs,
    1,
    function(x) paste0(x, collapse = "")
  )

  # a list of (num.piece) x 4 states
  lCandidate <- unlist(lCandidate, recursive = FALSE)
  # labels are "11U", "11D", ..., "33R"
  names(lCandidate) <- sub("\\.", "", names(lCandidate))

  # select valid states
  abValid <- sapply(
    lCandidate,
    function(x) isValidState(x, setting)
  )
  lOut <- lCandidate[abValid == 1]
  return(lOut)
}
isGoalState <- function(state, goalcondition){
  #' Internal. Check if a state satisfy goal conditions.
  #'
  #' @param state an object of 'slidepzl_state' class.
  #' @param goalcondition an object of 'slidepzl_state' class.
  #' @return logical. Does the state include all pieces in goal conditions?

  # trap: state
  stopifnot("slidepzl_state" %in% class(state))

  # trap: goalconditon
  stopifnot("slidepzl_state" %in% class(goalcondition))

  out <- ifelse(
    all(is.element(as.character(goalcondition), as.character(state))),
    TRUE, FALSE
  )
  return(out)
}
makeGraph <- function(
  setting, state, goalcondition,
  initsize_states      = 1000000,
  initsize_transitions = 2000000,
  max_depth            = Inf,
  max_num_states       = Inf,
  verbose              = 1
){
  #' make a network graph of possible states of given sliding puzzle.
  #'
  #' @export
  #' @param setting an object of 'slidepzl_setting' class. Setting of a puzzle.
  #' @param state an object of 'slidepzl_state' class. Initial state.
  #' @param goalcondition an object of 'slidepzl_state' class. Conditions of goal.
  #' @param initsize_states initial size of database of states.
  #'        Execution of this function may slow down when more states are found
  #'        than \code{initsize_states}.
  #' @param initsize_transitions initial size of database of transition.
  #'        Execution of this function may slow down when more transition are found
  #'        than \code{initsize_transitions}.
  #' @param max_depth max depth of states to search.
  #' @param max_num_states max number of states to find.
  #' @param verbose 0:no message, 1:normal messages, 2:full messsages.
  #' @return an object of 'igraph' class.
  #'
  #'         Vertexes and edges represents states and transition
  #'         between states, respectively.
  #'
  #'         Include following information as attributes of vertexes:
  #'         \describe{
  #'           \item{name}{character string, representing a state}
  #'           \item{depth}{integer, representing depth of the state}
  #'           \item{status}{1:the initial state, 2:a state
  #'           which is examined, 3:a state which is not examined,
  #'           4: a goal state}
  #'         }
  #'
  #'         Include following information as attributes of edges:
  #'         \describe{
  #'           \item{name}{which piece is moved to which direction.
  #'           e.g. "11U" means a piece located at (1,1) in original
  #'           state is moved up.}
  #'         }
  #'
  #'
  #' @importFrom data.table chmatch
  #' @importFrom igraph E
  #' @importFrom igraph V
  #' @importFrom igraph V<-
  #' @importFrom igraph E<-
  #' @importFrom igraph graph_from_edgelist
  #' @example example/makeGraph.R

  # trap
  stopifnot("slidepzl_setting" %in% class(setting))
  stopifnot("slidepzl_state" %in% class(state))
  stopifnot("slidepzl_state" %in% class(goalcondition))

  stopifnot(isValidState(state, setting))
  stopifnot(isValidState(goalcondition, setting))

  # 盤面DB
  #   abActive:         過去のすべての盤面について、その遷移先をまだ調べていないものに1
  #   abGoal:           過去のすべての盤面について、目標条件を満たすものに1
  #   anDepth:          過去のすべての盤面について、その探索の深さ
  #   lSTATE:           盤面データベース
  #   asSTATE:          盤面データベースのインデクス文字列
  #   nNUMSTATE:        盤面データベースの現在のサイズ
  abActive <- vector("integer", initsize_states)
  abGoal   <- vector("integer", initsize_states)
  anDepth  <- vector("integer", initsize_states)
  lState   <- vector("list", initsize_states)
  asState  <- vector("character", initsize_states)
  nNumState <- 0

  # 遷移DB
  #   lTransition:    遷移データベース
  #   asTransition:   遷移データベース
  lTransition  <- vector("list", initsize_transitions)
  asTransition <- vector("character", initsize_transitions)
  nNumTransition <- 0

  # 盤面DBに初期盤面を追加する
  nNumState   <- 1
  abActive[1] <- 1
  abGoal[1]   <- isGoalState(state, goalcondition)
  anDepth[1]  <- 0
  lState[[1]] <- state
  asState     <- paste0(as.character(state), collapse = "")

  repeat {
    # 盤面数が指定を超えていたら終了
    if (nNumState >= max_num_states) break

    # まだ遷移先を調べていない盤面をひとつみつける
    # もっとも盤面IDが小さいものとする(幅優先探索)
    nActiveStateID <- match(1, abActive[1:nNumState])

    # みつからなかったら終了
    if (is.na(nActiveStateID)) break
    # (その盤面のdepth+1)が指定を超えていたら終了
    if (anDepth[nActiveStateID] +1 > max_depth) break

    # 抽出
    oActiveState <- lState[[nActiveStateID]]

    # 遷移先の盤面リストをつくる
    # 要素名はコマ移動を表わす文字列
    lNewStates <- makeNextStates(oActiveState, setting)

    # そのインデクス文字列をつくり
    asNewStates <- sapply(lNewStates, function(x) paste0(as.character(x), collapse=""))
    # 盤面DBと照合
    anMatch <- chmatch(asNewStates, asState[1:nNumState])
    # 要素名(コマ移動を表す文字列)をコピーしておく
    names(anMatch) <- names(lNewStates)

    # 遷移先盤面リストlNewStatesの要素は次の3種類
    # a) 過去になかった盤面. anMatchがNAになっている
    #   -> 盤面DBにも遷移DBにも保存したい
    # b) 過去にあり、盤面IDが自分よりも前の盤面. anMatchがnActiveStateIDより小さい
    #   -> 保存は不要
    # c) 過去にあり、盤面IDが自分よりも後の盤面. anMatchがnActiveStateIDより大きい
    #   -> 遷移DBにのみ保存したい

    # a) 盤面DBに追加
    lAddStates <- lNewStates[is.na(anMatch)]
    asAddStates <- asNewStates[is.na(anMatch)]
    if (length(lAddStates) > 0){
      # lAddStatesが空でない場合
      # 盤面IDのベクトルを決める
      anAddStatesID <- nNumState + seq_along(lAddStates)
      # 終了判定
      abGoalTemp <- sapply(lAddStates, function(x) isGoalState(x, goalcondition))
      # 格納
      abActive[anAddStatesID] <- ifelse(abGoalTemp == 1, 0, 1)
      abGoal[anAddStatesID]   <- abGoalTemp
      anDepth[anAddStatesID]  <- rep(anDepth[nActiveStateID] + 1, length(lAddStates))
      lState[anAddStatesID]   <- lAddStates
      asState[anAddStatesID]  <- asAddStates
      nNumState               <- nNumState + length(lAddStates)
      # 状態数を更新
    } else {
      # lAddStatesが空である場合、返し値は空のベクトル
      anAddStatesID <- c()
    }

    # trap: 盤面DBに追加した個数は、anMatchがNAであった個数
    stopifnot(sum(is.na(anMatch)) == length(anAddStatesID))
    # ここでanMatchのNAを埋めておく
    anMatch[is.na(anMatch)] <- anAddStatesID

    ## a)c) 遷移DBに追加
    # 対象を選ぶ。もう調べた盤面への遷移は記録しない
    anMatch <- anMatch[anMatch > nActiveStateID]
    # 遷移先DBに登録
    anTransitionID <- nNumTransition + seq_along(anMatch)
    lTransition[anTransitionID] <- lapply(
      anMatch,
      function(nToID) c(nActiveStateID, nToID)
    )
    asTransition[anTransitionID] <- names(anMatch)
    nNumTransition <- nNumTransition + length(anMatch)

    # 盤面を調べたことを記録する
    abActive[nActiveStateID] <- FALSE

    # 報告
    if (verbose == 2) {
      cat("----- \n")
      cat("Processed ID:", nActiveStateID, "(depth", anDepth[nActiveStateID], ")\n")
      cat("index:", asState[nActiveStateID], "\n")
      print(as.matrix(lState[[nActiveStateID]], setting))
      cat(length(lAddStates), "new states are found.\n")
      cat("current # of total states:", nNumState, "\n")
      cat("current # of active states:", sum(abActive[1:nNumState]), "\n")
      cat("current # of transitions:", nNumTransition, "\n")
      cat("current # of goal states:", sum(abGoal[1:nNumState]), "\n")
    }
    if (verbose == 1) {
      cat(
        "Processed #", nActiveStateID, "(depth", anDepth[nActiveStateID], ")",
        "of", nNumState, "states;",
        "goal", sum(abGoal[1:nNumState]), ";",
        nNumTransition, "transitions\n"
      )
    }
  }

  g <- graph_from_edgelist(
    matrix(
      unlist(lTransition[1:nNumTransition]),
      ncol = 2, byrow = T
    )
  )
  # anStatus: {1:スタート, 2:inactive(遷移先を調べた), 3:active(調べてない), 4:goal}
  anStatus <- rep(2, nNumState)
  anStatus[1] <- 1
  anStatus[abActive[1:nNumState] == TRUE] <- 3
  anStatus[abGoal[1:nNumState] == TRUE] <- 4

  V(g)$name   <- asState[1:nNumState]
  V(g)$depth  <- anDepth[1:nNumState]
  V(g)$status <- anStatus

  E(g)$name   <- asTransition[1:nNumTransition]

  return(g)
}
plotGraph <- function(
  g,
  status_color = c("blue", "skyblue", "gray", "red"),
  status_label = FALSE,
  move_label   = TRUE,
  method       = 'igraph',
  ...
){
  #' plot the network graph.
  #'
  #' Provide easy way to plot the network graph generated by
  #' \code{makeGraph} function.
  #'
  #' To plot with more detailed
  #' specification, use \code{igraph::tkplot} function or
  #' \code{GGally::ggnet2} function instead.
  #'
  #' see \code{\link{makeGraph}} for example code.
  #'
  #' @export
  #' @param g an object generated by \code{makeGraph} function.
  #' @param status_color a vector of color, length 4.
  #'                     Color of (initial, unexamined,
  #'                     examined, goal) states (i.e. nodes).
  #' @param status_label logical. show labels of states?
  #' @param move_label logical. show labels of transition?
  #' @param method 'igraph': use igraph package; 'GGally': use GGally package.
  #' @param ... pass to \code{igraph::plot} or \code{GGally::ggnet2}
  #' @importFrom igraph E
  #' @importFrom igraph V
  #' @importFrom igraph plot.igraph
  #' @importFrom GGally ggnet2
  #'

  stopifnot(method %in% c('igraph', "GGally"))

  if (method == 'igraph'){

    if (status_label){
      vlabel = V(g)$name
    } else {
      vlabel = NA
    }

    if (move_label){
      elabel = E(g)$name
    } else {
      elabel = NA
    }

    plot.igraph(
      g,
      vertex.size        = 8,
      vertex.color       = status_color[V(g)$status],
      vertex.frame.color = NA,
      vertex.label       = vlabel,
      vertex.label.size  = 1,
      edge.arrow.size    = 0.2,
      edge.arrow.width   = 0.2,
      edge.label         = elabel,
      edge.label.family  = 'sans',
      edge.label.cex     = 1,
      edge.label.color   = "black",
      ...
    )
  }

  if (method == 'GGally'){

    if (status_label){
      vlabel = V(g)$name
    } else {
      vlabel = FALSE
    }

    if (move_label){
      elabel = E(g)$name
    } else {
      elabel = FALSE
    }

    ggnet2(
      g,
      size            = 6,
      node.alpha      = 0.5,
      label           = vlabel,
      edge.label      = elabel,
      edge.label.size = 4,
      arrow.size      = 6,
      arrow.gap       = 0.025,
      arrow.type      = "closed",
      color           = status_color[V(g)$status]
    )
  }
}
getAllShortestPaths <- function(g, target = NULL){
  #' get all of shortest paths to target
  #'
  #' Get all of shortest paths from initial states to goal states
  #' (or to specified states). It may take a long time.
  #'
  #' see \code{\link{makeGraph}} for example code.
  #'
  #' @export
  #' @param g an 'igraph' object generated by \code{makeGraph} function.
  #' @param target a numerical vector, which gives
  #'               IDs of target states. NULL means IDs of goal states.
  #' @importFrom igraph all_shortest_paths
  #' @importFrom igraph E
  #' @importFrom igraph is.igraph
  #' @return a list with following elements:
  #'         \describe{
  #'           \item{state}{a list of character vectors.
  #'           ith element gives states in the ith path.}
  #'           \item{transition}{a list of character vectors.
  #'           ith element gives transitions in the ith path.}
  #'         }
  #'

  stopifnot(is.igraph(g))

  if (is.null(target)){
    target <- seq_along(V(g))[V(g)$status == 4]
  }
  stopifnot(length(target) > 0)
  stopifnot(target %in% seq_along(V(g)))

  anDepth <- as.integer(V(g)$depth)
  anTarget <- target[anDepth == min(anDepth)]

  lPath <- all_shortest_paths(g, from = 1, to = anTarget, mode = "all")

  lState <- lapply(
    lPath$res,
    function(x) x$name
  )

  lMove <- lapply(
    lPath$res,
    function(nodes){
      anStateID <- as.integer(nodes)
      mnStateID <- matrix(NA, nrow = length(anStateID) - 1, 2)
      mnStateID[,1] <- anStateID[-length(anStateID)]
      mnStateID[,2] <- anStateID[-1]
      E(g, P = unlist(t(mnStateID)))$name
    }
  )

  list(state = lState, transition = lMove)
}
getShortestPath <- function(g, target = NULL){
  #' get a shortest paths to target
  #'
  #' Get a shortest path from initial states to goal states
  #' (or to specified states).
  #'
  #' @export
  #' @param g an 'igraph' object generated by \code{makeGraph} function.
  #' @param target a numerical vector, which gives
  #'               IDs of target states. NULL means IDs of goal states.
  #' @importFrom igraph shortest_paths
  #' @importFrom igraph E
  #' @return a list with following elements:
  #'         \describe{
  #'           \item{state}{a character vector, giving the path.}
  #'           \item{transition}{a character vectors,
  #'           giving the path.}
  #'         }
  #'

  stopifnot(is.igraph(g))

  if (is.null(target)){
    target <- seq_along(V(g))[V(g)$status == 4]
  }
  stopifnot(length(target) > 0)
  stopifnot(target %in% seq_along(V(g)))

  anDepth <- as.integer(V(g)$depth)
  anTarget <- target[anDepth == min(anDepth)]

  lPath <- shortest_paths(g, from = 1, to = anTarget[1], output = "vpath")

  anStateID <- as.integer(lPath$vpath[[1]])
  mnStateID <- matrix(NA, nrow = length(anStateID) - 1, 2)
  mnStateID[,1] <- anStateID[-length(anStateID)]
  mnStateID[,2] <- anStateID[-1]
  asMove <- E(g, P = unlist(t(mnStateID)))$name

  list(state = lPath$vpath[[1]], transition = asMove)
}
