makeSetting <- function(boardsize, piecesize) {
    #' make setting of a sliding puzzle
    #'
    #' @export
    #' @param boardsize a numeric vector of length 2.
    #'
    #'                  (number of rows, number of columns) of the board.
    #'                  Should be >= 1 and <= 9.
    #'
    #' @param piecesize a named list.
    #'
    #'                  The name of each element should be a alphabetic character
    #'                  representing a type of piece. Case sensitive.
    #'                  Duplicated names are not allowed.
    #'
    #'                  Each element should be a numeric vector of length 2,
    #'                  giving (vertical size, horizontal size) of that type of piece.
    #'                  Should be >= 1 and <= 9.
    #'
    #' @return an object of class 'slidepzl_setting',
    #'         which is a list with following elements:
    #'         \describe{
    #'           \item{boardsize}{
    #'             same as \code{boardsize} argument
    #'           }
    #'           \item{piecearea}{
    #'             a named list.
    #'
    #'             The name of each element is a alphabetic character,
    #'             giving the 'type' of piece.
    #'
    #'             Each element is a numeric matrix of size (k,2),
    #'             giving relative locations of cells which that type of piece occupies.
    #'           }
    #'         }
    #'
    #' @examples
    #' # simple 3x3 puzzle
    #' mysetting <- makeSetting(
    #'   boardsize = c(3,3),
    #'   piecesize = list(
    #'     "A" = c(1,1),
    #'     "B" = c(1,1),
    #'     "C" = c(1,1),
    #'     "D" = c(1,1),
    #'     "E" = c(1,1),
    #'     "F" = c(1,1),
    #'     "G" = c(1,1)
    #'   )
    #' )
    #' mysetting

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
    lPieceArea <- lapply(piecesize, function(anPieceSize) {
        # if anPieceSize == c(2,2) then return a matrix 0, 0 0, 1 1, 0 1, 1
        as.matrix(expand.grid(nRow = seq_len(anPieceSize[1]) - 1, nCol = seq_len(anPieceSize[2]) - 1))
    })

    # output
    out <- list(boardsize = boardsize, piecearea = lPieceArea)
    class(out) <- append(class(out), "slidepzl_setting")
    return(out)
}
makePiece <- function(type, loc) {
    #' make a piece
    #'
    #' @export
    #' @param type a alphabetic character giving 'type' of piece. Case sensitive.
    #' @param loc  a numeric vector of length 2,
    #'             giving the location (row, column) of the upper left corner
    #'             of the piece.
    #'             Should be >= 1 and <= 9, or NA.
    #' @return an object of class 'slidepzl_piece', which is a list with following elements:
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
    stopifnot(is.atomic(loc))
    stopifnot(length(loc) == 2)
    tmploc <- loc[!is.na(loc)]
    if (length(tmploc) > 0) {
        stopifnot(is.numeric(tmploc))
        stopifnot(tmploc >= 1)
        stopifnot(tmploc <= 9)
    }

    # output
    out <- list(type = type, loc = loc)
    class(out) <- append(class(out), "slidepzl_piece")
    return(out)
}
as.character.slidepzl_piece <- function(piece) {
    #' convert the location of a piece into a string
    #'
    #' Internal. Convert the location of a piece into a string.
    #'
    #' @param piece a object of 'slidepzl_piece' class.
    #'              Should have no NA in location.
    #' @return a character string of length 3

    # trap: piece
    stopifnot("slidepzl_piece" %in% class(piece))
    stopifnot(!is.na(piece$loc))

    # output
    paste0(c(as.character(piece$loc), piece$type), collapse = "")
}
makeState <- function(pieces) {
    #' make a state (or goal conditions) of a puzzle
    #'
    #' @export
    #' @param pieces a list of objects of 'slidepzl_piece' class.
    #'               They can have NAs in their locations.
    #' @return an object of class 'slidepzl_state', which is a list with following elements:
    #'         \describe{
    #'           \item{piecetypes}{
    #'             a character vector, whose ith element gives the 'type' of ith piece.
    #'           }
    #'           \item{piecelocs}{
    #'           a numeric matrix of size (k, 2),
    #'           where k is the number of pieces on the board.
    #'           The ith row gives (row, column) of the upper left corner
    #'           of the ith piece. It can have NAs.
    #'           }
    #'         }
    #'         Validity of the state is not guaranteed.
    #' @examples
    #' mystate <- makeState(
    #'   list(
    #'     makePiece(type = "A", loc = c(1,1)),
    #'     makePiece(type = "B", loc = c(1,2)),
    #'     makePiece(type = "C", loc = c(1,3)),
    #'     makePiece(type = "D", loc = c(2,1)),
    #'     makePiece(type = "E", loc = c(2,2)),
    #'     makePiece(type = "F", loc = c(2,3)),
    #'     makePiece(type = "G", loc = c(3,1)),
    #'     makePiece(type = "H", loc = c(3,2))
    #'   )
    #' )
    #' mystate

    # trap: pieces
    stopifnot(sapply(pieces, function(x) "slidepzl_piece" %in% class(x)))

    # piecetype
    asPieceType <- sapply(pieces, function(x) x$type)

    # piecelocs
    mnPieceLocs <- matrix(unlist(lapply(pieces, function(x) x$loc)), ncol = 2, byrow = T)

    # output
    out <- list(piecetype = asPieceType, piecelocs = mnPieceLocs)
    class(out) <- append(class(out), "slidepzl_state")
    return(out)
}
as.matrix.slidepzl_state <- function(state, setting, error_invalid = TRUE) {
    #' convert a state into a character matrix
    #'
    #' Internal. Convert a state into a character matrix.
    #' Used to show verbose messages in \code{makeGraph} function.
    #'
    #' @param state a object of 'slidepzl_state' class.
    #' @param setting a object of 'slidepzl_setting' class.
    #' @param error_invalid logical. Raise error if the state is invalid?
    #' @return a character matrix.

    # trap: state
    stopifnot("slidepzl_state" %in% class(state))
    if (error_invalid) {
        stopifnot(isValidState(state, setting))
    }

    # trap: setting
    stopifnot("slidepzl_setting" %in% class(setting))

    # loop for each piece
    lOccupied <- lapply(seq_along(state$piecetype), function(nRow) {
        # get relative area
        mnArea <- setting$piecearea[[state$piecetype[nRow]]]

        # characters of each cell
        asPiece <- rep(NA, nrow(mnArea))
        # upper left corner
        asPiece[mnArea[, 1] == 0 & mnArea[, 2] == 0] <- state$piecetype[nRow]
        # cells in the first row
        asPiece[mnArea[, 1] == 0 & mnArea[, 2] > 0] <- "-"
        # cells in the first column
        asPiece[mnArea[, 1] > 0 & mnArea[, 2] == 0] <- "|"
        # other cells
        asPiece[mnArea[, 1] > 0 & mnArea[, 2] > 0] <- "+"

        # convert to absolute area
        mnArea[, 1] <- mnArea[, 1] + state$piecelocs[nRow, 1]
        mnArea[, 2] <- mnArea[, 2] + state$piecelocs[nRow, 2]
        # add characters as rownames
        rownames(mnArea) <- asPiece
        return(mnArea)
    })
    mnOccupied <- do.call(rbind, lOccupied)

    # output
    out <- matrix(" ", nrow = setting$boardsize[1], ncol = setting$boardsize[2])
    out[mnOccupied] <- rownames(mnOccupied)
    return(out)
}
as.character.slidepzl_state <- function(state) {
    #' convert a state into a vector of character string
    #'
    #' Internal. Convert a state into a vector of character string.
    #' It is concatenated and used for a search index of the state.
    #'
    #' @param state an object of 'slidepzl_state' class.
    #' @return a character vector

    # trap: state
    stopifnot("slidepzl_state" %in% class(state))

    # e.g. state has pieces A at (1,1), B at (2,2) and B at (3,3)

    # asPiece: c('11', '22', '33')
    asPiece <- apply(state$piecelocs, 1, function(x) paste0(as.character(x), collapse = ""))
    # c('11A', '22B', '33C')
    sort(paste0(asPiece, state$piecetype))
}
