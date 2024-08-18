is_valid_move <- function(board, piece, start_pos, end_pos) {
  start_row <- start_pos[1]
  start_col <- start_pos[2]
  end_row <- end_pos[1]
  end_col <- end_pos[2]

  delta_row <- abs(end_row - start_row)
  delta_col <- abs(end_col - start_col)

  # Prevent moving to a square with a piece of the same color
  if (startsWith(board[end_row, end_col], substr(piece, 1, 5))) {
    return(FALSE)
  }

  if (piece == "white_pawn" || piece == "black_pawn") {
    direction <- ifelse(piece == "white_pawn", 1, -1)
    start_row_direction <- ifelse(piece == "white_pawn", 2, 7)

    if (start_col == end_col) {
      # Normal forward move
      if (delta_row == 1 && (end_row - start_row) == direction && board[end_row, end_col] == "") {
        return(TRUE)
      }
      # First move: two squares forward
      if (start_row == start_row_direction && delta_row == 2 && (end_row - start_row) == direction * 2 && board[end_row, end_col] == "" && board[start_row + direction, end_col] == "") {
        return(TRUE)
      }
    } else if (delta_row == 1 && delta_col == 1) {
      # Capture
      if (board[end_row, end_col] != "" && !startsWith(board[end_row, end_col], substr(piece, 1, 5))) {
        return(TRUE)
      }
    }

  } else if (piece == "white_rook" || piece == "black_rook") {
    if (delta_row == 0 || delta_col == 0) {
      # Check for clear path
      if (delta_row == 0 && abs(end_col - start_col) > 1) {
        step <- sign(end_col - start_col)
        for (c in seq(start_col + step, end_col - step, step)) {
          if (board[start_row, c] != "") {
            return(FALSE)
          }
        }
      } else if (delta_col == 0 && abs(end_row - start_row) > 1) {
        step <- sign(end_row - start_row)
        for (r in seq(start_row + step, end_row - step, step)) {
          if (board[r, start_col] != "") {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    }

  } else if (piece == "white_knight" || piece == "black_knight") {
    if ((delta_row == 2 && delta_col == 1) || (delta_row == 1 && delta_col == 2)) {
      return(TRUE)
    }

  } else if (piece == "white_bishop" || piece == "black_bishop") {
    if (delta_row == delta_col) {
      # Check for clear path
      if (delta_row > 1) {
        row_step <- sign(end_row - start_row)
        col_step <- sign(end_col - start_col)
        for (i in 1:(delta_row - 1)) {
          if (board[start_row + i * row_step, start_col + i * col_step] != "") {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    }

  } else if (piece == "white_queen" || piece == "black_queen") {
    if (delta_row == delta_col || delta_row == 0 || delta_col == 0) {
      # Check for clear path (combine rook and bishop logic)
      if (delta_row == delta_col && delta_row > 1) {
        row_step <- sign(end_row - start_row)
        col_step <- sign(end_col - start_col)
        for (i in 1:(delta_row - 1)) {
          if (board[start_row + i * row_step, start_col + i * col_step] != "") {
            return(FALSE)
          }
        }
      } else if (delta_row == 0 && abs(end_col - start_col) > 1) {
        step <- sign(end_col - start_col)
        for (c in seq(start_col + step, end_col - step, step)) {
          if (board[start_row, c] != "") {
            return(FALSE)
          }
        }
      } else if (delta_col == 0 && abs(end_row - start_row) > 1) {
        step <- sign(end_row - start_row)
        for (r in seq(start_row + step, end_row - step, step)) {
          if (board[r, start_col] != "") {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    }

  } else if (piece == "white_king" || piece == "black_king") {
    if (delta_row <= 1 && delta_col <= 1) {
      return(TRUE)
    }
  }

  return(FALSE)
}


is_king_in_check <- function(board, king_position, opponent_color) {
  for (row in 1:8) {
    for (col in 1:8) {
      piece <- board[row, col]
      if (startsWith(piece, opponent_color)) {
        if (is_valid_move(board, piece, c(row, col), king_position)) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

is_checkmate <- function(board, king_position, king_color, opponent_color) {
  # Check if the king can move to any adjacent square and escape check
  directions <- list(
    c(-1, 0), c(1, 0), c(0, -1), c(0, 1), c(-1, -1), c(-1, 1), c(1, -1), c(1, 1)
  )

  for (direction in directions) {
    new_row <- king_position[1] + direction[1]
    new_col <- king_position[2] + direction[2]
    if (new_row >= 1 && new_row <= 8 && new_col >= 1 && new_col <= 8) {
      if (!is_king_in_check(board, c(new_row, new_col), opponent_color)) {
        return(FALSE)
      }
    }
  }
}

