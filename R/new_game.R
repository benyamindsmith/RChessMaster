#' New Chess Game
#'
#' @export

new_game <- function() {
  screen_width <- 800
  screen_height <- 800
  board_size <- 8
  square_size <- screen_width / board_size
  selected_piece <- NULL
  selected_pos <- NULL

  # Board matrix to keep track of piece positions
  board <- matrix("", nrow = 8, ncol = 8)
  board[2, ] <- "white_pawn"
  board[7, ] <- "black_pawn"
  board[1, c(1, 8)] <- "white_rook"
  board[1, c(2, 7)] <- "white_knight"
  board[1, c(3, 6)] <- "white_bishop"
  board[1, 4] <- "white_queen"
  board[1, 5] <- "white_king"
  board[8, c(1, 8)] <- "black_rook"
  board[8, c(2, 7)] <- "black_knight"
  board[8, c(3, 6)] <- "black_bishop"
  board[8, 4] <- "black_queen"
  board[8, 5] <- "black_king"


  # Pieces
  pieces <- paste0(
    paste0(system.file(package="rChess"),"/png/"),
    paste0(system.file(package="rChess"),"/png") |>
    list.files()
    )


  # Define Window
  raylibr::init_window(width = screen_width,
                       height = screen_height,
                       title = "rChess")

  # Load pieces as textures
  piece_textures <- list(
    white_bishop = raylibr::load_image(pieces[[7]]) |> raylibr::load_texture_from_image(),
    white_king = raylibr::load_image(pieces[[8]]) |> raylibr::load_texture_from_image(),
    white_knight = raylibr::load_image(pieces[[9]]) |> raylibr::load_texture_from_image(),
    white_pawn = raylibr::load_image(pieces[[10]]) |> raylibr::load_texture_from_image(),
    white_queen = raylibr::load_image(pieces[[11]]) |> raylibr::load_texture_from_image(),
    white_rook = raylibr::load_image(pieces[[12]]) |> raylibr::load_texture_from_image(),
    black_bishop = raylibr::load_image(pieces[[1]]) |> raylibr::load_texture_from_image(),
    black_king = raylibr::load_image(pieces[[2]]) |> raylibr::load_texture_from_image(),
    black_knight = raylibr::load_image(pieces[[3]]) |> raylibr::load_texture_from_image(),
    black_pawn = raylibr::load_image(pieces[[4]]) |> raylibr::load_texture_from_image(),
    black_queen = raylibr::load_image(pieces[[5]]) |> raylibr::load_texture_from_image(),
    black_rook = raylibr::load_image(pieces[[6]]) |> raylibr::load_texture_from_image()
  )

  while (!raylibr::window_should_close()) {
    raylibr::begin_drawing()

    # Find the positions of the kings
    white_king_pos <- which(board == "white_king", arr.ind = TRUE)[1, ]
    black_king_pos <- which(board == "black_king", arr.ind = TRUE)[1, ]

    # Determine if kings are in check
    white_king_in_check <- rChess:::is_king_in_check(board, white_king_pos, "black")
    black_king_in_check <- rChess:::is_king_in_check(board, black_king_pos, "white")

    # Determine if kings are in checkmate (simplified)
    white_king_in_checkmate <- white_king_in_check && rChess:::is_checkmate(board, white_king_pos, "white", "black")
    black_king_in_checkmate <- black_king_in_check && rChess:::is_checkmate(board, black_king_pos, "black", "white")

    # Draw the board
    for (row in 0:(board_size - 1)) {
      for (col in 0:(board_size - 1)) {
        if ((row + col) %% 2 == 0) {
          raylibr::draw_rectangle(col * square_size, row * square_size, square_size, square_size, raylibr::color(240, 217, 181, 255))
        } else {
          raylibr::draw_rectangle(col * square_size, row * square_size, square_size, square_size, raylibr::color(181, 136, 99, 255))
        }

        # Highlight the selected square
        if (!is.null(selected_pos) && selected_pos[1] == 8 - row && selected_pos[2] == col + 1) {
          raylibr::draw_rectangle(col * square_size, row * square_size, square_size, square_size, raylibr::color(0, 255, 0, 100))  # Transparent green
        }

        # Highlight the king's square in red if in check
        if (white_king_in_check && white_king_pos[1] == 8 - row && white_king_pos[2] == col + 1) {
          raylibr::draw_rectangle(col * square_size, row * square_size, square_size, square_size, raylibr::color(255, 0, 0, 100))  # Transparent red
        } else if (black_king_in_check && black_king_pos[1] == 8 - row && black_king_pos[2] == col + 1) {
          raylibr::draw_rectangle(col * square_size, row * square_size, square_size, square_size, raylibr::color(255, 0, 0, 100))  # Transparent red
        }

        # Draw the piece at the current position
        piece <- board[8 - row, col + 1]
        if (piece != "") {
          raylibr::draw_texture(piece_textures[[piece]], col * square_size - 15, row * square_size - 20, tint = raylibr::as_color("white"))
        }
      }
    }

    # Detect clicks
    if (raylibr::is_mouse_button_pressed(raylibr::mouse_button$left)) {
      mouse_pos <- raylibr::get_mouse_position()
      col <- floor(mouse_pos[1]/ square_size) + 1
      row <- board_size - floor(mouse_pos[2] / square_size)

      if (is.null(selected_piece)) {
        # Selecting a piece
        selected_piece <- board[row, col]
        selected_pos <- c(row, col)
      } else {
        # Moving the selected piece
        if (selected_piece != "" && rChess:::is_valid_move(board, selected_piece, selected_pos, c(row, col))) {
          board[row, col] <- selected_piece
          board[selected_pos[1], selected_pos[2]] <- ""
        }
        selected_piece <- NULL
        selected_pos <- NULL
      }
    }
    raylibr::end_drawing()
  }

  raylibr::close_window()
}

#new_game()
