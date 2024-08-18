#' New Chess Game
#'
#' @export

new_game <- function() {
  screen_width <- 800
  screen_height <- 800
  board_size <- 8
  square_size <- screen_width / board_size

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
  # Define pieces

  # # Black
  # black_bishop <- pieces[[1]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  # black_king <- pieces[[2]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  # black_knight <- pieces[[3]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  black_pawn <- pieces[[4]] |>
    raylibr::load_image() |>
    raylibr::load_texture_from_image()
  # black_queen <- pieces[[5]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  black_rook <- pieces[[6]] |>
    raylibr::load_image() |>
    raylibr::load_texture_from_image()
  #
  # # White
  # white_bishop <- pieces[[7]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  # white_king <- pieces[[8]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  # white_knight <- pieces[[9]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  white_pawn <- pieces[[10]] |>
    raylibr::load_image() |>
    raylibr::load_texture_from_image()
  # white_queen <- pieces[[11]] |>
  #   raylibr::load_image() |>
  #   raylibr::load_texture_from_image()
  white_rook <- pieces[[12]] |>
    raylibr::load_image() |>
    raylibr::load_texture_from_image()


  while (!raylibr::window_should_close()) {
    raylibr::begin_drawing()
    # Setup Board
    for (row in 0:(board_size - 1)) {
      for (col in 0:(board_size - 1)) {
        if ((row + col) %% 2 == 0) {
          raylibr::draw_rectangle(
            col * square_size,
            row * square_size,
            square_size,
            square_size,
            col =  raylibr::color(240, 217, 181, 255)
          )
        } else {
          raylibr::draw_rectangle(
            col * square_size,
            row * square_size,
            square_size,
            square_size,
            col = raylibr::color(181, 136, 99, 255)
          )
        }
      }
      #Add pieces
      #Draw Pawns
      for (col in 0:(board_size - 1)) {
        # White pawns
        raylibr::draw_texture(white_pawn, col * square_size-15, 6 * square_size-20, tint = raylibr::as_color("white"))
        # Black pawns
        raylibr::draw_texture(black_pawn, col * square_size -15, 1 * square_size-20, tint = raylibr::as_color("white"))
      }

      # # Draw other pieces: Black
      #raylibr::draw_texture(black_rook, 0 * square_size-15, 0 * square_size-20, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_knight, 1 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_bishop, 2 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_queen, 3 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_king, 4 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_bishop, 5 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_knight, 6 * square_size, 0 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(black_rook, 7 * square_size, 0 * square_size, tint = raylibr::color("white"))
      #
      # # Draw other pieces: White
      #raylibr::draw_texture(white_rook, 0 * square_size, 6 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_knight, 1 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_bishop, 2 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_queen, 3 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_king, 4 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_bishop, 5 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_knight, 6 * square_size, 7 * square_size, tint = raylibr::color("white"))
      # raylibr::draw_texture(white_rook, 7 * square_size, 7 * square_size, tint = raylibr::color("white"))

    }
    raylibr::end_drawing()
  }

  raylibr::close_window()

}

new_game()
