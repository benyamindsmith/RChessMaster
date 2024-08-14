#' New Chess Game
#'
#' @export

new_game <- function() {
  screen_width <- 800
  screen_height <- 800
  board_size <- 8
  square_size <- screen_width / board_size

  # Define pieces
  # Define Window
  raylibr::init_window(width = screen_width,
                       height = screen_height,
                       title = "rChess")

  while (!raylibr::window_should_close()) {
    raylibr::begin_drawing()
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
    }
    raylibr::end_drawing()
  }

  raylibr::close_window()

}
