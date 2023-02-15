# Chess Board in R

library(tidyverse)

cols <- letters[1:8]
rows <- 1:8

board <- expand.grid(cols,rows) |>
  within({
    color = ifelse((Var1 %in% letters[c(1, 3, 5, 7)] &Var2 %% 2 == 0) |
                     (Var1 %in% letters[c(2, 4, 6, 8)] & Var2 %% 2 != 0),
                   TRUE,
                   FALSE)
  })

pieces <- {
  list(
    white_pawns = board |>
      subset(Var2 == 2) |>
      within({
        no = 1:8
        alive = TRUE
        turn_count = 0
      }),
    black_pawns = board |>
      subset(Var2 == 7) |>
      within({
        no = 1:8
        alive = TRUE
        turn_count = 0
      }),
    white_castles = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(1, 8)]) |>
      within({
        no = 1:2
        alive = TRUE
        turn_count = 0
      }),
    black_castles = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(1, 8)]) |>
      within({
        no = 1:2
        alive = TRUE
        turn_count = 0
      }),
    white_knights = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(2, 7)]) |>
      within({
        no = 1:2
        alive = TRUE
        turn_count =0
      }),
    black_knights = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(2, 7)]) |>
      within({
        no = 1:2
        alive = TRUE
        turn_count =0
      }),
    white_bishops = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(3, 6)]) |>
      within({
        no = 1:2
        alive = TRUE
        turn_count =0
      }),
    black_bishops = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(3, 6)]) |>
      within({
        no = 1:2
        alive=TRUE
        turn_count=0
      }),
    white_queen = board |> 
      subset(Var2 == 1 & Var1 == letters[4])|>
      within({
        no = 1
        alive=TRUE
        turn_count=0
      }),
    black_queen = board |> 
      subset(Var2 == 8 & Var1 == letters[4]) |>
      within({
        no = 1
        alive=TRUE
        turn_count=0
      }),
    white_king = board |> 
      subset(Var2 == 1 & Var1 == letters[5]) |>
      within({
        no = 1
        check=FALSE
        turn_count=0
      }),
    black_king = board |> 
      subset(Var2 == 8 & Var1 == letters[5]) |>
      within({
        no = 1
        check=FALSE
        turn_count=0
      })
  )
}
plot_board<- function(board,
                      pieces){
  
  white_pawns <- pieces[["white_pawns"]]
  black_pawns <- pieces[["black_pawns"]]
  
  white_castles <- pieces[["white_castles"]]
  black_castles <- pieces[["black_castles"]]
  
  white_knights <- pieces[["white_knights"]]
  black_knights <- pieces[["black_knights"]]
  
  white_bishops <- pieces[["white_bishops"]]
  black_bishops <- pieces[["black_bishops"]]
  
  white_queen <- pieces[["white_queen"]]
  black_queen <- pieces[["black_queen"]]
  
  white_king <- pieces[["white_king"]]
  black_king <- pieces[["black_king"]]
  
  ggplot()+
    theme_void()+
    geom_tile(data=board,
              mapping=aes(x=Var1,y=Var2,fill=color))+
    scale_fill_manual(values=c("#D3D3D3","#FFFFFF"))+
    geom_point(data = white_pawns,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2659",
               size=15)+
    geom_point(data = black_pawns,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265F",
               size=10)+
    geom_point(data = white_castles,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2656",
               size=15)+
    geom_point(data = black_castles,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265C",
               size=15)+
    geom_point(data = white_knights,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2658",
               size=15)+
    geom_point(data = black_knights,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265E",
               size=15)+
    geom_point(data = white_bishops,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2657",
               size=15)+
    geom_point(data = black_bishops,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265D",
               size=15)+
    geom_point(data = white_queen,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2655",
               size=15)+
    geom_point(data = black_queen,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265B",
               size=15)+
    geom_point(data = white_king,
               mapping=aes(x=Var1,y=Var2),
               shape="\u2654",
               size=15)+
    geom_point(data = black_king,
               mapping=aes(x=Var1,y=Var2),
               shape="\u265A",
               size=15)+
    theme(legend.position = "none")
}


validate_move_syntax <- function(move){
  check<-grepl("^((?:[KQRBN])|)[a-h][1-8]$",move)
  return(check)
}

get_player_move <- function(player){
  prompt <- paste(player, "'s Turn:", sep='', collapse='')
  
  move<-readline(prompt)
  
  # Recursive solution
  
  # Check for valid move
  if(validate_move_syntax(move)){
    return(move)
  } else{
    cat("Invalid Move")
    get_player_move(player)
  }
}

parse_move <- function(move){
  parsed_move <- strsplit(move,"")[[1]]
  if(length(parsed_move)==2){
    piece <- "pawn"
    col <- parsed_move[1]
    row <- parsed_move[2]
  }else{
    piece <- switch(parsed_move[1],
                    "K"="king",
                    "Q"="queen",
                    "R"="rook",
                    "B"="bishop",
                    "N"="knight")
    col <- parsed_move[2]
    row <- parsed_move[3]
  }
  
  return(list(piece,col,row))
}

check_legal_move<- function(parsed_move){
  
  switch(piece,
         "pawn" = {
           if(player =="White")
             return(TRUE)
         },
         
         "castle" = {
           return(TRUE)
         },
         
         "knight" = {
           return(TRUE)
         },
         
         "bishop" = {
           return(TRUE)
         },
         "queen" = {
           return(TRUE)
         },
         "king" = {
           return(TRUE)
         }
         
  )
}

parse_player_move<- function(player, move, pieces){
  parsed_move <-strsplit(move,split=" ")[[1]]
  piece <- strparsed_move
  
  switch(piece,
         "pawn"={},
         ""={},
         "pawn"={},
         "pawn"={},
         "pawn"={})
}


play_game <- function() {
  turn <- 0
  quit <- FALSE
  
  # Initialize the game
  plot_board(board,pieces) |> print() 
  
  while (quit == FALSE) {
    turn <- turn + 1
    player <- ifelse(turn %% 2 == 1, "White", "Black")
    move <- get_player_move(player)
    if(move == "quit"){
      quit <- TRUE
      break
    }
    pieces<-parse_player_move(player,move,pieces)
    
    
    # Update the game
    plot_board(board,pieces)|> 
      print() 
    
  }
  cat("Game Concluded")
}


play_game()
