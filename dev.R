# Chess Board in R

library(ggplot2)
library(dplyr)
cols <- letters[1:8]
rows <- 1:8

board <- expand.grid(cols,rows) |>
  within({
    color = ifelse((Var1 %in% letters[c(1, 3, 5, 7)] &Var2 %% 2 == 0) |
                     (Var1 %in% letters[c(2, 4, 6, 8)] & Var2 %% 2 != 0),
                   TRUE,
                   FALSE)
  })

# For filtering
pieces <- {
  list(
    white_pawns = board |>
      subset(Var2 == 2) |>
      within({
        piece = "white_pawn"
        no = 1:8
        alive = TRUE
        check=NA
        position = list(c("a",2), c("b",2),c("c",2),c("d",2),c("e",2),c("f",2),c("g",2),c("h",2))
        turn_count = 0
      }),
    black_pawns = board |>
      subset(Var2 == 7) |>
      within({
        piece = "black_pawn"
        no = 1:8
        alive = TRUE
        check=NA
        position = list(c("a",7), c("b",7),c("c",7),c("d",7),c("e",7),c("f",7),c("g",7),c("h",7))
        turn_count = 0
      }),
    white_castles = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(1, 8)]) |>
      within({
        piece = "white_castle"
        no = 1:2
        alive = TRUE
        check=NA
        position = list(c("a",1), c("h",1))
        turn_count = 0
      }),
    black_castles = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(1, 8)]) |>
      within({
        piece = "black_castle"
        no = 1:2
        alive = TRUE
        check=NA
        position = list(c("a",8), c("h",8))
        turn_count = 0
      }),
    white_knights = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(2, 7)]) |>
      within({
        piece = "white_knights"
        no = 1:2
        alive = TRUE
        check=NA
        position = list(c("b",1), c("g",1))
        turn_count =0
      }),
    black_knights = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(2, 7)]) |>
      within({
        piece = "black_knights"
        no = 1:2
        alive = TRUE
        check=NA
        position = list(c("b",8), c("g",8))
        turn_count =0
      }),
    white_bishops = board |>
      subset(Var2 == 1 & Var1 %in% letters[c(3, 6)]) |>
      within({
        piece="white_bishop"
        no = 1:2
        alive = TRUE
        check=NA
        position = list(c("c",1), c("f",1))
        turn_count =0
      }),
    black_bishops = board |>
      subset(Var2 == 8 & Var1 %in% letters[c(3, 6)]) |>
      within({
        piece = "black_bishop"
        no = 1:2
        alive=TRUE
        check=NA
        position = list(c("c",8), c("f",8))
        turn_count=0
      }),
    white_queen = board |> 
      subset(Var2 == 1 & Var1 == letters[4])|>
      within({
        piece="white_queen"
        no = 1
        alive=TRUE
        check=NA
        position = list(c("d",1))
        turn_count=0
      }),
    black_queen = board |> 
      subset(Var2 == 8 & Var1 == letters[4]) |>
      within({
        piece="black_queen"
        no = 1
        alive=TRUE
        check=NA
        position = list(c("d",8))
        turn_count=0
      }),
    white_king = board |> 
      subset(Var2 == 1 & Var1 == letters[5]) |>
      within({
        piece = "white_king"
        no = 1
        alive=TRUE
        check=FALSE
        position = list(c("e",1))
        turn_count=0
      }),
    black_king = board |> 
      subset(Var2 == 8 & Var1 == letters[5]) |>
      within({
        piece="black_king"
        no = 1
        position = list(c("d",8))
        alive=TRUE
        check=FALSE
        turn_count=0
      })
  )
}

############################################
# These datasets are not used for anything presently 
# For reference
pieces_dataset <- do.call(rbind, pieces) |>
                  subset(select=-color)

# white pieces

white_pieces <-subset(pieces_dataset, grepl("^white", piece))

# black pieces


white_pieces <-subset(pieces_dataset, grepl("^black", piece))

################################################

validate_move_syntax <- function(move){
  check<-grepl("(^((?:[KQRBN])|)[a-h][1-8]$)|(quit)",move)
  return(check)
}




parse_move <- function(player,move){
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
  # Check move legality here
  return(list("piece"=piece,
              "col"=col,
              "row"=row))
}

check_move_legality<- function(player,parsed_move){
  
  piece <- parsed_move[["piece"]]
  col <- parsed_move[["col"]]
  row <- parsed_move[["row"]] |> as.numeric()
  
  if(player=="White"){
    switch(piece,
           "pawn" = {
             
             pawn_selected<- subset(pieces[["white_pawns"]], 
                                    (Var1==col&Var2==row-1)|
                                    (Var1==col&Var2==row-2 & turn_count==0))
             
             
             if(nrow(pawn_selected)==0){
               cat("Error: No white pawns found to legally allow such a move")
               return(FALSE)
             }else if(nrow(pawn_selected)>1){
               cat("Error: More than one piece found. This is an issue with code logic")
               return(FALSE)
             }else{
               # Check turn count
               # Update data and return to TRUE
               return(TRUE)
             }
           },
           
           "rook" = {
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
           
    )}else{
      switch(piece,
             "pawn" = { 
               
               pawn_selected<- subset(pieces[["black_pawns"]], 
                                      (Var1==col&Var2==row+1)|
                                        (Var1==col&Var2==row+2 & turn_count==0))
               
               
               
               if(nrow(pawn_selected)==0){
                 cat("Error: No black pawns found to legally allow such a move")
                 return(FALSE)
               }else if(nrow(pawn_selected)>1){
                 cat("Error: More than one piece found. This is an issue with code logic")
                 return(FALSE)
               }else{
                 return(TRUE)
               }
             },
             
             "rook" = {
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
}

get_player_move <- function(player){
  prompt <- paste(player, "'s Turn:", sep='', collapse='')
  
  move<-readline(prompt)
  
  if(move=="quit"){
    return(move)
  }else{
  # Recursive solution
  
  # Check for valid move
  if(validate_move_syntax(move)){
    parsed_move<- parse_move(player,move)
    if(check_move_legality(player,parsed_move)){
      return(move)
    }else{
      get_player_move(player)
    }
  } else{
    cat("Invalid Move. Please use algebraic notation for making moves.")
    get_player_move(player)
  }
  }
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



# STILL NEED TO WORK ON
update_board<- function(player,parsed_move){
  
  piece <- parsed_move[["piece"]]
  col <- parsed_move[["col"]]
  row <- parsed_move[["row"]] |> as.numeric()
  if(player=="White"){
    switch(parsed_move[["piece"]],
           "pawn"={
             
             pieces[["white_pawns"]]<<- pieces[["white_pawns"]] %>% 
               mutate(Var1 = case_when(((Var1==col&Var2==row-1)|(Var1==col&Var2==row-2 & turn_count==0)) ~ col,
                                       TRUE ~ as.character(Var1)),
                      Var2 = case_when(((Var1==col&Var2==row-1)|
                                          (Var1==col&Var2==row-2 & turn_count==0)) ~ row,
                                       TRUE ~ as.numeric(Var2)),
                      turn_count = case_when(((Var1==col&Var2==row-1)|
                                                (Var1==col&Var2==row-2 & turn_count==0)) ~ turn_count+1,
                                             TRUE ~ as.numeric(turn_count)) )
           },
           "castle" = {},
           "knight"={},
           "bishop"={},
           "queen"={},
           "king" ={}
    )
  }else{
    switch(parsed_move[["piece"]],
           "pawn"={
             pieces[["black_pawns"]]<<- pieces[["black_pawns"]] %>% 
               mutate(Var1 = case_when((Var1==col&Var2==row+1)|
                                         (Var1==col&Var2==row+2 & turn_count==0) ~ col,
                                       TRUE ~ as.character(Var1)),
                      Var2 = case_when((Var1==col&Var2==row+1)|
                                         (Var1==col&Var2==row+2 & turn_count==0) ~ row,
                                       TRUE ~ as.numeric(Var2)),
                      turn_count = case_when(((Var1==col&Var2==row+1)|
                                                (Var1==col&Var2==row+2 & turn_count==0)) ~ turn_count+1,
                                             TRUE ~ as.numeric(turn_count)))
           },
           "castle" = {},
           "knight"={},
           "bishop"={},
           "queen"={},
           "king" ={}
    )
  }
  
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
    parsed_move <- parse_move(player,move)
    
    update_board(player,parsed_move)
    
    plot_board(board,pieces)|> 
       print() 
    
  }
  cat("Game Concluded")
}


play_game()
