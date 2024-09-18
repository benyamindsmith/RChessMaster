# RChessMaster <a href='https://github.com/benyamindsmith/rChess'><img src='https://github.com/user-attachments/assets/56ffa648-725d-45c5-9672-b0031cb23551' align="right" height="300" /></a>

An experimental chess engine written in R with [{raylibr}](https://github.com/jeroenjanssens/raylibr)

<a>
<img src= https://github.com/user-attachments/assets/6e71a629-7983-4c50-aa15-aa26fe9ee4ea width =50%>
</a>


# Installing this Package 

```r
devtools::install_github("benyamindsmith/RChessMaster")
```

# Play a new game

Presently the game does not have any AI enabled, so you (or someone else sitting next to you) are in control of both the white and black pieces seqentially. 


```r
RChessMaster::new_game()
```



# Similar Projects

- [rchess](https://github.com/jbkunst/rchess)

# TODO

- [ ] Rook Castling
- [ ] En-Pessant
- [ ] AI Opponent (minimax)
