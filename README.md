# rChess

An experimental chess engine written in R. 

![image](https://user-images.githubusercontent.com/46410142/218886309-1de92407-2ffb-4a58-88f1-4df98e89e041.png)


<details>
<summary>
<h3>Description</h3>
</summary>

Right now only liberal piece movement is available. Piece rules, capturing and castling needs to also be addressed.

`main.R` is what "works" so far. `dev.R` is what I'm working on. 

It doesn't work but I'm sharing it anyways.  
</details>


<details>
<summary>
<h3>How to use the rChess engine</h3>
</summary>

Right now nothing is packaged and the code is highly unstable so you will need to run the `main.R` script in the R console. 

The syntax right now is `piece no new_position`

An example of the gameplay is: 

```
> play_game()
White's Turn:pawn 3 c4
Black's Turn:knight 1 c6
White's Turn:queen a4
Black's Turn:knight 1 a5
White's Turn:quit
Game Concluded
```

![image](https://user-images.githubusercontent.com/46410142/218892237-d7f0e785-4480-44d0-863c-b195a55b78a9.png)

</details>
