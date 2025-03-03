source('wordleSolver.R')

makeNgrids = function(N){
  theGrids <- list()
  for(i in 1:N){
    theGrids[[length(theGrids)+1]] = wordGrid
  }
  return(theGrids)
}

elimFromGrid = function(theGrids, guess, results){
  gridLengths = unlist(lapply(theGrids,length))
  idx = which(gridLengths > 5)
  for(i in idx){
    grid = theGrids[[i]]
    res = results[[i]]
    if(nrow(grid)>1){
      grid = eliminateWords(grid,guess,res)
    }
    theGrids[[i]] = grid
  }
  return(theGrids)
}

gridToWords = function(grid){
  words = rep(NA,nrow(grid))
  for(i in 1:nrow(grid)){
    words[i] = paste(grid[i, ],collapse='')
  }
  return(words)
}

findCommonWords = function(theGrids){
  gridLengths = unlist(lapply(theGrids,length))/5
  idx = which(gridLengths>1)
  wordList = NULL
  for(i in idx){
    words = gridToWords(theGrids[[i]])
    wordList = c(wordList,words)
  }
}

enterGuess = function(){
  guess = readline(prompt="Enter guess ")
  return(guess)
}
enterResult = function(n){
  result = readline(prompt = paste("Enter result for grid",n," "))
  return(result)
}

playQuordle = function(){
  turn = 0
  theGrids = makeNgrids(4)
  gridLengths = unlist(lapply(theGrids,length))/5
  while(!all(gridLengths==1)){
    theGuess = enterGuess()
    if(gridLengths[1]>1){
      r1 = enterResult(1)
    }
    if(gridLengths[2]>1){
      r2 = enterResult(2)
    }
    if(gridLengths[3]>1){
      r3 = enterResult(3)
    }
    if(gridLengths[4]>1){
      r4 = enterResult(4)
    }
    theResult = list(r1, r2, r3, r4)
    theGrids = elimFromGrid(theGrids,theGuess,theResult)
  
    gridLengths = unlist(lapply(theGrids,length))/5
    if(!all(gridLengths==1)){
      minLength = min(gridLengths[gridLengths>1])
      shortest = which(gridLengths == minLength)
      for(i in 1:4){
        print(paste("Grid",i,'has length',gridLengths[i]))
      }
    }

    if(all(gridLengths==1)){
      print(theGrids)
    } else if(any(gridLengths < 15)){
      idx = which(gridLengths < 15)
      for(i in idx){
        print(paste("Grid ",i,":",sep=''))
        print(theGrids[[i]])
      }
    }
    turn = turn + 1
  }
  cat("\n\n")
  print(paste("Congratulations, you won in",turn,'turns'))
  cat("\n\n")
}

#############################################################
# It's good to guess "RAISE" and "DOUBT" as it helps you place vowels 
# and has a decent number of frequently used consonants.

# theGuess = "RAISE"
# theResult=list("XGXYX","YYXXG","XXYYX","YYXXG")
# theGrids = elimQuadGrid(theGrids,theGuess,theResult)
# 
# theGuess = "DOUBT"
# theResult=list("XXXXY","GXXXX","XXYXX","XXXXX")
# theGrids = elimQuadGrid(theGrids,theGuess,theResult)

# eliminateWords(theGrids[[1]],theGuess,theResult[[1]])

###########################################################
playOctordle = function(){
  turn = 0
  theGrids = makeNgrids(8)
  gridLengths = unlist(lapply(theGrids,length))/5
  theResult = list()
  while(!all(gridLengths==1)){
    theGuess = enterGuess()
    if(turn==0){
      for(g in 1:length(theGrids)){
        theResult[[length(theResult)+1]] = enterResult(g)
      }
    } else {
      for(g in 1:length(theGrids)){
        if(gridLengths[g]>1){
          theResult[[g]] = enterResult(g)
        }
      }
    }

    theGrids = elimFromGrid(theGrids,theGuess,theResult)
    
    gridLengths = unlist(lapply(theGrids,length))/5
    if(!all(gridLengths==1)){
      minLength = min(gridLengths[gridLengths>1])
      shortest = which(gridLengths == minLength)
      for(i in 1:length(theGrids)){
        print(paste("Grid",i,'has length',gridLengths[i]))
      }
    }
    
    if(all(gridLengths==1)){
      print(theGrids)
    } else if(any(gridLengths < 15)){
      idx = which(gridLengths < 15)
      for(i in idx){
        print(paste("Grid ",i,":",sep=''))
        print(theGrids[[i]])
      }
    }
    turn = turn + 1
  }
  cat("\n\n")
  print(paste("Congratulations, you won in",turn,'turns'))
  cat("\n\n")
}
