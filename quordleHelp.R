source('wordleSolver.R')

g1 = wordGrid
g2 = wordGrid
g3 = wordGrid
g4 = wordGrid
theGrids = list(g1, g2, g3, g4)

elimQuadGrid = function(theGrids, guess, results){
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
  g1 = wordGrid
  g2 = wordGrid
  g3 = wordGrid
  g4 = wordGrid
  theGrids = list(g1, g2, g3, g4)
  gridLengths = unlist(lapply(theGrids,length))/5
  while(!all(gridLengths==1)){
    theGuess = enterGuess()
    r1 = enterResult(1)
    r2 = enterResult(2)
    r3 = enterResult(3)
    r4 = enterResult(4)
    theResult = list(r1, r2, r3, r4)
    theGrids = elimQuadGrid(theGrids,theGuess,theResult)
  
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
  }
  cat("\n\nCongratulations, you win!\n\n")
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
