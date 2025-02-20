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

#############################################################
# It's good to guess "RAISE" and "DOUBT" as it helps you place vowels 
# and has a decent number of frequently used consonants.

theGuess = "RAISE"
theResult=list("YYXXG","XXXXY","YXGXY","XXXGG")
theGrids = elimQuadGrid(theGrids,theGuess,theResult)

theGuess = "DOUBT"
theResult=list("GXXXX","GYXXY","YXXXX","XGXXX")
theGrids = elimQuadGrid(theGrids,theGuess,theResult)

# eliminateWords(theGrids[[1]],theGuess,theResult[[1]])

###########################################################
gridLengths = unlist(lapply(theGrids,length))/5
minLength = min(gridLengths[gridLengths>1])
shortest = which(gridLengths == minLength)
for(i in 1:4){
  print(paste("Grid",i,'has length',gridLengths[i]))
}

if(all(gridLengths==1)){
  print(theGrids)
} else {
  print(theGrids[[shortest]])
}
