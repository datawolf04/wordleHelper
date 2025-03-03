allWordsRaw = read.csv('allWords.csv',header=FALSE)
allWordsChar = apply(allWordsRaw, MARGIN = 1, as.character)

wordToVec = function(word){
  nLett = nchar(word)
  lett = character(nLett)
  for(i in 1:nLett){
    lett[i] = substr(word,i,i)
  }
  return(lett)
}

nWords = length(allWordsChar)
wordGrid = NULL
for(i in 1:nWords){
  wordGrid = rbind(wordGrid, wordToVec(allWordsChar[i]))
}

eliminateWords = function(grid, guess, result){
  guessVec = tolower(wordToVec(guess))
  resultVec = wordToVec(result)
  
  if(any(resultVec=="G")){ # Letter correctly placed
    idx = which(resultVec %in% "G")
    for(i in idx){
      if(length(grid)>5){
        # Keep words with correctly placed letter
        grid = grid[ grid[ ,i] == guessVec[i], ]
      }
    }
  } 
  
  if(any(resultVec=="Y")){ # Letter is correct, but in wrong place
    idx = which(resultVec %in% "Y")
    for(i in idx){
      if(length(grid)>5){
        # Remove words with letter in wrong place
        grid = grid[ grid[ ,i] != guessVec[i], ]
      }
      if(length(grid)>5){
        # Require that words have at least one of this letter
        grid = grid[rowSums(grid == guessVec[i]) > 0.5, ]
      }
    }
  } 
  
  if(any(resultVec=="X")){ # Letter is incorrect and should be removed
    idx = which(resultVec %in% "X")
    for(i in idx){
      if(length(grid)>5){
        guessLetter = guessVec[i]
        if(sum(guessVec==guessLetter)<1.5){ # Letter is not repeated
          grid = grid[rowSums(grid == guessVec[i]) < 0.5, ]
        }
      }
    }
  }
  return(grid)
}

enterGuess = function(){
  guess = readline(prompt="Enter guess ")
  return(guess)
}
enterTheResult = function(){
  result = readline(prompt = "Enter result ")
  return(result)
}

playWordle = function(){
  thisGrid = wordGrid
  gridLength = length(thisGrid)/5
  while(gridLength>1){
    theGuess = enterGuess()
    theResult = enterTheResult()
    thisGrid = eliminateWords(thisGrid,theGuess,theResult)
    gridLength = length(thisGrid)/5
    print(paste("There are",gridLength,"words remaining"))
    if(gridLength > 20){
      print(head(thisGrid))
    } else {
      print(thisGrid)
    }
  }
  cat("\n\nCongratulations, you win!\n\n")
}
