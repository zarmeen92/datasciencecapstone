library(tm)
library(stringi)
library(RWeka)
library(ggplot2) 
library(wordcloud)
library(dplyr)
library(SnowballC)   
library(stringr)


loadfiles <- function(){

  wf_B <- readRDS(file = "./wf_B.RDS")
  wf_T <- readRDS(file = "./wf_T.RDS")
  wf_Q <- readRDS(file = "./wf_Q.RDS")
  tri_SGT_DT <- readRDS(file = "./tri_SGT_DT.RDS")
  quad_SGT_DT <- readRDS(file = "./quad_SGT_DT.RDS")
  bi_SGT_DT <- readRDS(file = "./bi_SGT_DT.RDS")
  
  
}
clean <-function(text){
  
  cleanText <- tolower(text)
  cleanText <- removePunctuation(cleanText)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  #cleanText <- removeWords(cleanText,stopwords("english"))
  print(cleanText)
  return(cleanText)
}


#predict using 4gram
predict_model <- function(sentence) {
  # given a sentence/phrase, extract the last two words
  sentence <- clean(sentence)
  print(sentence)
  sl <- unlist(str_split(sentence," "))
  len <- length(sl)
  trigram <- paste(sl[len-2],sl[len-1],sl[len])
  trigram
  
  # get the subset of the quadgram data table witha matching trigram start
  swf_Q <- wf_Q[wf_Q$start == trigram,]
  #check if trigram was found in the quadgram table
  if(nrow(swf_Q) > 0) {
    # use the counts in the Simple GT table to extract the probability
    swf_Q$p <- sapply(swf_Q$count,FUN=function(x) quad_SGT_DT$p[quad_SGT_DT$r==x])
    # order by probability
    #swf_T <- swf_T[with(swf_T,order(-p))]
    # find the largest probability
    maxP <-max(swf_Q$p)
    #get the end words with the highest probability
    predictList <- swf_Q$end[swf_Q$p == maxP]
    predictions <- vector()
    for(i in 1:length(predictList)) {
      predictions[i] <- paste(sentence,predictList[i])
    }
    # return(predictions)
    pl_T <- data.frame(words=swf_Q$end,probs=swf_Q$p)
    return(pl_T[1:5,])
  } else {
    print(paste("No match for trigram",trigram,"in",sentence,"--looking for bigram match"))
    bigram <- paste(sl[len-1],sl[len])
    bigram
    swf_B <- wf_T[wf_T$start == bigram,]
    if(nrow(swf_B) > 0) {
      # use the counts in the Simple GT table to extract the probability
      swf_B$p <- sapply(swf_B$count,FUN=function(x) tri_SGT_DT$p[tri_SGT_DT$r==x])
      # order by probability
      swf_B <- swf_B[with(swf_B,order(-p)),]
      # find the largest probability
      maxP <-max(swf_B$p)
      #get the end words with the highest probability
      predictList <- swf_B$end[swf_B$p == maxP]
      predictions <- vector()
      
      for(i in 1:length(predictList)) {
        predictions[i] <- paste(sentence,predictList[i])
      }
      pl_B <- data.frame(words=swf_B$end,probs=swf_B$p)
      return(pl_B[1:5,])
    } else {
      print(paste("No match for bigram",bigram,"in",sentence,"--looking for unigram match"))
      unigram <- sl[len]
      unigram
      swf_B <- wf_B[wf_B$start == unigram,]
      if(nrow(swf_B) > 0) {
        # use the counts in the Simple GT table to extract the probability
        swf_B$p <- sapply(swf_B$count,FUN=function(x) bi_SGT_DT$p[bi_SGT_DT$r==x])
        # order by probability
        swf_B <- swf_B[with(swf_B,order(-p)),]
        # find the largest probability
        maxP <-max(swf_B$p)
        #get the end words with the highest probability
        predictList <- swf_B$end[swf_B$p == maxP]
        predictions <- vector()
        
        for(i in 1:length(predictList)) {
          predictions[i] <- paste(sentence,predictList[i])
        }
        pl_B <- data.frame(words=swf_B$end,probs=swf_B$p)
        return(pl_B[1:5,])
      }
      else {
        return(data.frame(words=c("No suggestions found"),probs=c(0)))
      }
    }
  }
  
}
