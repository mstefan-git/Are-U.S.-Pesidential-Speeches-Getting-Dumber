# Replication file for: "Are U.S. Pesidential Speeches Getting Dumber?"
# RPubs-link: https://rpubs.com/mstefan-rpubs/speeches
# (c) Martin Stefan, June 2020

rm(list = ls())

library(tm)
library(stringr)
library(quanteda)
library(tidyverse)

# "not in" function
'%!in%' <- function(x,y)!('%in%'(x,y))

# function to compute readability scores
readScores <- function(text){
  
  # split text into sentences
  sentences <- strsplit(text, "[\\.:;?!]")
  sentences <- unlist(sentences)
  while(TRUE){
    if(min(nchar(sentences),na.rm=T) > 2) break()
    for(i in 1:length(sentences)) {
      if(nchar(sentences[i]) < 3){
        sentences[i-1] <- paste(sentences[i-1],
                                sentences[i])
        sentences[i] <- NA
        if(i != length(sentences)){
          sentences[i-1] <- paste(sentences[i-1],
                                  sentences[i+1])
          sentences[i+1] <- NA
        }
        sentences <- sentences[!is.na(sentences)]
        break()
      }
    }
  }
  sentences <- tolower(sentences)
  
  # remove punctuation and split text into words
  words <- paste(sentences, collapse="")
  words <- gsub(pattern="\\W", replace=" ", text)
  words <- stripWhitespace(words)
  words <- unlist(str_split(words, " "))
  words <- words[nchar(words) != 0]
  words <- tolower(words)
  
  # identify syllables
  syllables <- sapply(words,nsyllable)
  
  # number of characters, words, sentences and syllables
  n_c <- sum(nchar(words))
  n_w <- length(words)
  n_se <- length(sentences)
  n_sy <- sum(syllables, na.rm=T)

  # flesch reading ease score (0 to 100)
  fres <- 206.835 - 1.015*n_w/n_se - 84.6*n_sy/n_w
  
  # flesch-kincaid grade level
  fkgl <- 0.39*n_w/n_se + 11.8*n_sy/n_w - 15.59
  
  # automated readability index (grade level)
  ari <- 4.71*n_c/n_w + .5*n_w/n_se - 21.43
  
  # coleman-liau index (grade level)
  cli <- 0.0588*n_c/n_w*100 - 0.296*n_se/n_w*100 - 15.8
  
  # linsear write metric (grade level)
  tab <- sort(table(syllables))
  value <- sum(tab[c("1","2")])
  value <- value + sum(3*tab[which(names(tab)%!in%c("1","2"))])
  value <- value/n_se
  lwm <- ifelse(value > 20, value/2, (value-2)/2)
  
  # return
  return(list(
    n_se=n_se, n_w=n_w, n_sy=n_sy, n_c=n_c,
    fres=fres, fkgl=fkgl, ari=ari, cli=cli, lwm=lwm)
    )
  
} # end function

# load speeches 
speeches <- readRDS("speeches.RDS")

# compute readability scores
scores <- lapply(speeches, function(x) readScores(x$text))
df <- as.data.frame(matrix(unlist(scores),ncol=9,byrow=T))
names(df) <- names(scores[[1]])

# order df by date (instead of alphabet)
dates <- as.Date(unlist(lapply(speeches,function(x) x$date)), "%B %d, %Y")
presidents <- unlist(lapply(speeches,function(x) x$president))
df$date <- dates
df$president <- presidents
df <- df[,c( (ncol(df)-1):ncol(df), 1:(ncol(df)-2) )]
df <- arrange(df,date)

# save dataframe
saveRDS(df, "scores.RDS")
beepr::beep(2)
