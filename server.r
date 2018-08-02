#

library(shiny)
library(quanteda)
library(stringr)

shinyServer(function(input, output) {
  
  freqn1<-readRDS(file = 'f1s.rds')
  freqn2<-readRDS(file = 'f2s.rds')
  freqn3<-readRDS(file = 'f3s.rds')
  freqn4<-readRDS(file = 'f4s.rds')
  
  profList <- read.csv('full-list-of-bad-words-csv-file_2018_03_26.csv', header = FALSE, as.is = TRUE)
  profList<- profList$V1
  
  #Function that cuts of preceding word in a 'formatted' (formed by tokens function) N-gram
  splaft <- function(inp)
  {
    sub(pattern = '_', replacement = "", x=strsplit(x=inp, split = '^[^_]+')[[1]][2])
  }
  
  KATZ <- function(input){
    
    #Pre-process the input so that it is in the same format as training data
    tokens(input, what = "sentence", remove_numbers = TRUE, 
           remove_punct= TRUE, remove_symbols = TRUE, remove_twitter = TRUE, 
           remove_url = TRUE, include_docvars = FALSE) %>% tokens_tolower() -> tokensCUST
    
    d<-unlist(tokensCUST)
    
    for(i in 1:length(d)) {
      
      d[i] <- paste0("<s> ", d[i])
    }
    
    tokens(d, what='fasterword', remove_numbers = TRUE, 
           remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, 
           remove_url = TRUE, ngrams = 2:3) %>% tokens_remove(profList) -> tokens
    
    input <- tokens[[length(tokens)]]
    input <- input[length(input)]
    
    nwords <- str_count(pattern = '_', input) + 1
    
    { # If the input phrase is short and has less than 3 words (including <s> symbol)
      # re-create input with 2 gram as opposed to 3 gram
      if (nwords == 2) {
        res<-  katz2(input)
      }
      
      else if (nwords == 3 ) {
        res <- katz3(input)
      }
    }
    #If there are any predictions with NA values replace them with predictions based on lower order N-gram
    if (sum(is.na(res$last)) > 0) {
      nwords2 <- str_count(pattern = '_', splaft(res$prev[1])) + 1 
      res2<-switch(nwords2, katz(splaft(res$prev[1])), katz2(splaft(res$prev[1])), katz3(splaft(res$prev[1]))) 
      
      #Combine the results to fill missing values
      naval <- is.na(res$last)
      Result <- rbind( res[!naval,c('last', 'prob')], res2[1:sum(naval),c('last', 'prob')])
    }
    else if (sum(is.na(res$last)) == 0) {Result <- res}
    
    #If end of sentence is predicted, it is replaced with a period.
    i<-Result$last == '</s>'
    Result$last[i] <- '.'
    
    Result[,c('last', 'prob')]
    
  }
  #Finction that accepts 3-gram input
  katz3 <- function(inp) {
    #Check if we have the input as a part of existing 4 gram
    ind <- freqn4$prev == inp
    if (sum(ind) >0) {
      
      data <- freqn4[ind]
      data <- data[order(data$prob, decreasing = TRUE),]
      data <- data[1:5,]
    }
    #If the input is not found in the existing 4-gram then we check if we have it as a separate N-1 gram
    else if (sum(ind) == 0) {
      #First check if we have necessary tri-gram
      ind2 <- freqn3$feature == inp
      if (sum(ind2 > 0)) {
        #If yes we use weighted Katz probablity:
        data <- katz2(splaft(inp))
        data$prob1 <- freqn3$alpha[ind2] * data$prob 
        data <- data[order(data$prob1, decreasing = TRUE),]
        data<-data[1:5,]
      }
      #If not then we use Katz probability of based on N-1 gram 
      else if(sum(ind2 == 0)) {
        
        data <- katz2(splaft(inp))
        data <- data[1:5,]
      }
    }
    
    
    data
  }
  
  
  #Function that accepts bi-gram input
  katz2 <- function(inp) {
    
    ind <- freqn3$prev == inp
    if (sum(ind) >0) {
      
      data <- freqn3[ind]
      data <- data[order(data$prob, decreasing = TRUE),]
      data <- data[1:5,]
    }
    
    else if (sum(ind) == 0) {
      #First check if we have necessary bi-gram
      ind2 <- freqn2$feature == inp
      if (sum(ind2 > 0)) {
        
        data <- katz(splaft(inp))
        data$prob1 <- freqn2$alpha[ind2] * data$prob 
        data <- data[order(data$prob1, decreasing = TRUE),]
        data<-data[1:5,]
      }
      
      else if(sum(ind2 == 0)) {
        
        data <- katz(splaft(inp)) 
        data <- data[1:5,]
      }
    }
    
    data
  }
  
  #Function that accepts a unigram as input
  katz <- function(inp) {
    
    ind <- freqn2$prev == inp
    if (sum(ind) >0) {
      
      data <- freqn2[ind]
      data <- data[order(data$prob, decreasing = TRUE),]
      data <- data[1:5,]
    }
    
    else if (sum(ind) == 0) {
      #Check if we have necessary uni-gram
      ind2 <- freqn1$feature == inp
      if (sum(ind2 > 0)) {
        
        data <- freqn1 
        data$prob <- data$prob * freqn1$alpha[ind2]
        data <- data[order(data$prob, decreasing = TRUE),]
        data <- data[1:5,]
      }
      #If the entered unigram is not found we predict most common unigram
      else if (sum(ind2 == 0)){
        
        data <- freqn1[order(freqn1$prob, decreasing = TRUE),]
        #Drop beginning & end of sentence markers as they are most probable by themselves
        data <- data[3:8,]
      }
    }
    
    data
  }
  
  
    output$data <- renderTable({ 
    if ( !input$text == "") {                
      pred <- KATZ(input$text)
      #Drop NA from pred if any
      pred <- pred[!is.na(pred$last),]
      colnames(pred) <- c('Prediction', "Probability")
      pred
    }
  })
  
  output$dataMain <- renderTable({ 
    if ( !input$text == "") {
      pred <- KATZ(input$text)
      #Drop NA from pred if any
      pred <- pred[!is.na(pred$last),]
      colnames(pred) <- c('Prediction', "Probability")
      p<-pred[1,]
      p
    }
  })
  url<-a("See Project Code on GitHub", href='https://github.com/darykov/cap')
  output$ur <- renderUI({
    tagList("URL link:", url)
  })
  
  
  
})
