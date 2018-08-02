library(quanteda)
library(dplyr)
library(stringr)
#Some utility functions to be used for splitting processed N-grams
splprev <- function(inp) {
  strsplit(x = inp, split = '_([^_]+)$')[[1]]
}
spllast <- function(inp)
{
  strsplit(x=inp, split = '^.+_')[[1]][2]
  
}

splaft <- function(inp)
{
  sub(pattern = '_', replacement = "", x=strsplit(x=inp, split = '^[^_]+')[[1]][2])
}

quanteda_options("threads" = 4)

con0 <- file(description = "en_US/en_US.twitter.txt", open = "r" )
tw <- readLines(con = con0)
close(con0)

con1 <- file(description = "en_US/en_US.news.txt", open = "r")
ns <- readLines(con = con1)
close(con1)

con2 <- file(description = "en_US/en_US.blogs.txt", open = "r")
bl <- readLines(con = con2)
close(con2)

set.seed(12)

size <- 0.01

trialstw <- rbinom(n = length(tw), size = 1, prob = size)
trialsns <- rbinom(n = length(ns), size = 1, prob = size)
trialsbl <- rbinom(n = length(bl), size = 1, prob = size)

seltw <- tw[trialstw == 1]
seltw <- gsub(pattern = "_+", replacement = "", x = seltw)
seltw <- iconv(seltw, "latin1", "ASCII", sub="")
selns <- iconv(seltw, "ASCII", "utf8", sub="")

selns <- tw[trialsns == 1]
selns <- gsub(pattern = "_+", replacement = "", x = selns)
selns <- iconv(selns, "latin1", "ASCII", sub="")
selns <- iconv(selns, "ASCII", "utf8", sub="")
selbl <- tw[trialsbl == 1]
selbl <- gsub(pattern = "_+", replacement = "", x = selbl)
selbl <- iconv(selbl, "latin1", "ASCII", sub="")
selns <- iconv(selbl, "ASCII", "utf8", sub="")

twcorpus <- corpus(seltw)
nscorpus <- corpus(selns)
blcorpus <- corpus(selbl)


combinedCorp <- twcorpus + nscorpus + blcorpus

#Download a list of profane words:

if(!file.exists('pr.zip')) {
  download.file(url = "https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-csv-file_2018_03_26_26.zip", destfile = "pr.zip", method = "auto")}

profList <- read.csv(unzip('pr.zip'), header = FALSE, as.is = TRUE)
profList<- profList$V1
###################################################################################
###Custom processing
tokens(combinedCorp, what = "sentence", remove_numbers = TRUE, 
       remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, 
       remove_url = TRUE, verbose = TRUE, include_docvars = FALSE) %>% tokens_tolower() -> tokensCUST

d<-unlist(tokensCUST)
#Add beginning & end of sentence markers
for(i in 1:length(d)) {
  
  d[i] <- paste0("<s> ", d[i], " </s>")
} 

tokens(d, what='fasterword', remove_numbers = TRUE, 
       remove_punct = TRUE, remove_symbols = TRUE, remove_twitter = TRUE, 
       remove_url = TRUE, verbose = TRUE, include_docvars = FALSE) %>% tokens_remove(profList) -> tokens

DFM1 <- dfm(tokens, ngrams = 1)
DFM2 <- dfm(tokens, ngrams = 2)
DFM3 <- dfm(tokens, ngrams = 3)
DFM4 <- dfm(tokens, ngrams = 4)

freqn1 <- textstat_frequency(DFM1)
freqn1 <- freqn1[,1:2]
freqn1$last <- freqn1$feature

freqn2 <- textstat_frequency(DFM2)
freqn2 <- freqn2[,1:2]
freqn2$prev <- sapply(freqn2$feature, splprev)
freqn2$last <- sapply(freqn2$feature, spllast)

freqn3 <- textstat_frequency(DFM3)
freqn3 <- freqn3[,1:2]
freqn3$prev <- sapply(freqn3$feature, splprev)
freqn3$last <- sapply(freqn3$feature, spllast)

freqn4 <- textstat_frequency(DFM4)
freqn4 <- freqn4[,1:2]
freqn4$prev <- sapply(freqn4$feature, splprev)
freqn4$last <- sapply(freqn4$feature, spllast)

#Katz backopff computation
#First we need to compute the adjusted count as in Good-Turing smoothing

# 1. Compute adjusted counts
# Use a Linear regression model in Log space

a<-table(freqn1$frequency)
Nc <- data.frame(as.numeric(a), as.numeric(dimnames(a)[[1]]))
names(Nc) <- c("Nc","count")

#plot(y=log(Nc$Nc), x=log(Nc$count))
fit <- lm(log(Nc$Nc)~log(Nc$count))

#Function to calculate the predicted Nc value
NC_predict <- function(fit, input) {
  
  value <- max(fit$coefficients[1] + fit$coefficients[2]*log(input), 0)
  exp(value)
  
}

#Used to calculate adjusted count (Good-Turing) using a linear model fir in log space
adj <- function(count, fit) {
  
  if (count > 6) res <- count
  else {res <- ((count + 1)*(NC_predict(fit, count+1)/NC_predict(fit, count)) - count * 6*(NC_predict(fit, 6))/NC_predict(fit, 1)) / (1 - (6*(NC_predict(fit, 6))/NC_predict(fit, 1))) }
  res
}

#Calculate adjusted count
freqn1$adjust <- freqn1$frequency
freqn1$adjust <- sapply(X = freqn1$frequency, adj, fit = fit)

#Check the number of tokens (ntoken) sum(freqn1$frequency)
total1 <- sum(freqn1$frequency)
mutate(freqn1, prob = adjust / total1) -> freqn1

a2<-table(freqn2$frequency)
Nc2 <- data.frame(as.numeric(a2), as.numeric(dimnames(a2)[[1]]))
names(Nc2) <- c("Nc","count")

#plot(y=log(Nc2$Nc), x=log(Nc2$count))
fit2 <- lm(log(Nc2$Nc)~log(Nc2$count))

#Calculate adjusted count
freqn2$adjust <- freqn2$frequency
freqn2$adjust <- sapply(X = freqn2$frequency, adj, fit = fit2)

#Check the number of tokens (ntoken) sum(freqn1$frequency)

#Use for loop to calculate adjusted probabilities for 

for (i in 1:length(freqn2$feature)) {
  
  freqn2$prob[i] <- freqn2$adjust[i] / freqn1$frequency[freqn1$feature == freqn2$prev[i]]
  #ML probability only used in development portion to make sure they provide proper probability distribution
  # freqn2$MLprob[i] <- freqn2$frequency[i] / freqn1$frequency[freqn1$feature == freqn2$prev[i]]
  
}

a3<-table(freqn3$frequency)
Nc3 <- data.frame(as.numeric(a3), as.numeric(dimnames(a3)[[1]]))
names(Nc3) <- c("Nc","count")

#plot(y=log(Nc3$Nc), x=log(Nc3$count))
fit3 <- lm(log(Nc3$Nc)~log(Nc3$count))

#Calculate adjusted count
freqn3$adjust <- freqn3$frequency
freqn3$adjust <- sapply(X = freqn3$frequency, adj, fit = fit3)

#Use for loop to calculate adjusted probabilities

for (i in 1:length(freqn3$feature)) {
  
  freqn3$prob[i] <- freqn3$adjust[i] / freqn2$frequency[freqn2$feature == freqn3$prev[i]]
  #ML probability only used in development portion to make sure they provide proper probability distribution
  #  freqn3$MLprob[i] <- freqn3$frequency[i] / freqn2$frequency[freqn2$feature == freqn3$prev[i]]
  
}

# for 4-grams

a4<-table(freqn4$frequency)
Nc4 <- data.frame(as.numeric(a4), as.numeric(dimnames(a4)[[1]]))
names(Nc4) <- c("Nc","count")

#plot(y=log(Nc4$Nc), x=log(Nc4$count))
fit4 <- lm(log(Nc4$Nc)~log(Nc4$count))

#Calculate adjusted count
freqn4$adjust <- freqn4$frequency
freqn4$adjust <- sapply(X = freqn4$frequency, adj, fit = fit4)

#Use for loop to calculate adjusted probabilities

for (i in 1:length(freqn4$feature)) {
  
  freqn4$prob[i] <- freqn4$adjust[i] / freqn3$frequency[freqn3$feature == freqn4$prev[i]]
  #ML probability only used in development portion to make sure they provide proper probability distribution
  # freqn4$MLprob[i] <- freqn4$frequency[i] / freqn3$frequency[freqn3$feature == freqn4$prev[i]]
  
}

#Beta calculations
for (i in 1:length(freqn3$feature)) {
  
  ind<-freqn4$prev ==  freqn3$feature[i]
  freqn3$beta[i] <- (1 - sum(freqn4$prob[ind]))
  
}

for (i in 1:length(freqn2$feature)) {
  
  ind<-freqn3$prev ==  freqn2$feature[i]
  freqn2$beta[i] <- (1 - sum(freqn3$prob[ind]))
  
}

for (i in 1:length(freqn1$feature)) {
  
  ind<-freqn2$prev ==  freqn1$feature[i]
  freqn1$beta[i] <- (1 - sum(freqn2$prob[ind]))
  
}

#Computes denominator in alpha formula
alphaDenom <- function(W) {
  
  subs <- freqn3[freqn3$prev == splaft(W)]
  ind <-  subs$last %in% freqn4$last[freqn4$prev == W] 
  s<- sum(subs$prob[ind])
  s
}

#Calculate alphas
for (i in 1:length(freqn3$feature)) {
  
  freqn3$alpha[i] <- freqn3$beta[i]/(1-alphaDenom(freqn3$feature[i]))
  
}


alphaDenom2 <- function(W) {
  
  subs <- freqn2[freqn2$prev == splaft(W)]
  ind <-  subs$last %in% freqn3$last[freqn3$prev == W] 
  s<- sum(subs$prob[ind])
  s
}

#Calculate alphas
for (i in 1:length(freqn2$feature)) {
  
  freqn2$alpha[i] <- freqn2$beta[i]/(1-alphaDenom2(freqn2$feature[i]))
  
}

#Calculate alpha for unigrans
alphaDenom3 <- function(W) {
  
  subs <- freqn1[freqn1$prev == splaft(W)]
  ind <-  subs$last %in% freqn2$last[freqn2$prev == W] 
  s<- sum(subs$prob[ind])
  s
}

#Calculate alphas
for (i in 1:length(freqn1$feature)) {
  
  freqn1$alpha[i] <- freqn1$beta[i]/(1-alphaDenom3(freqn1$feature[i]))
  
}

#Select columns with final results omitting intermidiate calculations
freqn1 <- freqn1[,c("feature", "last", "prob", "alpha")]
freqn2 <- freqn2[,c("feature", "prev", "last", "prob", "alpha")]
freqn3 <- freqn3[,c("feature", "prev", "last", "prob", "alpha")]
freqn4 <- freqn4[,c("feature", "prev", "last", "prob")]
rownames(freqn1) <- NULL
rownames(freqn2) <- NULL
rownames(freqn3) <- NULL
rownames(freqn4) <- NULL


#Save the resulting tables in RDS format
saveRDS(freqn1, file = 'f1s.rds')
saveRDS(freqn2, file = 'f2s.rds')
saveRDS(freqn3, file = 'f3s.rds')
saveRDS(freqn4, file = 'f4s.rds')


rm(list = ls())
#The tables are ready to be used in the Shiny app
freqn1<-readRDS(file = 'f1s.rds')
freqn2<-readRDS(file = 'f2s.rds')
freqn3<-readRDS(file = 'f3s.rds')
freqn4<-readRDS(file = 'f4s.rds')
