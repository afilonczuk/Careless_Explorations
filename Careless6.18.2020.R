library(tidyverse)
library(careless)
library(dplyr)
library(psych)
library(DT)

#Read data
dat <- read_csv("~/Desktop/PEMLab copy/pilot5_merged_anon.csv")
   #t1_ma_ t1_macro_ t2_bfi_ t3_micro_ t4_math_recog t4_micro_
   #validityitem <- dat$t1_validity_1
   #correctanswer=5

#############Function that generates statistics that may indicate carelessness###################
 carelesstable <- function(dat2, factors = ncol(dat2), validityitem = NULL, correctanswer = 0){
  #dat2 is the subsetted data
  #validityitem = vector of responses to the validity item 
  #correctanswer = the correct answer to the validity item
  #factors	= a vector of integers specifying the length of each factor in the dataset
   if (is.null(validityitem)){
     val <- matrix(NA, nrow = nrow(dat2), ncol = 1)
   }else if (!is.null(validityitem)){
      val <- matrix(0, nrow = length(validityitem), ncol = 1)
      validityitem <- as.matrix(validityitem)
  #the val column will report 1 for an incorrect (careless) response, 0 for correct response, and NA for NA
        for (i in 1:length(validityitem)){
          if (is.na(validityitem[i])==TRUE){
              val[i] <- NA
              } else
          if (validityitem[i] != correctanswer){
              val[i] <- 1
             }
         }
   }
    #IRV - measure of variability
    irv(dat2)
    hist(irv(dat2))
    dat2.irv <- irv(dat2, split = TRUE, num.split = 4)
   
    out <- cbind(val, dat2.irv$irvTotal, dat2.irv$irv1, dat2.irv$irv2, dat2.irv$irv3, dat2.irv$irv4)
    
    #Longstring - calculates the longest string of same consecutive responses, and the average length of same consecutive responses
    longstring(dat2)
    dat2.longstring <- longstring(dat2, avg = TRUE)
    hist(longstring(dat2))
    hist(dat2.longstring$avgstr)
    out2 <- cbind(out, dat2.longstring$longstr, dat2.longstring$avgstr)
    
    #Mahalanobis Distance (D) and flag potential outlier
    #mahad(x, plot = TRUE, flag = FALSE, confidence = 0.99, na.rm = TRUE)
    dat2.mahad <- mahad(dat2, plot = TRUE, flag = TRUE, confidence = 0.99, na.rm = TRUE)
    out3 <- cbind(out2, dat2.mahad$raw, dat2.mahad$flagged)
     
    #EvenOdd - correlates the even-numbered items with the odd-numbered items
    dat2.eo <- evenodd(dat2, factors, diag = FALSE)
    
    #psychometric synonym/antonym score - calculates the responder's correlation between pairs of items that correlate beyond .5 (synonyms)
    #can also be done for antonyms (best before reverse coding)
    dat2.psychsyn <- psychsyn(dat2, critval = 0.5, anto = FALSE, diag = FALSE)
    out4 <- cbind(out3, dat2.eo, dat2.psychsyn)
    colnames(out4) <- cbind("Validity Item", "irvTotal", "irv1", "irv2", "irv3", "irv4", "Longest String", "Average String Length", "Mahalanobis D", "Flagged Potential Outliers", "Even-Odd Score", "Synonyms")
    
    return(out4)
  }
  
  #psychometric antonym score
  psychant(dat3, critval = -0.4, diag = FALSE)
  #no strong antonyms - see psychsyn_critval(dat, anto = TRUE)
  #correlations between all possible item pairs and order them by the magnitude of the correlation
  psychsyn_critval(dat2, anto = TRUE)
  psychsyn_critval(dat2, anto = FALSE)

#####################################################################################################
#####  Implementing carelesstable() on pilot 5 data
#####################################################################################################
#Subset Data: t1 responses
#dat2 has survey data from t1
dat2 <- dat %>% select(starts_with( 't1_ma_'), starts_with( 't1_macro_'))
#validityitem = dat$t1_validity_1 #the t1 validity item is not in the codebook. I'm assuming its correct response is 5 given most of the answers are 5
#correctanswer=5
#factors = c(19,33)
t1.carelesstable <- carelesstable(dat2, dat$t1_validity_1, 5, c(19,33))
dat2.irv <- irv(dat2, split = TRUE, num.split = 4)
boxplot(dat2.irv$irvTotal) 
boxplot(dat2.irv$irv1)     
boxplot(dat2.irv$irv2)
boxplot(dat2.irv$irv3)
boxplot(dat2.irv$irv4)

dat2.longstring <- longstring(dat2, avg = TRUE)
boxplot(longstring(dat2))
boxplot(dat2.longstring$avgstr)

#################################################################################################
#Subset Data: t2 responses
#dat3 has survey data from t2
dat3 <- dat %>% select(starts_with( 't2_bfi_'))
dat %>% select(starts_with( 't2_validity'))
#validityitem = dat$t2_validity1 
#correctanswer=4
#factors = rep(12, 5)
t2.carelesstable <- carelesstable(dat3, dat$t2_validity1, 4, rep(12, 5))
dat3.irv <- irv(dat3, split = TRUE, num.split = 4)
boxplot(dat3.irv$irvTotal) 
boxplot(dat3.irv$irv1)     
boxplot(dat3.irv$irv2)
boxplot(dat3.irv$irv3)
boxplot(dat3.irv$irv4)

dat3.longstring <- longstring(dat3, avg = TRUE)
boxplot(longstring(dat3))
boxplot(dat3.longstring$avgstr)

#################################################################################################
#Subset Data: t3 responses
#dat4 has survey data from t3
dat4 <- dat %>% select(starts_with( 't3_micro_'))
dat %>% select(starts_with( 't3_validity'))
#validityitem = dat$t3_validity 
#correctanswer=4
#factors = c(4, rep(8, 3))
t3.carelesstable <- carelesstable(dat4, dat$t3_validity, 4, c(4, rep(8, 3)))
dat4.irv <- irv(dat4, split = TRUE, num.split = 4)
boxplot(dat4.irv$irvTotal) 
boxplot(dat4.irv$irv1)     
boxplot(dat4.irv$irv2)
boxplot(dat4.irv$irv3)
boxplot(dat4.irv$irv4)

dat4.longstring <- longstring(dat4, avg = TRUE)
boxplot(longstring(dat4))
boxplot(dat4.longstring$avgstr)

#################################################################################################
#Subset Data: t4 responses
#dat5 has survey data from t4
dat5 <- dat %>% select(starts_with( 't4_math_recog'), starts_with( 't4_micro_'))
dat %>% select(starts_with( 't4_validity'))
#validityitem = dat$t4_validity 
#correctanswer=4
#factors = c(7, rep(8, 3))
t4.carelesstable <- carelesstable(dat5, dat$t4_validity, 4, c(7, rep(8, 3)))
dat5.irv <- irv(dat5, split = TRUE, num.split = 4)
boxplot(dat5.irv$irvTotal) 
boxplot(dat5.irv$irv1)     
boxplot(dat5.irv$irv2)
boxplot(dat5.irv$irv3)
boxplot(dat5.irv$irv4)

dat5.longstring <- longstring(dat5, avg = TRUE)
boxplot(longstring(dat5))
boxplot(dat5.longstring$avgstr)

#######################################################################################################
#### Flag careless responses (typically outliers) in red in an html file
#######################################################################################################
flag.table <- function(dat.table){
 
  datatable(dat.table, rownames = FALSE) %>%
  formatStyle(columns = "Validity Item", 
              background = styleEqual(c(1), c("red"))) %>%
    formatStyle(columns = "irvTotal", 
                background = styleInterval(c(boxplot.stats(dat.table[,2])$stats[1], boxplot.stats(dat.table[,2])$stats[5]), c("red", "white", "red"))) %>%
    formatStyle(columns = "irv1", 
            background = styleInterval(c(boxplot.stats(dat.table[,3])$stats[1], boxplot.stats(dat.table[,3])$stats[5]), c("red", "white", "red"))) %>%
  formatStyle(columns = "irv2", 
              background = styleInterval(c(boxplot.stats(dat.table[,4])$stats[1], boxplot.stats(dat.table[,4])$stats[5]), c("red", "white", "red"))) %>%
  formatStyle(columns = "irv3", 
              background = styleInterval(c(boxplot.stats(dat.table[,5])$stats[1], boxplot.stats(dat.table[,5])$stats[5]), c("red", "white", "red"))) %>%
  formatStyle(columns = "irv4", 
              background = styleInterval(c(boxplot.stats(dat.table[,6])$stats[1], boxplot.stats(dat.table[,6])$stats[5]), c("red", "white", "red"))) %>%
  formatStyle(columns = "Longest String", 
              background = styleInterval(boxplot.stats(dat.table[,7])$stats[5], c( "white", "red"))) %>%
  formatStyle(columns = "Average String Length", 
              background = styleInterval(boxplot.stats(dat.table[,8])$stats[5], c("white", "red"))) %>%
  formatStyle(columns = "Mahalanobis D", 
              background = styleInterval(75, c( "white", "red"))) %>%
  formatStyle(columns = "Flagged Potential Outliers", 
              background = styleEqual(1, c("red"))) %>%
  formatStyle(columns = "Even-Odd Score", 
              background = styleInterval(0.1, c( "red", "white"))) %>%
    formatStyle(columns = "Synonyms", 
                background = styleInterval(0.1, c( "red", "white")))
  
}

#############################################################################
## Implement flag.table() on the pilot 5 data
#############################################################################

t1.flagged <- flag.table(t1.carelesstable)
t2.flagged <- flag.table(t2.carelesstable)
t3.flagged <- flag.table(t3.carelesstable)
t4.flagged <- flag.table(t4.carelesstable)


##########################################################################################
#### Generate the rates of careless responders
####################################################################

careless.rates <- function(dat.table){
  out <- matrix(0, nrow = 1, ncol = ncol(dat.table))
  for (i in 1:length(dat.table[,1])){ 
    if(is.na(dat.table[i,1]) ){} else
    if (dat.table[i,1] == 1){
      out[1,1] <- out[1,1] + 1
    }
  }
  for (i in 1:length(dat.table[,2])){
    if(is.na(dat.table[i,2]) ){} else
    if (dat.table[i,2] < boxplot.stats(dat.table[,2])$stats[1] || dat.table[i,2] > boxplot.stats(dat.table[,2])$stats[5]){
      out[1,2] <- out[1,2] + 1
    }
  }
  for (i in 1:length(dat.table[,3])){
    if(is.na(dat.table[i,3]) ){} else
    if (dat.table[i,3] < boxplot.stats(dat.table[,3])$stats[1] || dat.table[i,3] > boxplot.stats(dat.table[,3])$stats[5]){
      out[1,3] <- out[1,3] + 1
    }
  }
  for (i in 1:length(dat.table[,4])){
    if(is.na(dat.table[i,4]) ){} else
    if (dat.table[i,4] < boxplot.stats(dat.table[,4])$stats[1] || dat.table[i,4] > boxplot.stats(dat.table[,4])$stats[5]){
      out[1,4] <- out[1,4] + 1
    }
  }
  for (i in 1:length(dat.table[,5])){
    if(is.na(dat.table[i,5]) ){} else
    if (dat.table[i,5] < boxplot.stats(dat.table[,5])$stats[1] ){
      out[1,5] <- out[1,5] + 1
    } else if  (dat.table[i,5] > boxplot.stats(dat.table[,5])$stats[5]){
      out[1,5] <- out[1,5] + 1
    }
  }
  for (i in 1:length(dat.table[,6])){
    if(is.na(dat.table[i,6]) ){} else
    if (dat.table[i,6] < boxplot.stats(dat.table[,6])$stats[1] || dat.table[i,6] > boxplot.stats(dat.table[,6])$stats[5]){
      out[1,6] <- out[1,6] + 1
    }
  }
  for (i in 1:length(dat.table[,7])){
    if(is.na(dat.table[i,7]) ){} else
    if (dat.table[i,7] > boxplot.stats(dat.table[,7])$stats[5]){
      out[1,7] <- out[1,7] + 1
    }
  }
  for (i in 1:length(dat.table[,8])){
    if(is.na(dat.table[i,8]) ){} else
    if (dat.table[i,8] > boxplot.stats(dat.table[,8])$stats[5]){
      out[1,8] <- out[1,8] + 1
    }
  }
  for (i in 1:length(dat.table[,9])){
    if(is.na(dat.table[i,9]) ){} else
    if (dat.table[i,9] > 75){
      out[1,9] <- out[1,9] + 1
    }
  }
  for (i in 1:length(dat.table[,10])){
    if(is.na(dat.table[i,10]) ){} else
    if (dat.table[i,10] == 1){
      out[1,10] <- out[1,10] + 1
    }
  }
  for (i in 1:length(dat.table[,11])){
    if(is.na(dat.table[i,11]) ){} else
    if (dat.table[i,11] <0.1){
      out[1,11] <- out[1,11] + 1
    }
  }
  for (i in 1:length(dat.table[,12])){
    if(is.na(dat.table[i,12]) ){} else
      if (dat.table[i,12] <0.1){
        out[1,12] <- out[1,12] + 1
      }
  }
  out <- out/nrow(dat.table)
  colnames(out) <- colnames(dat.table)
  return(out)
  }

############################################################################################################
######## Implement careless.rates() on pilot 5 data
############################################################################################################
 
 t1.rates <- careless.rates(t1.carelesstable)
 t2.rates <- careless.rates(t2.carelesstable)
 t3.rates <- careless.rates(t3.carelesstable)
 t4.rates <- careless.rates(t4.carelesstable)
   
 write.table( signif(t1.rates, 4), sep = ",", quote = FALSE)
 write.table( signif(t2.rates, 4), sep = ",", quote = FALSE)
 write.table( signif(t3.rates, 4), sep = ",", quote = FALSE)
 write.table( signif(t4.rates, 4), sep = ",", quote = FALSE)

############################################################################################################
######## Example response vectors that were flagged
############################################################################################################

  write.table(dat2[69,40:52], quote = FALSE)
  write.table(dat2[128, 40:52], quote = FALSE)
 
