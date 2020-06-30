library(tidyverse)
library(careless)
library(dplyr)
library(psych)
library(DT)
library(corrplot)

#Read data
dat <- read_csv("~/Desktop/PEMLab copy/pilot5_merged_anon.csv")
#t1_ma_ t1_macro_ t2_bfi_ t3_micro_ t4_math_recog t4_micro_

###Create a table calculating each measure of carelessness for each responder
 carelesstable <- function(dat2, factors = ncol(dat2), validityitem = NULL, correctanswer = 0 ){
  #dat2 is the subsetted data
  #validityitem = vector of responses to the validity item from the administration subsetted
  #correctanswer = the correct answer to the validity item
  #factors	= a vector of integers specifying the length of each factor in the dataset
   if (is.null(validityitem)){
     val <- matrix(NA, nrow = nrow(dat2), ncol = 1)
   }else if (!is.null(validityitem)){
      val <- matrix(0, nrow = length(validityitem), ncol = 1)
      validityitem <- as.matrix(validityitem)
  #the val column will report 1 for an incorrect (careless) response, 0 for correct response, and NA for NA
        for (i in 1:length(validityitem)){
          if (is.na(validityitem[i])){
              val[i] <- NA
              } else
          if (validityitem[i] != correctanswer){
              val[i] <- 1
             }
         }
   }
    #IRV 
    irv(dat2)
    dat2.irv <- irv(dat2, split = TRUE, num.split = 4)
    out <- cbind(val, dat2.irv$irvTotal, dat2.irv$irv1, dat2.irv$irv2, dat2.irv$irv3, dat2.irv$irv4)
    
    #Longstring
    longstring(dat2)
    dat2.longstring <- longstring(dat2, avg = TRUE)
    out2 <- cbind(out, dat2.longstring$longstr, dat2.longstring$avgstr)
    
    #Mahalanobis Distance (D) and flag potential outlier
    dat2.mahad <- mahad(dat2, plot = TRUE, flag = TRUE, confidence = 0.99, na.rm = TRUE)
    out3 <- cbind(out2, dat2.mahad$raw, dat2.mahad$flagged)
     
    #EvenOdd
    dat2.eo <- evenodd(dat2, factors, diag = FALSE)
    
    #psychometric synonym/antonym score
    dat2.psychsyn <- psychsyn(dat2, critval = 0.5, anto = FALSE, diag = FALSE)
    out4 <- cbind(out3, dat2.eo, dat2.psychsyn)
    colnames(out4) <- cbind("Validity Item", "irvTotal", "irv1", "irv2", "irv3", "irv4", "Longest String", "Average String Length", "Mahalanobis D", "Flagged Potential Outliers", "Even-Odd Score", "Synonyms")
    
    return(out4)
  }
  
  #psychometric antonym score
  psychant(dat3, critval = -0.4, diag = FALSE)
  #no strong antonyms in pilot 5 data
  # see psychsyn_critval()for list of correlations between  
  # item pairs, ordered by the magnitude of the correlation
  psychsyn_critval(dat2, anto = TRUE)
  psychsyn_critval(dat2, anto = FALSE)
  
#####################################################################################################
#Subset Data: t1 responses
#dat2 has survey data from t1
dat2 <- dat %>% select(starts_with( 't1_ma_'), starts_with( 't1_macro_'))
#validityitem = dat$t1_validity_1 #the t1 validity item is not in the codebook. I'm assuming its correct response is 5 given most of the answers are 5
#correctanswer=5
#factors = c(19,33)
t1.carelesstable <- carelesstable(dat2, c(19,33), dat$t1_validity_1, 5)
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
#validityitem = dat$t2_validity1 
#correctanswer=4
#factors = rep(12, 5)
t2.carelesstable <- carelesstable(dat3, rep(12, 5), dat$t2_validity1, 4)
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
#validityitem = dat$t3_validity 
#correctanswer=4
#factors = c(4, rep(8, 3))
t3.carelesstable <- carelesstable(dat4, c(4, rep(8, 3)), dat$t3_validity, 4)
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
#validityitem = dat$t4_validity 
#correctanswer=4
#factors = c(7, rep(8, 3))
t4.carelesstable <- carelesstable(dat5, c(7, rep(8, 3)), dat$t4_validity, 4)
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
##Correlation Plots
write.table(round(cor(t1.carelesstable, use = 'pairwise.complete.obs'), 2), quote = FALSE, sep=",")
write.table(round(cor(t2.carelesstable, use = 'pairwise.complete.obs'), 2), quote = FALSE, sep = ",")
write.table(round(cor(t3.carelesstable, use = 'pairwise.complete.obs'), 2), quote = FALSE, sep= ",")
write.table(round(cor(t4.carelesstable, use = 'pairwise.complete.obs'), 2), quote = FALSE, sep= ",")
corrplot(corPlot(t1.carelesstable), tl.cex = .5)
corrplot(corPlot(t2.carelesstable), tl.cex = .5)
corrplot(corPlot(t3.carelesstable), tl.cex = .5)
corrplot(corPlot(t4.carelesstable), tl.cex = .5)

#######################################################################################################
##Color flagged measures red in tabe of carelessness
redflag.table <- function(dat.table){
 
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

t1.flagged <- redflag.table(t1.carelesstable)
t2.flagged <- redflag.table(t2.carelesstable)
t3.flagged <- redflag.table(t3.carelesstable)
t4.flagged <- redflag.table(t4.carelesstable)

##########################################################################################
##Mark flagged measures with "1"; unflagged measures with "0"
flagged.table <- function(dat.table){
  out <- matrix(0, nrow = nrow(dat.table), ncol = ncol(dat.table))
  out[,1] <- dat.table[,1]
  
  for (i in 1:nrow(dat.table)){
    for (j in 2:6){
      if(is.na(dat.table[i,j]) ){
        out[i,j]<- NA
      } else
        if (dat.table[i,j] <= boxplot.stats(dat.table[,j])$stats[1] || dat.table[i,j] >= boxplot.stats(dat.table[,j])$stats[5]){
          out[i,j] <-  1
        }
    }}
  
  for (i in 1:nrow(dat.table)){
    for (j in 7:8){
      if(is.na(dat.table[i,j])){
        out[i,j] <- NA
      } else if(dat.table[i,j] >= boxplot.stats(dat.table[,j])$stats[5] ){
        out[i,j] <- 1
      }
    }
  }
  for (i in 1:nrow(dat.table)){
    if(is.na(dat.table[i,9]) ){
      out[i,9] <- NA
    } else
      if (dat.table[i,9] > 75){
        out[1,9] <-  1
      }
  }
  out[,10] <- dat.table[,10]
  
  for (i in 1:nrow(dat.table)){
    for(j in 11:12){
      if(is.na(dat.table[i,j]) ){
        out[i,j] <- NA
      } else
        if (dat.table[i,j] <0.1){
          out[i,j] <- 1
        }
    }
  }
  colnames(out) <- colnames(dat.table)
  return(out)
}

t1.binary <- flagged.table(t1.carelesstable)
t2.binary <-flagged.table(t2.carelesstable)
t3.binary <-flagged.table(t3.carelesstable)
t4.binary <-flagged.table(t4.carelesstable)

##########################################################################################
##Calculate the rate of carelessness within each measure
careless.rates <- function(carelesstable){
  flagged<- flagged.table(carelesstable)
  out <- colSums(flagged, na.rm = TRUE)/nrow(flagged)
}

t1.rates <- careless.rates(t1.carelesstable)
t2.rates <- careless.rates(t2.carelesstable)
t3.rates <- careless.rates(t3.carelesstable)
t4.rates <- careless.rates(t4.carelesstable)

write.table( signif(t1.rates, 4), sep = ",", quote = FALSE)
write.table( signif(t2.rates, 4), sep = ",", quote = FALSE)
write.table( signif(t3.rates, 4), sep = ",", quote = FALSE)
write.table( signif(t4.rates, 4), sep = ",", quote = FALSE)

##########################################################################################

 
 
 