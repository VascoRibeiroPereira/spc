# Libraries and Functions

## Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)

## Functions

### Normality

testNormal <- function(myDFTest) {
        
        colnames(myDFTest) <- c("batch", "value")
        myEval <- shapiro.test(myDFTest$value)
        
        if (length(myDFTest$value) >= 20){
                
                
                if (myEval$p.value < 0.05) testResult <- paste("Data is not Normal Distributed with p-value = ", round(myEval$p.value,2), sep="")
                if (myEval$p.value >= 0.05) testResult <- paste("Data is Normal Distributed with p-value = ", round(myEval$p.value,2), sep="")
                
        } else {
                testResult <- "Not enough Data to evaluate (use more than 20 records)"
        }
        
        return(list(testResult, 
                    ggplot(myDFTest, aes(value)) + 
                            geom_histogram(aes(y = stat(density))) + 
                            geom_density()))
}


### Rules


rule_one <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in 1:length(myDFTest$value)) {
                
                if (myDFTest$value[i] > dataMean+3*dataSD | myDFTest$value[i] < dataMean-3*dataSD) {
                        myDFTest$OOT[i]<- 1
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        
        #myDFTest$batch <- factor(myDFTest$batch, levels = myDFTest$batch)
        
        oneplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + 
                geom_point() +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
                      axis.ticks.x = element_blank())
        oneplotly <- ggplotly(oneplot)
        
        return(list(myDFTest, oneplotly))
}


rule_two <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in 1:(length((myDFTest$batch))-2)){
                
                ### Making a DF to test in a for loop
                littleDF <- myDFTest[i:(i+2),] %>%
                        mutate(upLimit = value > dataMean+2*dataSD, lowLimit = value < dataMean-2*dataSD)
                
                
                if (sum(littleDF$upLimit) >= 2 | sum(littleDF$lowLimit) >= 2) {
                        
                        myDFTest[i:(i+2),]$OOT <- 2
                        
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        twoplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        
        return(list(myDFTest, twoplot))
}


rule_three <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in 1:(length((myDFTest$batch))-4)){
                
                ### Making a DF to test in a for loop
                littleDF <- myDFTest[i:(i+4),] %>%
                        mutate(limitTest = (value > dataMean+dataSD | value < dataMean-dataSD))
                
                if (sum(littleDF$limitTest) >= 4) {
                        
                        myDFTest[i:(i+4),]$OOT <- 3
                        
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        threeplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        
        return(list(myDFTest, threeplot))
}


rule_four <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in (1:(length(myDFTest$batch)-7))) {
                
                #myDFTest$value[i:(i+7)]
                littleDF <- myDFTest[i:(i+7),]
                
                if (sum(littleDF$value > dataMean) == 8 | sum(littleDF$value < dataMean) == 8){
                        
                        
                        myDFTest$OOT[i:(i+7)] <- 4
                }
                
                
        }
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        fourplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        
        return(list(myDFTest, fourplot))
}


rule_five <- function(myDFTest) { ## this rule is not working
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        
        for (i in (1:(length(myDFTest$batch)-5))) {
                
                littleDF <- myDFTest[i:(i+5),]
                
                evalVectUp <- logical()
                evalVectDown <- logical()
                
                for (n in (1:(length(littleDF$batch)-1))) {
                        evalVectUp <- c((littleDF$value[n] < littleDF$value[n+1]), evalVectUp)
                }
                
                for (m in (1:(length(littleDF$batch)-1))) {
                        evalVectDown <- c((littleDF$value[m] > littleDF$value[m+1]), evalVectDown)
                }
                
                if (sum(evalVectDown) == 5 | sum(evalVectUp) == 5){ 
                        
                        myDFTest$OOT[i:(i+5)] <- 5
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        fiveplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        
        return(list(myDFTest, fiveplot))
}

rule_six <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        
        for (i in (1:(length(myDFTest$batch)-14))) {
                
                
                littleDF <- myDFTest[i:(i+14),]
                
                up_down <- numeric()
                for (n in (1:(length(littleDF$batch)-1))) {
                        transientN <- (lm(littleDF$value[n:(n+1)] ~ c(n:(n+1))))$coef[2]
                        up_down <- c(transientN, up_down)
                }
                
                my_logical <- logical()
                for (m in (1:length(up_down))) {
                        my_logical <- c(my_logical, up_down[m] > 0)
                }
                
                OCL_T <- c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE) 
                
                if (sum(my_logical == OCL_T) == 14 | sum(my_logical == OCL_T) == 0) {
                        
                        
                        myDFTest[i:(i+13),]$OOT <- 6 
                        
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        sixplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        
        return(list(myDFTest, sixplot))
}


rule_seven <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in (1:(length(myDFTest$batch)-14))) {
                littleDF <- myDFTest[i:(i+14),]
                evalStrat <- logical()
                for (n in (1:(length(littleDF$batch)))) {
                        evalStrat <- c((littleDF$value[n] < (dataMean + dataSD) && littleDF$value[n] > (dataMean - dataSD)), evalStrat)
                }
                if (sum(evalStrat) == 15) {
                        myDFTest[i:(i+14),]$OOT <- 7 
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        sevenplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        return(list(myDFTest, sevenplot))
}

rule_eight <- function(myDFTest) {
        
        # Preping Data and values to process
        
        colnames(myDFTest) <- c("batch", "value")
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in (1:(length(myDFTest$batch)-7))) {
                littleDF <- myDFTest[i:(i+7),]
                evalmix <- logical()
                for (n in (1:(length(littleDF$batch)))) {
                        evalmix <- c((littleDF$value[n] > (dataMean + dataSD) | littleDF$value[n] < (dataMean - dataSD)), evalmix)
                }
                if (sum(evalmix) == 8) {
                        myDFTest[i:(i+7),]$OOT <- 8 
                }
        }
        
        ## outputs
        
        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
        eightplot <- ggplot(myDFTest, aes(batch, value, color = OOT)) + geom_point()
        return(list(myDFTest, eightplot))
}


## Capacity indexes

### Two sided

#### Prep my variables

dataMean <- mean(testData$value)
sigma <- sd(testData$value)
USL <- 525
LSL <- 475

#### Cp -> Estimates what the process is capable of producing if the process mean were to be centered between the specification limits. Assumes process output is approximately normally distributed.

Cp <- (USL-LSL)/(6*sigma)

#### Cpk -> Estimates what the process is capable of producing, considering that the process mean may not be centered between the specification limits. Assumes process output is approximately normally distributed.

Cpk <- min(((USL-dataMean)/(3*sigma)), ((dataMean-LSL)/(3*sigma)))


#### Conversion for sigma levels and capability indexes (long term)

if (min(Cpk,Cp) < .67) {yield <- 30.85; sigmaLevel <- 1} ## Process not capable - actions necessary
if (min(Cpk,Cp) >= .67 && min(Cpk,Cp) < 1) {yield <- 68.15; sigmaLevel <- 2} ## Process not capable - actions necessary
if (min(Cpk,Cp) >= 1 && min(Cpk,Cp) < 1.33) {yield <- 93.32; sigmaLevel <- 3} ## Process is capable but actions are recommended
if (min(Cpk,Cp) >= 1.33 && min(Cpk,Cp) < 1.67) {yield <- 99.38; sigmaLevel <- 4} ## Process capable
if (min(Cpk,Cp) >= 1.67 && min(Cpk,Cp) < 2) {yield <- 99.9767; sigmaLevel <- 5} ## Process capable
if (min(Cpk,Cp) >= 2) {yield <- 99.99966; sigmaLevel <- 6} ## Process capable



