# Functions

rule_one <- function(myDFTest) {
        
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
        return(myDFTest)
}


rule_two <- function(myDFTest) {
        
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
        return(myDFTest)
}


rule_three <- function(myDFTest) {
        
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
        return(myDFTest)
}


rule_four <- function(myDFTest) {

        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
        for (i in (1:(length(myDFTest$batch)-7))) {
                littleDF <- myDFTest[i:(i+7),]

                if (sum(littleDF$value > dataMean) == 8 | sum(littleDF$value < dataMean) == 8){
                        myDFTest$OOT[i:(i+7)] <- 4
                }


        }
        ## outputs

        myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))

        return(myDFTest)
}


rule_five <- function(myDFTest) { ## this rule is not working
        
        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)

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
        return(myDFTest)
}

rule_six <- function(myDFTest) {

        myDFTest$OOT <- 0
        dataMean <- mean(myDFTest$value)
        dataSD <- sd(myDFTest$value)
        
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

        return(myDFTest)
}


rule_seven <- function(myDFTest) {
        
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
        return(myDFTest)
}

rule_eight <- function(myDFTest) {
        
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
        return(myDFTest)
}