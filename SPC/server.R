#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header)
    })
    
    output$densityplot <- renderPlot({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        mydata <- read.csv(inFile$datapath, header = input$header)
        colnames(mydata) <- c("batch", "value")
        
        ggplot(mydata, aes(value)) + 
            geom_histogram(aes(y = stat(density))) + 
            geom_density()
        
    })
    
    output$shapirotest <- renderText({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        mydata <- read.csv(inFile$datapath, header = input$header)
        colnames(mydata) <- c("batch", "value")
        
        myEval <- shapiro.test(mydata$value)
        
        if (length(mydata$value) >= 20){
            
            
            if (myEval$p.value < 0.05) testResult <- paste("Data is not Normal Distributed with p-value = ", round(myEval$p.value,2), sep="")
            if (myEval$p.value >= 0.05) testResult <- paste("Data is Normal Distributed with p-value = ", round(myEval$p.value,2), sep="")
            
        } else {
            testResult <- "Not enough Data to evaluate (use more than 20 records)"
        }
        
    })
    
    output$rulesplot <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        testData <- read.csv(inFile$datapath, header = input$header)
        colnames(testData) <- c("batch", "value")
        testData$OOT <- 0
        dataMean <- mean(testData$value)
        dataSD <- sd(testData$value)
        
        rule_one <- function(myDFTest) {
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
            
            for (i in (1:(length(myDFTest$batch)-7))) {
                littleDF <- myDFTest[i:(i+7),]
                
                if (sum(littleDF$value > dataMean) == 8 | sum(littleDF$value < dataMean) == 8){
                    for (n in 1:length(littleDF$batch)) {
                        
                        myDFTest$OOT[grep(littleDF$batch[n], myDFTest$batch)] <- 4
                        
                    }
                }
                
                
            }
            ## outputs
            
            myDFTest <- myDFTest %>% mutate(OOT = as.factor(OOT))
            
            return(myDFTest)
        }
        
        
        rule_five <- function(myDFTest) { ## this rule is not working
            
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
        
        R_one <- rule_one(testData)
        R_two <- rule_two(testData)
        R_three <- rule_three(testData)
        R_four <- rule_four(testData)
        R_five <- rule_five(testData)
        R_six <- rule_six(testData)
        R_seven <- rule_seven(testData)
        R_eight <- rule_eight(testData)
        
        Rules_list <- list(R_one, R_two, R_three, R_four, R_five, R_six, R_seven, R_eight)
        
        allRules <- tibble(join_all(Rules_list, type = "full")) %>% mutate(OOT = as.factor(OOT))
        
        RulesPlot <- ggplot(allRules, aes(batch, value, color = OOT)) + geom_line(color = "grey") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
                  axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
            geom_hline(yintercept=dataMean, color = "lightgreen") +
            annotate(geom = "text", x = length(testData$value), y = (dataMean + 1), label = "Average", color = "lightgreen") +
            geom_hline(yintercept=(dataMean+dataSD), linetype="dotted", color = "black") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean+dataSD+1), label = "Average + σ", color = "black") +
            geom_hline(yintercept=(dataMean-dataSD), linetype="dotted", color = "black") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean-dataSD+1), label = "Average - σ", color = "black") +
            geom_hline(yintercept=(dataMean+2*dataSD), linetype="dashed", color = "orange4") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean+2*dataSD+1), label = "Average + 2σ", color = "orange4") +
            geom_hline(yintercept=(dataMean-2*dataSD), linetype="dashed", color = "orange4") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean-2*dataSD+1), label = "Average - 2σ", color = "orange4") +
            geom_hline(yintercept=(dataMean+3*dataSD), linetype="longdash", color = "tomato2") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean+3*dataSD+1), label = "Average + 3σ", color = "tomato2") +
            geom_hline(yintercept=(dataMean-3*dataSD), linetype="longdash", color = "tomato2") +
            annotate(geom = "text", x = (length(testData$value)-5), y = (dataMean-3*dataSD+1), label = "Average - 3σ", color = "tomato2") +
            geom_point()
        
        ggplotly(RulesPlot) ## in the "Compare data on hover" we can check the OOT associated with each data point
        
    })

})
