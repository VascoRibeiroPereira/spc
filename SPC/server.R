shinyServer(function(input, output) {
    
    data <- eventReactive(input$file1,{

        inFile <- input$file1
        myDF <- read.csv(inFile$datapath, header = input$header, sep = ";")
        myDF
    })
    

    output$contents <- renderTable({
        data()
    })
    
    output$densityplot <- renderPlot({
        
        mydata <- data()
        
        colnames(mydata) <- c("batch", "value")
        
        ggplot(mydata, aes(value)) + 
            geom_histogram(aes(y = stat(density))) + 
            geom_density()
        
    })
    
    output$shapirotest <- renderText({
        
        mydata <- data()
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
        
        testData <- data()
        colnames(testData) <- c("batch", "value")
        
        testData$OOT <- 0
        dataMean <- mean(testData$value)
        dataSD <- sd(testData$value)

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
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_text(size = 7)) +
            labs(color = "Rules broken") +
            geom_hline(yintercept=dataMean, color = "lightgreen") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean)*1.02, label = "μ", color = "lightgreen") +
            geom_hline(yintercept=(dataMean+dataSD), linetype="dotted", color = "black") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean+dataSD)*1.02, label = "μ + σ", color = "black") +
            geom_hline(yintercept=(dataMean-dataSD), linetype="dotted", color = "black") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean-dataSD)*1.02, label = "μ - σ", color = "black") +
            geom_hline(yintercept=(dataMean+2*dataSD), linetype="dashed", color = "orange4") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean+2*dataSD)*1.02, label = "μ + 2σ", color = "orange4") +
            geom_hline(yintercept=(dataMean-2*dataSD), linetype="dashed", color = "orange4") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean-2*dataSD)*1.02, label = "μ - 2σ", color = "orange4") +
            geom_hline(yintercept=(dataMean+3*dataSD), linetype="longdash", color = "tomato2") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean+3*dataSD)*1.02, label = "μ + 3σ", color = "tomato2") +
            geom_hline(yintercept=(dataMean-3*dataSD), linetype="longdash", color = "tomato2") +
            annotate(geom = "text", x = (testData$batch[nrow(testData)]+5), y = (dataMean-3*dataSD)*1.02, label = "μ - 3σ", color = "tomato2") +
            geom_point()
        
        ggplotly(RulesPlot) ## in the "Compare data on hover" we can check the OOT associated with each data point
        
    })

})
