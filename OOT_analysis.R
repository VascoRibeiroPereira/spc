# Source all R codes

source("libs_funct.R")
source("data.R")


# Test Normality

testNormal(testData)


# Test Rules of identification of OOT behavior

R_one <- rule_one(testData)[[1]]
R_two <- rule_two(testData)[[1]]
R_three <- rule_three(testData)[[1]]
R_four <- rule_four(testData)[[1]]
R_five <- rule_five(testData)[[1]]
R_six <- rule_six(testData)[[1]]
R_seven <- rule_seven(testData)[[1]]
R_eight <- rule_eight(testData)[[1]]

Rules_list <- list(R_one, R_two, R_three, R_four, R_five, R_six, R_seven, R_eight)

allRules <- tibble(join_all(Rules_list, type = "full")) %>% mutate(OOT = as.factor(OOT))

dataMean <- mean(testData$value)
dataSD <- sd(testData$value)

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



