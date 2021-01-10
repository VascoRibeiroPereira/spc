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

RulesPlot <- ggplot(allRules, aes(batch, value, color = OOT)) + geom_point()

ggplotly(RulesPlot) ## in the "Compare data on hover" we can check the OOT associated with each data point



