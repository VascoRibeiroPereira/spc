# Generate Data and recreate OOT for testing

set.seed(123)
testData <- tibble(batch = c(1:200), value = rnorm(200, 500, 10))
#testData <- tibble(batch = paste("A",c(1:200), sep=""), value = rnorm(200, 500, 10)) ## testing this

## rule 2
testData$value[199] <- testData$value[199] - 2*sd(testData$value)
testData$value[198] <- testData$value[198] - 2*sd(testData$value)

## rule 3
testData$value[180] <- testData$value[180] - sd(testData$value)
testData$value[181] <- testData$value[181] - sd(testData$value)
testData$value[182] <- testData$value[182] + sd(testData$value)
testData$value[183] <- testData$value[183] - sd(testData$value)

## rule 4
set.seed(123)
testData$value[160:167] <- rnorm(8, 510, 5)

## rule 5
set.seed(123)
testData$value[140:145] <- sort(rnorm(6, 510, 5))

## rule 6
set.seed(123)
testData$value[120:133] <- c(rbind(rnorm(7, 490, 5), rnorm(7, 510, 5)))


## rule 7
set.seed(123)
testData$value[100:114] <- rnorm(15, 500, 5)

## rule 8
set.seed(123)
testData$value[40:47] <- c(rbind(rnorm(4, 520, 5), rnorm(4, 480, 5)))


## Save data to file

write.csv(testData, "testData.csv", row.names = FALSE)

