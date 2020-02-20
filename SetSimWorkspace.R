source('https://raw.githubusercontent.com/occ-data/data-simulator/master/ValidateFunction.R')
source('https://raw.githubusercontent.com/occ-data/data-simulator/master/SimtoJson.R')
source('SimData.R')
require(charlatan)
require(simstudy)
compendium <- read.csv("trialhmiscompendium2.csv", header=T, stringsAsFactors = F)
numberpersons <- 30
hmis <- simData(compendium, numberpersons)
source("makeFamilies.R")
dtIndiv <- makeFamilies(numberpersons, 1)

testdata <- genCorGen(30, nvars = 2, params1 = c(.5, .7), dist = "binary", rho = .8, corstr = "cs", wide = TRUE)
testdata$result = "unknown"
testdata$result[testdata$V2==1] = "death"

#df <- data.frame(hmis)
#df <- data.frame(hmis$race, hmis$eth, hmis$gen, hmis$vet, hmis$dis, hmis$dest)
#df$loc <- NULL
#validation_index <- createDataPartition(df$hmis.dest, p = .8, list = F)
#validation <- df[-validation_index,]
#dataset <- df[validation_index,]
#fit.lda <- train(hmis.dest~., data = dataset, method = "lda", metric = metric, trcontrol=control)