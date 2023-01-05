library("RecordLinkage")
library("dplyr")

myOptions <- options(digits.secs = 2)
linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year", "zip_code", "id")

#x <- c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000,22000,24000,26000,28000,30000,32000,34000,36000,38000,40000)
x <- c(2000)
for (size in x) {

    dfAName <- file.path("sample_df", paste(size, "_dfA.csv", sep = ""))
    dfBName <- file.path("sample_df", paste(size, "_dfB.csv", sep = ""))

  dfA <- read.csv(dfAName, header = TRUE)
  dfB <- read.csv(dfBName, header = TRUE)
  
  print(head(dfA))
  timeStart <- Sys.time()

  rPairsRL <- compare.linkage(dataset1 = dfA,
                              dataset2 = dfB,
                              blockfld = c(7), #Blocking used here
                              strcmpfun = levenshteinDist,
                              exclude = c(1),
                              identity1 = dfA[["id"]],
                              identity2 = dfB[["id"]])
            

  rPairsWeights <- emWeights(rpairs = rPairsRL)

  rPairsClassify <- emClassify(rpairs = rPairsWeights,
                               threshold.upper = 11) #Have to provide a threshold

  timeEnd <- Sys.time()

  linkagePairs <- nrow(rPairsRL[[3]])

  pred <- rPairsClassify[["prediction"]]
  predDf <- data.frame(pred)

  levels <- dplyr::count(predDf, pred)
  linksPredicted <- levels[levels$pred == "L", ][["n"]]

  timeTaken <- as.character(difftime(timeEnd, timeStart, units = "secs")[[1]])

  falsePositive <- 0L
  pairs <- rPairsClassify[["pairs"]]

  for (i in 1:nrow(predDf)) {
    if (predDf[i,] == "L") {
      match <- pairs[i, ]

      idA <- match["id1"]
      idB <- match["id2"]

      if(idA != idB) {
        falsePositive <- falsePositive + 1
      }
    }
  }

  truePositive <- linksPredicted - falsePositive
  falseNegative <- (size / 2) - truePositive

  precision <- truePositive / (truePositive + falsePositive)
  recall <- truePositive / (truePositive + falseNegative)

  output <- paste("Sample Size: ", size,
                  "|Links Predicted: ", linksPredicted,
                  "|Time Taken: ", timeTaken,
                  "|Precision: ", precision,
                  "|Recall: ", recall,
                  "|Linkage Pairs: ", linkagePairs, sep = "")

  write(output, file = "results/test.txt", append = TRUE)

  #Sys.sleep(600)
}
