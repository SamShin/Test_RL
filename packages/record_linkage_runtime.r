library(RecordLinkage)

myOptions <- options(digits.secs = 2)

dataSet <- read.csv("data/clean_county.csv", header = TRUE)

linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year", "zip_code", "id")
dataSet <- dataSet[linkageFields]

x <- c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000,22000,24000,26000,28000,30000,32000,34000,36000,38000,40000)
for (size in x) {
  sampleSize <- size
  sampleSize <- as.integer(sampleSize) * 1.5
  sampleCutA <- sampleSize / 3

  sampleSet <- dataSet
  sampleSet <- sampleSet[sample(nrow(sampleSet), sampleSize), ]

  columns <- colnames(sampleSet)
  for (name in columns[1:5]) {
    sampleSet[[name]][sample(nrow(sampleSet), sampleSize * 0.1)] <- NA
  }

  dfAFirstHalf <- sampleSet[1:sampleCutA, ]
  dfBFirstHalf <- sampleSet[1:sampleCutA, ]

  dfASecondHalf <- sampleSet[(sampleCutA + 1):(2 * sampleCutA), ]
  dfBSecondHalf <- tail(sampleSet, n = sampleCutA)

  dfA <- rbind(dfAFirstHalf, dfASecondHalf)
  dfB <- rbind(dfBFirstHalf, dfBSecondHalf)

  timeStart <- (format(Sys.time(), "%OS"))

  rPairsRL <- compare.linkage(dataset1 = dfA,
                              dataset2 = dfB,
                              blockfld = c(6),
                              strcmpfun = levenshteinDist,
                              identity1 = dfA[["id"]],
                              identity2 = dfB[["id"]])

  rPairsWeights <- emWeights(rpairs = rPairsRL)

  rPairsClassify <- emClassify(rpairs = rPairsWeights,
                               threshold.upper = 11) #Have to provide a threshold

  timeEnd <- (format(Sys.time(), "%OS"))

  linksPredicted <- (nrow(getPairs(rPairsClassify, min.weight = 11, single.rows = TRUE)))
  linkagePairs <- nrow(rPairsRL[[3]])
  timeTaken <- abs(as.numeric(timeEnd) - as.numeric(timeStart))

  errorMeasures <- (getErrorMeasures(rPairsClassify))
  precision <- errorMeasures[["precision"]]
  recall <- errorMeasures[["sensitivity"]]

  output <- paste("Sample Size: ", size,
                  "|Links Predicted: ", linksPredicted,
                  "|Time Taken: ", timeTaken,
                  "|Precision: ", precision,
                  "|Recall: ", recall,
                  "|Linkage Pairs: ", linkagePairs, sep = "")

  write(output, file = "results/r_record_linkage_block.txt", append = TRUE)

  Sys.sleep(600)
}