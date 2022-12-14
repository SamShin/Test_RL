library(fastLink)

myOptions <- options(digits.secs = 2)

dataSet <- read.csv("data/clean_county.csv", header = TRUE)

linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year", "zip_code")
dataSet <- dataSet[linkageFields]
dataSet$birth_year <- as.character((dataSet$birth_year))

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

  blockOut <- blockData(dfA, dfB, varnames = c("zip_code"))
  fOut <- vector(mode = "list", length = length(blockOut))

  for (i in 1:length(blockOut)) {

    sub1 <- dfA[blockOut[[i]]$dfA.inds, ]
    sub2 <- dfB[blockOut[[i]]$dfB.inds, ]

    fOut[[i]] <- fastLink(
      dfA = sub1,
      dfB = sub2,
      varnames = linkageFields[1:5],
      stringdist.match = linkageFields[1:5],
      stringdist.method = "lv",
      return.all = TRUE)
  }

  timeEnd <- (format(Sys.time(), "%OS"))

  links <- 0
  truePositive <- 0
  falsePositive <- 0
  linksPair <- 0
  timeTaken <- abs(as.numeric(timeEnd) - as.numeric(timeStart))

  for (i in 1:length(fOut)) {
    matches <- fOut[[i]]

    indsA <- matches[["matches"]][["inds.a"]]
    indsB <- matches[["matches"]][["inds.b"]]

    inds <- data.frame(indsA, indsB)
    truePositive <- truePositive + nrow(inds[inds$indsA == inds$indsB, ])
    falsePositive <- falsePositive + nrow(inds[inds$indsA != inds$indsB, ])
    links <- links + nrow(matches[["matches"]])

    blocks <- blockOut[[i]]
    pairsA <- length(blocks[["dfA.inds"]])
    pairsB <- length(blocks[["dfB.inds"]])

    linksPair <- linksPair + (pairsA * pairsB)
  }

  falseNegative <- round(size / 2) - truePositive

  precision <- truePositive / (truePositive + falsePositive)
  recall <- truePositive / (truePositive + falseNegative)

  output <- paste("Sample Size: ", size,
                  "|Links Predicted: ", links,
                  "|Time Taken: ", timeTaken,
                  "|Precision: ", precision,
                  "|Recall: ", recall,
                  "|Linkage Pairs: ", linksPair, sep = "")

  write(output, file = "results/fastLink_block.txt", append = TRUE)

  Sys.sleep(600)
}