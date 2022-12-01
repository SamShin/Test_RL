library(fastLink)
library(tictoc)

dataSet <- read.csv("data/clean_county.csv", header=TRUE)

linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year", "zip_code", "id")
dataSet <- dataSet[linkageFields]
dataSet$birth_year <- as.character((dataSet$birth_year))

x <- c(1000)
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


  
  
  
  
  
  blockOut <- blockData(dfA, dfB, varnames = c("zip_code"))

  matchOut <- vector(mode = "list", length = length(blockOut))
  # <- vector(mode = "list", length = length(blockOut)) #To file
  matchFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year")
  for (i in 1:length(blockOut)) {
    print(paste("Block number is", i))

    subA <- dfA[blockOut[[i]]$dfA.inds,]
    subB <- dfB[blockOut[[i]]$dfB.inds,]

    flOut <- fastLink(
      dfA = subA,
      dfB = subB,
      varnames = matchFields,
      stringdist.match = matchFields,
      stringdist.method = "lv",
      return.all = TRUE)

     mOut <- getMatches(
      dfA = sub1,
      dfB = sub2,
      fl.out = flOut,
      threshold.match = 0.95, #Normally set to 0.85 but program is set to 0.95 so following along
      combine.dfs = FALSE
    )

    fOut[[i]] <- flOut
    matchOut[[i]] <- mOut
  }

  #saveRDS(fOut, file = "flTestFile.rds")

  dfAMatch <- do.call("rbind", lapply(matchOut, "[[", "dfA.match"))
  dfBMatch <- do.call("rbind", lapply(matchOut, "[[", "dfB.match"))

  finalTest <- aggregateEM(fOut)
  #out <- readRDS("flTestFile.rds")
  out <- fOut
  print(confusion(out, threshold = 0.95))
}
