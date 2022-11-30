library(fastLink)
library(tictoc)

dataSet <- read.csv("data/clean_county.csv", header=TRUE)

linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year")
dataSet <- dataSet[linkageFields]
dataSet$birth_year <- as.numeric(as.character(dataSet$birth_year))
x <- c(1000)
for (size in x) {
  sampleSize <- size
  sampleSize <- as.integer(sampleSize) * 1.5
  sampleCutA <- sampleSize / 3

  sampleSet <- dataSet
  sampleSet <- sampleSet[sample(nrow(sampleSet), sampleSize), ]

  for (name in colnames(sampleSet)) {
    sampleSet[[name]][sample(nrow(sampleSet), sampleSize * 0.1)] <- NA
  }

  dfAFirstHalf <- sampleSet[1:sampleCutA, ]
  dfBFirstHalf <- sampleSet[1:sampleCutA, ]

  dfASecondHalf <- sampleSet[(sampleCutA + 1):(2 * sampleCutA), ]
  dfBSecondHalf <- tail(sampleSet, n = sampleCutA)

  dfA <- rbind(dfAFirstHalf, dfASecondHalf)
  dfB <- rbind(dfBFirstHalf, dfBSecondHalf)

  tic("FastLink")

  rPairsFL <- fastLink(dfA = dfA,
                       dfB = dfB,
                       varnames = linkageFields,
                       stringdist.match = c("first_name", "middle_name", "last_name", "res_street_address"),
                       stringdist.method = "lv",
                       numeric.match = c("birth_year"),
                       n.cores = 8,
                       return.all = TRUE)
  
  print(confusion(rPairsFL))
  #sink("sink.txt", append=TRUE, type=c("output", "message"))
  print(summary(rPairsFL))
  print(paste("[",as.character(size),"]", " ---------------------------",  sep = ""))
  #sink()
  toc()
  Sys.sleep(600)
}
