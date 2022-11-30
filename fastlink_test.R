sampleSize <- 1000
sampleSize <- as.integer(sampleSize) * 1.5
sampleCutA <- sampleSize / 3

sampleSet <- dataSet
sampleSet <- sampleSet[sample(nrow(sampleSet), sampleSize), ]

for (name in head(colnames(sampleSet), -1)) {
  sampleSet[[name]][sample(nrow(sampleSet), sampleSize * 0.1)] <- NA
}

dfAFirstHalf <- sampleSet[1:sampleCutA, ]
dfBFirstHalf <- sampleSet[1:sampleCutA, ]

dfASecondHalf <- sampleSet[(sampleCutA + 1):(2 * sampleCutA), ]
dfBSecondHalf <- tail(sampleSet, n = sampleCutA)

dfA <- rbind(dfAFirstHalf, dfASecondHalf)
dfB <- rbind(dfBFirstHalf, dfBSecondHalf)