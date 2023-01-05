library("RecordLinkage")
library("dplyr")

rRecordLinkage <- function(sampleSize, 
                           linkageField,
                           blockStatus,
                           blockName,
                           idName,
                           outputFile,
                           sleep) {
  myOptions <- options(digits.secs = 2)
  
  if (is.integer(sampleSize)) {
    sampleSize <- c(sampleSize)
  }
  
  for (size in sampleSize) {
    dfAName <- file.path("sample_df", paste(size, "_dfA.csv", sep = ""))
    dfBName <- file.path("sample_df", paste(size, "_dfB.csv", sep = ""))
    
    if (blockStatus == FALSE) {
      blockField = FALSE
    } else {
      blockField = blockName
    }
    
    linkageField <- append(linkageField, blockField)
    linkageField <- append(linkageField, idName)
    
    dfA <- read.csv(dfAName, header = TRUE)
    dfB <- read.csv(dfBName, header = TRUE)
    
    linkageField <- unlist(linkageField)
    dfA <- dfA[linkageField]
    dfB <- dfB[linkageField]
    
    timeStart <- Sys.time()
    
    
    
    rPairsRL <- compare.linkage(dataset1 = dfA,
                                dataset2 = dfB,
                                blockfld = head(tail(linkageField,2),1),
                                strcmpfun = levenshteinDist,
                                identity1 = dfA[[idName]],
                                identity2 = dfB[[idName]])
    
    rPairsWeights <- emWeights(rpairs = rPairsRL)
    
    rPairsClassify <- emClassify(rpairs = rPairsWeights,
                                 threshold.upper = 11)
    
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
      if (predDf[i, ] == "L") {
        match <- pairs[i, ]
        
        idA <- match["id1"]
        idB <- match["id2"]
        
        if (idA != idB) {
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
    
    
                    
    write(output, file = file.path("results", outputFile), append = TRUE)
    
    Sys.sleep(sleep)
  }
}

testFunction <- function(x) {
  return (2*x)
}


fastlink <- function(sampleSize,
                     linkageField,
                     blockStatus,
                     blockName,
                     idName) {
  
}


