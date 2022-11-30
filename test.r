## Load the package and data
library(fastLink)

linkageFields <- c("first_name", "middle_name", "last_name", "res_street_address", "birth_year")



blockgener_out <- blockData(dfA, dfB, varnames = "zip_code")
print(names(blockgener_out))
dfA_block1 <- dfA[blockgener_out$block.1$dfA.inds,]
dfA_block2 <- dfA[blockgener_out$block.2$dfA.inds,]

dfB_block1 <- dfB[blockgener_out$block.1$dfB.inds,]
dfB_block2 <- dfB[blockgener_out$block.2$dfB.inds,]

fl_out_block1 <- fastLink(
  dfA_block1, dfB_block1,
  varnames = linkageFields,
  return.all = TRUE
)

fl_out_block2 <- fastLink(
  dfA_block2, dfB_block2,
  varnames = linkageFields,
  return.all = TRUE
)

print(confusion(fl_out_block2))
agg.out <- aggregateEM(em.list = list(fl_out_block1, fl_out_block2))
print(aggconfusion(agg.out))

#matches.out <- fastLink(dfA = dfA,
#                     dfB = dfB,
#                     varnames = linkageFields,
#                     stringdist.match = c("first_name", "middle_name", "last_name", "res_street_address"),
#                     stringdist.method = "lv",
#                     numeric.match = c("birth_year"),
#                     n.cores = 8,
#                     return.all = FALSE)



#dfA.match <- dfA[matches.out$matches$inds.a,]
#dfB.match <- dfB[matches.out$matches$inds.b,]