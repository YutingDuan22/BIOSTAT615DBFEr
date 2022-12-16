## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(DBFEr)

## -----------------------------------------------------------------------------
data("ovarian_cnv")
data("labels_ovarian")
head(ovarian_cnv)
dat = ovarian_cnv
head(labels_ovarian)
labels = labels_ovarian

## -----------------------------------------------------------------------------
library(tidyverse)
variant_type <- "DEL"
pos_class <- "RES"
stat_vals <- dat[dat$SVCLASS==variant_type,] ## get variant_type="DEL"
stat_vals <- stat_vals[,c("SAMPLEID", "LEN")]
#stat_vals 
stat_vals_agg <- as.data.frame(aggregate(stat_vals, by=list(stat_vals$SAMPLEID), as.vector))[,-2]
#stat_vals_agg


labels$CLASS_LABEL <- ifelse(labels$CLASS_LABEL==pos_class, 1, 0)
colnames(labels) = c("SAMPLEID", "CLASS_LABEL")

colnames(stat_vals_agg) = c("SAMPLEID", "LEN")
stat_vals_agg['LEN'][1,][[1]]
stat_df <- stat_vals_agg %>% inner_join(labels, by="SAMPLEID")
#stat_df
## y variable
y = stat_df[,-2]
colnames(y) <- c("sample_id", "label")
#y
rownames(stat_df) <- stat_df$SAMPLEID
#stat_df['LEN'][1,][[1]] # correct
unstack_sample <- function(X){
  df <- data.frame("sample_id" = )
}
stat_df <- stat_df[,2, drop=FALSE]
#stat_df
sample_id = c()
len = c()
#stat_df[,-1]
nrow_df <- nrow(stat_df)
for (i in 1:nrow_df){
  var_len = stat_df[i, ][[1]]
  sample_id = c(sample_id, rep(rownames(stat_df)[i], length(var_len)))
  len = c(len, var_len)
}
#print(length(len))
df <- data.frame("sample_id" = sample_id, 
                 "var_len"=len)

rownames(y) <- y$sample_id
head(df)

## -----------------------------------------------------------------------------
result = DistributionBasedFeatureExtractor(values=df, labels=y)
head(result)

