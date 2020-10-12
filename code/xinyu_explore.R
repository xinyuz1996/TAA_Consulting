rm(list=ls())

library(knitr) # kable
library(readr) # read_delim

# read data
main_path= "F:/zxy/Project/TAA/TAA_Consulting/"
data <- readr::read_delim("F:/zxy/Project/TAA/TAA_Consulting/data/tabular data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
str(data)

# export missing value
pMiss <- function(x){sum(is.na(x))/length(x)*100}
psMiss <- round(apply(data,2,pMiss), 2)
psMiss <- psMiss[order(psMiss, decreasing = T)]
missing_percentage <- paste(psMiss,"%", sep="")
missMat <- cbind(c(1:56), names(psMiss), missing_percentage)
knitr::kable(missMat)

# plot missing map
missmap(data)

# missing value imputation
# add na as a new factor level for each categorical variable
# delete variables that have more than 50% missing value

mis50 <- c(1:56)[which(round(apply(data,2,pMiss), 2) > 50)]
data1 <- data[,-mis50] # reduce 15 variables
dim(data1)
names(data)[7:11]

# use EM Algorithm to impute missing value, take column 7-11 as example
missP <- round(apply(data1,2,pMiss), 2)
index <- which((missP < 20 & missP > 10) )
dat_ame <- as.data.frame(data1[,index])
a.out <- amelia(x = dat_ame)
summary(a.out)
plot(a.out)

plot(density(data1[,8], na.rm = T))
x <- unlist(as.vector(na.omit(data1[,8])))
plot(density(x))
density(na.omit(x))






