library(readr)
setwd('C:/Users/Qiang/Desktop/Quick access/course/TAA_Consulting')
data <- read_delim("./data/tabular data_impute.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

Y <- data.matrix(data[,c(1,2,3)])
X <- data[,-c(1,2,3)]
multi_reg <- lm(Y~.-1,data=X)
mvsum <- summary(multi_reg)
mvsum[[1]]
mvsum[[2]]
mvsum[[3]]

multi_reg7 <- lm(Y~.-1-X7_regulatory,data=X)
anova(multi_reg, multi_reg7, test="Wilks")
anova(multi_reg, multi_reg7, test="Pillai")
anova(multi_reg, multi_reg7, test="Hotelling-Lawley")
anova(multi_reg, multi_reg7, test="Roy")
