rm(list=ls())

library(knitr) # kable
library(readr) # read_delim
library(dplyr) # manipulate data
library(Amelia) # missing value imputation

#### Load Data regression ----------------------------------------------------------------------------------------
main_path= "F:/zxy/Project/TAA/TAA_Consulting/"
setwd(main_path);getwd()
data <- readr::read_delim("./data/tabular data_final.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
data = rename(data,  Y1_initiation = init_composite, Y2_adoption = adoption_score, Y3_routinization = rout_composite,
              X1_readiness = TAA_capability, X2_integration = integrate_composite, X3_firmSize = Q26_num,
              X4_global = Domestic_International, X5_manag = manag_composite, X6_compet = comp_composite, 
              X7_regulatory = gov_composite)
data_imput = data = as.data.frame( data %>% select(Y1_initiation, Y2_adoption, Y3_routinization, X1_readiness, X2_integration, X3_firmSize,
                X4_global, X5_manag, X6_compet, X7_regulatory) )
names(data)
summary(data)
missmap(data)

#### Impute missing value ----------------------------------------------------------------------------------------

m = 5 # number of simulated datsets to create # See definition of m in ?amelia()
data_amelia <- amelia(x = data, logs="X3_firmSize", m = 5)

# Average the imputations between different simulated datasets
for( col in c(6:7)){
  temp=numeric()
  for (i in 1:m){
    temp = cbind(temp, data_amelia$imputations[[i]][,col])
  }
  data_imput[,col] = apply(temp, 1, mean)
}
data_new = scale(data_imput)
write.table(data_new, file = "./data/tabular data_impute.txt", sep = "\t", col.names = TRUE, row.names = FALSE)

#### Missing Value Visualization ----------------------------------------------------------------------------------------
par(mfrow=c(2,1))
plot(data_amelia)
compare.density(data_amelia, var="X3_firmSize")
compare.density(data_amelia, var="X4_global")

#### Final regression ----------------------------------------------------------------------------------------
data_imput <- read_delim("./data/tabular data_impute.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

lm_init <- lm(Y1_initiation ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_adopt <- lm(Y2_adoption ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_rout <- lm(Y3_routinization ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
summary(lm_init); plot(lm_init)
summary(lm_adopt); plot(lm_adopt)
summary(lm_rout); plot(lm_rout)





