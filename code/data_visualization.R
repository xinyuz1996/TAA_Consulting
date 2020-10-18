library(readr)
library(ggcorrplot)
library(reshape2)
setwd('C:/Users/Qiang/Desktop/Quick access/course/TAA_Consulting')
data_imput <- read_delim("./data/tabular data_impute.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#boxplots
ggplot(data = melt(data_imput), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))+
                                                                   theme(axis.title.x=element_blank(),
                                                                         axis.text.x=element_blank(),
                                                                         axis.ticks.x=element_blank())

#correlogram
corr <- round(cor(data_imput), 1)
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of variables", 
           ggtheme=theme_bw)

#regression plots
lm_init <- lm(Y1_initiation ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_adopt <- lm(Y2_adoption ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_rout <- lm(Y3_routinization ~ X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
par(mfrow=c(2,2))
plot(lm_init)
plot(lm_adopt)
plot(lm_rout)

