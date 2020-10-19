library(readr)
library(ggcorrplot)
library(reshape2)
library(coefplot)
setwd('C:/Users/HP/Desktop/TAA_Consulting')
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
lm_init <- lm(Y1_initiation ~ -1+X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_adopt <- lm(Y2_adoption ~ -1+X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
lm_rout <- lm(Y3_routinization ~ -1+X1_readiness + X2_integration + X3_firmSize + X4_global + X5_manag + X6_compet + X7_regulatory, data=data_imput)
buildModelCI(lm_init)
buildModelCI(lm_adopt)
buildModelCI(lm_rout)
coefplot(lm_init)
coefplot(lm_adopt)
coefplot(lm_rout)

