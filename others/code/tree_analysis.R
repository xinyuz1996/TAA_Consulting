library(rpart)
library(rpart.plot)
TAA.dat <- read.delim("~/ST841/TAA_Consulting/data/tabular data_final.txt")
#TAA.dat<-TAA.dat[-c(64,67,68),]

TAA.dat$Q35_ERP<-factor(TAA.dat$Q35_ERP)
TAA.dat$Q40_TAA<-factor(TAA.dat$Q40_TAA)
TAA.dat$Domestic_International<-factor(TAA.dat$Domestic_International)


tree_mod_rout<-rpart(rout_composite~Q35_ERP+Q40_TAA+Q36_ITcount+
        Q37_ITassist+Q38_Taxcount+Q39_TAAexpert+
        integrate_composite+manag_composite+Q26+Q27+Q29+comp_composite+
          gov_composite+TAA_capability+Domestic_International, 
      data=TAA.dat, method="anova",control=list(minsplit=5,cp=0.014837))

rpart.plot(tree_mod_rout)

tree_mod_adopt<-rpart(adoption_score~Q35_ERP+Q40_TAA+Q36_ITcount+
                  Q37_ITassist+Q38_Taxcount+Q39_TAAexpert+
                  integrate_composite+manag_composite+Q26+Q27+Q29+comp_composite+
                  gov_composite+TAA_capability+Domestic_International, 
                data=TAA.dat, method="anova",control=list(minsplit=5,cp=0.031847))

rpart.plot(tree_mod_adopt)


tree_mod_init<-rpart(init_composite~Q35_ERP+Q40_TAA+Q36_ITcount+
                        Q37_ITassist+Q38_Taxcount+Q39_TAAexpert+
                        integrate_composite+manag_composite+Q26+Q27+Q29+comp_composite+
                        gov_composite+TAA_capability+Domestic_International, 
                      data=TAA.dat, method="anova",control=list(minsplit=5,cp=0.02))

rpart.plot(tree_mod_init)
