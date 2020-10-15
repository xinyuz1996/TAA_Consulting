library(Gifi)
main_path = "~/ST841/TAA_Consulting/data/tabular data.txt"
# main_path = "../data/tabular data.txt"
TAA.data <- read.delim(main_path)

#Init Response Varaible
init_data_names<-c("Q5_1","Q5_2","Q5_3","Q5_4","Q5_5","Q5_6") #Set Aside Data
init_data<-TAA.data[init_data_names]

init_data<-apply(init_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(init_data)) #Tabulate Missing values

###Warning 2 Composites may be more appropriate
pca_init<-princals(init_data,ndim=2,missing = "a",degrees=2) #Impute, transform, do PCA
pca_init$evals #Examine Eigenvalues
plot(1:length(pca_init$evals),pca_init$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_init, plot.type = "transplot") #Examine Transformed Scores
init_score<-pca_init$objectscores[,1]
barplot(pca_init$loadings[,1])


#Routinization Response Variable
rout_data_names<-c("Q10_1","Q10_2","Q10_3","Q10_4") #Set Aside Data
rout_data<-TAA.data[rout_data_names]

rout_data<-apply(rout_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(rout_data)) #Tabulate Missing values

##Warning there may be evidence of two components
pca_rout<-princals(rout_data,ndim=1,missing = "a",degrees=2) #Impute, transform, do PCA
pca_rout$evals #Examine Eigenvalues
plot(1:length(pca_rout$evals),pca_rout$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_rout, plot.type = "transplot") #Examine Transformed Scores
rout_score<-pca_rout$objectscores[,1]


#Integration Independent Variable
integ_data_names<-c("Q23_1","Q23_2") #Set Aside Data
integ_data<-TAA.data[integ_data_names]

integ_data<-apply(integ_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(integ_data)) #Tabulate Missing values


pca_integ<-princals(integ_data,ndim=1,missing = "a",degrees=2) #Impute, transform, do PCA
pca_integ$evals #Examine Eigenvalues
plot(1:length(pca_integ$evals),pca_integ$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_integ, plot.type = "transplot") #Examine Transformed Scores
integ_scores<-pca_integ$objectscores[,1]


#Management Obstacles Independent Variable
manag_data_names<-c("Q30_1","Q30_2","Q30_3","Q30_4") #Set Aside Data
manag_data<-TAA.data[manag_data_names]

manag_data<-apply(manag_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(manag_data)) #Tabulate Missing values

##Strong Evidence of 2 components
pca_manag<-princals(manag_data,ndim=1,missing = "a",degrees=2) #Impute, transform, do PCA
pca_manag$evals #Examine Eigenvalues
plot(1:length(pca_manag$evals),pca_manag$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_manag, plot.type = "transplot") #Examine Transformed Scores
manag_scores<-pca_manag$objectscores[,1]

#Competition Independent Variable
comp_data_names<-c("Q31_1","Q31_2","Q31_3") #Set Aside Data
comp_data<-TAA.data[comp_data_names]

comp_data<-apply(comp_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(comp_data)) #Tabulate Missing values


pca_comp<-princals(comp_data,ndim=1,missing = "a",degrees=2) #Impute, transform, do PCA
pca_comp$evals #Examine Eigenvalues
plot(1:length(pca_comp$evals),pca_comp$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_comp, plot.type = "transplot") #Examine Transformed Scores
comp_scores<-pca_comp$objectscores[,1]

##Warning: Evidence of two components
#Government Reg Independent Variable
gov_data_names<-c("Q32_1","Q32_2","Q32_3","Q32_4") #Set Aside Data
gov_data<-TAA.data[gov_data_names]

gov_data<-apply(gov_data,2,function(x){ifelse(x==6,NA,x)}) #Add Missing Values
table(complete.cases(gov_data)) #Tabulate Missing values


pca_gov<-princals(gov_data,ndim=1,missing = "a",degrees=2) #Impute, transform, do PCA
pca_gov$evals #Examine Eigenvalues
plot(1:length(pca_gov$evals),pca_gov$evals,main="Scree Plot",
     xlab="Number of Components",ylab = "Eigenvalues",type="b") #Examine Scree Plot
plot(pca_gov, plot.type = "transplot") #Examine Transformed Scores
gov_scores<-pca_gov$objectscores[,1]

TAA.data$init_composite<-init_score
TAA.data$rout_composite<-rout_score
TAA.data$integrate_composite<-integ_scores
TAA.data$manag_composite<-manag_scores
TAA.data$comp_composite<-comp_scores
TAA.data$gov_composite<-gov_scores



