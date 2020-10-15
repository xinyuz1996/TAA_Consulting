TAA.data <- read.delim("~/ST841/TAA_Consulting/data/tabular data_w_composites.txt")

#Correct the Anamolous and Strange Data Mistake 
prev=TAA.data[1,7:10]
TAA.data[1,7:10]=TAA.data[68,7:10]
for(i in 2:68){
  temp=TAA.data[i,7:10]
  TAA.data[i,7:10]=prev
  prev=temp
}

#Transform 4's
TAA.data[2,9]=3
TAA.data[16,7]=1

#Create Compotsite Adoption Score
adopt_data<-TAA.data[c("Q6box_1","Q6box_2","Q6box_3")]
adopt_comp<-apply(adopt_data,1,sum,na.rm=TRUE)/6
TAA.data$adoption_score<-adopt_comp
TAA.data$adoption_score<-scale(TAA.data$adoption_score)

#Create Composite TAA Score-Treating Other as group 2
TAA_capability_data<-TAA.data[c("Q22_box1","Q22_box2","Q22_box3","Q22_box4","Q22_box5","Q22_box6",
                                "Q22_box7","Q22_box8","Q22_box9", "Q22_box10")]

TAA_capability_score<-rep(0,nrow(TAA_capability_data))
max_score=1+2*4+3+4+5*2
for(i in 1:length(TAA_capability_score)){
  score=0
  if(!is.na(TAA_capability_data[i,1])){
    score=score+0
  }
  if(!is.na(TAA_capability_data[i,2])){
    score=score+1
  }
  if(!is.na(TAA_capability_data[i,3])){
    score=score+2
  }
  if(!is.na(TAA_capability_data[i,4])){
    score=score+2
  }
  if(!is.na(TAA_capability_data[i,5])){
    score=score+2
  }
  if(!is.na(TAA_capability_data[i,6])){
    score=score+3
  }
  if(!is.na(TAA_capability_data[i,7])){
    score=score+4
  }
  if(!is.na(TAA_capability_data[i,8])){
    score=score+5
  }
  if(!is.na(TAA_capability_data[i,9])){
    score=score+5
  }
  if(!is.na(TAA_capability_data[i,10])){
    score=score+2
  }
  TAA_capability_score[i]=score/max_score
}
TAA.data$TAA_capability<-TAA_capability_score
TAA.data$TAA_capability<-scale(TAA.data$TAA_capability)

#Domestic vs. International Composite
dom_int_data<-TAA_capability_data<-TAA.data[c("Q24_box1","Q24_box2","Q24_box3","Q24_box4")]
dom_int<-ifelse(is.na(dom_int_data[,4]),0,1)
TAA.data$Domestic_International<-dom_int
NA_index=apply(is.na(TAA.data[c("Q24_box1","Q24_box2","Q24_box3","Q24_box4")]),1,all)
TAA.data$Domestic_International[NA_index]=NA

columns_to_remove<-c("Q5_1","Q5_2","Q5_3","Q5_4","Q5_5","Q5_6","Q6box_1","Q6box_2",
                     "Q6box_3","Q6box_4","Q6_score","Q10_1","Q10_2","Q10_3","Q10_4",
                     "Q22_box1","Q22_box2","Q22_box3","Q22_box4","Q22_box5","Q22_box6",
                     "Q22_box7","Q22_box8","Q22_box9","Q22_box10","Q23_1","Q23_2",
                     "Q24_box1","Q24_box2","Q24_box3","Q24_box4","Q30_1",
                     "Q30_2","Q30_3","Q30_4","Q31_1","Q31_2","Q31_3","Q32_1","Q32_2","Q32_3",
                     "Q32_4","Q42","Q45","Q43","Q44")
TAA.data<-TAA.data[,!names(TAA.data) %in% columns_to_remove]

write.table(TAA.data, file = "tabular data_final.txt", sep = "\t", col.names = TRUE,
            row.names = FALSE)

NA_index=apply(is.na(tabular.data_w_composites[c("Q24_box1","Q24_box2","Q24_box3","Q24_box4")]),1,all)
tabular.data_final$Domestic_International[NA_index]=NA
