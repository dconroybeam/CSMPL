######Packages######
library(ggplot2)
library(lavaan)
library(reshape2)
library(lmerTest)





######Functions#######

#Calculate Euclidean mate value
eucmvcalc<-function(preferences,traits){
  
  #Rename preference and traits to be the same
  #This is sometimes necessary for calculating distance
  names(preferences)<-1:16
  names(traits)<-1:16
  
  #Calculate the Euclidean distance between preferences and traits
  distance<-dist(rbind(preferences,traits))
  
  #Rescale to a 0-10 scale
  distance<-10*(-1*distance+sqrt(10^2*16))/sqrt(10^2*16)
  
  return(distance)
}






#####Load Data######
data<-read.csv("C:/Users/conroy-beam/Google Drive/Research/CSMPI/Data/Raw Data/CAREER+1_January+9,+2020_11.59.csv",stringsAsFactors=F)

#Remove participants whom Qualtrics did not define as "good completes"
data<-subset(data,data$gc==1)


#Assign a couple ID to all couples
data$CIN<-1:nrow(data)


#Split the dataframe into A and B data
adata<-data[,c(21:319,621,632)]
bdata<-data[,-c(1:321,622:631)]


#Rename the columns
colnames(adata)<-c("relstat", "relstat_text", "rellength", "kids_currnt", 
                   "kids_prev", "sex","sex_text", "age", "sexorient", "self_race1",
                   "self_race2", "self_race3", "self_race4", "self_race5", "self_race6",
                   "self_race7","self_religion","self_height_ft", "self_height_in", 
                   "self_weight", "self_smoke","self_education", "self_class", "self_politics", 
                   "ideal_affectionate1", "ideal_affectionate2", "ideal_ambition1",
                   "ideal_ambition2", "ideal_artistic1", "ideal_artistic2", "ideal_disposition1", 
                   "ideal_disposition2", "ideal_family1","ideal_family2","ideal_health1", 
                   "ideal_health2", "ideal_humor1","ideal_humor2",
                   "ideal_intelligent1", "ideal_intelligent2", "ideal_kind1", "ideal_kind2", 
                   "ideal_parenting1", "ideal_parenting2", "ideal_physatt1", "ideal_physatt2", 
                   "ideal_religious1", "ideal_religious2", "ideal_resources1", "ideal_resources2", 
                   "ideal_sexy1", "ideal_sexy2", "ideal_status1", "ideal_status2", "ideal_age",
                   "self_affectionate1", "self_affectionate2", "self_ambition1", "self_ambition2", 
                   "self_artistic1", "self_artistic2", "self_disposition1", 
                   "self_disposition2", "self_family1","self_family2","self_health1", 
                   "self_health2", "self_humor1","self_humor2",
                   "self_intelligent1", "self_intelligent2", "self_kind1", "self_kind2", 
                   "self_parenting1", "self_parenting2", "self_physatt1", "self_physatt2", 
                   "self_religious1", "self_religious2", "self_resources1", "self_resources2", 
                   "self_sexy1", "self_sexy2", "self_status1", "self_status2","mate_affectionate1",
                   "mate_affectionate2", "mate_ambition1", "mate_ambition2", 
                   "mate_artistic1", "mate_artistic2", "mate_disposition1", 
                   "mate_disposition2", "mate_family1","mate_family2","mate_health1", 
                   "mate_health2", "mate_humor1","mate_humor2",
                   "mate_intelligent1", "mate_intelligent2", "mate_kind1", "mate_kind2", 
                   "mate_parenting1", "mate_parenting2", "mate_physatt1", "mate_physatt2", 
                   "mate_religious1", "mate_religious2", "mate_resources1", "mate_resources2", 
                   "mate_sexy1", "mate_sexy2", "mate_status1", "mate_status2", "qmi_1", 
                   "qmi_2", "qmi_3", "qmi_4", "qmi_5", "qmi_happy","dq_kids_1",
                   "dq_kids_2","dq_kids_3","dq_kids_4","dq_kids_5",
                   "dq_ethnicity_1","dq_ethnicity_2","dq_ethnicity_3",
                   "dq_ethnicity_4","dq_ethnicity_5","dq_ethnicity_6",
                   "dq_religion1","dq_religion2","dq_religion3","dq_religion4",
                   "dq_religion5","dq_religion6","dq_religion7","dq_religion8",
                   "dq_religion9","dq_religion10","dq_religion11","dq_height1",
                   "dq_height2","dq_height3","dq_height4","dq_height5","dq_height6",
                   "dq_height7", "dq_weight1", "dq_weight2","dq_weight3","dq_weight4",
                   "dq_weight5","dq_weight6","dq_weight7","dq_smoke_1","dq_smoke_2",
                   "dq_smoke_3","dq_education1", "dq_education2","dq_education3",
                   "dq_education4","dq_education5","dq_education6","dq_class1",
                   "dq_class2","dq_class3","dq_class4","dq_class5",
                   "dq_politics1", "dq_politics2","dq_politics3","dq_politics4",
                   "dq_politics5","dq_politics6","dq_politics7",
                   "prqc_sat1", "prqc_sat2", "prqc_sat3", "prqc_comm1",
                   "prqc_comm2", "prqc_comm3","min_affectionate1",
                   "min_affectionate2", "min_ambition1", "min_ambition2", 
                   "min_artistic1", "min_artistic2", "min_disposition1", 
                   "min_disposition2", "min_family1","min_family2","min_health1", 
                   "min_health2", "min_humor1","min_humor2",
                   "min_intelligent1", "min_intelligent2", "min_kind1", "min_kind2", 
                   "min_parenting1", "min_parenting2", "min_physatt1", "min_physatt2", 
                   "min_religious1", "min_religious2", "min_resources1", "min_resources2", 
                   "min_sexy1", "min_sexy2", "min_status1", "min_status2", "min_age",
                   "attcheck","prqc_int1", "prqc_int2", "prqc_int3",
                   "prqc_trust1", "prqc_trust2", "prqc_trust3","max_affectionate1",
                   "max_affectionate2", "max_ambition1", "max_ambition2", 
                   "max_artistic1", "max_artistic2", "max_disposition1", 
                   "max_disposition2", "max_family1","max_family2","max_health1", 
                   "max_health2", "max_humor1","max_humor2",
                   "max_intelligent1", "max_intelligent2", "max_kind1", "max_kind2", 
                   "max_parenting1", "max_parenting2", "max_physatt1", "max_physatt2", 
                   "max_religious1", "max_religious2", "max_resources1", "max_resources2", 
                   "max_sexy1", "max_sexy2", "max_status1", "max_status2", "max_age",
                   "prqc_pass1", "prqc_pass2", "prqc_pass3", "prqc_love1",
                   "prqc_love2", "prqc_love3", "imp_affectionate", 
                   "imp_ambition", "imp_artistic", "imp_disposition", 
                   "imp_family", "imp_health", "imp_humor","imp_intelligent",
                   "imp_kind", "imp_parenting", "imp_physatt",
                   "imp_religious", "imp_resources","imp_sexy", "imp_status", 
                   "imp_age", "mjs_c1", "mjs_c2", "mjs_c3", "mjs_c4", "mjs_c5",
                   "mjs_c6", "mjs_c7", "mjs_c8", "rank_affectionate", 
                   "rank_ambition", "rank_artistic", "rank_disposition", 
                   "rank_family", "rank_health", "rank_humor","rank_intelligent",
                   "rank_kind", "rank_parenting", "rank_physatt",
                   "rank_religious", "rank_resources","rank_sexy", "rank_status", 
                   "rank_age", "PID","CIN")


colnames(bdata)<-c("relstat", "relstat_text", "rellength", "kids_currnt", 
                   "kids_prev", "sex","sex_text", "age", "sexorient", "self_race1",
                   "self_race2", "self_race3", "self_race4", "self_race5", "self_race6",
                   "self_race7","self_religion","self_height_ft", "self_height_in", 
                   "self_weight", "self_smoke","self_education", "self_class", "self_politics", 
                   "ideal_affectionate1", "ideal_affectionate2", "ideal_ambition1",
                   "ideal_ambition2", "ideal_artistic1", "ideal_artistic2", "ideal_disposition1", 
                   "ideal_disposition2", "ideal_family1","ideal_family2","ideal_health1", 
                   "ideal_health2", "ideal_humor1","ideal_humor2",
                   "ideal_intelligent1", "ideal_intelligent2", "ideal_kind1", "ideal_kind2", 
                   "ideal_parenting1", "ideal_parenting2", "ideal_physatt1", "ideal_physatt2", 
                   "ideal_religious1", "ideal_religious2", "ideal_resources1", "ideal_resources2", 
                   "ideal_sexy1", "ideal_sexy2", "ideal_status1", "ideal_status2", "ideal_age",
                   "self_affectionate1", "self_affectionate2", "self_ambition1", "self_ambition2", 
                   "self_artistic1", "self_artistic2", "self_disposition1", 
                   "self_disposition2", "self_family1","self_family2","self_health1", 
                   "self_health2", "self_humor1","self_humor2",
                   "self_intelligent1", "self_intelligent2", "self_kind1", "self_kind2", 
                   "self_parenting1", "self_parenting2", "self_physatt1", "self_physatt2", 
                   "self_religious1", "self_religious2", "self_resources1", "self_resources2", 
                   "self_sexy1", "self_sexy2", "self_status1", "self_status2","mate_affectionate1",
                   "mate_affectionate2", "mate_ambition1", "mate_ambition2", 
                   "mate_artistic1", "mate_artistic2", "mate_disposition1", 
                   "mate_disposition2", "mate_family1","mate_family2","mate_health1", 
                   "mate_health2", "mate_humor1","mate_humor2",
                   "mate_intelligent1", "mate_intelligent2", "mate_kind1", "mate_kind2", 
                   "mate_parenting1", "mate_parenting2", "mate_physatt1", "mate_physatt2", 
                   "mate_religious1", "mate_religious2", "mate_resources1", "mate_resources2", 
                   "mate_sexy1", "mate_sexy2", "mate_status1", "mate_status2", "qmi_1", 
                   "qmi_2", "qmi_3", "qmi_4", "qmi_5", "qmi_happy","dq_kids_1",
                   "dq_kids_2","dq_kids_3","dq_kids_4","dq_kids_5",
                   "dq_ethnicity_1","dq_ethnicity_2","dq_ethnicity_3",
                   "dq_ethnicity_4","dq_ethnicity_5","dq_ethnicity_6",
                   "dq_religion1","dq_religion2","dq_religion3","dq_religion4",
                   "dq_religion5","dq_religion6","dq_religion7","dq_religion8",
                   "dq_religion9","dq_religion10","dq_religion11","dq_height1",
                   "dq_height2","dq_height3","dq_height4","dq_height5","dq_height6",
                   "dq_height7", "dq_weight1", "dq_weight2","dq_weight3","dq_weight4",
                   "dq_weight5","dq_weight6","dq_weight7","dq_smoke_1","dq_smoke_2",
                   "dq_smoke_3","dq_education1", "dq_education2","dq_education3",
                   "dq_education4","dq_education5","dq_education6","dq_class1",
                   "dq_class2","dq_class3","dq_class4","dq_class5",
                   "dq_politics1", "dq_politics2","dq_politics3","dq_politics4",
                   "dq_politics5","dq_politics6","dq_politics7",
                   "prqc_sat1", "prqc_sat2", "prqc_sat3", "prqc_comm1",
                   "prqc_comm2", "prqc_comm3","min_affectionate1",
                   "min_affectionate2", "min_ambition1", "min_ambition2", 
                   "min_artistic1", "min_artistic2", "min_disposition1", 
                   "min_disposition2", "min_family1","min_family2","min_health1", 
                   "min_health2", "min_humor1","min_humor2",
                   "min_intelligent1", "min_intelligent2", "min_kind1", "min_kind2", 
                   "min_parenting1", "min_parenting2", "min_physatt1", "min_physatt2", 
                   "min_religious1", "min_religious2", "min_resources1", "min_resources2", 
                   "min_sexy1", "min_sexy2", "min_status1", "min_status2", "min_age",
                   "attcheck","prqc_int1", "prqc_int2", "prqc_int3",
                   "prqc_trust1", "prqc_trust2", "prqc_trust3","max_affectionate1",
                   "max_affectionate2", "max_ambition1", "max_ambition2", 
                   "max_artistic1", "max_artistic2", "max_disposition1", 
                   "max_disposition2", "max_family1","max_family2","max_health1", 
                   "max_health2", "max_humor1","max_humor2",
                   "max_intelligent1", "max_intelligent2", "max_kind1", "max_kind2", 
                   "max_parenting1", "max_parenting2", "max_physatt1", "max_physatt2", 
                   "max_religious1", "max_religious2", "max_resources1", "max_resources2", 
                   "max_sexy1", "max_sexy2", "max_status1", "max_status2", "max_age",
                   "prqc_pass1", "prqc_pass2", "prqc_pass3", "prqc_love1",
                   "prqc_love2", "prqc_love3", "imp_affectionate", 
                   "imp_ambition", "imp_artistic", "imp_disposition", 
                   "imp_family", "imp_health", "imp_humor","imp_intelligent",
                   "imp_kind", "imp_parenting", "imp_physatt",
                   "imp_religious", "imp_resources","imp_sexy", "imp_status", 
                   "imp_age", "mjs_c1", "mjs_c2", "mjs_c3", "mjs_c4", "mjs_c5",
                   "mjs_c6", "mjs_c7", "mjs_c8", "rank_affectionate", 
                   "rank_ambition", "rank_artistic", "rank_disposition", 
                   "rank_family", "rank_health", "rank_humor","rank_intelligent",
                   "rank_kind", "rank_parenting", "rank_physatt",
                   "rank_religious", "rank_resources","rank_sexy", "rank_status", 
                   "rank_age", "PID","CIN")

#Label which partner was which
adata$partnerab<-"A"
bdata$partnerab<-"B"

#Put the data back together
data<-rbind(adata,bdata)

#Sort by couple ID
data<-data[order(data$CIN,data$sex),]

#Assign each participant a unique ID number
data$PIN<-1:nrow(data)



###Compute Composite Ratings###

#Create a dataframe of just ratings
ratings<-data[,c(25:54,56:115,185:214,223:252)]

#Subtract 1 from all ratings as the scale is wrong
ratings<-ratings-1

#Compute composite ratings
comps<-sapply(seq(1,150,2),function(x) rowMeans(ratings[,x:(x+1)]))

#Generate composite names
compnames<-colnames(ratings)[seq(1,150,2)]
compnames<-gsub("1","",compnames)

colnames(comps)<-compnames

#Cbind composite ratings into data
data<-cbind(data,comps)

compratecors<-sapply(seq(1,150,2),function(x) cor(ratings[,x],ratings[,(x+1)],use="pairwise.complete.obs"))



###Compute Self-Other Rating Composites###

#Change rating names to reflect that they are self ratings
colnames(data)[319:348]<-paste0(colnames(data[,319:348]),"_sr")

#Pull out ratings for partners A and B seperately
amrdata<-data[data$partnerab=="A",319:348]
bmrdata<-data[data$partnerab=="B",319:348]

#Relabel these to show that they reflect mate ratings
mrnames<-colnames(amrdata)
mrnames<-gsub("_sr","_mr",mrnames)

mrnames<-c(mrnames[16:30],mrnames[1:15])

colnames(amrdata)<-mrnames
colnames(bmrdata)<-mrnames

#Separate A and B participants
adata<-subset(data,data$partnerab=="A")
bdata<-subset(data,data$partnerab=="B")

#Bind in the mate ratings
adata<-cbind(adata,bmrdata)
bdata<-cbind(bdata,amrdata)

#Also get partner age in now
adata$mate_age<-bdata$age
bdata$mate_age<-adata$age

#Compute self-other agreements
aagreecor<-sapply(1:15,function(x) cor(amrdata[,x],bmrdata[,x+15],use="pairwise.complete.obs"))
bagreecor<-sapply(1:15,function(x) cor(bmrdata[,x],amrdata[,x+15],use="pairwise.complete.obs"))

#Put the data back together
data<-rbind(adata,bdata)

#Separate out self and mate ratings
selfratings<-data[,c(319:333,394:408)]
materatings<-data[,c(334:348,379:393)]


#Compute self-other composites
matecomps<-sapply(1:15,function(x) rowMeans(materatings[,c(x,(x+15))]))
selfcomps<-sapply(1:15,function(x) rowMeans(selfratings[,c(x,(x+15))]))

#Create column name vectors
matenames<-colnames(data[,334:348])
selfnames<-colnames(data[,319:333])

selfnames<-gsub("_sr","_comp",selfnames)
matenames<-gsub("_sr","_comp",matenames)

#Rename the columns
colnames(matecomps)<-matenames
colnames(selfcomps)<-selfnames

#Put the data back together
data<-cbind(data,selfcomps,matecomps)

#Create a vector for Likert-type age
data$self_age<-data$age

#Rearrange the variables for ease of use
data<-data[,c(301:303,300,6:9,1:3,122:178,4:5,10:24,25:54,56:115,185:214,223:252,304:318,55,349:363,215,364:378,253,260:275,284:299,319:348,394:408,379:393,410:424,440,425:439,409,116:121,179:184,217:222,254:259,276:283)]

#Recode age to be comparable to ideal age
data$self_age<-ifelse(data$age>75,10,ifelse(data$age>69,9,ifelse(data$age>63,8,ifelse(data$age>57,7,ifelse(data$age>51,6,ifelse(data$age>45,5,ifelse(data$age>39,4,ifelse(data$age>33,3,ifelse(data$age>27,2,ifelse(data$age>21,1,0))))))))))
data$mate_age<-ifelse(data$mate_age>75,10,ifelse(data$mate_age>69,9,ifelse(data$mate_age>63,8,ifelse(data$mate_age>57,7,ifelse(data$mate_age>51,6,ifelse(data$mate_age>45,5,ifelse(data$mate_age>39,4,ifelse(data$mate_age>33,3,ifelse(data$mate_age>27,2,ifelse(data$mate_age>21,1,0))))))))))


######Calculate Mate Value Variables######

#Pregenerate vectors for all variables
#Makes merging data back together easier later
data$prefmatch_sr<-NA
data$selfmv_sr<-NA
data$matemv_sr<-NA
data$mvdpp_sr<-NA
data$mvdps_sr<-NA

data$prefmatch_mr<-NA
data$selfmv_mr<-NA
data$matemv_mr<-NA
data$mvdpp_mr<-NA
data$mvdps_mr<-NA

data$prefmatch_comp<-NA
data$selfmv_comp<-NA
data$matemv_comp<-NA
data$mvdpp_comp<-NA
data$mvdps_comp<-NA

data$idealmv<-NA




#Remove non-heterosexual participants from the data for these calculations
datanhet<-data[sapply(data$CIN,function(x) sum(duplicated(data$sex[data$CIN==x])))>0,]
datanhet<-rbind(datanhet,data[data$CIN %in% data$CIN[data$sex>1],])

data<-data[!(data$CIN %in% datanhet$CIN),]

#Ideal MV#

#Split into male and female dataframes
dataf<-subset(data,data$sex==0)
datam<-subset(data,data$sex==1)

dataf$ideal_age<-dataf$ideal_age-1
datam$ideal_age<-datam$ideal_age-1


#Compute average male and female prefs
femaleprefs<-colMeans(dataf[,236:251],na.rm=T)
maleprefs<-colMeans(datam[,236:251],na.rm=T)

#Calculate ideal mate value
dataf$idealmv<-apply(dataf,1,function(x) eucmvcalc(femaleprefs,x[236:251]))
datam$idealmv<-apply(datam,1,function(x) eucmvcalc(maleprefs,x[236:251]))



#Self MV#
#Calculate selfmv

dataf$selfmv_sr<-apply(dataf,1,function(x) eucmvcalc(maleprefs,x[c(316:330,391)]))
datam$selfmv_sr<-apply(datam,1,function(x) eucmvcalc(femaleprefs,x[c(316:330,391)]))

dataf$selfmv_mr<-apply(dataf,1,function(x) eucmvcalc(maleprefs,x[c(346:360,391)]))
datam$selfmv_mr<-apply(datam,1,function(x) eucmvcalc(femaleprefs,x[c(346:360,391)]))


dataf$selfmv_comp<-apply(dataf,1,function(x) eucmvcalc(maleprefs,x[376:391]))
datam$selfmv_comp<-apply(datam,1,function(x) eucmvcalc(femaleprefs,x[376:391]))



#Calculate mate preference fulfillment
dataf$prefmatch_sr<-apply(dataf,1,function(x) eucmvcalc(x[236:251],x[c(331:345,407)]))
datam$prefmatch_sr<-apply(datam,1,function(x) eucmvcalc(x[236:251],x[c(331:345,407)]))

dataf$prefmatch_mr<-apply(dataf,1,function(x) eucmvcalc(x[236:251],x[c(361:375,407)]))
datam$prefmatch_mr<-apply(datam,1,function(x) eucmvcalc(x[236:251],x[c(361:375,407)]))

dataf$prefmatch_comp<-apply(dataf,1,function(x) eucmvcalc(x[236:251],x[392:407]))
datam$prefmatch_comp<-apply(datam,1,function(x) eucmvcalc(x[236:251],x[392:407]))


#Horrible, ugly line
#For each participant, calculates the degree to which their mate preferences are fulfilled by all mates in the sample
#And then calculates the participant's own mate's percentile rank within this distribution
datam$mvdpp_sr<-sapply(1:nrow(datam),function(x) ecdf(matrix(eucmvcalc(datam[x,236:251],datam[,c(331:345,407)]))[1:nrow(datam)])(matrix(eucmvcalc(datam[x,236:251],datam[,c(331:345,407)]))[x]))
dataf$mvdpp_sr<-sapply(1:nrow(dataf),function(x) ecdf(matrix(eucmvcalc(dataf[x,236:251],dataf[,c(331:345,407)]))[1:nrow(dataf)])(matrix(eucmvcalc(dataf[x,236:251],dataf[,c(331:345,407)]))[x]))

datam$mvdpp_mr<-sapply(1:nrow(datam),function(x) ecdf(matrix(eucmvcalc(datam[x,236:251],datam[,c(361:375,407)]))[1:nrow(datam)])(matrix(eucmvcalc(datam[x,236:251],datam[,c(361:375,407)]))[x]))
dataf$mvdpp_mr<-sapply(1:nrow(dataf),function(x) ecdf(matrix(eucmvcalc(dataf[x,236:251],dataf[,c(361:375,407)]))[1:nrow(dataf)])(matrix(eucmvcalc(dataf[x,236:251],dataf[,c(361:375,407)]))[x]))

datam$mvdpp_comp<-sapply(1:nrow(datam),function(x) ecdf(matrix(eucmvcalc(datam[x,236:251],datam[,392:407]))[1:nrow(datam)])(matrix(eucmvcalc(datam[x,236:251],datam[,392:407]))[x]))
dataf$mvdpp_comp<-sapply(1:nrow(dataf),function(x) ecdf(matrix(eucmvcalc(dataf[x,236:251],dataf[,392:407]))[1:nrow(dataf)])(matrix(eucmvcalc(dataf[x,236:251],dataf[,392:407]))[x]))

datam$matemv_sr<-apply(datam,1,function(x) eucmvcalc(maleprefs,x[c(331:345,407)]))
dataf$matemv_sr<-apply(dataf,1,function(x) eucmvcalc(femaleprefs,x[c(331:345,407)]))

datam$matemv_mr<-apply(datam,1,function(x) eucmvcalc(maleprefs,x[c(361:375,407)]))
dataf$matemv_mr<-apply(dataf,1,function(x) eucmvcalc(femaleprefs,x[c(361:375,407)]))

datam$matemv_comp<-apply(datam,1,function(x) eucmvcalc(maleprefs,x[392:407]))
dataf$matemv_comp<-apply(dataf,1,function(x) eucmvcalc(femaleprefs,x[392:407]))

datam$mvdps_sr<-datam$matemv_sr-datam$selfmv_sr
datam$mvdps_mr<-datam$matemv_mr-datam$selfmv_mr
datam$mvdps_comp<-datam$matemv_comp-datam$selfmv_comp


dataf$mvdps_sr<-dataf$matemv_sr-dataf$selfmv_sr
dataf$mvdps_mr<-dataf$matemv_mr-dataf$selfmv_mr
dataf$mvdps_comp<-dataf$matemv_comp-dataf$selfmv_comp

#Put the data back together
data<-rbind(dataf,datam)



#######Compute Relationship Quality####

###QMI Satisfaction###

##QMI Satisfaction##
data$qmi_happy<-7*(data$qmi_happy/10)
data$qmi_sat<-rowMeans(data[,408:413],na.rm=T)/7


###PRQC###
##PRQC Relationship Quality Subscales##
data$prqc_sat<-rowMeans(data[,414:416],na.rm=T)/7
data$prqc_comm<-rowMeans(data[,417:419],na.rm=T)/7
data$prqc_int<-rowMeans(data[,420:422],na.rm=T)/7
data$prqc_trust<-rowMeans(data[,423:425],na.rm=T)/7
data$prqc_pass<-rowMeans(data[,426:428],na.rm=T)/7
data$prqc_love<-rowMeans(data[,429:431],na.rm=T)/7


###MJS###
#Jealousy has to be reverse coded so that higher values indicate greater degrees of jealousy
data$mjs_c<-rowMeans(data[,432:439],na.rm=T)/7
data$mjs_c<-1-data$mjs_c


#Rearrange the data one last time
data<-data[,c(1:330,346:360,376:391,331:345,361:375,392:407,440:455,408:439,456:463)]

#Remove the Qualtrics ID as this is no longer necessary
data<-data[,-4]



######Save the Data#######
#Create a unique filename so the model can output results without overwriting old results.
#Name format is "MPICC Processed Data MonthDayYear HourMinuteSecond"
#This filepath will need to be made specific to your computer.
path<-"C:/Users/conroy-beam/Google Drive/Research/CSMPI/Data/Processed Data/CAREER1 PROCESSED Data "

format<-".csv"
date<-format(Sys.time(),format="%m%d%Y %H%M%S")
file<-file.path(paste0(path,date,format))

write.csv(data,file=file,row.names=F)


