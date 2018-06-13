library(randomForest)
library(ggplot2)
library(grid)
library(gridExtra)
library(bnlearn)
library(reshape2)
library(Rgraphviz)

set.seed(1234)

dataFull <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data", sep=',')
#names<- readLines("https://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.names")
names<-c("state", "county", "community", "communityname", "fold", "population", "householdsize", "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29", "agePct16t24", "agePct65up", "numbUrban", "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov", "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8", "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", "RentMedian", "RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85", "PctSameState85", "LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop")
colnames(dataFull)<-names

states <- c(23, 25, 33, 44, 50)
data <- dataFull[dataFull$state %in% states,]
data[,5:ncol(data)]<-as.data.frame(lapply(data[,5:ncol(data)],as.numeric))

data$OtherPerCap = data$OtherPerCap/100
dataOrg = data
data = data[,!(names(data) %in% c('LemasSwornFT', 'LemasSwFTPerPop', 'LemasSwFTFieldOps', 'LemasSwFTFieldPerPop', 'LemasTotalReq', 'LemasTotReqPerPop', 'PolicReqPerOffic', 'PolicPerPop', 'RacialMatchCommPol', 'PctPolicWhite', 'PctPolicBlack', 'PctPolicHisp', 'PctPolicAsian', 'PctPolicMinor', 'OfficAssgnDrugUnits', 'NumKindsDrugsSeiz', 'PolicAveOTWorked', 'PolicCars', 'PolicOperBudg', 'LemasPctPolicOnPatr', 'LemasGangUnitDeploy', 'PolicBudgPerPop'))]


sample =  sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
dataTrain <- data[sample,]
dataTest <-data[-sample,]


dataTrainVariables <- dataTrain[,5:106]
dataTrainVariables$ViolentCrimesPerPop<-NULL


rf_model<-randomForest(x = dataTrainVariables, y =dataTrain$ViolentCrimesPerPop,set.seed(1234))

rfcv(dataTrainVariables,dataTrain$ViolentCrimesPerPop)

importance    <- importance(rf_model)
print(importance)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'IncNodePurity'],2))

View(varImportance)

imporatnt_variables = varImportance[which(varImportance$Importance>0.02),]

data_autoselect = data[,which(colnames(data) %in% imporatnt_variables$Variables)]
data_autoselect$ViolentCrimesPerPop = data$ViolentCrimesPerPop

data_autoselect
colnames(data_autoselect)
imporatnt_variables$Variables

cormat <- round(cor(data_autoselect),2)

melted_cormat <- melt(cormat)

melted_cormat$value<- abs(melted_cormat$value)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, hjust =1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(),
#        axis.title.y=element_blank(),
#        axis.text.y=element_blank(),
#        axis.ticks.y=element_blank())

mean(melted_cormat$value)
median(melted_cormat$value)


####Jako że NumStreet~NumInShelters (a oprócz tego potem sie wywala :D)
data_autoselect$NumStreet<-NULL





rf_model<-randomForest(x =  data_autoselect_discrete[,-which(colnames(data_autoselect_discrete) %in% c('ViolentCrimesPerPop'))], y =data_autoselect_discrete$ViolentCrimesPerPop,set.seed(1234))


cv_rf = rfcv(data_autoselect_discrete[,-which(colnames(data_autoselect_discrete) %in% c('ViolentCrimesPerPop'))],data_autoselect_discrete$ViolentCrimesPerPop)

cv_rf$error.cv

importance    <- importance(rf_model)
print(importance)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'IncNodePurity'],2))

View(varImportance)

imporatnt_variables = varImportance[which(varImportance$Importance>0.02),]


data_autoselect = data[,c(imporatnt_variables$Variables)]
data_autoselect$ViolentCrimesPerPop = data$ViolentCrimesPerPop
