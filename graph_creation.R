########## Graph creation automatic ##########

sample =  sample.int(n = nrow(data_autoselect), size = floor(.3*nrow(data)), replace = F,set.seed(12))
data_auto_net <- data_autoselect[sample,]
data_auto_test <-data_autoselect[-sample,]


View(data_autoselect)

dag_hc = hc(data_auto_net)

dag_tabu = tabu(data_auto_net)

dag_rsmax = rsmax2(data_auto_net,whitelist = data.frame('PctFam2Par',"ViolentCrimesPerPop"))

plot(dag_hc)

plot(dag_tabu)

plot(dag_rsmax)

cv_hc = bn.cv(data_auto_test, dag_hc, loss = "mse" , loss.args = list(target = 'ViolentCrimesPerPop', set.seed(1234)))
cv_tabu = bn.cv(data_auto_test, dag_tabu, loss = "mse" , loss.args = list(target = 'ViolentCrimesPerPop',set.seed(1234)))
cv_rsmax = bn.cv(data_auto_test, dag_rsmax, loss = "mse" , loss.args = list(target = 'ViolentCrimesPerPop',set.seed(1234)))

cv_hc
cv_tabu
cv_rsmax
#bn.fit(dag.test,data_autoselect)

#############Discretize#############

data_autoselect = data_autoselect[,!names(data_autoselect) %in% c('NumInShelters','NumStreet')]

nr_breaks = rep(3,ncol(data_autoselect))
nr_breaks[which(colnames(data_autoselect) %in% c('racePctHisp','HousVacant'))]=4
  
  
data_autoselect_discrete<-discretize(data_autoselect,breaks = nr_breaks, method = 'quantile')
#data_autoselect_discrete<-discretize(data_autoselect, method = 'interval')
summary(data_autoselect_discrete)
discrete_unique_count <- apply(data_autoselect_discrete, 2, function(x) length(unique(x)))

discrete_unique_count

data_auto_net_discrete <- data_autoselect_discrete[sample,]
data_auto_test_discrete <-data_autoselect_discrete[-sample,]

dag_hc_discrete = hc(data_auto_net_discrete,whitelist = data.frame('PctFam2Par',"ViolentCrimesPerPop"))

dag_tabu_discrete = tabu(data_auto_net_discrete,whitelist = data.frame('PctFam2Par',"ViolentCrimesPerPop"))

dag_rsmax_discrete = rsmax2(data_auto_net_discrete,whitelist = data.frame('PctFam2Par',"ViolentCrimesPerPop"))

plot(dag_hc_discrete)

plot(dag_tabu_discrete)

plot(dag_rsmax_discrete)

cv_hc_discrete = bn.cv(data_auto_test_discrete, dag_hc_discrete,fit='bayes', loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop', set.seed(1234)))
cv_tabu_discrete = bn.cv(data_auto_test_discrete, dag_tabu_discrete, loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop',set.seed(1234)))
cv_rsmax_discrete = bn.cv(data_auto_test_discrete, dag_rsmax_discrete, loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop',set.seed(1234)))

cv_hc_discrete

cv_hc_discrete[[1]]$observed
cv_tabu_discrete
cv_rsmax_discrete

View(data_auto_test_discrete)


dag.test_descrete = hc(data_autoselect_discrete)
bn.cv(data_autoselect_discrete, dag.test_descrete, loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop' ))

bn.cv(data_autoselect_discrete, dag.test_descrete, fit='bayes', loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop' ))

bn_descrete_fitted = bn.fit(dag.test_descrete,data_autoselect_discrete)
plot(bn_descrete_fitted$racePctWhite$parents)

plot(dag.test_descrete)

att<-getDefaultAttrs()

att$node$fontsize=25



setAttrs(att)
graph.par(list(nodes=list(fill="lightgray", textCol="black",fontsize=45)))
graphviz.plot(dag_rsmax)
graphviz.plot(dag_tabu)
graphviz.plot(dag_hc,layout = 'fdp')
plot(dag_hc)


################################## Create maual graph ##################3


nodes = colnames(data_autoselect)
nodes = nodes[-which(nodes %in% c("PopDens","PctEmploy","MedOwnCostPctInc","PctOccupMgmtProf","MedRentPctHousInc","LemasPctOfficDrugUn","PctPersOwnOccup","pctWRetire","pctWInvInc","MedYrHousBuilt","PctLargHouseFam","PctHousOccup"))]
manual_dag = empty.graph(nodes)
#dag_string = modelstring(manual_dag)
#dag_string ="[racePctWhite][racePctHisp][pctWWage][pctWInvInc][pctWPubAsst][pctWRetire][PctUnemployed][PctEmploy][PctOccupMgmtProf][PctFam2Par][PctKids2Par][PctYoungKids2Par][PctTeen2Par][PctWorkMomYoungKids][PctWorkMom][NumIlleg][PctIlleg][PctImmigRec5][PctImmigRec8][PctRecImmig5][PctRecImmig10][PctLargHouseFam][PersPerOwnOccHous][PctPersOwnOccup][PctPersDenseHous][HousVacant][PctHousOccup][PctHousOwnOcc][MedYrHousBuilt][MedRentPctHousInc][MedOwnCostPctInc][NumInShelters][NumStreet][PopDens][LemasPctOfficDrugUn][ViolentCrimesPerPop]"
manual_dag=set.arc(manual_dag,'PctTeen2Par','PctFam2Par')
#manual_dag=set.arc(manual_dag,'NumStreet','NumInShelters')
#manual_dag=set.arc(manual_dag,'MedRent','PctHousOwnOcc')
manual_dag=set.arc(manual_dag,'PctKids2Par','PctTeen2Par')
manual_dag=set.arc(manual_dag,'PctUnemployed','NumStreet')
manual_dag=set.arc(manual_dag,'PctWorkMom','PctFam2Par')
manual_dag=set.arc(manual_dag,'PctWorkMomYoungKids','PctWorkMom')
manual_dag=set.arc(manual_dag,'PctYoungKids2Par','PctKids2Par')
#manual_dag=set.arc(manual_dag,'RentMedian','PctHousOwnOcc')
manual_dag=set.arc(manual_dag,'PersPerOwnOccHous','PctHousOwnOcc')
#manual_dag=set.arc(manual_dag,'RentMedian','HousVacant')
#manual_dag=set.arc(manual_dag,'MedRent','HousVacant')


manual_dag=set.arc(manual_dag,'NumInShelters','NumStreet')
manual_dag=set.arc(manual_dag,'pctWPubAsst','NumStreet')
manual_dag=set.arc(manual_dag,'NumInShelters','NumStreet')
manual_dag=set.arc(manual_dag,'pctWWage','NumStreet')
manual_dag=set.arc(manual_dag,'PctPersDenseHous','NumStreet')



manual_dag=set.arc(manual_dag,'racePctHisp','PctIlleg')
manual_dag=set.arc(manual_dag,'pctWPubAsst','PctIlleg')
manual_dag=set.arc(manual_dag,'racePctWhite','PctIlleg')

manual_dag=set.arc(manual_dag,'PctRecImmig5','racePctHisp')
manual_dag=set.arc(manual_dag,'PctRecImmig5','pctWPubAsst')
manual_dag=set.arc(manual_dag,'PctRecImmig5','racePctWhite')


manual_dag=set.arc(manual_dag,'PctRecImmig10','PctRecImmig5')
manual_dag=set.arc(manual_dag,'PctImmigRec5','PctRecImmig5')
manual_dag=set.arc(manual_dag,'PctImmigRec8','PctRecImmig5')
manual_dag=set.arc(manual_dag,'PctRecImmig5','PctIlleg')
manual_dag=set.arc(manual_dag,'NumIlleg','PctIlleg')


manual_dag=set.arc(manual_dag,'HousVacant','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'PctHousOwnOcc','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'PctFam2Par','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'PctIlleg','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'NumStreet','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'PctKids2Par','ViolentCrimesPerPop')
manual_dag=set.arc(manual_dag,'HousVacant','ViolentCrimesPerPop')

node.ordering(manual_dag)

#manual_dag=node
att<-getDefaultAttrs()
graph.par(list(node=list(textCol="black", fonsize=30, shape="box"),
               fixedsize=FALSE, height=1, width=2))
graphviz.plot(manual_dag,layout = 'fdp')

part <- data_autoselect[,-which(colnames(data_autoselect) %in% c("PopDens","PctEmploy","MedOwnCostPctInc","PctOccupMgmtProf","MedRentPctHousInc","LemasPctOfficDrugUn","PctPersOwnOccup","pctWRetire","pctWInvInc","MedYrHousBuilt","PctLargHouseFam","PctHousOccup"))]
cv_manual = bn.cv(part, manual_dag, loss = "mse" , loss.args = list(target = 'ViolentCrimesPerPop', set.seed(1234)))
cv_manual

from <- c("PctImmigRec8", "PctImmigRec5", "PctRecImmig10")
to <- c("pctWPubAsst", "PctIlleg")
for (i in 1:3) {
  for (j in 1:2) {
    f <- from[i]
    t <- to[j]
    print(dsep(manual_dag, f, t, c("racePctHisp", "PctRecImmig5", "racePctWhite")))
    print(dsep(manual_dag, t, f, c("racePctHisp", "PctRecImmig5", "racePctWhite")))
  }
}
