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
