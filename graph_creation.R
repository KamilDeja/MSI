########## Graph creation automatic ##########

sample =  sample.int(n = nrow(data_autoselect), size = floor(.75*nrow(data)), replace = F)
data_auto_train <- data_autoselect[sample,]
data_auto_test <-data_autoselect[-sample,]


View(data_autoselect)

dag.test = hc(data_auto_train)

plot(dag.test)

bn.cv(data_autoselect, dag.test, loss = "mse-lw" , loss.args = list(target = 'ViolentCrimesPerPop' ))


#############Discretize#############

summary(data_autoselect$NumStreet)
boxplot(data_autoselect$NumInShelters)

data_autoselect = data_autoselect[,!names(data_autoselect) %in% c('NumInShelters')]

data_autoselect_discrete<-discretize(data_autoselect,method = 'quantile')
discrete_uqniue_count <- apply(data_autoselect_discrete, 2, function(x) length(unique(x)))

dag.test_descrete = hc(data_autoselect_discrete)
bn.cv(data_autoselect_discrete, dag.test_descrete, loss = "pred" , loss.args = list(target = 'ViolentCrimesPerPop' ))

bn_descrete_fitted = bn.fit(dag.test_descrete,data_autoselect_discrete)
plot(bn_descrete_fitted$racePctWhite$parents)

plot(dag.test_descrete)
