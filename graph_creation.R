########## Graph creation automatic ##########

sample =  sample.int(n = nrow(data_autoselect), size = floor(.75*nrow(data)), replace = F)
data_auto_train <- data_autoselect[sample,]
data_auto_test <-data_autoselect[-sample,]


View(data_autoselect)

dag.test = hc(data_auto_train)

plot(dag.test)

bn.cv(data_autoselect, dag.test, loss = "mse-lw" , loss.args = list(target = 'ViolentCrimesPerPop' ))
