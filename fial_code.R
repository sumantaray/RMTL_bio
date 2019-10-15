library(kernlab)
library(RMTL)
# data_mtl=list()
# data_mtl[[1]]=t(scrna_data)
# data_mtl[[2]]=t(methyl_data)
# data_mtl[[3]]=t(cnvsnp_data)
# data_mtl[[4]]=t(mut_data)
# 
# Y_mtl=list()
# Y_mtl[[1]]= match(cell_type,unique(cell_type))
# 
# a=specc(methyl_data, centers=9)
# Y_mtl[[2]]= a@.Data
# a=specc(cnvsnp_data, centers=9)
# Y_mtl[[3]]= a@.Data
# a=specc(t(mut_data), centers=9)
# Y_mtl[[4]]= a@.Data

#load('mtl_data.RData')
print('creating train and test data')
##create train and test data
mtl_data=list()
Y_mtl=list()

uniq_cell=unique(cell_type)
for(i in 1:9){
    mtl_data[[i]]=t(datafiltfinal)
    Y_mtl[[i]]=matrix(-1,1,ncol(datafiltfinal))
    Y_mtl[[i]][which(cell_type==uniq_cell[i])]=1
    }



train_mtl=list()
test_mtl=list()
ytrain_mtl=list()
ytest_mtl=list()
for(p in 1:9){
  #for(j in 1:9){ 
  a=sample(nrow(mtl_data[[p]]))
  d<-mtl_data[[p]][a,]
  y=Y_mtl[[p]][a]
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(d)),breaks=10,labels=FALSE)
  testData=list()
  trainData=list()
  ytest_data=list()
  ytrain_data=list()
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData[[i]] <- d[testIndexes, ]
    ytest_data[[i]]<-y[testIndexes]
    trainData[[i]] <- d[-testIndexes, ]
    ytrain_data[[i]]<-y[-testIndexes]
    #Use the test and train data partitions however you desire...
  }
  
  train_mtl[[p]]=trainData
  test_mtl[[p]]=testData
  ytrain_mtl[[p]]=ytrain_data
  ytest_mtl[[p]]=ytest_data
  }

train_set=list()
test_set=list()
y_train=list()
y_test=list()

for (i in 1:10){
  a=list()
  b=list()
  c=list()
  d=list()
  for(j in 1:9){
    a[[j]]=train_mtl[[j]][[i]]
    #train_set[[i]][[j]]=train_mtl[[j]][[i]]
    b[[j]]=test_mtl[[j]][[i]]
    #test_set[[i]][[j]]=test_mtl[[j]][[i]]
    c[[j]]=ytrain_mtl[[j]][[i]]
    # y_train[[i]][[j]]=ytrain_mtl[[j]][[i]]
    d[[j]]=ytest_mtl[[j]][[i]]
    #y_test[[i]][[j]]=ytest_mtl[[j]][[i]]
    # k=k+1   
  }
  train_set[[i]]=a
  test_set[[i]]=b
  y_train[[i]]=c
  y_test[[i]]=d
  
}

#############################
######strat training and calculating error on test set
print('strat training and calculating error on test set')
mtl_model=list()
error=array()
for (i in 1:1){
  data_mtl=train_set[[i]]
  Y_mtl=y_train[[i]]
  data_mtl_tst=test_set[[i]]
  Y_mtl_tst=y_test[[i]]
  cvfitr <- cvMTL(data_mtl, Y_mtl, type="Regression", Regularization="L21", Lam1_seq=10^seq(1,-4, -1),  Lam2=0, opts=list(init=0,  tol=10^-6, maxIter=1500), nfolds=5, stratify=FALSE, parallel=FALSE)
  
  mtl_model[[i]]=MTL(data_mtl, Y_mtl, type = "Classification", Regularization = "L21",Lam1 = cvfitr$Lam1.min, Lam1_seq = NULL, Lam2 = 0, opts = list(init = 0, tol
                                                                                                                                                     = 10^-3, maxIter = 10000), G = NULL, k = 2)
  
  error[i]= calcError(mtl_model[[i]], newX=data_mtl_tst, newY=Y_mtl_tst)
  print(i)
}

##########
save(file="mtl_results.RData",error,mtl_model)