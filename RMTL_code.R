#load(mtl_data.RData)
mtl_data=list()
uniq_cell=unique(cell_type)
Y_mtl=list()
for(i in 1:length(uniq_cell)){
#  mtl_data[[i]]= t(datafiltfinal[,which(cell_type==uniq_cell[i])])
  a=matrix(-1,ncol(datafiltfinal),1)
  a[which(cell_type==uniq_cell[i]),1]=1
  Y_mtl[[i]]=a
}

for(i in 1:9){
mtl_data[[i]]=t(datafiltfinal)}

cvfitr <- cvMTL(mtl_data, Y_mtl, type="Classification", Regularization="L21", Lam1_seq=10^seq(1,-4, -1),  Lam2=0, opts=list(init=0,  tol=10^-6, maxIter=1500), nfolds=5, stratify=FALSE, parallel=FALSE)
mtl_model=MTL(mtl_data, Y_mtl, type = "Classification", Regularization = "L21",Lam1 = cvfitr$Lam1.min, Lam1_seq = NULL, Lam2 = 0, opts = list(init = 0, tol= 10^-3, maxIter = 1000), G = NULL, k = 2)

error= calcError(mtl_model[[i]], newX=data_mtl_tst, newY=Y_mtl_tst)
print(error)
