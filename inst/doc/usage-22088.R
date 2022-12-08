## ----eval=FALSE---------------------------------------------------------------
#  scaleX<-scale(X,center=TRUE,scale=FALSE)
#  Eigvec=svd(scaleX)$v
#  AIC<-loglikelihood<-rep(0,k)
#  for (i in 1:k) {
#    est_Eigfun=Eigvec[,1:i]/sqrt(h)
#    eigenscore<-scaleX%*%est_Eigfun*h
#    designmatrix<-cbind(eigenscore,Z)
#    fullcoxresult<-coxph(formula=Surv(time,event)~designmatrix)
#    finalresult<-summary(fullcoxresult)
#    loglikelihood[i]<-finalresult$loglik[2]
#    AIC[i]<-2*i-2*loglikelihood[i]
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp22088)
data(time)
data(event)
data(X)
data(Z)
res1<-flcrm(time,event,X,Z,10,1)
res1

## ----eval=FALSE---------------------------------------------------------------
#  mlearning<-function(label,data,k=5,methods=c("nnet","knn","SVM","rF")){
#  ##knn
#  if(is.null(intersect("knn",methods))==FALSE){
#  mth[2]<-1
#  for(i in 1:k){
#    index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
#    train_data=data[-index,]
#    test_data=data[index,]
#    train_label<-label[-index]
#    test_label<-label[index]
#    obj <- tune.knn(train_data,train_label,k=2:10,tunecontrol=tune.control(sampling = "boot"))
#    best_k<-as.numeric(obj$best.parameters)
#    dt_knn<-knn(train = train_data,test = test_data,cl=train_label,k=best_k)
#    ConfM<-table(test_label,dt_knn)
#    Acc[i]<-sum(diag(ConfM))/sum(ConfM)
#    P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
#    R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
#    F1[i]<-2/(1/R[i]+1/P[i])
#    test_label<-as.numeric(test_label)
#    dt_knn<-as.numeric(dt_knn)
#    Auc[i]<-multiclass.roc(test_label,dt_knn)$auc[1]
#  }
#  best_knn<-obj$best.parameters
#  res_knn<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
#  names(res_knn)<-c("Acc","R","P","F1","Auc")
#  }
#  ##nnet
#  ##svm
#  ##rF
#  }

## ----eval=TRUE----------------------------------------------------------------
#library(nnet)
data(label)
data(data)
res2<-mlearning(label,data,k=5,methods="knn")
res2

## ----eval=FALSE---------------------------------------------------------------
#  arma::mat scaleC(arma::mat X,bool center,bool scale){
#    arma::mat S,V;
#    S=arma::mean(X,0);
#    V=arma::stddev(X,0,0);
#    if(center==TRUE){
#      for(arma::uword i=0;i<X.n_rows;i++){
#        X.row(i)=X.row(i)-S;
#      }
#    }
#    if(scale==TRUE){
#      for(arma::uword i=0;i<X.n_rows;i++){
#        X.row(i)=X.row(i)/V;
#      }
#    }
#    return(X);
#  }
#  arma::mat SVD_V(arma::mat X) {
#    arma::mat U;
#    arma::vec s;
#    arma::mat V;
#    arma::svd_econ(U, s, V, X);
#    return V;
#  }
#  arma::mat SVDS_V(arma::sp_mat X,int k=10) {
#    arma::mat U;
#    arma::vec s;
#    arma::mat V;
#    arma::svds(U, s, V, X, k);
#    return V;
#  }

## ----eval=TRUE----------------------------------------------------------------
library(microbenchmark)
X<-matrix(rnorm(300),ncol=30)
res3 <- microbenchmark(
  SVDR = svd(X),
  SVDC = SVD_V(X)
)
knitr::kable(summary(res3)[,c(1,3,5,6)])

## ----eval=FALSE---------------------------------------------------------------
#  flcrm<-function(time,event,X,Z,hh=40,k=5){
#  h<-1/ncol(X)
#  scaleX<-scale(X,center=TRUE,scale=FALSE)
#  Eigvec=svd(scaleX)$v
#  ordertime<-order(time)
#  sorttime<-time[ordertime]
#  sortevent<-event[ordertime]
#  sortfailureindex<-which(sortevent==1)
#  AIC<-loglikelihood<-rep(0,hh)
#  for (ss in 1:hh) {
#  est_Eigfun=Eigvec[,1:ss]/sqrt(h)
#  eigenscore<-scaleX%*%est_Eigfun*h
#  designmatrix<-cbind(eigenscore,Z)
#  fullcoxresult<-coxph(formula=Surv(time,event)~designmatrix)
#  orderdesignmatrix<-designmatrix[ordertime,]
#  estimatecoefficient<-fullcoxresult$coefficients
#  fullcoxcurve<-survfit(fullcoxresult)
#  finalresult<-summary(fullcoxresult)
#  loglikelihood[ss]<-finalresult$loglik[2]
#  AIC[ss]<-2*ss-2*loglikelihood[ss]
#  }
#  AICselectindex<-which.min(AIC[-(1:k)])+k
#  est_Eigfun=Eigvec[,1:AICselectindex]/sqrt(h)
#  eigenscore<-scaleX%*%est_Eigfun*h
#  designmatrix<-cbind(eigenscore,Z)
#  newdataframe<-data.frame(cbind(time, event, designmatrix))
#  totalvariable<-"V3"
#  for (i in 4:(ncol(designmatrix)+2)){
#  totalvariable<-paste(totalvariable,"+V", i, sep="")
#  }
#  totalformular<-as.formula(paste("Surv(time,event)~", totalvariable, sep=""))
#  fullcoxresult<-coxph(formula=totalformular, data=newdataframe)
#  fullcoxcurve<-survfit(fullcoxresult)
#  finalresult<-summary(fullcoxresult)
#  summarycoefficient<-finalresult$coefficients
#  coefficient<-summarycoefficient[,1]
#  secoefficient<-summarycoefficient[,3]
#  AICestimatebeta<-est_Eigfun%*%as.matrix(coefficient[1:AICselectindex])
#  AICestimategamma<-coefficient[-(1:AICselectindex)]
#  return(list(beta=AICestimatebeta,gamma=AICestimategamma))
#  }
#  
#  
#  mlearning<-function(label,data,k=5,methods=c("nnet","knn","SVM","rF")){
#  n<-length(label)
#  ind<-sample(1:n)
#  Acc<-numeric(k)
#  R<-numeric(k)
#  P<-numeric(k)
#  F1<-numeric(k)
#  Auc<-numeric(k)
#  mth<-c(0,0,0,0)
#  ##nnet
#  if(is.null(intersect("nnet",methods))==FALSE){
#  mth[1]<-1
#  for(i in 1:k){
#    index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
#    train_data=data[-index,]
#    test_data=data[index,]
#    train_label<-label[-index]
#    test_label<-label[index]
#    datatrain<-cbind(train_data,train_label)
#    obj<-tune.nnet(train_label~., data=datatrain,size=2:10,maxit=1000)
#    best_size<-as.numeric(obj$best.parameters)
#    nnet.sol <- nnet(train_label~., datatrain, size =best_size, maxit = 1000)
#    pred.prob <- predict(nnet.sol, test_data)
#    pred<-character(nrow(pred.prob))
#    for(j in 1:nrow(pred.prob)){
#      pred[j]<-names(which(pred.prob[j,]==max(pred.prob[j,])))
#    }
#    pred<-as.factor(pred)
#    ConfM<-table(test_label,pred)
#    c<-numeric(nrow(ConfM))
#    for(j in 1:nrow(ConfM)){
#      c[j]<-which(names(ConfM[1,])==names(ConfM[,1])[j])
#    }
#    ConfM<-ConfM[,c]
#    Acc[i]<-sum(diag(ConfM))/sum(ConfM)
#    R[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
#    P[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
#    F1[i]<-2/(1/R[i]+1/P[i])
#    test_label<-as.numeric(test_label)
#    pred<-as.numeric(pred)
#    Auc[i]<-multiclass.roc(test_label,pred)$auc[1]
#  }
#  best_nnet<-obj$best.parameters
#  res_nnet<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
#  names(res_nnet)<-c("Acc","R","P","F1","Auc")
#  }
#  ##knn
#  if(is.null(intersect("knn",methods))==FALSE){
#  mth[2]<-1
#  for(i in 1:k){
#    index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
#    train_data=data[-index,]
#    test_data=data[index,]
#    train_label<-label[-index]
#    test_label<-label[index]
#    obj <- tune.knn(train_data,train_label,k=2:10,tunecontrol=tune.control(sampling = "boot"))
#    best_k<-as.numeric(obj$best.parameters)
#    dt_knn<-knn(train = train_data,test = test_data,cl=train_label,k=best_k)
#    ConfM<-table(test_label,dt_knn)
#    Acc[i]<-sum(diag(ConfM))/sum(ConfM)
#    P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
#    R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
#    F1[i]<-2/(1/R[i]+1/P[i])
#    test_label<-as.numeric(test_label)
#    dt_knn<-as.numeric(dt_knn)
#    Auc[i]<-multiclass.roc(test_label,dt_knn)$auc[1]
#  }
#  best_knn<-obj$best.parameters
#  res_knn<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
#  names(res_knn)<-c("Acc","R","P","F1","Auc")
#  }
#  ##SVM
#  if(is.null(intersect("SVM",methods))==FALSE){
#  mth[3]<-1
#  for(i in 1:k){
#    index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
#    train_data=data[-index,]
#    test_data=data[index,]
#    train_label<-label[-index]
#    test_label<-label[index]
#    datatrain<-cbind(train_data,train_label)
#    tObj<-tune.svm(train_label~.,data=datatrain,type="C-classification",kernel="radial",cost=2^seq(-10,10,1),gamma=2^seq(-10,10,1),scale=FALSE)
#    first<-tObj$best.parameters
#    num1<-as.numeric(log2(first[1]))
#    num2<-as.numeric(log2(first[2]))
#    tObj<-tune.svm(train_label~.,data=datatrain,type="C-classification",kernel="radial",cost=2^seq(num2-1,num2+1,0.1),gamma=2^seq(num1-1,num1+1,0.1),scale=FALSE)
#    BestSvm<-tObj$best.model
#    yPred<-predict(BestSvm,test_data)
#    ConfM<-table(test_label,yPred)
#    Acc[i]<-sum(diag(ConfM))/sum(ConfM)
#    P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
#    R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
#    F1[i]<-2/(1/R[i]+1/P[i])
#    test_label<-as.numeric(test_label)
#    yPred<-as.numeric(yPred)
#    Auc[i]<-multiclass.roc(test_label,yPred)$auc[1]
#  }
#  best_svm<-tObj$best.parameters
#  res_svm<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
#  names(res_svm)<-c("Acc","R","P","F1","Auc")
#  }
#  ##rF
#  if(is.null(intersect("rF",methods))==FALSE){
#  mth[4]<-1
#  for(i in 1:k){
#    index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
#    train_data=data[-index,]
#    test_data=data[index,]
#    train_label<-label[-index]
#    test_label<-label[index]
#    datatrain<-cbind(train_data,train_label)
#    obj<-tune.randomForest(train_label~.,data =datatrain, ntree = seq(100,1000,100))
#    best_ntree<-as.numeric(obj$best.parameters)
#    model <- randomForest(train_label~.,data =datatrain,ntree=best_ntree)
#    pred.model <- predict(model,test_data)
#    ConfM<-table(test_label,pred.model)
#    Acc[i]<-sum(diag(ConfM))/sum(ConfM)
#    P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
#    R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
#    F1[i]<-2/(1/R[i]+1/P[i])
#    test_label<-as.numeric(test_label)
#    pred.model<-as.numeric(pred.model)
#    Auc[i]<-multiclass.roc(test_label,pred.model)$auc[1]
#  }
#  best_rF<-obj$best.parameters
#  res_rF<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
#  names(res_rF)<-c("Acc","R","P","F1","Auc")
#  }
#  final<-list()
#  if(mth[1]==1){
#    final$nnet=list(best_parameters=best_nnet,results=res_nnet)
#  }
#  if(mth[2]==1){
#    final$knn=list(best_parameters=best_knn,results=res_knn)
#  }
#  if(mth[3]==1){
#    final$svm=list(best_parameters=best_svm,results=res_svm)
#  }
#  if(mth[4]==1){
#    final$rF=list(best_parameters=best_rF,results=res_rF)
#  }
#  return(final)
#  }

