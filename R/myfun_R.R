#' @title failure machines data
#' @name data
#' @description A prediction variable data of failure machines
#' @format A data frame with 297 rows and 5 variables:
#' \describe{
#'   \item{D}{Temperature difference}
#'   \item{V}{speed}
#'   \item{N}{torque}
#'   \item{T}{service time}
#'   \item{L}{machine level}
#' }
NULL

#' @title machines error data
#' @name label
#' @description A response vector of failure machines, 297 in length, divided into five kinds
NULL

#' @title observed event
#' @name event
#' @description equals 1 if the observed event is a failure time and 0 otherwise
NULL

#' @title survival time
#' @name time
#' @description survival time for ADNI
NULL

#' @title Hippocampal surface data
#' @name X
#' @description Hippocampal surface data in ADNI
#' @format A data frame with 300 rows and 373 individuals
#' @details Complete data with 30000 rows are available in https://adni.loni.usc.edu/study-design/
NULL

#' @title Scalar factor
#' @name Z
#' @description Scalar predictors after treatment, such as age, blood pressure
#' @format A data frame with 373 individuals and 12 variables
NULL

#' @title Estimation of parameters for Functional Cox Model
#' @description Using functional principal component analysis method to estimate the parameters of Functional Cox Model
#' @param time survival time (vector)
#' @param event equals 1 if the observed event is a failure time and 0 otherwise (vector)
#' @param X functional factor (high-dimensional matrix) 
#' @param Z scalar factor (matrix)
#' @param hh optional parameters to control the number of principal components (default 40)
#' @param k optional parameters to control the number of principal components (default 5)
#' @return a list of estimation of parameters for \code{X} and \code{Z}
#' @importFrom stats as.formula
#' @importFrom survival coxph survfit Surv
#' @examples
#' \dontrun{
#' data(time)
#' data(event)
#' data(X)
#' data(Z)
#' attach(time)
#' attach(event)
#' attach(X)
#' attach(Z)
#' res <- flcrm(time,event,X,Z)
#' }
#' @export
flcrm<-function(time,event,X,Z,hh=40,k=5){
  h<-1/ncol(X)
  scaleX<-scale(X,center=TRUE,scale=FALSE)
  Eigvec=svd(scaleX)$v
  ordertime<-order(time)
  sorttime<-time[ordertime]
  sortevent<-event[ordertime]
  sortfailureindex<-which(sortevent==1)
  AIC<-loglikelihood<-rep(0,hh)
  for (ss in 1:hh) {
    est_Eigfun=Eigvec[,1:ss]/sqrt(h)
    eigenscore<-scaleX%*%est_Eigfun*h
    designmatrix<-cbind(eigenscore,Z)
    fullcoxresult<-survival::coxph(formula=survival::Surv(time,event)~designmatrix)
    orderdesignmatrix<-designmatrix[ordertime,]
    estimatecoefficient<-fullcoxresult$coefficients
    fullcoxcurve<-survival::survfit(fullcoxresult)
    finalresult<-summary(fullcoxresult)
    loglikelihood[ss]<-finalresult$loglik[2]
    AIC[ss]<-2*ss-2*loglikelihood[ss]
  }
  AICselectindex<-which.min(AIC[-(1:k)])+k
  est_Eigfun=Eigvec[,1:AICselectindex]/sqrt(h)
  eigenscore<-scaleX%*%est_Eigfun*h
  designmatrix<-cbind(eigenscore,Z)
  newdataframe<-data.frame(cbind(time, event, designmatrix))
  totalvariable<-"V3"
  for (i in 4:(ncol(designmatrix)+2)){
    totalvariable<-paste(totalvariable,"+V", i, sep="")
  }
  totalformular<-stats::as.formula(paste("Surv(time,event)~", totalvariable, sep=""))
  fullcoxresult<-survival::coxph(formula=totalformular, data=newdataframe)
  fullcoxcurve<-survival::survfit(fullcoxresult)
  finalresult<-summary(fullcoxresult)
  summarycoefficient<-finalresult$coefficients
  coefficient<-summarycoefficient[,1]
  secoefficient<-summarycoefficient[,3]
  AICestimatebeta<-est_Eigfun%*%as.matrix(coefficient[1:AICselectindex])
  AICestimategamma<-coefficient[-(1:AICselectindex)]
  return(list(beta=AICestimatebeta,gamma=AICestimategamma))
}

#' @title Integration of machine learning algorithms
#' @description Using k-fold cross validation combined with grid search to give various scores of nnet svm knn randomForest
#' @param label response variable (logical)
#' @param data predictive Variables (numeric)
#' @param k k fold cross validation
#' @param methods choose machine learning algorithms
#' @return a list given scores of used algorithms
#' @importFrom stats predict
#' @importFrom class knn
#' @importFrom e1071 tune.nnet tune.knn tune.control tune.svm tune.randomForest
#' @importFrom nnet nnet
#' @importFrom pROC multiclass.roc
#' @importFrom randomForest randomForest
#' @examples
#' \dontrun{
#' data(label)
#' data(data)
#' attach(label)
#' attach(data)
#' res <- mlearning(label,data,methods=c("knn","rF"))
#' }
#' @export
mlearning<-function(label,data,k=5,methods=c("nnet","knn","SVM","rF")){
  n<-length(label)
  ind<-sample(1:n)
  Acc<-numeric(k)
  R<-numeric(k)
  P<-numeric(k)
  F1<-numeric(k)
  Auc<-numeric(k)
  mth<-c(0,0,0,0)
  if(length(intersect("nnet",methods))==1){
    mth[1]<-1
    for(i in 1:k){
      index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
      train_data=data[-index,]
      test_data=data[index,]
      train_label<-label[-index]
      test_label<-label[index]
      datatrain<-cbind(train_data,train_label)
      obj<-e1071::tune.nnet(train_label~., data=datatrain,size=2:10,maxit=1000)
      best_size<-as.numeric(obj$best.parameters)
      nnet.sol <- nnet::nnet(train_label~., datatrain, size =best_size, maxit = 1000)
      pred.prob <- stats::predict(nnet.sol, test_data)
      pred<-character(nrow(pred.prob))
      for(j in 1:nrow(pred.prob)){
        pred[j]<-names(which(pred.prob[j,]==max(pred.prob[j,])))
      }
      pred<-as.factor(pred)
      ConfM<-table(test_label,pred)
      c<-numeric(nrow(ConfM))
      for(j in 1:nrow(ConfM)){
        c[j]<-which(names(ConfM[1,])==names(ConfM[,1])[j])
      }
      ConfM<-ConfM[,c]
      Acc[i]<-sum(diag(ConfM))/sum(ConfM)
      R[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
      P[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
      F1[i]<-2/(1/R[i]+1/P[i])
      test_label<-as.numeric(test_label)
      pred<-as.numeric(pred)
      Auc[i]<-pROC::multiclass.roc(test_label,pred)$auc[1]
    }
    best_nnet<-obj$best.parameters
    res_nnet<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
    names(res_nnet)<-c("Acc","R","P","F1","Auc")
  }
  if(length(intersect("knn",methods))==1){
    mth[2]<-1
    for(i in 1:k){
      index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
      train_data=data[-index,]
      test_data=data[index,]
      train_label<-label[-index]
      test_label<-label[index]
      obj <- e1071::tune.knn(train_data,train_label,k=2:10,tunecontrol=e1071::tune.control(sampling = "boot"))
      best_k<-as.numeric(obj$best.parameters)
      dt_knn<-class::knn(train = train_data,test = test_data,cl=train_label,k=best_k)
      ConfM<-table(test_label,dt_knn)
      Acc[i]<-sum(diag(ConfM))/sum(ConfM)
      P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
      R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
      F1[i]<-2/(1/R[i]+1/P[i])
      test_label<-as.numeric(test_label)
      dt_knn<-as.numeric(dt_knn)
      Auc[i]<-pROC::multiclass.roc(test_label,dt_knn)$auc[1]
    }
    best_knn<-obj$best.parameters
    res_knn<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
    names(res_knn)<-c("Acc","R","P","F1","Auc")
  }
  if(length(intersect("SVM",methods))==1){
    mth[3]<-1
    for(i in 1:k){
      index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
      train_data=data[-index,]
      test_data=data[index,]
      train_label<-label[-index]
      test_label<-label[index]
      datatrain<-cbind(train_data,train_label)
      tObj<-e1071::tune.svm(train_label~.,data=datatrain,type="C-classification",kernel="radial",cost=2^seq(-10,10,1),gamma=2^seq(-10,10,1),scale=FALSE)
      first<-tObj$best.parameters
      num1<-as.numeric(log2(first[1]))
      num2<-as.numeric(log2(first[2]))
      tObj<-e1071::tune.svm(train_label~.,data=datatrain,type="C-classification",kernel="radial",cost=2^seq(num2-1,num2+1,0.1),gamma=2^seq(num1-1,num1+1,0.1),scale=FALSE)
      BestSvm<-tObj$best.model
      yPred<-stats::predict(BestSvm,test_data)
      ConfM<-table(test_label,yPred)
      Acc[i]<-sum(diag(ConfM))/sum(ConfM)
      P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
      R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
      F1[i]<-2/(1/R[i]+1/P[i])
      test_label<-as.numeric(test_label)
      yPred<-as.numeric(yPred)
      Auc[i]<-pROC::multiclass.roc(test_label,yPred)$auc[1]
    }
    best_svm<-tObj$best.parameters
    res_svm<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
    names(res_svm)<-c("Acc","R","P","F1","Auc")
  }
  if(length(intersect("rF",methods))==1){
    mth[4]<-1
    for(i in 1:k){
      index<-ind[(floor((i-1)*n/5)+1):floor(i*n/5)]
      train_data=data[-index,]
      test_data=data[index,]
      train_label<-label[-index]
      test_label<-label[index]
      datatrain<-cbind(train_data,train_label)
      obj<-e1071::tune.randomForest(train_label~.,data =datatrain, ntree = seq(100,1000,100))
      best_ntree<-as.numeric(obj$best.parameters)
      model <- randomForest::randomForest(train_label~.,data =datatrain,ntree=best_ntree)
      pred.model <- stats::predict(model,test_data)
      ConfM<-table(test_label,pred.model)
      Acc[i]<-sum(diag(ConfM))/sum(ConfM)
      P[i]<-mean(diag(ConfM)/apply(ConfM,2,sum))
      R[i]<-mean(diag(ConfM)/apply(ConfM,1,sum))
      F1[i]<-2/(1/R[i]+1/P[i])
      test_label<-as.numeric(test_label)
      pred.model<-as.numeric(pred.model)
      Auc[i]<-pROC::multiclass.roc(test_label,pred.model)$auc[1]
    }
    best_rF<-obj$best.parameters
    res_rF<-c(mean(Acc),mean(R),mean(P),mean(F1),mean(Auc))
    names(res_rF)<-c("Acc","R","P","F1","Auc")
  }
  final<-list()
  if(mth[1]==1){
    final$nnet=list(best_parameters=best_nnet,results=res_nnet)
  }
  if(mth[2]==1){
    final$knn=list(best_parameters=best_knn,results=res_knn)
  }
  if(mth[3]==1){
    final$svm=list(best_parameters=best_svm,results=res_svm)
  }
  if(mth[4]==1){
    final$rF=list(best_parameters=best_rF,results=res_rF)
  }
  return(final)
}
