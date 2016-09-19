#Cuiqing Li
#cli92@jhu.edu

library(dplyr)
library(xlsx)
library(MASS)

#this function is used to get coefficients of a linear regression givn a known data.frame
linreg=function(){
   d<-read.table("Jobs-NoMiss-Cont.tab",header=TRUE)
   m<-as.matrix(d)
   
   #extract the first 11 columns from the original matrix
   new_m<-m[,1:11]
   
   #create a constant list and add it to the new_m matrix
   x<-rep(1,times=nrow(new_m))
   new_m<-cbind(x,new_m)
   colnames(new_m)[1]<-"constant"
   
   #new we can copmute the coefficients using closed forms 
   transpose<-t(new_m)
   inverse1<-solve(transpose %*% new_m)
   target<-m[,12]
   target<-as.matrix(target)
   cof<-inverse1%*%(transpose %*% target)
   
   print(cof)
   #print(target)
}

#this function is used to name matrix's column names
df_make=function(mat){
  #change matrix into a data frame
  new_mat<-data.frame(mat)
  col_names=paste('x',1:ncol(mat),sep="")
  colnames(new_mat)<-col_names
  
  print(new_mat)
}

#this function is used to get a sampled new data fram from a original data frame
df_resample=function(df){
  sampled_rowindex=sample(1:nrow(df),size=nrow(df),replace = TRUE)
  new_dataframe<-df[sampled_rowindex,]
  new_dataframe
  
}

#this function is used to implement bootstap strategies
bootstrap_ci=function(df,k,f,q){
  #this is the mean value of the original data frame
  obtain_statistics<-f(df)
  
  #then we resample the data frame for k times
  statistics<-c()
  for(i in 1:k){
    temp_dataframe<-df_resample(df)
    temp<-f(temp_dataframe)
    statistics<-c(statistics,temp)
  }
  
  #get the confidence interval
  difference<-obtain_statistics-statistics
  values<-quantile(difference,c(q,1-q))
  values<-values+obtain_statistics
  
  final_vectors<-c(obtain_statistics,values)
  final_vectors
}

main <- function() {
  set.seed(0)
  m<-c(1,2,3,0,0)
  b<-matrix(c(1,1,1,1,1,
              1,3,1,1,1,
              1,1,5,1,1,
              1,1,1,2,0,
              1,1,1,0,2),nrow = 5,ncol = 5)
  mvn<-mvrnorm(n=1000,mu=m,Sigma=b)
  cnames<-paste("X",1:5,sep="")
  colnames(mvn)<-cnames
  mvn<-data.frame(mvn)
  
  #set default k=1000
  k=1000
  vector1<-bootstrap_ci(mvn,k,mean_X1,0.025)
  vector2<-bootstrap_ci(mvn,k,mean_X2,0.025)
  vector3<-bootstrap_ci(mvn,k,median_X3,0.025)
  vector4<-bootstrap_ci(mvn,k,cov_X4X5,0.025)
  
  vec<-rbind(vector1,vector2,vector3,vector4)
  row.names(vec)<-c("mean_X1","mean_X2","median_X3","covraince_X4X5")
  
  print(vec)
  linreg()
}


mean_X1<-function(df){
  mean(df$X1)
}
mean_X2<-function(df){
  mean(df$X2)
}
median_X3<-function(df){
  median(df$X3)
}
cov_X4X5<-function(df){
  cov(df$X4,df$X5)
}

main()