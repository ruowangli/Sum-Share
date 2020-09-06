library(Rcpp)
library(RcppArmadillo)
library(data.table)



sourceCpp("/home/ruowang/distributed_score_realdata/score_hessian.cpp")

args <- commandArgs(TRUE)
X<-fread(args[1],header=T)
pheno<-read.table(args[2],header=T)
theta<-read.table(args[3],header=F)

age_bin=c(50,75)

#convert age to catagorical variable
for (i in 1:((ncol(pheno)-2)/2)){
pheno[pheno[,2*i+1]<=age_bin[1],2*i+1]=1
pheno[(pheno[,2*i+1]>age_bin[1])&(pheno[,2*i+1]<age_bin[2]),2*i+1]=2
pheno[pheno[,2*i+1]>=age_bin[2],2*i+1]=3
}


Y<-as.matrix(pheno[,seq(2,by=2, (ncol(pheno)-2))])
Age<-as.matrix(pheno[,seq(3,by=2, (ncol(pheno)-1))])
gender<-as.matrix(pheno$gender)
X<-as.matrix(X[,7:ncol(X)])
theta<-as.matrix(theta)


distributed_covariate_score_hessian = function(X,Y,Age,gender,theta){

  s = score(theta,X,Age,gender,Y)
  ss_x = score_square_X(theta,X,Age,gender,Y)
  ss_z = score_square_Z(theta,Age,gender,Y)
  ss_xz = score_square_XZ(theta,X,Age,gender,Y)
  h_zz = hessian_ZZ(theta,Age,gender)
  h_xz = hessian_XZ(theta,X,Age,gender)
  
  
  return(list("s"=s,"ss_x"=ss_x,"ss_z"=ss_z,"ss_xz"=ss_xz,"h_zz"=h_zz,"h_xz"=h_xz))


}

total<-ncol(X)

first_third_x<-floor(total/3)
second_third_x<-2*first_third_x


result1<-distributed_covariate_score_hessian(as.matrix(X[,1:first_third_x]),Y,Age,gender,theta)
result2<-distributed_covariate_score_hessian(as.matrix(X[,(first_third_x+1):second_third_x]),Y,Age,gender,theta)
result3<-distributed_covariate_score_hessian(as.matrix(X[,(second_third_x+1):total]),Y,Age,gender,theta)


s<-rbind(result1$s,result2$s,result3$s)
ss_x<-rbind(result1$ss_x,result2$ss_x,result3$ss_x)
ss_z<-result1$ss_z
ss_xz<-rbind(result1$ss_xz,result2$ss_xz,result3$ss_xz)
h_zz<-result1$h_zz
h_xz<-rbind(result1$h_xz,result2$h_xz,result3$h_xz)


write.table(s,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_s"),row.names=F,col.names=F)
write.table(ss_x,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_ss_x"),row.names=F,col.names=F)
write.table(ss_z,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_ss_z"),row.names=F,col.names=F)
write.table(ss_xz,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_ss_xz"),row.names=F,col.names=F)
write.table(h_zz,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_h_zz"),row.names=F,col.names=F)
write.table(h_xz,paste0(args[4],"/",basename(args[1]),"_",basename(args[2]),"_h_xz"),row.names=F,col.names=F)

