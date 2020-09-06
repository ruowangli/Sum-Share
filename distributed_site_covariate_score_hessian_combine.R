library(Rcpp)
library(RcppArmadillo)

sourceCpp("/home/ruowang/distributed_score_realdata/score_hessian.cpp")


args <- commandArgs(TRUE)

s<-data.frame()
ss_x<-data.frame()
ss_z<-data.frame()
ss_xz<-data.frame()
h_zz<-data.frame()
h_xz<-data.frame()
N=0

sites<-""
for (i in 1:(length(args)-1)){

#s_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_s_common"),header=F)
#ss_x_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_ss_x_commmon"),header=F)
#ss_z_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_ss_z"),header=F)
#ss_xz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_ss_xz_common"),header=F)
#h_zz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_h_zz"),header=F)
#h_xz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_h_xz_common"),header=F)



s_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_s"),header=F)
ss_x_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_ss_x"),header=F)
ss_z_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_ss_z"),header=F)
ss_xz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_ss_xz"),header=F)
h_zz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_h_zz"),header=F)
h_xz_temp<-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","white_",args[i],"_chr",args[length(args)],".raw_ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_h_xz"),header=F)


pheno<-read.table<-read.table(paste0("/home/ruowang/distributed_score_realdata/ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites"),header=T)
N=N+nrow(pheno)

sites=paste(sites,args[i],sep="_")

if (nrow(s)==0){
  s =  s_temp
  ss_x =  ss_x_temp
  ss_z =  ss_z_temp
  ss_xz = ss_xz_temp
  h_zz =  h_zz_temp
  h_xz =  h_xz_temp
   }
 else {
  s = s + s_temp
  ss_x = ss_x + ss_x_temp
  ss_z = ss_z + ss_z_temp
  ss_xz = ss_xz + ss_xz_temp
  h_zz = h_zz + h_zz_temp
  h_xz = h_xz + h_xz_temp
     } 
}

s=as.matrix(s/N)
ss_x=as.matrix(ss_x/N)
ss_z=as.matrix(ss_z/N)
ss_xz=as.matrix(ss_xz/N)
h_zz=as.matrix(h_zz/N)
h_xz=as.matrix(h_xz/N)



distributed_covariate_score_hessian_combine = function(s,ss_x,ss_z,ss_xz,h_zz,h_xz,N){

  
  test_stat = test_function(s,ss_x,ss_z,ss_xz,h_zz,h_xz,N)

  return(list("test_stat"=test_stat))

}

result<-distributed_covariate_score_hessian_combine(s,ss_x,ss_z,ss_xz,h_zz,h_xz,N)


p = 1-pchisq(result$test_stat,5)

write.table(p,paste0("/home/ruowang/distributed_score_realdata/white_chr",args[length(args)],sites,"_covariates_2sites_pvalue"),row.names=F,col.names=F)



