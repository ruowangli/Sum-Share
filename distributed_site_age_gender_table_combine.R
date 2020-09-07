
args <- commandArgs(TRUE)


weights<-data.frame()

for (i in 1:length(args)){
 #this is for all sites
 # weights_temp <-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_table"),header=F) 
  #this is for 2 sites
  weights_temp <-read.table(paste0("/home/ruowang/distributed_score_realdata/white_",args[i],"/","ICD_9_matrix_white_",args[i],"_metabolic_imputed_covariates_2sites_table"),header=F) 
  
 
if (nrow(weights)==0){
  weights  = weights_temp }
 else {weights = weights + weights_temp
	}
}

Y = c(rep(c(0,1), each = 3),rep(c(0,1), each = 3))
gender = rep(c(0,1), each = 6)
age = rep(c(1,2,3),4)

theta = matrix(0, nrow = 4, ncol = ncol(weights))
for (i in 1:ncol(weights)){
  fit = glm(formula = Y ~ as.factor(gender) + as.factor(age), family = binomial(logit), weights=weights[,i])
  theta[,i] = fit$coefficients
}

#write.table(theta,paste0("/home/ruowang/distributed_score_realdata/ICD_9_matrix_white_metabolic_imputed_covariates_theta"),row.names=F,col.names=F)
write.table(theta,paste0("/home/ruowang/distributed_score_realdata/ICD_9_matrix_white_metabolic_imputed_covariates_2sites_theta"),row.names=F,col.names=F)



