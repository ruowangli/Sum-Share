
args <- commandArgs(TRUE)

Y<-read.table(args[1],header=T)

age_bin=c(50,75)

#convert age to catagorical variable
for (i in 1:((ncol(Y)-2)/2)){
Y[Y[,2*i+1]<=age_bin[1],2*i+1]=1
Y[(Y[,2*i+1]>age_bin[1])&(Y[,2*i+1]<age_bin[2]),2*i+1]=2
Y[Y[,2*i+1]>=age_bin[2],2*i+1]=3
}
#3 age group, 2 gender, case control = 12 rows
weight<-matrix(nrow=2*3*2,ncol=((ncol(Y)-2)/2))
for (i in 1:((ncol(Y)-2)/2)){
tab=table(Y[,2*i],Y[,2*i+1],Y$gender)
weight[,i]=c(tab[1,,1],tab[2,,1],tab[1,,2],tab[2,,2])
}

write.table(weight,paste0(args[2],"/",basename(args[1]),"_table"),row.names=F,col.names=F)
