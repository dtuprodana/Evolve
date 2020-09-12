
library(readxl)
data <- read_excel("C:/Postdoc/MADE Digital/LEGO/Additive_manufac/Homogeneity_Tensile_postupgrade_new_setting Q419/1571395275_appLogs.xlsx")

library(dplyr)
names(data)
data<-data %>% select(-Time,-TransferCycleCount,-Layer,-Tile)
cov(data)


##################### Principap component ############

names(data[, sapply(data, function(v) var(v, na.rm=TRUE)==0)])
data<-data[,sapply(data, function(v) var(v, na.rm=TRUE)!=0)]
library("factoextra")
normal_pc<-prcomp(data, scale = TRUE)

fviz_eig(normal_pc)

aa<-summary(normal_pc)

bb<-aa$importance[3,]
ncomp=length(bb[bb<=0.8])


library(nsprcomp)

base_nsprcomp<-nsprcomp(data, ncomp=ncomp , scale. = TRUE)

peav(as.matrix(data),as.matrix(base_nsprcomp$rotation),scale = T)
sum(peav(as.matrix(data),as.matrix(base_nsprcomp$rotation),scale = T))

percent_exp<-matrix(nrow=ncol(data),ncol =3)
sum_percent_exp<-numeric()
for(i in 2:ncol(data))
{
  bb<-nsprcomp(data,ncomp=ncomp, k = c(i,rep(2,ncomp-1)), scale. = TRUE)
  percent_exp[i,]<-peav(as.matrix(data),as.matrix(bb$rotation),scale = T)
  sum_percent_exp[i]<-sum(percent_exp[i,])
}

tst<-diff(sum_percent_exp,1)

tst

for(i in 2:ncol(data))
{
  bb<-nsprcomp(data,ncomp=ncomp, k = c(36,i,2), scale. = TRUE)
  percent_exp[i,]<-peav(as.matrix(data),as.matrix(bb$rotation),scale = T)
  sum_percent_exp[i]<-sum(percent_exp[i,])
}


for(i in 2:ncol(data))
{
  bb<-nsprcomp(data,ncomp=ncomp, k = c(36,12,i), scale. = TRUE)
  percent_exp[i,]<-peav(as.matrix(data),as.matrix(bb$rotation),scale = T)
  sum_percent_exp[i]<-sum(percent_exp[i,])
}


bb<-nsprcomp(data,ncomp=ncomp, k = c(25,16,6), scale. = TRUE)
peav(as.matrix(data),as.matrix(bb$rotation),scale = T)

sum(peav(as.matrix(data),as.matrix(bb$rotation),scale = T))

bb$x
############################################
54^3

optim_func<-function(x)
{
  bb<-nsprcomp(data,k = c(x), scale. = TRUE)
  percent_exp<-peav(as.matrix(data),as.matrix(bb$rotation),scale = T)
  return(sum(percent_exp)/sum(x))
}


optim_func(c(10,17,8))


GA <- ga(type = "permutation", 
         fitness =  optim_func,
         lower = c(2,2,2), upper = c(25,25,25), 
         popSize = 50, maxiter = 5,
         optim = TRUE)
summary(GA)$solution

GA@

summary(result)$solution

GA@bestSol

library(GA)
gaControl("useRcpp" = F)
f <-function(z) sum((z-c(3,2,1))^2)  # best order is 1, 2, 3
result <- ga(type="permutation", fitness=f, 
             min=c(1,1,1), max=c(3,3,3), names=paste0("pizza",1:3)
             )
summary(result)$solution


###################################

library(elasticnet)
out0<-spca(cov(data),K=2,type="Gram",sparse="varnum",trace=TRUE,para=c(2,2))
out0
sum(out0$pev)



for (i in 2:58)
{
  val1<-numeric()
  val1[1]<-0.75
  j=2
  lp1=0.01
  while(lp1 >= 0.01)
  {
    out1<-spca(cov(data),K=4,type="Gram",sparse="varnum",trace=TRUE,para=c(j,2,2,2))
    val1[j]<-sum(out1$pev)
    lp1 <- val1[j]- val1[j-1]
    j=j+1
  }
  val2<-numeric()
  val2[1]<-0.75
  k=2
  lp2=0.01
  while(lp2 >= 0.01)
  {
    out1<-spca(cov(data),K=4,type="Gram",sparse="varnum",trace=TRUE,para=c(j,k,2,2))
    val2[k]<-sum(out1$pev)
    lp2 <- val2[k]- val2[k-1]
    k=k+1
  }
  
  
  val2<-numeric()
  val2[1]<-0.75
  k=2
  lp2=0.01
  while(lp2 >= 0.01)
  {
    out1<-spca(cov(data),K=4,type="Gram",sparse="varnum",trace=TRUE,para=c(j,k,2,2))
    val2[k]<-sum(out1$pev)
    lp2 <- val2[k]- val2[k-1]
    k=k+1
  }

}






## to see the contents of out2
names(out1) 
## to get the loadings
out1$loadings
sum(out1$pev)

sol_matrix<-matrix(rep(1:13,13),nrow=13)
sol_matrix








library(tidyr)

sol_matrix<-as.matrix(crossing(var1 = 1:13, var2 = 1:13, var3 = 1:13))

sl2<-1:3

x<-c(1,0,1)


f <- function(x) {
  out1<-spca(pitprops,K=3,type="Gram",sparse="varnum",trace=TRUE,para=c(x))
  var_explained<-abs(sum(out1$pev)-sum(out0$pev))
  return(var_explained)
  }

f(c(2,5,3))


fitness <- function(x) -f(x)
GA <- ga(type = "permutation", fitness = fitness,lower = 1,upper = 13,popSize = 50, maxiter = 5)

summary(GA)
plot(GA)

sol_matrix[5,]

fitness_function<-function(x)
{
  out1<-spca(pitprops,K=3,type="Gram",sparse="varnum",trace=TRUE,para=sol_matrix[1,])
  var_explained<-abs(sum(out1$pev)-sum(out0$pev))
}
  

library(GA)
?ga
