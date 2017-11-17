require(ISLR)
require(boot)
library(tidyverse)
library(modelr)
require(MASS)
library(leaps)
library(glmnet)
#
library(e1071)
require(gbm)
#
library(ggplot2)
require(dplyr)
require(data.world)
project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
data.world::set_config(cfg_env("DW_API"))
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

# boosting
###########
df2 = dplyr::select(df, -subject)
df2 <- df2 %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))
df2 = dplyr::select(df2, -sex)
set.seed(1)
train = sample(nrow(df2), 97)
test = df2[-train,]

boost.df2=gbm(age~.,data=df2[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
boost.df2
summary(boost.df2)

# pca?
dimnames(df2)
apply(df2,2,mean)
apply(df2,2, var)
pca.out=prcomp(df2, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)

# k means
x = df%>%dplyr::select(age, total_updrs)
km.out=kmeans(x,42)
km.out
par(mfrow = c(1, 2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=df$subject,cex=2,main = "Actual subject",pch=1,lwd=2)
# ggplot alternative:
km.out$cluster
dfcluster <- data.frame(x, km.out$cluster)
names(dfcluster)
dfcluster2 <- dplyr::bind_cols(dfcluster, data.frame(df$subject))

kmeansubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point()
actualsubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(df.subject))) + geom_point()

df1 <- data.frame(km.out$centers, km.out$size)
names(df1)
dfcluster2 %>% ggplot() + geom_point(mapping = aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(age, total_updrs, size=km.out.size))

df2 %>% ggplot(aes(x=age, y = total_updrs, colour = sex2)) + geom_point()

# hierchical 
hc.complete=hclust(dist(x),method="complete")
hc.single=hclust(dist(x),method="single")
hc.average=hclust(dist(x),method="average")
par(mfrow = c(3, 1))
plot(hc.complete)
plot(hc.single)
plot(hc.average)

hc.cut=cutree(hc.complete,42)
table(hc.cut,df$subject)
table(hc.cut,km.out$cluster)


### SVM
# use boosting to see what predictors influence sex
boost.df=gbm(sex2~.,data=df2[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
boost.df
summary(boost.df)
x=subset(df2, select = c(dfa, jitter_abs))
x=matrix(unlist(x), ncol = 2)
y=df2$sex2
dat=data.frame(x,y=as.factor(y))
svmfit=svm(y~.,data=dat,type="nu-classification",kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit,dat)

make.grid=function(x,n=150){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length=n)
  x2=seq(from=grange[1,2],to=grange[2,2],length=n)
  expand.grid(X1=x1,X2=x2)
}
xgrid=make.grid(x)
ygrid=predict(svmfit,xgrid)

beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho
plot(xgrid,col=c("yellow","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)

nlsvmfit=svm(factor(y)~.,data=dat,type="nu-classification",scale=TRUE,kernel="radial",cost=10)
plot(nlsvmfit, dat)
x1=seq(.5, .85, by=0.35/149.0)
x2=seq(0, .0004, by=0.0004/149.0)
xgrid=expand.grid(X1=x1,X2=x2)
ygrid=predict(nlsvmfit,xgrid)
func=predict(nlsvmfit,xgrid,decision.values=TRUE)
func=attributes(func)$decision
plot(xgrid,col=as.numeric(ygrid),pch=20,cex=.2)
points(x,col=y+1,pch=19)
contour(x1,x2,matrix(func,150,150),level=0,add=TRUE)
