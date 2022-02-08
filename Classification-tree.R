library(ISLR)
View(Carseats)
Carseats=transform(Carseats,High=ifelse((Sales>=8),"Yes","No"))
Carseats$High=factor(Carseats$High)

library(tree)
n=nrow(Carseats)
train=sample(1:n,n/2)
T=tree(High~.-Sales,data=Carseats,subset=train)
plot(T)
text(T,pretty=0)
pred=predict(T,newdata=Carseats[-train,],type="class")
CM=table(pred,Carseats[-train,]$High)
CM
TE=1-(sum(diag(CM))/sum(CM))
TE
Acc=sum(diag(CM))/sum(CM)
Acc

CVT=cv.tree(T,FUN=prune.misclass)
plot(CVT$size,CVT$dev)
SS=CVT$size[which.min(CVT$dev)]
SS

FT=prune.misclass(T,best=SS)
plot(FT)
text(FT,pretty=0)
pred2=predict(FT,newdata=Carseats[-train,],type="class")
CM1=table(pred2,Carseats[-train,]$High)
CM1
Acc1=sum(diag(CM1))/sum(CM1)
Acc1

p=ncol(Carseats)-2
library(randomForest)
BagT=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=p,ntree=100)
pred3=predict(BagT,newdata=Carseats[-train,],type="class")
CM2=table(pred3,Carseats[-train,]$High)
CM2
Acc2=sum(diag(CM2))/sum(CM2)
Acc2

RFT=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=sqrt(p),ntree=100)
pred4=predict(RFT,newdata=Carseats[-train,],type="class")
CM3=table(pred4,Carseats[-train,]$High)
CM3
Acc3=sum(diag(CM3))/sum(CM3);Acc3
names(RFT)
summary(RFT$.oob.times)
summary(RFT$importance)

library(gbm)
Carseats$High=ifelse((Carseats$High=="Yes"),1,0)
BT=gbm(High~.-Sales,data=Carseats[train,],distribution="bernoulli",shrinkage=0.01,n.trees=100)
pred5=predict(BT,newdata=Carseats[-train,],type="response",n.trees=100)
pred5
par(mfrow=c(1,1))
par(mar=c(2,6,6,5))
summary(BT,las=1)
BT

