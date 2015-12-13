churn1=read.table("d:\\churn.csv")

summaryt(churn1)

str(churn1)

head(churn1)

library(rpart)
f<-rpart(Churn.~CustServ.Calls+Eve.Calls+Intl.Calls+Night.Calls
         +Day.Calls,method="class", data=churn1)
plot(f, uniform=TRUE,main="Classification Tree for Churn")
text(f, use.n=TRUE, all=TRUE, cex=.7)

library(rpart)
f<-rpart(Churn.~CustServ.Calls+Eve.Charge+Intl.Charge+Night.Charge+Day.Charge,method="class" ,data=churn1)
plotcp(f,lty=4,col="red")

library(rpart)
tree <- rpart(Churn. ~CustServ.Calls + Eve.Charge + Intl.Charge + Night.Charge + Day.Charge ,data = churn1 ,method = "class")
plot(tree,main="decision tree")
text(tree,pretty = 0)

logmodel <- glm(Churn. ~ CustServ.Calls + Eve.Charge + Intl.Charge + Night.Charge + Day.Charge ,data = churn1,family=binomial())
 summary(logmodel)

churn1$predicted <- predict(tree, data = churn1, type ="class")
cm <- print(table(churn1$predicted, churn1$Churn., 
                  dnn=c("Predicted", "Actual")))


reg1 <- lm(State~Churn.)
par(cex=.8)
plot(State,Churn.)
abline(reg1)

churn1$predicted <- predict(tree, data = churn1, type ="class")
cm <- print(table(churn1$predicted, churn1$Churn., 
                  dnn=c("Predicted", "Actual")))