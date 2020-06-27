startup <- read.csv(file.choose())
View(startup)
str(startup)
summary(startup)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup$Profit <- as.numeric(startup$Profit)

startup_norm<-as.data.frame(lapply(startup[,-c(4,5)],FUN=normalize))

summary(startup$Profit)

startup_norm <- cbind(startup_norm,startup$Profit)
colnames(startup_norm)[4] <- "Profit"
startup_train<-startup_norm[1:30,]
startup_test<-startup_norm[31:50,]

# regression
library(neuralnet) 
# classification
library(nnet)

# NN model
startup_nn <- paste("Profit",paste(colnames(startup[-c(4,5)]),collapse ="+"),sep="~")
startup_model <- neuralnet(formula = startup_nn,data = startup_train, linear.output = F)
str(startup_model)
plot(startup_model)

set.seed(12323)
model_results <- compute(startup_model,startup_test[1:3])
str(model_results)
predicted_profit <- model_results$net.result

cor(predicted_profit,startup_test$Profit)
plot(predicted_profit,startup_test$Profit)
model_5<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,data= startup_norm,hidden = 5, linear.output = F)
plot(model_5)
model_5_res<-compute(model_5,startup_test[1:3])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startup_test$Profit)
plot(pred_strn_5,startup_test$Profit)
