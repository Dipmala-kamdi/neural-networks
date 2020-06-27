forest <- read.csv(file.choose())
View(forest)
str(forest)
summary(forest)
forest_fire<- forest[-c(1,2,31)]
str(forest_fire)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}


forest_norm<-as.data.frame(lapply(forest_fire[,-9],FUN=normalize))

summary(forest_fire$area)

forest_norm <- cbind(forest_norm,forest_fire$area)
colnames(forest_norm)[28] <- "area"
forest_train<-forest_norm[1:30,]
forest_test<-forest_norm[31:50,]

# regression
library(neuralnet) 
# classification
library(nnet)

# NN model
forest_nn <- paste("area",paste(colnames(forest_fire[-9]),collapse ="+"),sep="~")
forest_model <- neuralnet(formula = forest_nn,data = forest_train, linear.output = F)
str(forest_model)
plot(forest_model)

set.seed(12323)
model_results <- compute(forest_model,forest_test[1:27])
str(model_results)
predicted_profit <- model_results$net.result

cor(predicted_profit,forest_test$area)
plot(predicted_profit,forest_test$area)
model_5<-neuralnet(area~.,data= forest_norm,hidden = 5, linear.output = F)
plot(model_5)
model_5_res<-compute(model_5,forest_test[1:27])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forest_test$Profit)
plot(pred_strn_5,forest_test$Profit)
