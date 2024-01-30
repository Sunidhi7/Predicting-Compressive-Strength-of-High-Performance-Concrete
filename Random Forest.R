#################################################################################################
#Random Forest
#################################################################################################
library(ranger)
library(randomForest)
library(dplyr)

#getting start time
old <- Sys.time() 

concrete <- read.table("Concrete_Data.csv", header=TRUE, sep=",", na.strings =" ")

#The column headers were changed.
names(concrete) <- c("Cement", "Slag", "Flyash", "Water", "Superplasticizer", "Coarse", "Fine", "Age", "Strength")

#Removing the duplicate rows
concrete <- concrete[!duplicated(concrete),]

#for reproducibility
set.seed(123)

#Splitting the dataset into training(75%) and test(25%) dataset.
U <- runif(n=nrow(concrete))
concrete.set <- ifelse(U <= 0.75, yes=1, no=2)

concrete.train = concrete[concrete.set==1,]
concrete.test = concrete[concrete.set==2,]

#Training model using default values of arguments (ntree=500, nodesize=5, mtry=p/3)
concrete.rfdef <- randomForest(data=concrete.train, Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age, 
                             importance=TRUE, keep.forest=TRUE,)
#Plot of OOB error vs. number of trees 
win.graph(h=7,w=6,pointsize=12)
plot(concrete.rfdef)

#Tuning parameters mtry and nodesize using package ranger.
#constructing experimental grid of hyperparameters
  grid <- expand.grid(
  mtry       = seq(1, 8, by = 1),
  node_size  = c(1,3,5,9, seq(10, 30, by = 5)),
  OOB_MSE   = 0
)

#Grid search  
for(i in 1:nrow(grid)) {
   #Training model
    rf <- ranger(
    formula         = Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age, 
    data            = concrete.train, 
    num.trees       = 800,
    mtry            = grid$mtry[i],
    min.node.size   = grid$node_size[i],
    seed            = 123 
  )
    grid$OOB_MSE[i] <- rf$prediction.error
}
  grid %>% 
  dplyr::arrange(OOB_MSE) %>%
  head(10)

#Optimal values of tuning parameters obtained from grid search: mtry=5 and nodesize=1.
#Training final model
concrete.rf1 <- randomForest(data=concrete.train, Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age, 
                             importance=TRUE, ntree=800, keep.forest=TRUE, mtry=5, nodesize=1)
concrete.rf1

#Plot of OOB error vs. number of trees 
win.graph(h=7,w=6,pointsize=12)
plot(concrete.rf1)
 
#Printing out variable importance measures
importance(concrete.rf1)
#Variable importance measures plot
win.graph(h=7,w=6,pointsize=12)
varImpPlot(concrete.rf1, main = "Variable importance plot")

#OOB error
concrete.rf1$mse[800]

pred=predict(concrete.rf1, newdata = concrete.train)
sMSE <- mean((pred - concrete.train$Strength)^2)
sMSE

predm=predict(concrete.rf1, newdata = concrete.test)
MSPE <- mean((predm - concrete.test$Strength)^2)
MSPE

rss <- sum((predm - concrete.test$Strength) ^ 2)  # residual sum of squares
tss <- sum((concrete.test$Strength - mean(concrete.test$Strength)) ^ 2)  # total sum of squares
rsq <- 1 - rss/tss
rsq #R^2 on testing dataset

# printing elapsed time
new <- Sys.time() - old 
print(new)


######################################################################################################
#Random forest with 'Cement' and 'Water' dropped and water-cement ratio added
######################################################################################################
set.seed(123)
#getting start time
oldwc <- Sys.time() 

#Including Water by Cement ratio and deleting Water and Cement columns in training as well as test dataset
concretewc.train = cbind(concrete.train$Water/concrete.train$Cement, concrete.train[,c(-1,-4)])
concretewc.test = cbind(concrete.test$Water/concrete.test$Cement,concrete.test[,c(-1,-4)])

#Changing the header of column containing water by cement ratio to "wcratio"
colnames(concretewc.train)[which(colnames(concretewc.train)=="concrete.train$Water/concrete.train$Cement")] <- "wcratio"
colnames(concretewc.test)[which(colnames(concretewc.test)=="concrete.test$Water/concrete.test$Cement")] <- "wcratio"

#Tuning parameters mtry and nodesize using package ranger.
#constructing experimental grid of hyperparameters
gridwc <- expand.grid(
  mtry       = seq(1, 7, by = 1),
  node_size  = c(1,3,5,7,9, seq(10, 30, by = 5)),
  OOB_MSE   = 0
)

#Grid search
for(i in 1:nrow(gridwc)) {
  #Training model
  rf1 <- ranger(
    formula         = Strength ~ wcratio+Slag+Flyash+Superplasticizer+Coarse+Fine+Age, 
    data            = concretewc.train, 
    num.trees       = 800,
    mtry            = gridwc$mtry[i],
    min.node.size   = gridwc$node_size[i],
    seed            = 123
  )
  gridwc$OOB_MSE[i] <- rf1$prediction.error
}
gridwc %>% 
  dplyr::arrange(OOB_MSE) %>%
  head(10)

#Optimal values of tuning parameters obtained from grid search: mtry=6 and nodesize=1.
#Training final model
concretewc.rf1 <- randomForest(data=concretewc.train, Strength ~ wcratio+Slag+Flyash+Superplasticizer+Coarse+Fine+Age, 
                             importance=TRUE, ntree=800, keep.forest=TRUE, mtry=6, nodesize=1)
concretewc.rf1

#Plot of OOB error vs. number of trees 
win.graph(h=7,w=6,pointsize=12)
plot(concretewc.rf1)

#Printing out variable importance measures
importance(concretewc.rf1)
#Variable importance measures plot
win.graph(h=7,w=6,pointsize=12)
varImpPlot(concretewc.rf1, main = "Variable importance plot")

#OOB error
win.graph(h=7,w=6,pointsize=12)
concretewc.rf1$mse[800]


pred1=predict(concretewc.rf1, newdata = concretewc.train)
sMSE1 <- mean((pred1 - concretewc.train$Strength)^2)
sMSE1 

pred11=predict(concretewc.rf1, newdata = concretewc.test)
MSPE1 <- mean((pred11 - concretewc.test$Strength)^2)
MSPE1


rss1 <- sum((pred11 - concretewc.test$Strength) ^ 2)  # residual sum of squares
tss1 <- sum((concretewc.test$Strength - mean(concretewc.test$Strength)) ^ 2)  # total sum of squares
rsq1 <- 1 - rss1/tss1
rsq1  #R^2 on testing dataset

newwc <- Sys.time() - oldwc 
print(newwc)


















