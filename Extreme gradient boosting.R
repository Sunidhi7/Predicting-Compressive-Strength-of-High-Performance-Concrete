####################################
#eXtreme Gradient Boosting
####################################
library(xgboost)
library(dplyr)

#getting start time
old <- Sys.time() 

concrete <- read.table("Concrete_Data.csv", header=TRUE, sep=",", na.strings=" ")

#The column headers were changed.
names(concrete) <- c("Cement", "Slag", "Flyash", "Water", "Superplasticizer", "Coarse", "Fine", "Age", "Strength")
#Removing the duplicate rows
concrete <- concrete[!duplicated(concrete),]

#for reproducibility
set.seed(123)
#Splitting the dataset into training and test dataset.
U <- runif(n=nrow(concrete))
concrete.set <- ifelse(U <= 0.75, yes=1, no=2)

concrete.train = concrete[concrete.set==1,]
concrete.test = concrete[concrete.set==2,]

#constructing experimental grid of hyperparameters
grid <- expand.grid(
  eta = c(.1, .3, .5),
  max_depth = c(1, 3, 5, 7, 9),
  subsample = c(.60, .75, 1),
  optimal_trees = 0,              
  min_RMSE = 0                     
)
#Grid search
for(i in 1:nrow(grid)) {
  
#creating list of parameters
  params <- list(
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    subsample = grid$subsample[i]
    )
  set.seed(123)
  xgb.tune <- xgb.cv(
    params = params,
    data = as.matrix(concrete.train[,-9]),
    nrounds = 1000,
    nfold = 5,
    label= concrete.train$Strength,
    objective = "reg:linear"
  )
  grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}
    
grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)

new <- Sys.time() - old # calculate difference
print(new)

#Optimal values of hyperparameters: max_depth=3, eta=.1, subsample=0.60, nrounds=862

#Final model
concrete.xgb <- xgboost(data=as.matrix(concrete.train[,-9]), label=concrete.train$Strength, 
                   max_depth=3, eta=.1, subsample=0.60,
                   nrounds=862, objective="reg:linear")
concrete.xgb

#Variable importance plot
xgb.imp <- xgb.importance(model=concrete.xgb)
xgb.plot.importance(xgb.imp)

xgb.pred.test = predict(concrete.xgb, newdata=as.matrix(concrete.test[,-9]))
xgb.MSPE <- mean((xgb.pred.test - concrete.test$Strength)^2)
xgb.MSPE

xgb.pred.train=predict(concrete.xgb, newdata = as.matrix(concrete.train[,-9]))
xgb.sMSE <- mean((xgb.pred.train - concrete.train$Strength)^2)
xgb.sMSE

# residual sum of squares
rss1 <- sum((xgb.pred.test - concrete.test$Strength ) ^ 2) 
# total sum of squares
tss1 <- sum((concrete.test$Strength - mean(concrete.test$Strength)) ^ 2) 
rsq1 <- 1 - rss1/tss1
rsq1

#Plotting predicted value vs actual value of strength
win.graph(h=7,w=6,pointsize=12)
pred<- predict(concrete.xgb, newdata=as.matrix(concrete[,-9]))
plot(x=concrete$Strength, y= pred, xlab="Actual strength(MPa)", ylab="Predicted strength(MPa)", main="XGBOOST on dataset 1")
abline(lm(pred~concrete$Strength))
#########################################################################################
#eXtreme Gradient Boosting with 'Cement' and 'Water' dropped and water-cement ratio added
########################################################################################
oldwc <- Sys.time()
concretewc.train = cbind(concrete.train$Water/concrete.train$Cement, concrete.train[,c(-1,-4)])
concretewc.test = cbind(concrete.test$Water/concrete.test$Cement,concrete.test[,c(-1,-4)])

#Changing the column header of column containing water by cement ratio values
colnames(concretewc.train)[which(colnames(concretewc.train)=="concrete.train$Water/concrete.train$Cement")] <- "wcratio"
colnames(concretewc.test)[which(colnames(concretewc.test)=="concrete.test$Water/concrete.test$Cement")] <- "wcratio"

#constructing experimental grid of hyperparameters
gridwc <- expand.grid(
  eta = c(.1, .2, .3, .5),
  max_depth = c(1, 3, 5, 7, 9),
  subsample = c(.60, .75, 0.85, 1),
  optimal_trees = 0,               
  min_RMSE = 0                    
)

for(i in 1:nrow(gridwc)) {
  
  #creating parameter list
  params <- list(
    eta = gridwc$eta[i],
    max_depth = gridwc$max_depth[i],
    subsample = gridwc$subsample[i]
  )
  set.seed(123)
  xgb.tunewc <- xgb.cv(
    params = params,
    data = as.matrix(concretewc.train[,-8]),
    label= concretewc.train$Strength,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear"
  )
  gridwc$optimal_trees[i] <- which.min(xgb.tunewc$evaluation_log$test_rmse_mean)
  gridwc$min_RMSE[i] <- min(xgb.tunewc$evaluation_log$test_rmse_mean)
}

gridwc %>%
  dplyr::arrange(min_RMSE) %>%
  head(36)

newwc <- Sys.time() - oldwc # calculate difference
print(newwc)

#Optimal values of hyperparameters: max_depth=3, eta=.1, subsample=0.75, nrounds=880
concrete.xgbwc <- xgboost(data=as.matrix(concretewc.train[,-8]), label=concretewc.train$Strength, 
                        max_depth=3, eta=.1, subsample=0.75,
                        nrounds=880, objective="reg:linear")
concrete.xgbwc
xgbwc.imp <- xgb.importance(model=concrete.xgbwc)
xgb.plot.importance(xgbwc.imp)

xgbwc.pred.test = predict(concrete.xgbwc, newdata=as.matrix(concretewc.test[,-8]))
xgbwc.MSPE <- mean((xgbwc.pred.test - concretewc.test$Strength)^2)
xgbwc.MSPE

xgbwc.pred.train=predict(concrete.xgbwc, newdata = as.matrix(concretewc.train[,-8]))
xgbwc.sMSE <- mean((xgbwc.pred.train - concretewc.train$Strength)^2)
xgbwc.sMSE

rss1wc <- sum((xgbwc.pred.test - concretewc.test$Strength ) ^ 2)  # residual sum of squares
tss1wc <- sum((concretewc.test$Strength - mean(concretewc.test$Strength)) ^ 2)  # total sum of squares
rsq1wc <- 1 - rss1wc/tss1wc
rsq1wc

