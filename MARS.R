#####################################################################
#MARS
#####################################################################
library(caret)
old <- Sys.time() # starting time

concrete <- read.table("Concrete_Data.csv", header=TRUE, sep=",", na.strings=" ")

#The column headers were changed.
names(concrete) <- c("Cement", "Slag", "Flyash", "Water", "Superplasticizer", "Coarse", "Fine", "Age", "Strength")

#Removing the duplicated rows
concrete <- concrete[!duplicated(concrete),]

#for reproducibility
set.seed(123)
#Splitting the dataset into training and test dataset.
U <- runif(n=nrow(concrete))
concrete.set <- ifelse(U <= 0.75, yes=1, no=2)

concrete.train = concrete[concrete.set==1,]
concrete.test = concrete[concrete.set==2,]

#constructing experimental grid of hyperparameters
hyper_grid <- expand.grid(
  degree  = 1:8, 
  nprune = seq(2, 50, length.out=14) %>% floor()
)
hyper_grid

#Grid search
mars_tune <- train(
  x = concrete.train[,-9],
  y = concrete.train$Strength,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyper_grid,
  nk=70
)

#best model
mars_tune$bestTune

#Default value of nk was 21 and it acted as the stopping criteria for foward pass. Its value was increased so as to prevent it from stopping early.
#optimal hyperparameter values: nprune=38, degree=5

#Fitting final MARS model
concrete.earth <- earth(Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age, data=concrete.train, trace=3, degree=5, nprune = 38, nk=70) 
summary(concrete.earth)
plot(concrete.earth)

pred=predict(concrete.earth, newdata = concrete.train)
sMSE <- mean((pred - concrete.train$Strength)^2)
sMSE
pred1=predict(concrete.earth, newdata = concrete.test)
MSPE1 <- mean((pred1 - concrete.test$Strength)^2)
MSPE1
rss <- sum((pred1 - concrete.test$Strength) ^ 2)  ## residual sum of squares
tss <- sum((concrete.test$Strength - mean(concrete.test$Strength)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq #R^2 on testing dataset



#Variable importance plot using "nsubsets", "gcv", and "rss"  criteria
win.graph(h=7, w=6, pointsize=12)
ev <- evimp(concrete.earth, trim=FALSE)
plot(ev)
#printing variable impoertance measures
print(ev)

# print elapsed time
new <- Sys.time() - old 
print(new)

#####################################################################
#MARS with 'Cement' and 'Water' dropped and water-cement ratio added
####################################################################
oldwc <- Sys.time()

concretewc.train = cbind(concrete.train$Water/concrete.train$Cement, concrete.train[,c(-1,-4)])
concretewc.test = cbind(concrete.test$Water/concrete.test$Cement,concrete.test[,c(-1,-4)])

#Changing the column header of column containing water by cement ratio values
colnames(concretewc.train)[which(colnames(concretewc.train)=="concrete.train$Water/concrete.train$Cement")] <- "wcratio"
colnames(concretewc.test)[which(colnames(concretewc.test)=="concrete.test$Water/concrete.test$Cement")] <- "wcratio"

set.seed=123
#Creating a tuning grid
hyper_gridwc <- expand.grid(
  degree = 1:8, 
  nprune = seq(2, 50, length.out = 14) %>% floor()
)

#Grid search
tuned_marswc <- train(
  x = concretewc.train[,-8],
  y = concretewc.train$Strength,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyper_gridwc,
  nk=70
)

tuned_marswc$bestTune
#optimal hyperparameter values: nprune=38, degree=5

#Fitting MARS model
concretewc.earth <- earth(Strength ~ wcratio+Slag+Flyash+Superplasticizer+Coarse+Fine+Age, data=concretewc.train, trace=3, nk=70, degree=4, nprune = 35) 
summary(concretewc.earth)
plot(concretewc.earth)

predwc=predict(concretewc.earth, newdata = concretewc.train)
sMSEwc <- mean((predwc - concretewc.train$Strength)^2)
sMSEwc
pred1wc=predict(concretewc.earth, newdata = concretewc.test)
MSPE1wc <- mean((pred1wc - concretewc.test$Strength)^2)
MSPE1wc
rsswc <- sum((pred1wc - concretewc.test$Strength) ^ 2)  ## residual sum of squares
tsswc <- sum((concretewc.test$Strength - mean(concretewc.test$Strength)) ^ 2)  ## total sum of squares
rsqwc <- 1 - rsswc/tsswc
rsqwc



#Variable importance plot using "nsubsets", "gcv", and "rss"  criteria
win.graph(h=7, w=6, pointsize=12)
evwc <- evimp(concretewc.earth, trim=FALSE)
plot(evwc)
print(evwc)

# print elapsed time
newwc <- Sys.time() - oldwc # calculate difference
print(newwc)