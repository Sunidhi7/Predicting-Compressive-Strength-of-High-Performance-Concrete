#The original Concrete_Data.xls file was saved with .csv extension before importing it to R.
concrete <- read.table("Concrete_Data.csv", header=TRUE, sep=",", na.strings=" ")

#The column headers were changed.
names(concrete) <- c("Cement", "Slag", "Flyash", "Water", "Superplasticizer", "Coarse", "Fine", "Age", "Strength")

#Removing the duplicate rows
concrete <- concrete[!duplicated(concrete),]
head(concrete)

dim(concrete)
summary(concrete)

#Making a scatterplot matrix
x11(width=10,height=5,pointsize=18)
pairs((concrete[,1:ncol(concrete)]), pch=20)

#Correlation matrix
cor <- cor(concrete)

#Heatmap of correlation matrix
library(corrplot)
corrplot(cor, method = "color")
# Correlation of around -0.65 between superplasticizer and water.

#Scatterplot of concrete compressive strength and water to cement ratio
win.graph(h=7,w=6,pointsize=12)
wcratio <- concrete$Water/ concrete$Cement
plot(x= wcratio, y=concrete$Strength, xlab= "water to cement ratio", ylab= "Concrete compressive strength", pch=20)










