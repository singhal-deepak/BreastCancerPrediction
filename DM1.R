csv <- read.csv("~yourdir/trainX.csv", stringsAsFactors = FALSE, header= F)
colnames(csv) <- c("Radius_mean", "Texture_mean","Perimeter_mean","Area_mean","Smoothness_mean","Compactness_mean",
                   "Concavity_mean","Number of concave portions of contour_mean",
                   "Symmetry_mean","Fractal dimension_mean",
                   "Radius_stdev", "Texture_stdev","Perimeter_stdev","Area_stdev","Smoothness_stdev","Compactness_stdev",
                   "Concavity_stdev","Number of concave portions of contour_stdev",
                   "Symmetry_stdev","Fractal dimension_stdev",
                   "Radius_max", "Texture_max","Perimeter_max","Area_max","Smoothness_max","Compactness_max",
                   "Concavity_max","Number of concave portions of contour_max",
                   "Symmetry_max","Fractal dimension_max")
solution <- read.csv("~yourdir/trainY.csv", stringsAsFactors = FALSE, header= F)
colnames(solution) <- c("Target_Variable_Binary")
data = data.frame(csv, solution)
# a)
#clean data , remove null entries
# we are not removing outliers at this point, as decision tree is not sesnsitive to outliers
notnulldata <- na.omit(data)
# we are also converting binary results into categorical values ("YES", "NO")
notnulldata$Target_Variable <- ifelse(data$Target_Variable_Binary == 1, "Yes", "No")
notnulldata = subset(notnulldata, select = -c(Target_Variable_Binary))
Treemodel = rpart(Target_Variable ~ . , data = notnulldata, )
rpart.plot(Treemodel)
# b)
printcp(Treemodel)
# IMPORTANT : we are attaching our treemodel, in case in your treemodel turns out to be different from us. Please find it in the attached
# pdf as "RplotInitial"
# Number.of.concave.portions.of.contour_mean
# Perimeter_max
# Texture_max
# as these variables are in top nodes of our Tree, thus we concluded them as the primary independent variables
# c)
print(Treemodel)
#Here we have 5 terminal nodes and in each has 3 values( yes/no, confidence of the class, error rate).
#Any node with high confidence, support and lower error is considered as the strong rule.
# Thus, we have concluded two strongest rule.
# IMP: please take reference from attached decision tree
# Scenario First-
#when "number of concave portions of contour mean" is less than 0.056
# Perimeter_max>=107 and Texture_max>=20 -> Decision is "Yes"
# here, Support is - 23/455 = 0.05
# Confidence of getting a yes is 26% and error is 5%.
# Scenario Second-
# Number of concave.portions.of.contour_mean>=0.05592
# Texture_max< 21 -> Decision is "No"
#Support - 14/455 = 0.031 , Confidence is 29% and error is 3%
# d) What is the accuracy of your decision tree model on the training data? What is the accuracy
# of this model on the test data?
predict_unseen = predict(Treemodel, notnulldata, type = "class")
mean(notnulldata$Target_Variable == predict_unseen)
# accuracy on train data = 96.04%
# accuracy on test data is below
# on test data
csvtest <- read.csv("~yourdir/testX.csv", stringsAsFactors = FALSE, header= F)
colnames(csvtest) <- c("Radius_mean", "Texture_mean","Perimeter_mean","Area_mean","Smoothness_mean","Compactness_mean",
                       "Concavity_mean","Number of concave portions of contour_mean",
                       "Symmetry_mean","Fractal dimension_mean",
                       "Radius_stdev", "Texture_stdev","Perimeter_stdev","Area_stdev","Smoothness_stdev","Compactness_stdev",
                       "Concavity_stdev","Number of concave portions of contour_stdev",
                       "Symmetry_stdev","Fractal dimension_stdev",
                       "Radius_max", "Texture_max","Perimeter_max","Area_max","Smoothness_max","Compactness_max",
                       "Concavity_max","Number of concave portions of contour_max",
                       "Symmetry_max","Fractal dimension_max")
solutiontest <- read.csv("~yourdir/testY.csv", stringsAsFactors = FALSE, header= F)
colnames(solutiontest) <- c("Target_Variable_Binary")
datatest = data.frame(csvtest, solutiontest)
#clean data , remove null entries
notnulldatatest <- na.omit(datatest)
notnulldatatest$Target_Variable <- ifelse(notnulldatatest$Target_Variable_Binary == 1, "Yes", "No")
notnulldatatest = subset(notnulldatatest, select = -c(Target_Variable_Binary))
predict_unseentest = predict(Treemodel, notnulldatatest, type = "class")
mean(notnulldatatest$Target_Variable == predict_unseentest)
# accuracy on test data = 84.21%
# e) Is it possible to improve the performance of your model?
# Ans: Validation of decision tree using the ‘Complexity Parameter’ and cross validated error :
# To validate the model we use the printcp and plotcp functions. ‘CP’ stands for Complexity Parameter of the tree.
# This function provides the optimal prunings based on the cp value.
# This function provides the optimal prunings based on the cp value.
# We prune the tree to avoid any overfitting of the data. The convention is to have a small tree and the one
# with least cross validated error given by printcp() function i.e. ‘xerror’.
# We prune the tree to avoid any overfitting of the data. The convention is to have a small tree and the one with least
# cross validated error given by printcp() function i.e. ‘xerror’.
printcp(Treemodel)
# as per above command, "min xerror" is at 4th node, which is the optimal node in our case,
# but if we want to further increase our accuracy from 84% (test data), and to do that, we will decrease the cp value
# which will decrease the error in the model
# f) Construct the best possible decision tree to predict the Y labels. Explain how you construct
# such tree.
# we are choosing cp as 0.01 as its optimal cp value from "printcp(Treemodel)"
Treemodel_revised = rpart(Target_Variable ~ . , data = notnulldata, parms = list(split = "information"),
                          control = rpart.control(minsplit = 0, minbucket = 0, cp = 0.01))
printcp(Treemodel_revised)
print(Treemodel_revised)
predict_revised = predict(Treemodel_revised, notnulldata, type = "class")
mean(notnulldata$Target_Variable == predict_revised)
# IMPORTANT : we are attaching our treemodel, in case in your treemodel turns out to be different from us. Please find it in the attached
# pdf as "RplotRevised"
# accuracy on train data = 98.9%
predict_revised_test = predict(Treemodel_revised, notnulldatatest, type = "class")
mean(notnulldatatest$Target_Variable == predict_revised_test)
# accuracy on test data = 92.98%
# g) Plot your final decision tree model.
rpart.plot(Treemodel_revised)
Number.of.concave.portions.of.contour_mean < 0.056
Perimeter_max < 107
Texture_max < 20
Texture_max < 21
