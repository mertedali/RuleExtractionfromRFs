# Specify your path
setwd("C:/path/to/all/r/files")
source("listRules.r")
source("rulePredictReg.r")

# Load the required libraries
library(randomForest)
library(caret)
library(gurobi)
library(MASS)	# For Boston dataset

# Error function
rmse <- function(real, pred){
	error = pred - real
	return(sqrt(mean(error^2)))
}

# Specify hyperparametes
ntree <- c(50, 100, 150)
maxnodes <- c(10, 25, 50)
hyperparams <- expand.grid(ntree, maxnodes)

# Create a data partition: 80% for training and 20% for testing
set.seed(1)
part <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)

# Generate training and test sets using the partition given above
tr <- Boston[part, ]
test <- Boston[-part, ]

# Create data partition for 5-fold cross-validation over the training set
innercv <- createFolds(tr$medv, k = 5, list = TRUE, returnTrain = TRUE)
err_inner <- NULL

# Perform cross-validation
for (i in 1:5){

	tr_inner <- tr[innercv[[i]], ]
	test_inner <- tr[-innercv[[i]], ]
	
	for (j in 1:nrow(hyperparams)){
	
		rf <- randomForest(medv ~ ., data = tr_inner, ntree = hyperparams[j, 1], maxnodes = hyperparams[j, 2])
		pred <- predict(rf, newdata = subset(test_inner, select = -c(medv)))
		err_inner <- rbind(err_inner, c(hyperparams[j, 1], hyperparams[j, 2], rmse(test_inner$medv, pred)))
	
	}

}

# Find the most accurate hyperparameter combination
err_inner <- as.data.frame(err_inner)
err_inner$V1 <- as.factor(err_inner$V1)
err_inner$V2 <- as.factor(err_inner$V2)
err_by_hyper <- aggregate(V3 ~ V1 + V2, data = err_inner, FUN = "mean")

# Train a random forest model with the selected hyperparameter combination
rf <- randomForest(medv ~ ., data = tr,
				   ntree = as.numeric(as.character(err_by_hyper[which.min(err_by_hyper$V3), 1])),
				   maxnodes = as.numeric(as.character(err_by_hyper[which.min(err_by_hyper$V3), 2])))
				   
# Generate matrix A and cost vector c (cost)
pred_tr <- predict(rf, newdata = subset(tr, select = -c(medv)), nodes = TRUE, predict.all = TRUE)
nodeids <- attr(pred_tr, "nodes")
individual <- pred_tr$individual

A <- NULL
rulelist <- NULL
cost <- NULL
for(i in 1:rf$ntree){
	g <- getTree(rf, k = i, labelVar = TRUE)
	nofrules <- length(which(g$status == -1))
	binarym <- matrix(0, nrow = dim(tr)[1], ncol = nofrules)
	colnames(binarym) <- which(g$status == -1)
	for(j in 1:dim(tr)[1]){
		binarym[j, as.character(nodeids[j, i])] <- 1
	}
	for(k in 1:nofrules){
		cost <- c(cost, rmse(tr[which(binarym[, k] == 1), "medv"], individual[which(binarym[, k] == 1), i]))
	}
	binarym <- replace(binarym, which(binarym == -1), 1)
	colnames(g) <- sapply(colnames(g), collapse)
	rulelist <- append(rulelist, getRules(g))
	A <- cbind(A, binarym)
}

# Solve the optimization problem
model <- list()
model$A          <- A
# model$obj		 <- cost					# Equation 1
model$obj		 <- 1 + cost / max(cost)	# Equation 5
model$modelsense <- "min"
model$rhs        <- rep(1, dim(tr)[1])		
model$sense      <- rep("=", dim(tr)[1])	
model$vtype      <- rep("B", ncol(A))		
params <- list(TimeLimit = 1200)

t1 <- proc.time()
result <- gurobi(model, params)
t2 <- proc.time()

print(result$objval)

# Extract rules and display them
extruleset <- rulelist[which(result$x >= 1 - 1e-05)]
extruleset

# Use the extracted rules as a classifier
pred_rules <- rulePredictReg(test, extruleset)
pred_rules$labels		# Predicted labels
pred_rules$fr_missed	# Fraction of the missed test instances	
