# Specify your path
setwd("C:/path/to/all/r/files")
source("listRules.r")
source("rulePredictClass.r")

# Macro F1 Score
m_f1 <- function(real, pred){
	m <- table(pred, real)
	f1 <- NULL
	prec <- NULL
	recall <- NULL
	for(i in 1:ncol(m)){
		prec <- c(prec, m[i, i] / sum(m[i, ]))
		recall <- c(recall, m[i, i] / sum(m[, i]))
		f1 <- c(f1, (2 * prec[i] * recall[i]) / (prec[i] + recall[i]))
	}
	f1[is.nan(f1)] <- 0
	mf1 <- mean(f1)
	return(mf1)
}

# Load the required libraries
library(randomForest)
library(caret)
library(gurobi)

# Specify hyperparametes
ntree <- c(50, 100, 150)
maxnodes <- c(10, 25, 50)
hyperparams <- expand.grid(ntree, maxnodes)

# Create a data partition: 80% for training and 20% for testing
set.seed(1)
part <- createDataPartition(iris$Species, p = 0.8, list = FALSE)

# Generate training and test sets using the partition given above
tr <- iris[part, ]
test <- iris[-part, ]

# Create data partition for 5-fold cross-validation over the training set
innercv <- createFolds(tr$Species, k = 5, list = TRUE, returnTrain = TRUE)
m_f1_inner <- NULL

# Perform cross-validation
for (i in 1:5){

	tr_inner <- tr[innercv[[i]], ]
	test_inner <- tr[-innercv[[i]], ]
	
	for (j in 1:nrow(hyperparams)){
	
		rf <- randomForest(Species ~ ., data = tr_inner, ntree = hyperparams[j, 1], maxnodes = hyperparams[j, 2])
		pred <- predict(rf, newdata = subset(test_inner, select = -c(Species)))
		m_f1_inner <- rbind(m_f1_inner, c(hyperparams[j, 1], hyperparams[j, 2], m_f1(test_inner$Species, pred)))
	
	}

}

# Find the most accurate hyperparameter combination
m_f1_inner <- as.data.frame(m_f1_inner)
m_f1_inner$V1 <- as.factor(m_f1_inner$V1)
m_f1_inner$V2 <- as.factor(m_f1_inner$V2)
m_f1_by_hyper <- aggregate(V3 ~ V1 + V2, data = m_f1_inner, FUN = "mean")

# Train a random forest model with the selected hyperparameter combination
rf <- randomForest(Species ~ ., data = tr,
				   ntree = as.numeric(as.character(m_f1_by_hyper[which.max(m_f1_by_hyper$V3), 1])),
				   maxnodes = as.numeric(as.character(m_f1_by_hyper[which.max(m_f1_by_hyper$V3), 2])))

# Generate matrix A and cost vector c (cost)
pred_tr <- predict(rf, newdata = subset(tr, select = -c(Species)), nodes = TRUE, predict.all = TRUE)
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
		if (individual[j, i] == tr$Species[j]){
			binarym[j, as.character(nodeids[j, i])] <- 1
		} else {
		binarym[j, as.character(nodeids[j, i])] <- -1
		}
	}
	for(k in 1:nofrules){
		cost <- c(cost, 1 - length(which(binarym[, k] == 1)) / length(which(binarym[, k] != 0)))
	}
	binarym <- replace(binarym, which(binarym == -1), 1)
	colnames(g) <- sapply(colnames(g), collapse)
	rulelist <- append(rulelist, getRules(g))
	A <- cbind(A, binarym)
}

# Solve the optimization problem
model <- list()
model$A          <- A
# model$obj		 <- cost		# Equation 1
model$obj		 <- 1 + cost	# Equation 4
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
pred_rules <- rulePredictClass(test, extruleset)
pred_rules$labels		# Predicted labels
pred_rules$fr_missed	# Fraction of the missed test instances	
