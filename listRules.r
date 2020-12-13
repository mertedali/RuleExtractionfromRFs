# These codes are taken from the link given below:
# https://stats.stackexchange.com/questions/41443/how-to-actually-plot-a-sample-tree-from-randomforestgettree

###################################
#### Return the rules of a tree ###
###################################

getRules <- function(tree){
	# store all rules into a list
	rules <- list()
	# start by the terminal nodes and find previous conditions
	id.leafs <- which(tree$status == -1)
	j <- 0
	for(i in id.leafs){
		j <- j + 1
		prevConds <- prevCond(tree,i)
		rules[[j]] <- prevConds$cond
		while(prevConds$id > 1){
			prevConds <- prevCond(tree, prevConds$id)
			rules[[j]] <- paste(rules[[j]], "&", prevConds$cond)
        }
		if(prevConds$id == 1){
			rules[[j]] <- paste(rules[[j]], "=>", tree$prediction[i])
		}
    }
	return(rules)
}

################################################
### Find the previous conditions in the tree ###
################################################

prevCond <- function(tree, i){
	if(i %in% tree$right_daughter){
		id <- which(tree$right_daughter == i)
		cond <- paste(tree$split_var[id], ">", tree$split_point[id])
	}
	if(i %in% tree$left_daughter){
		id <- which(tree$left_daughter == i)
		cond <- paste(tree$split_var[id], "<=", tree$split_point[id])
	}
	return(list(cond = cond, id = id))
}

###############################
### Remove spaces in a word ###
###############################

collapse <- function(x){
	x <- sub(" ", "_", x)
	return(x)
}
