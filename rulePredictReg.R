##################################################################################
### Perform prediction with a given dataset and extracted rules for regression ###
##################################################################################

rulePredictReg <- function(dataset, rules){
    rule_pred_labels <- NULL
    nofinputs <- ncol(dataset) - 1
	missed <- 0
    for(i in 1:nrow(dataset)){
        input_vals <- dataset[i, 1:nofinputs]	# Input (feature) values
        pred_labels <- NULL						# Predicted labels
        rules_avg_trues <- numeric(length(rules))
        for(j in 1:length(rules)){
            rule <- as.character(rules[j])
            unl <- unlist(strsplit(rule, " => ")) 	# Extract the predicted label
            pred_label <- as.numeric(unl[2]) 		# Extract the conditions
            conditions <- unlist(strsplit(unl[1], "&"))
            evl <- NULL
            for(k in 1:length(conditions)){
                evl <- cbind(evl, eval(parse(text = paste("input_vals$", conditions[k]))))
            }
            rules_avg_trues[j] <- sum(evl) / length(conditions)
            pred_labels <- c(pred_labels, pred_label)
        }
        candidate_rules <- which(abs(max(rules_avg_trues) - rules_avg_trues) <= 1.0e-6)
		
		if(all(rules_avg_trues < 1)){
			missed <- missed + 1
		}
        
        rule_pred_labels <- c(rule_pred_labels, mean(pred_labels[candidate_rules]))
    }
    return(list("labels" = rule_pred_labels, "fr_missed" = missed / nrow(dataset)))
}
