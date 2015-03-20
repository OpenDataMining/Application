Training_SelectModel = function (instances = c(),
				metric = "",
				learningAlgo = "",
				parameterGrid = data.frame(),
				modelName = "",
				modelFile = ""
				)

##################################################################################
# This function selects the best-performing model (parameter set for a learning algorithm).
# 
# Output:
# Best performing parameter set
##################################################################################

{
	#op <- options(warn = (-1));
		
	instanceIDs = instances$instanceIDs;
	labels = factor(instances$labels);
	featureVector = instances$featureVector;

	# Specify re-sampling such as K-fold cross-validation etc.
	fitControl = trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 5,
                           ## repeated ten times
                           repeats = 1,
				                   classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary,
				                   allowParallel = TRUE,
                           returnData = FALSE);

	# Call train function to identify the best parameter set
	model = train (featureVector,
			    labels,
			    method = learningAlgo,
			    trControl = fitControl,
			    tuneGrid = parameterGrid,
			    metric = metric);

  # Compute variable importance
  varImp = varImp(model);
  
	# Write model to a file
	save(model, varImp, file = modelFile);

	#options(op);
	return (model);	
}