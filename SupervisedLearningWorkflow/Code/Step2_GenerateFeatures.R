GenerateFeatures = function (trainTestMode = "Train",
				     instances = c(),
				     meanVarianceFile = "",
				     featureSetFile = ""
				     )

##################################################################################
# This function performs 
# 1) feature transformations (mean normalization and feature scaling) 
# 2) feature selection (remove correlated and multi co-linearity features, select 
#    relevant features)
# 
# Output:
# labels
# featureVector
# final feature set
##################################################################################

{
	instanceIDs = instances$instanceIDs;
	labels = instances$labels;
	featureVector = instances$featureVector;

	rm(instances);

	library (caret);

	############################################################################
	# Feature Transformations
	############################################################################

	if(trainTestMode == "Train")
	{
		# Remove zero variance and near zero variance features
		zeroVarFeatures = c();
	
		for (i in 1:length(colnames(featureVector)))
		{
			nzv = nearZeroVar (featureVector[, i]);
		
			if (length(nzv) > 0)
			{
				zeroVarFeatures = c(zeroVarFeatures, i);
			}
		}

		if(length(zeroVarFeatures) > 0)
		{
			tempFeatureVector = featureVector[, -zeroVarFeatures];
			rm (featureVector);
			featureVector = tempFeatureVector;
			rm(tempFeatureVector);			
		}
		print("Near zero variance feature removal completed");		
		print(length(colnames(featureVector)));

		# Mean normalization and feature scaling
		tempFeatureVector = Matrix();
		for (i in 1:length(colnames(featureVector)))
		{
			iFeature = matrix(featureVector[, i]);

			preProcValues = preProcess(iFeature, method = c("center", "scale"));
			tempIFeature = Matrix(predict(preProcValues, iFeature));
			colnames(tempIFeature) = c(colnames(featureVector)[i]);			

			if (length(colnames(tempFeatureVector)) > 0)
			{
				tempFeatureVector = cbind2(tempFeatureVector, tempIFeature); # Add columns				
			}
			else
			{
				tempFeatureVector = tempIFeature;
			}
		}

		rm(featureVector);
		featureVector = tempFeatureVector;
		rm(tempFeatureVector);
		print("Mean normalization and feature scaling completed");
	}
	else if(trainTestMode == "Test")
	{
		
	}

	############################################################################
	# Feature Selection
	############################################################################

	if(trainTestMode == "Train")
	{
		# Remove correlated features
		for(i in 1:length(colnames(featureVector)))
		{
			iFeature = matrix(featureVector[, i]);
			colnames(iFeature) = c(colnames(featureVector)[i]);

			if (length(colnames(tempFeatureVector)) > 0)
			{
				tempFeatureVector = cbind(tempFeatureVector, iFeature); # Add columns				
			}
			else
			{
				tempFeatureVector = iFeature;
			}

		}		
		rm(featureVector);

		corFeatures = cor(tempFeatureVector);
		highlyCorFeatures = findCorrelation(corFeatures, cutoff = 0.75);
		featureVector = tempFeatureVector[, -highlyCorFeatures];		
		rm(tempFeatureVector);
		
		print("Correlated feature removal completed");
		print(length(colnames(featureVector)));

		# Remove linearly dependent features
		linearComboFeatures = findLinearCombos(featureVector);
		
		if(length(linearComboFeatures$remove) > 0)
		{
			tempFeatureVector = featureVector[, -linearComboFeatures$remove];
			rm(featureVector);
			featureVector = tempFeatureVector;
			rm(tempFeatureVector);
		}
		
		print("Linearly dependent feature removal completed");
		print(length(colnames(featureVector)));

		# Select relevant features
	}
	else if (trainTestMode == "Test")
	{
		
	}

	############################################################################
	# Missing Value Imputation
	############################################################################

	

	# Create a list of InstanceIDs, Labels and FeatureVector
	instances = list(instanceIDs = instanceIDs, labels = labels, featureVector = featureVector);
	return (instances);
}