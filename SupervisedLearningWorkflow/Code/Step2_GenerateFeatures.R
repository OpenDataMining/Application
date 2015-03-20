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
	if(trainTestMode == "Train")
	{
		labels = instances$labels;
	}
	featureVector = instances$featureVector;

	featureMean = hash();
	featureStdDev = hash();

	rm(instances);
  gc();

	############################################################################
	# Feature Transformations
	############################################################################

	if(trainTestMode == "Train")
	{
		# Remove zero variance and near zero variance features
		zeroVarFeatures = c();
	
		for (i in 1:length(colnames(featureVector)))
		{
			nzv = nearZeroVar (featureVector[, i], freqCut = 100, uniqueCut = 0.1);
		
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
    gc();
		print("Near zero variance feature removal completed");		
		print(length(colnames(featureVector)));

		# Mean normalization and feature scaling
		tempFeatureVector = Matrix();
		for (i in 1:length(colnames(featureVector)))
		{
			iFeature = matrix(featureVector[, i]);

			#.set(featureMean, keys = colnames(featureVector)[i], values = mean(iFeature));
			#.set(featureStdDev, keys = colnames(featureVector)[i], values = sd(iFeature));
			featureMean[[colnames(featureVector)[i]]] = mean(iFeature);
			featureStdDev[[colnames(featureVector)[i]]] = sd(iFeature);
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
  gc();
  
	############################################################################
	# Feature Selection
	############################################################################

	if(trainTestMode == "Train")
	{
		tempFeatureVector = matrix();

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

		tempFeatureMeanHash = featureMean;
		tempFeatureStdDevHash = featureStdDev;

		featureMean = hash();
		featureStdDev = hash();
	
		for(col in colnames(featureVector))
		{
		  #.set(featureMean, keys = col, values = tempFeatureMeanHash[[col]]);
		  #.set(featureStdDev, keys = col, values = tempFeatureStdDevHash[[col]]);
		  featureMean[[col]] = tempFeatureMeanHash[[col]];
		  featureStdDev[[col]] = tempFeatureStdDevHash[[col]];
		}
		rm(tempFeatureMeanHash);
		rm(tempFeatureStdDevHash);
	}
	else if (trainTestMode == "Test")
	{
		load(meanVarianceFile);

		tempFeatureVector = matrix();

		# Keep features selected during training phase
		for(i in 1:length(colnames(featureVector)))
		{
			iFeature = matrix(featureVector[, i]);
			colnames(iFeature) = c(colnames(featureVector)[i]);

			if(has.key(colnames(featureVector)[i], featureMean))
			{
				iFeature = iFeature - featureMean[[colnames(featureVector)[i]]];
				iFeature = iFeature / featureStdDev[[colnames(featureVector)[i]]];

				if (length(colnames(tempFeatureVector)) > 0)
				{
					tempFeatureVector = cbind(tempFeatureVector, iFeature); # Add columns				
				}
				else
				{
					tempFeatureVector = iFeature;
				}
			}
		}

		rm(featureVector);
		featureVector = tempFeatureVector;
		rm(tempFeatureVector);
	}
  gc();
  
	############################################################################
	# Missing Value Imputation
	############################################################################

	
	############################################################################
	# Write 1) Final set of features and 2) Mean and Standard Deviation to files during training
	############################################################################	
	featureSet = colnames(featureVector);
	save(featureSet, file = featureSetFile);

	if(trainTestMode == "Train")
	{
		save(featureMean, featureStdDev, file = meanVarianceFile);
	}

	# Create a list of InstanceIDs, Labels and FeatureVector
	if(trainTestMode == "Train")
	{
		instances = list(instanceIDs = instanceIDs, labels = labels, featureVector = featureVector);
	}
	else
	{
		instances = list(instanceIDs = instanceIDs, featureVector = featureVector);

	}
	return (instances);
}