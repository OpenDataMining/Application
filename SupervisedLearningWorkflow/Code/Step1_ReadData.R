ReadData = function (trainTestMode = "Train", 
			   instanceDataFile, 
			   sep = "\t", 
			   hasHeader = FALSE, 
			   instIdx = -1, 
			   labelIdx = -1,
			   categoricalAttrs = c())

##################################################################################
# This function reads in data file and loads instances (label (training mode only) 
# and features) into variables.
##################################################################################

{
	# Read data into variable	
	instanceData = read.table (instanceDataFile, header = hasHeader, sep = sep);

	# Output Variables
	instanceIDs = c();
	labels = c();
	featureVector = data.frame();

	# List of columns to be excluded from instance data, to generate feature vector	
	excludeCols = c();

	if (instIdx > 0)
	{
		instanceIDs = instanceData[, instIdx];
		excludeCols = instIdx;
	}

	# Input data contains labels (training mode only)
	if (labelIdx > 0)
	{
		labels = instanceData[, labelIdx];
		excludeCols = c(excludeCols, labelIdx);
	}

	# Populate featureVector variable
	if (length(excludeCols) > 0)
	{
		tempFeatureVector = instanceData[, -excludeCols];
	}
	else
	{
		tempFeatureVector = instanceData;
	}

	rm(instanceData);
	print(excludeCols);

	# Convert categorical features into numerical features
	if (length(categoricalAttrs) > 0)
	{		
		library (Matrix);
		
		for (i in 1:length(names(tempFeatureVector)))
		{
			if (!(i %in% categoricalAttrs))
			{
				dummies = model.matrix(as.formula(paste("~", names(tempFeatureVector)[i], "-1")), data = tempFeatureVector);
			} 
			else
			{
				tempFeatureVector[, i] = factor(tempFeatureVector[, i]);
				dummies = sparse.model.matrix(as.formula(paste("~", names(tempFeatureVector)[i], "-1")), data = tempFeatureVector);
			}
			
			if (length(colnames(featureVector)) > 0)
			{
				featureVector = cbind2(featureVector, dummies); # Add columns
			}
			else 
			{
				featureVector = dummies;
			}
		}
	}

	rm(tempFeatureVector);

	# Create a list of InstanceIDs, Labels and FeatureVector
	instances = list(instanceIDs = instanceIDs, labels = labels, featureVector = featureVector);
	return (instances);
}