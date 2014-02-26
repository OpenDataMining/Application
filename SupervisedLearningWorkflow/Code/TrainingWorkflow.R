TrainingWorkflow = function (TrainTestMode, TrainingDataFile, Delimiter, InstIdx, LabelIdx)

#########################################################################################################################
# Step 1 - Read in data file and load instances (label (training mode only) and features) into variables.
#########################################################################################################################

source("C:\\Users\\bhaumic\\Documents\\GitHub\\Application\\SupervisedLearningWorkflow\\Code\\Step1_ReadData.R");
instances = ReadData (trainTestMode = "Train", 
			    instanceDataFile = "C:\\Users\\bhaumic\\Documents\\GitHub\\Application\\SupervisedLearningWorkflow\\Data\\train.csv", 
			    sep = ",", 
			    hasHeader = TRUE, 
			    instIdx = 1, 
			    labelIdx = 2,
			    categoricalAttrs = c(1:9));

#############################################################
# Step 2 - Perform pre-processing to generate feature vector.
#############################################################
source("C:\\Users\\bhaumic\\Documents\\GitHub\\Application\\SupervisedLearningWorkflow\\Code\\Step2_GenerateFeatures.R");
instances = GenerateFeatures (trainTestMode = "Train",
					instances = instances,
					featureSetFile = "");

#############################################################
# Step 3 - Perform model selection using cross-validation.
#############################################################

