TrainingWorkflow = function (
                            trainTestMode, 
                            instanceDataFile, 
                            sep, 
                            hasHeader,
                            instIdx, 
                            labelIdx,
                            categoricalAttrs,
                            meanVarianceFile,
                            featureSetFile,
                            modelFile
                            )
{
  #########################################################################################################################
  # Step 0 - Load all the required packages.
  #########################################################################################################################
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Step0_LoadPackages.R");
  LoadPackages();
  
  #########################################################################################################################
  # Step 1 - Read in data file and load instances (label (training mode only) and features) into variables.
  #########################################################################################################################  
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Step1_ReadData.R");
  instances = ReadData (trainTestMode = "Train", 
                        instanceDataFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\BankTeleMarketing\\data\\bank-additional-full.csv",
                        sep = ";", 
                        hasHeader = TRUE, 
                        instIdx = -1, 
                        labelIdx = 21,
                        categoricalAttrs = c(2:10, 15));
  
  #############################################################
  # Step 2 - Perform pre-processing to generate feature vector.
  #############################################################
  start.time = Sys.time();
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Step2_GenerateFeatures.R");
  instances = GenerateFeatures (trainTestMode = "Train",
                                instances = instances,
                                meanVarianceFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Data\\FeatureMeanVariance_BankTeleMarketing.RData",
                                featureSetFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Data\\FeatureSet_BankTeleMarketing.RData");
  
  end.time = Sys.time();
  time.taken = end.time - start.time;
  print(time.taken);
  
  #############################################################
  # Step 3 - Perform model selection using cross-validation.
  #############################################################
  start.time = Sys.time();
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Training_Step3_SelectModel.R");
  
  parameterGrid = expand.grid(interaction.depth = c(10),
                              n.trees = c(20, 30, 40),
                              shrinkage = c(0.05));
  
  model = Training_SelectModel (instances = instances,
                                metric = "ROC",
                                learningAlgo = "gbm", # Use algorithm from http://caret.r-forge.r-project.org/bytag.html
                                parameterGrid = parameterGrid,
                                modelFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Model\\BankTeleMarketing_Boosting.RData");
  
  end.time = Sys.time();
  time.taken = end.time - start.time;
  print(time.taken);
}