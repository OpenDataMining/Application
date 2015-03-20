
PredictionWorkflow = function (
                              trainTestMode, 
                              instanceDataFile, 
                              sep,
                              hasHeader,
                              instIdx,
                              labelIdx,
                              categoricalAttrs,
                              meanVarianceFile,
                              featureSetFile,
                              modelFile,
                              predictionFile
                            )
{
  #########################################################################################################################
  # Step 0 - Load all the required packages.
  #########################################################################################################################
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Step0_LoadPackages.R");
  LoadPackages();
  
  #########################################################################################################################
  # Step 1 - Read in data file and load instances into variables.
  #########################################################################################################################
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Step1_ReadData.R");
  instances = ReadData (trainTestMode = "Test", 
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
  instances = GenerateFeatures (trainTestMode = "Test",
                                instances = instances,
                                meanVarianceFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Data\\FeatureMeanVariance_BankTeleMarketing.RData",
                                featureSetFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Data\\FeatureSet_BankTeleMarketing.RData");
  
  end.time = Sys.time();
  time.taken = end.time - start.time;
  print(time.taken);
  
  #############################################################
  # Step 3 - Perform model selection using cross-validation.
  #############################################################
  source("C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Code\\Prediction_Step3_Predict.R");
  
  testPredictions = Predict (instances = instances,
                             modelFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Model\\BankTeleMarketing_Boosting.RData",
                             predictionFile = "C:\\Users\\Bhaumik\\Documents\\GitHub\\OpenDataMining\\Application\\SupervisedLearningWorkflow\\Data\\BankTeleMarketing_Output.txt");
}