Predict = function (instances = c(),
			  modelFile = "",
        predictionFile = ""
		)

{
	load(modelFile);
	testPred = predict(model, 
                     newdata = instances$featureVector);
  
  write.table(testPred, file = predictionFile, sep = "\t", quote = FALSE)
  
	return (testPred);
}