classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
file_polarity = "/Users/yingli/EVAnalysisCorp/irtm/sentiment/data/subjectivity.csv.gz"
 	lexicon <- read.csv(file_polarity,header=FALSE)

	counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
	documents <- c()

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(positive=0,negative=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
			index <- pmatch(word,lexicon[,1],nomatch=0)
			if (index > 0) {
				entry <- lexicon[index,]
				
				polarity <- as.character(entry[[2]])
				category <- as.character(entry[[3]])
				count <- counts[[category]]
	
				score <- pweak
                if (polarity == "strongsubj") score <- pstrong
				if (algorithm=="bayes") score <- abs(log(score*prior/count))
		
				if (verbose) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
				}
				
				scores[[category]] <- scores[[category]]+score
			}		
		}
		
		if (algorithm=="bayes") {
			for (key in names(scores)) {
				count <- counts[[key]]
				total <- counts[["total"]]
				score <- abs(log(count/total))
				scores[[key]] <- scores[[key]]+score
			}
		} else {
			for (key in names(scores)) {
				scores[[key]] <- scores[[key]]+0.000001
			}
		}
		
        best_fit <- names(scores)[which.max(unlist(scores))]
        ratio <- as.integer(abs(scores$positive/scores$negative))
        if (ratio==1) best_fit <- "neutral"
		documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
		if (verbose) {
			print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
			cat("\n")
		}
	}
	
	colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
	return(documents)
}