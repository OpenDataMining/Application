require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
require(Snowball)
require(SnowballC)
require(twitteR)
require(ggplot2)
require(stringr)
require(slam)
require(sentiment)
require(JSON)
library(lattice)
library(RJSONIO)
library(data.table)
library(igraph)
library(TraMineR) 
library(dtw)
library(ggplot2) 
library(shiny)
library(PerformanceAnalytics)
library(plyr)
library(stringr)
library(lubridate)


u = "http://cran.r-project.org/web/packages/available_packages_by_date.html"
t = readHTMLTable(u)[[1]]
ap.corpus <- Corpus(DataframeSource(data.frame(as.character(t[,3]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
#png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#dev.off()


t = read.csv("/Users/yingli/EVAnalysisCorp/irtm/twitter/tweets.csv", header=TRUE)
t.corpus <- Corpus(DataframeSource(data.frame(as.character(t$text))), encoding = "UTF-8")

#tm_map(t$text, as.PlainTextDocument)
t.corpus = Corpus(VectorSource(t$text, encoding = "UTF-8"))
t.corpus = Corpus(VectorSource(t$text[1:200]))

t.corpus <- tm_map(t.corpus, removePunctuation)
t.corpus <- tm_map(t.corpus, tolower)
t.corpus <- tm_map(t.corpus, function(x) removeWords(x, stopwords("english")))
ap.corpus <- tm_map(ap.corpus, removeWords, stopwords("english"))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
quartz()
#png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(5,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#dev.off()


cranTweets <- userTimeline('cranatic')
tweets=searchTwitter('BMW', n=500,lang="en")
tweets_df = twListToDF(tweets)

tweets[[1]]$getText()
tweets[[1]]$getScreenName()
tweets[[1]]$getId()
tweets[[1]]$getCreated()
tweets[[1]]$getStatusSource()

# extract the text content of the all the tweets
sapply(tweets, function(x) x$getText())

# extract the user name of the all the tweets
sapply(tweets, function(x) x$getScreenName())

# extract the Id number of the all the tweets
sapply(tweets, function(x) x$getId())

# extract the date and time of publication of the all the tweets
sapply(tweets, function(x) x$getCreated())

# extract the source user agent of the all the tweets
sapply(tweets, function(x) x$getStatusSource())

tweets_v=sapply(tweets, function(x) x$getText())
mycorpus = Corpus(VectorSource(tweets_v))

#stripWhitespace: eliminate extra white-spaces
mycorpus1 = tm_map(mycorpus, stripWhitespace)

#tolower: convert text to lower case
mycorpus2 = tm_map(mycorpus, tolower)

#removeWords: remove words like stopwords
mycorpus3 = tm_map(mycorpus, removeWords, stopwords("english"))

#removePunctuation: remove punctuation symbols
mycorpus4 = tm_map(mycorpus, removePunctuation)

#removeNumber: remove numbers
mycorpus5 = tm_map(mycorpus, removeNumber)

tweets = searchTwitter("data mining", lang="en")


prepareWordCloud <- function(query = 'BMW', num_tweets = 50, lang = 'en')
{
  tweets=searchTwitter(query, n=num_tweets,lang)
  tweets_df = twListToDF(tweets)
  text_clean = preProcessing(tweets_df$text)
  # lower case using tryTolower with sapply 
  text_lower = sapply(text_clean, tryTolower)
  # remove NAs in some_txt
  text_noNA = text_lower[!is.na(text_lower)]
  names(text_noNA) = NULL
  
  mycorpus = Corpus(VectorSource(text_noNA))
  x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
  z.tdm=TermDocumentMatrix(x)
  ap.m <- as.matrix(z.tdm)
  ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v),freq=ap.v)
  #table(ap.d$freq)
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(ap.d$word,ap.d$freq, scale=c(10,.5),min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
}



corpus1 <- tm_map(corpus, removePunctuation)
corpus2 <- tm_map(corpus, tolower)
corpus3 <- tm_map(corpus, removeWords, stopwords("english"))
ap.tdm <- TermDocumentMatrix(corpus3)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
#png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#dev.off()


# create corpus
corpus = Corpus(VectorSource(tweets_df$text))

# remove stopwords
skipwords = c(stopwords("english"), 
              "genetics", "genomics", "genetic", "genome")
corpus = tm_map(corpus, removeWords, skipwords)

# term-document matrix
tdm = TermDocumentMatrix(corpus)
tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = TRUE, removeWhiteSpace = TRUE))

# convert tdm to matrix
m = as.matrix(tdm)


t = read.csv("/Users/yingli/EVAnalysisCorp/irtm/twitter/tweets.csv", header=TRUE)
t.corpus <- Corpus(DataframeSource(data.frame(as.character(t$text))), encoding =
                     "UTF-8")
x=tm_map(t.corpus, function(x) removeWords(x, stopwords("english")))
inspect(x)
y <- tm_map(x, removePunctuation)
z <- tm_map(y, tolower)
inspect(z)
TermDocumentMatrix(z)
z.tdm=TermDocumentMatrix(z)
ap.m <- as.matrix(z.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
#png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#dev.off()
wordcloud(ap.d$word,ap.d$freq, scale=c(6,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

strsplit_space_tokenizer <- function(x) unlist(strsplit(x, "[[:space:]]+"))
ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = c("reuter", "that"),
             stemming = TRUE,
             wordLengths = c(4, 20))
termFreq(crude[[14]], control = ctrl)

# this below works on "data mining", "visualization", but still not on BMW
tweets = searchTwitter("visualization", n=500,lang="en")
tweets_df = twListToDF(tweets)
mycorpus = Corpus(VectorSource(tweets_df$text))
x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
y <- tm_map(x, removePunctuation)
z = tm_map(y, stripWhitespace)
z.tdm=TermDocumentMatrix(z)
ap.m <- as.matrix(z.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
png("wordcloud_visualization.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

ctrl = list(weighting = function(x) weightTfIdf(x, normalize=TRUE), stopwords = TRUE)
z.tdm=TermDocumentMatrix(z, control = ctrl)

tweets_text = sapply(tweets, function(x) x$getText())
mycorpus = Corpus(VectorSource(tweets_text))
x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
y <- tm_map(x, removePunctuation)
z = tm_map(y, stripWhitespace)
z.tdm=TermDocumentMatrix(z)
ap.m <- as.matrix(z.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
png("wordcloud_visualization.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

tryTolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

some_tweets = searchTwitter("visualization", n=500, lang="en")
some_tweets = searchTwitter("BMW", n=500, lang="en")
some_tweets = searchTwitter("starbucks", n=500, lang="en")
some_tweets = searchTwitter("mission", n=500, lang="en")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# lower case using tryTolower with sapply 
some_txt = sapply(some_txt, tryTolower)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

mycorpus = Corpus(VectorSource(some_txt))
x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
z.tdm=TermDocumentMatrix(x)
ap.m <- as.matrix(z.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_mission.png", width=800,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(13,.5),min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
quartz()  
ggplot(sent_df, aes(x=emotion)) + 
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") + 
  labs(x="emotion categories", y="number of tweets", title = "Sentiment Analysis of Tweets about Excel\n(classification by emotion)") +
  theme(plot.title = element_text(size = rel(1), colour = "blue"))
  
  plot.title = element_text(size=12)
#  theme(title = "Sentiment Analysis of Tweets about Excel\n(classification by emotion)",

# plot distribution of polarity
quartz()
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets", title = "Sentiment Analysis of Tweets about starbucks\n(classification by polarity)") + 
  theme(plot.title = element_text(size=12, color = "blue"))

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
quartz()
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(2,.3), random.order = FALSE, title.size = 1.5)

source("/Users/yingli/EVAnalysisCorp/irtm/sentiment/R/classify_emotion.R")
source("/Users/yingli/EVAnalysisCorp/irtm/sentiment/R/classify_polarity.R")
source("/Users/yingli/EVAnalysisCorp/irtm/sentiment/R/create_matrix.R")

filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130726.txt"
obj1 <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
df1=records2df(obj1)
save(df1, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")

filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130725.txt"
obj <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
df=records2df(obj)
save(df, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df")

load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df")
load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")

removeWords(df1$text[1:10], stopwords(kind="en"))


### working with time stamps
#1. Clean timestamp string, format to "America/New_York" as timezone
### this below does not work because the input time stamp is not the same now
all_tweets$timestamp_clean <- str_sub(all_tweets$created_at, 1, -12)
all_tweets$tweets_datetime <- format(ymd_hms(all_tweets$timestamp_clean))

#2. Create day of week, year, month, year_mo (yyyymm), hour, am_pm
all_tweets$day_of_week <- wday(all_tweets$tweets_datetime, label = TRUE, abbr = FALSE)
all_tweets$year <- year(all_tweets$tweets_datetime)
all_tweets$month <- month(all_tweets$tweets_datetime)
all_tweets$year_mo <- all_tweets$year * 100 + all_tweets$month
all_tweets$hour <- hour(all_tweets$tweets_datetime)
all_tweets$am_pm <- ifelse(am(all_tweets$tweets_datetime), "AM", "PM")

#3. Tweet was an officially recognized retweet or contains RT text pattern (ignoring case)
all_tweets$RT <- ifelse(all_tweets$retweeted_status.id != "", 1,
                        ifelse(str_detect(all_tweets$text, ignore.case("rt @")), 1,0))

#4. Tweet was in response to another tweet, but not an RT of some kind
all_tweets$response_tweet <- ifelse(all_tweets$in_reply_to_status_id != "" & all_tweets$RT == 0, 1, 0)

#5. Starting (or attempting) to start a conversation - Not an RT, not a response
#   but contains someone's username
all_tweets$conversation_starts <- ifelse(all_tweets$RT == 0 & all_tweets$response_tweet == 0 & all_tweets$in_reply_to_user_id != "", 1, 0)

#6. Just general blabbing - Not an RT, not a response, not starting a conversation
all_tweets$blabbing <- ifelse(all_tweets$RT == 0 & all_tweets$response_tweet == 0 & all_tweets$conversation_starts == 0, 1, 0)

#7. Tweet contained at least one hashtag
all_tweets$used_hashtag <- ifelse(str_detect(all_tweets$text, "#"), 1, 0)

#8. Calculate total number of hashtags used in a tweet
all_tweets$num_hashtags <- str_count(all_tweets$text, "#")

#9. Hashtags used - Could possibly be a better regex, works for now
hashtag.regex <- perl("#\\S+")
all_tweets$hashtags <- tolower(ifelse(str_count(all_tweets$text, "#"),
                                      str_extract_all(all_tweets$text,hashtag.regex ), ""))

#10. Get rid of misc garbage still in hashtags column
all_tweets$hashtags <- str_replace_all(str_replace_all(all_tweets$hashtags, '["(),!.:]', ""), "c#", "#")

#11. People responded to - Could possibly be a better regex, works for now
username.regex <- perl("@\\S+")
all_tweets$usernames <- tolower(ifelse(str_count(all_tweets$text, "@"),
                                       str_extract_all(all_tweets$text,username.regex ), ""))

#12. Get rid of misc garbage still in username column
all_tweets$usernames <- str_replace_all(str_replace_all(all_tweets$usernames, '["(),!.:]', ""), "c@", "@")



#1.  Clean text data by setting everything to lowercase, removing stopwords & punctuation
tweets_nostopwords <- removeWords(tweets_lower , stopwords(kind="en")) #remove common stop words
tweets_nocustomwords <- removeWords(tweets_nostopwords, c("rt", "randyzwitch")) #remove custom words
tweets_nopunc <- removePunctuation(tweets_nocustomwords) #strip all punctuation symbols

# remove retweet entities
tweets_noRT = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", all_tweets$text)
# remove at people
tweets_noPeople = gsub("@\\w+", "", tweets_noRT)
# remove punctuation
tweets_nopunc = gsub("[[:punct:]]", "", tweets_noPeople)
# remove numbers
tweets_noNumber = gsub("[[:digit:]]", "", tweets_nopunc)
# remove html links
tweets_noHtml = gsub("http\\w+", "", tweets_noNumber)
# remove unnecessary spaces
tweets_noSpace = gsub("[ \t]{2,}", "", tweets_noHtml)
tweets_noSpace = gsub("^\\s+|\\s+$", "", tweets_noSpace)


#tweets_lower <- tolower(all_tweets$text) 
#make everything lowercase
tweets_lower = sapply(all_tweets$text, tryTolower)

# remove NAs 
tweets_noSpace = tweets_noSpace[!is.na(tweets_noSpace)]
#names(tweets_noSpace) = NULL

tweets_nostopwords <- removeWords(tweets_noSpace , stopwords(kind="en")) #remove common stop words
tweets_nocustomwords <- removeWords(tweets_nostopwords, c("rt", "randyzwitch")) #remove custom words
tweets_nopunc <- removePunctuation(tweets_nocustomwords) #strip all punctuation symbols


#2.  From the single character vector, create a corpus, a special list object from the tm package
#    The corpus will be a list with each list element representing 1 tweet
tweet_corpus <- Corpus(VectorSource(tweets_noSpace))  

#3.  Create a document term matrix
#terms as columns, rows as tweets
tweet_dtm <- DocumentTermMatrix(tweet_corpus) 
tweet_tdm <- TermDocumentMatrix(tweet_corpus) 

#4. Collapse dtm down to an array, where the names(word_frequency) as the words and the array
#   contains the count of each word 
word_frequency <- colSums(as.array(tweet_dtm))

ap.tdm <- tweet_tdm
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
quartz()
#png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(5,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#dev.off()
