library(streamR)
library(tm)
library(ggplot2)
library(reshape)

load("my_oauth.Rdata")
filterStream(file.name = "current_tweets.json",track = "#ModiInUK",language = "en",tweets = 300,oauth = my_oauth)
tweetList<- parseTweets(tweets = "current_tweets.json")
tweetList$text<-iconv(tweetList$text, 'UTF-8', 'ASCII')

length(tweetList$text)

tweet.text<-tweetList$text

# characters per tweet
chars_per_tweet = sapply(tweet.text, nchar)

# split words
words_list = strsplit(tweet.text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
png("Distribution of words per tweet.png", width = 500, height = 500, res = 72)
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)


# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
# barplot
png("Distribution of words length per tweet.png", width = 500, height = 500, res = 72)
barplot(table(round(wsize_per_tweet)), border=NA,
        xlab = "word length in number of characters",
        main="Distribution of words length per tweet", cex.main=1)

# how many unique words per tweet
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
png("Distribution of unique words per tweet.png", width = 500, height = 500, res = 72)
barplot(table(uniq_words_per_tweet), border=NA,
        main="Distribution of unique words per tweet", cex.main=1)

# how many http links per tweet
links_per_tweet = sapply(words_list, function(x) length(grep("http", x)))
table(links_per_tweet)
prop.table(table(links_per_tweet))

# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))
png("Distribution of hashtags.png", width = 500, height = 500, res = 72)
barplot(table(hash_per_tweet), border=NA,main="Distribution of hashtags", cex.main=1)

# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))
png("Distribution of mentions.png", width = 500, height = 500, res = 72)
barplot(table(ats_per_tweet), border=NA, main="Distribution of mentions", cex.main=1)

# data frame
icedf = data.frame(
  chars=chars_per_tweet,
  words = words_per_tweet,
  lengths = wsize_per_tweet,
  uniqs = uniq_words_per_tweet,
  hashs = hash_per_tweet,
  ats = ats_per_tweet,
  links = links_per_tweet
)


# words -vs- chars
ggplot(icedf, aes(x=words, y=chars)) +
  geom_point(colour="gray20", alpha=0.2) +
  stat_smooth(method="lm") +
  labs(x="number of words per tweet", y="number of characters per tweet") +
  ggtitle(label = "Number of words vs Number of characters per tweet")
ggsave(file="words Vs word length.pdf")

# words -vs- word length
ggplot(icedf, aes(x=words, y=lengths)) +
  geom_point(colour="gray20", alpha=0.2) +
  stat_smooth(method="lm") +
  labs(x="number of words per tweet", y="size of words per tweet") +
  ggtitle(label = "Number of words vs size of words per tweet")
ggsave(file="words Vs word length.pdf")

# unique words in total
uniq_words = unique(unlist(words_list))

# lexical diversity
length(uniq_words) / length(unlist(words_list))


# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot
png("Top 20 most frequent terms.png", width = 500, height = 500, res = 72)
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)
