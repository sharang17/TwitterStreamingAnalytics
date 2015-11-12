# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# load packages
library(XML)
library(tm)
library(ggplot2)


# vector to store results
results = character(0)

# paginate 20 times to harvest tweets
for (page in 1:20)
{
  # create twitter search query to be parsed
  # tweets in english containing 'icecream'
  twitter_search = paste(twitter_url, "q=icecream",
                         "&rpp=100&lang=en&page", page, sep="")
  
  # let's parse with xmlParseDoc
  tmp = xmlParseDoc(twitter_search, asText=FALSE)
  
  # extract titles
  results = c(results, xpathSApply(tmp, "//s:entry/s:title", xmlValue,
                                   namespaces=c('s'='http://www.w3.org/2005/Atom')))
}

length(results)