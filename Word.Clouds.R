# Word Cloud of First Sentences of Abstracts on Molecular Ecology RSS feed

#~~ Load libraries

library(XML)
library(tm)
library(wordcloud)

#~~ Load data

doc <- xmlTreeParse("view-source_onlinelibrary.wiley.com_rss_journal_10.1111_(ISSN)1365-294X.xml", encoding = "ANSI")
xmltop = xmlRoot(doc)

#~~ remove first entry

xmltop <- xmltop[-1]


#~~ issues with lapply so lazily using loop. Create empty list and extract "description" identifier from xmltop


desc.list <- list()

for(i in 1:length(xmltop)){
  tempvar <- xmlValue(xmltop[i][[1]][["description"]])  # extract abstract
  tempvar <- gsub("e.g.", "", tempvar, fixed = T)       # remove e.g.
  tempvar <- gsub("i.e.", "", tempvar, fixed = T)       # remove i.e.
  tempvar<- strsplit(tempvar, split = ". ", fixed = T)[[1]][1]  # get first sentence
  desc.list[i]  <- gsub("[^0-9A-Za-z///' ]", "", tempvar)  # remove non-alpha characters
  
}

#~~ remove punctuation, stopwords, numbers and tidy things up

desc.list <- lapply(desc.list, removePunctuation)
desc.list <- lapply(desc.list, tolower)
desc.list <- lapply(desc.list, removeWords, stopwords("en"))
desc.list <- lapply(desc.list, removeNumbers)
desc.list <- lapply(desc.list, stripWhitespace)

#~~ create data frame with frequency of words

word.vec <- unlist(lapply(desc.list, function(x) strsplit(x, split = " ")[[1]]))
word.freq <- data.frame(table(word.vec))

#~~ Make word cloud

wordcloud(word.freq$word.vec,  
          word.freq$Freq,
          colors = brewer.pal(9, "YlOrRd")[5:9],
          random.order=TRUE,
          min.freq=4,
          scale=c(3, 0.3))


