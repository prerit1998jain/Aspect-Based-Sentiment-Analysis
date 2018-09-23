# Download the java version which is compatible with your R version.
# You can check the current java version available in your system by the following command.
### system("java -version")

##library(devtools)
# Installation of RDRPOSTagger
### devtools::install_github("bnosac/RDRPOSTagger")

library(RDRPOSTagger)
library(tokenizers)
library(tm)
library(tidyr)
library(dplyr)
library(tidytext)


setwd("E:/Data Analytics/Text Analytics/Project/Dataset")
airport = read.csv('sentiment_score.csv',stringsAsFactors = FALSE)
airport$X = NULL
corpus = Corpus(VectorSource(airport$Reviews))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removeWords,stopwords("english"))

corpus1 = Corpus(VectorSource(airport$Review))

# DATA CLEANING
corpus1 = tm_map(corpus1,tolower)
corpus1 = tm_map(corpus1,removePunctuation)
corpus1 = tm_map(corpus1,removeWords,stopwords("English"))
dtm = DocumentTermMatrix(corpus1)
# Removing Sparsity
sparse = removeSparseTerms(dtm,0.98)
glassdoorSparse = as.data.frame(as.matrix(sparse))
colnames(glassdoorSparse) = make.names(colnames(glassdoorSparse))

# Document Term Matrix Creation


# A function for a pos tagging of a string 
textPOSTagger = function(text){
  
  # A dictionary to convert the short names to complete names of POS
  unipostag_types <- c("ADJ" = "adjective", "ADP" = "adposition", "ADV" = "adverb", "AUX" = "auxiliary",
                       "CONJ" = "coordinating conjunction", "DET" = "determiner", "INTJ" = "interjection",
                       "NOUN" = "noun", "NUM" = "numeral", "PART" = "particle", "PRON" = "pronoun", 
                       "PROPN" = "proper noun", "PUNCT" = "punctuation", "SCONJ" = "subordinating conjunction", 
                       "SYM" = "symbol", "VERB" = "verb", "X" = "other")  
  
  # Tokenizing sentences on text by text basis.
  sentences <- tokenize_sentences(text, simplify = TRUE)
  sentences <- removePunctuation(sentences)
  
  # building a model for POS tagging.
  unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
  
  # Final POS tagging
  unipostags <- rdr_pos(unipostagger, sentences)
  
  # using the dictionary to give full names to parts of speech
  unipostags$pos <- unipostag_types[unipostags$pos]
  
  return(unipostags)
}

final1 = data.frame(matrix(ncol = 5,nrow = 0))
colnames(final1) = c('doc_id','token_id','token','pos','index')

i = 1
for(i in 1:nrow(airport)){
  tagged = textPOSTagger(corpus[[i]]$content)
  tagged$index = i
  final1 = rbind(tagged,final1)
}

# Taking a subset of all the nouns in the system
noun = subset(final1, final1$pos == 'noun')

# Creation of DTM
counted = count(noun, reviews = index %/% 1 , token)
noun_dtm = spread(counted,token,n,fill = FALSE)

matched = match(colnames(noun_dtm),colnames(glassdoorSparse))
imp_nouns = list()
j = 1
for(i in 1:ncol(noun_dtm)){
  if(is.na(matched[i]) == FALSE){
    imp_nouns[j] = colnames(noun_dtm)[i]
    j = j+1
  }
}

