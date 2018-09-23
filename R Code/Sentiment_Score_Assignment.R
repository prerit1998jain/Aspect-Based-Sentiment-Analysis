# Reading the csv file
airport = read.csv('median_imputed.csv',stringsAsFactors = FALSE)

# Certain Preprocessing step
airport$X = NULL
airport$airport_name = as.factor(airport$airport_name)
airport$author_country = as.factor(airport$author_country)

# Importing the necessary packages for the sentiment analysis
library(tidytext)
library(dplyr)
library(tidyr)

# Converting the data into tibble format as it is easy to process it further.
airport_tibble = data_frame(Reviews = airport$content, overall_score = airport$overall_rating,
                            airport_name = airport$airport_name,author_country = airport$author_country,
                            queuing_rating = airport$queuing_rating,terminal_cleaniless_rating = airport$terminal_cleanliness_rating,
                            airport_shopping_rating = airport$airport_shopping_rating,
                            recommended = airport$recommended)

## As we have to tokenize the data into words in order to compare it with the dictionary.
# Hence to keep an account that which word comes from which review we have to assign a new variable.##

# Assigning line number.
airport_tibble_mu = mutate(airport_tibble,line = row_number())

# tokenizing the words from the reviews
airport_words = unnest_tokens(airport_tibble_mu,word,Reviews)

##  Inner joining the words from the review and the afinn dictionary
# which is a general purpose dictionary provided by the tidytext package in R.
joined = inner_join(airport_words,get_sentiments("afinn"))

# Counting the number of words assigned to each score.
counted = count(joined,line = line %/% 1,score)

# Converting the descrete scores to over_all score 
spreaded = spread(counted,score,n,fill =FALSE)
final = mutate(spreaded, sentiment_score = spreaded$'-4'*(-4) + spreaded$'-3'*(-3) + spreaded$'-2'*(-2) + spreaded$'-1'*(-1) +
                 spreaded$'5'*(5) + spreaded$'4'*(4) + spreaded$'3'*(3) + spreaded$'2'*(2) + spreaded$'1'*(1))

# Removing the unnecesssary variables
final$`1` = NULL
final$`2` = NULL
final$`3` = NULL
final$`4` = NULL
final$`5` = NULL
final$`-1` = NULL
final$`-2` = NULL
final$`-3` = NULL
final$`-4` = NULL

## An iterative loop to map the sentiment scores to a scale of 1 to 10.
i = 1
for (i in 1:nrow(final)) {
  max = final$sentiment_score[which.max(final$sentiment_score)]
  min = final$sentiment_score[which.min(final$sentiment_score)]
  final$sentiment_score[i] = (((final$sentiment_score[i] - min)*9)/(max-min)) + 1
}

# A round of function
round = function(x,n)
{
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Calling the round off function
final$sentiment_score = as.integer(round(final$sentiment_score,0))

# Final dataframe storing the review and the respective scores assigned and given.
final_assignment = inner_join(airport_tibble_mu,final)

# Removing the variable which we used to keep into account the review-word relationship.
final_assignment$line = NULL

# storing the data into a csv file.
write.csv(final_assignment,'sentiment_score.csv')

