# "airport_short.csv" file is a csv file consisting of useful columns taken from the dataset which we are provided with.

# Reading the csv file into a dataframe called airport
airport = read.csv('airport_short.csv',stringsAsFactors = FALSE)

# Converting the airport_name and author_country into factors
airport$airport_name = as.factor(airport$airport_name)
airport$author_country = as.factor(airport$author_country)

# As the imputation should be logical hence we had split the airport dataframe 
# on the basis of the airport_name and done median imputation on that. 

# Splitting
X <- split(airport,airport$airport_name)

# Iterative loop for median imputation
for (i in 1:741)
{
  X[[i]]$overall_rating[is.na(X[[i]]$overall_rating)] = median(X[[i]]$overall_rating[!is.na(X[[i]]$overall_rating)])
  X[[i]]$queuing_rating[is.na(X[[i]]$queuing_rating)] = median(X[[i]]$queuing_rating[!is.na(X[[i]]$queuing_rating)])
  X[[i]]$terminal_cleanliness_rating[is.na(X[[i]]$terminal_cleanliness_rating)] = median(X[[i]]$terminal_cleanliness_rating[!is.na(X[[i]]$terminal_cleanliness_rating)])
  X[[i]]$airport_shopping_rating[is.na(X[[i]]$airport_shopping_rating)] = median(X[[i]]$airport_shopping_rating[!is.na(X[[i]]$airport_shopping_rating)])
}

# Recombining the dataframes with imputed value into a dataframe of the size equal to that of original
imputed1 = do.call("rbind", X)

# Now after doing this some NA values are left. These corrosponds to the airports which 
# has only one review and the variable value is missing for that also. So keeping such rows 
# in the dataset make no sense.

#Removal of unnecesary rows
imputed2 = na.omit(imputed1)

# Since our variable is categorical we don't want the variable to take decimal 
# values and hence we round it off to nearest integer

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

# Calling the round of function
imputed2$overall_rating = as.factor(round(imputed2$overall_rating,0))
imputed2$queuing_rating = as.factor(round(imputed2$queuing_rating,0))
imputed2$terminal_cleanliness_rating = as.factor(round(imputed2$terminal_cleanliness_rating,0))
imputed2$airport_shopping_rating = as.factor(round(imputed2$airport_shopping_rating,0))

# Storing the file into a csv file.
write.csv(imputed2,"median_imputed.csv")

