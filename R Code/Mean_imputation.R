#reading the initial file
airport=read.csv("airport.csv",stringsAsFactors = FALSE)
str(airport)
summary(airport)
table(airport$airport_staff_rating)
#removing all the variables which are not useful/whic have NA's exceeding 50% of total data
airport$terminal_seating_rating=NULL
airport$airport_staff_rating=NULL
airport$wifi_connectivity_rating=NULL
airport$terminal_signs_rating=NULL
airport$food_beverages_rating=NULL
#saving the new csv file in the same directory
write.csv(airport,"airport1.csv")
airport=read.csv("airport1.csv",stringsAsFactors = FALSE)
airport$date=NULL
airport=read.csv("airport1.csv",stringsAsFactors = FALSE)
airport$date=NULL
airport$date_visit=NULL
airport$X=NULL
airport$link=NULL
airport$title=NULL
airport$author=NULL
airport$author_country=NULL
airport$experience_airport=NULL
airport$type_traveller=NULL
write.csv(airport,"airport2.csv")
airport=read.csv("airport2.csv",stringsAsFactors = FALSE)


#splitting the data in to list according to airport name
X=split(airport,airport$airport_name)
str(X[1])


#Using mean imputation of individual list 
for (i in 1:741)
{
  X[[i]]$queuing_rating[is.na(X[[i]]$queuing_rating)] = mean(X[[i]]$queuing_rating[!is.na(X[[i]]$queuing_rating)])
X[[i]]$overall_rating[is.na(X[[i]]$overall_rating)] = mean(X[[i]]$overall_rating[!is.na(X[[i]]$overall_rating)])
X[[i]]$terminal_cleanliness_rating[is.na(X[[i]]$terminal_cleanliness_rating)] = mean(X[[i]]$terminal_cleanliness_rating[!is.na(X[[i]]$terminal_cleanliness_rating)])
X[[i]]$airport_shopping_rating[is.na(X[[i]]$airport_shopping_rating)] = mean(X[[i]]$airport_shopping_rating[!is.na(X[[i]]$airport_shopping_rating)])


}

#merging all the list
cleandata=do.call("rbind",X)

#omitting all the na values left after imputation which is the countries  which have only one or two reviews and all of them are value .so there is no method to impute that if we are imputing on the basis of countries 
imputed_mean=na.omit(cleandata)
#saving the csv files in the same location after all the cleaning
write.csv(imputed_mean,"imputed_mean.csv")
#rounding off to nearest integers
round = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
#finally saving the final imputed file in the same folder
write.csv(imputed_mean,"imputed_mean2.csv")




  

