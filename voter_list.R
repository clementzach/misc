


library(readxl)
library(openxlsx)
library(bestglm)
library(dplyr)




## Fit a glm to predict one's odds of voting.

## get our dataframe to be easier to work with.

###Reading from excel
voter_list <- read_excel("/Volumes/CLEMENT/Data/RegisteredVoters.xlsx", 
                        col_names = TRUE)

primaries_2020 <- read_excel("/Volumes/CLEMENT/Data/PrimaryVotersLindonPG.xlsx", 
                             col_names = TRUE)




##Columns need to be interpretable
string_to_date = function(x) {
  as.character(as.Date(as.numeric(x), origin = "1899-12-30"))
}

for (i in 1:length(colnames(voter_list))){
  if (!is.na(string_to_date(colnames(voter_list)[i]))){
    colnames(voter_list)[i] <- string_to_date(colnames(voter_list)[i])
  }
}


#### Joining our new primary list with the old one

join_df <- data.frame(primaries_2020$VOTER_ID, primaries_2020$VOTED)

colnames(join_df) <- c("Voter ID", "VOTED_2020_primary")

voter_list <- left_join(voter_list, join_df, by = "Voter ID")

voter_list[["2020-06-30"]] <- voter_list[["VOTED_2020_primary"]]



 

voting_years <- as.character(2001:2020)

year_cols <- colnames(voter_list)[substr(colnames(voter_list), start = 1, stop = 4) %in% voting_years]

## We want the number of elections they participated in for each year, 
## and the binary general election for the year. 



for (year in voting_years){
  gen_for_year <- year_cols[max(which(substr(year_cols, start = 1, stop = 4) == year))]
  
  num_primaries <- sum(substr(year_cols, start = 1, stop = 4) == year) -1
  all_elections <- year_cols[substr(year_cols, start = 1, stop = 4) == year]
  all_primaries <- head(all_elections, -1)
  
  sum_all_primaries <- as.numeric(!is.na(voter_list[, all_primaries[1]]))
  if (length(all_primaries) > 1){
    for (i in tail(all_primaries, -1)){
      sum_all_primaries <- sum_all_primaries + !is.na(voter_list[, i])
    }
  }
  
  voter_list[, paste("primaries_", year, sep = "")]  <- as.numeric(sum_all_primaries > 0)
  voter_list[, paste("general_", year, sep = "")] <- as.numeric(!is.na(voter_list[, gen_for_year]))
}


voting_methods <- levels(unique(as.factor(voter_list[["How Voted...113"]])))
##assuming this election will capture all of the possible methods

method_col_names <- paste("voted_",voting_methods,sep="")

voter_list[,method_col_names]<- c(0) ##creating the dummy variables

## We want easy to understand column names for how they voted.
for (i in 35:131) {
  if(substr(colnames(voter_list)[i], start = 1, stop = 3) == "How"){
    voter_list[is.na(voter_list[,i]),i] <- "NONE"
    # now we will change the voting level dummy variables
    for (j in 1:length(method_col_names)) {
      voter_list[[method_col_names[j]]] <- voter_list[[method_col_names[j]]] +
        (voter_list[,i] == voting_methods[j])
    }
    
  }
}

##we want this to be categorical, so we'll make their primary voting method a 1 and the rest zeroes
for (i in 1:nrow(voter_list)){
  if (max(voter_list[i, method_col_names]) > 0){
    voter_list[i, method_col_names] <- as.list(as.numeric(voter_list[i, method_col_names] == max(voter_list[i, method_col_names])))
  }
}

## making dummy variables for their party affiliation
party_options <- unique(voter_list[["Party"]])

party_col <- paste("party_",party_options,sep="")

voter_list[,party_col] <- 0

for (i in 1:length(party_col)) {
  voter_list[[party_col[i]]] <- as.numeric(voter_list[["Party"]] == party_options[i])
}

voter_list[["sum_8_years_2016"]] <- c(0)

for (i in which(colnames(voter_list)=="primaries_2008"):which(colnames(voter_list)=="primaries_2016")){
  voter_list[["sum_8_years_2016"]] <- voter_list[["sum_8_years_2016"]] + voter_list[,i]
}

voter_list[ ,"sum_8_years_2016"] <- voter_list[ ,"sum_8_years_2016"]$sum_8_years_2016


general_2016_party <- cbind(voter_list[ ,"party_Republican"],
                            voter_list[ ,"party_Democratic"],
                            voter_list[ ,"party_Libertarian"],
                            voter_list[ ,"party_United Utah"],
                         voter_list[ ,"primaries_2015"],
                         voter_list[ ,"general_2008"],
                         voter_list[, general_until_2016_11] )

bestglm_2016_party <- bestglm::bestglm(general_2016_party, 
                                       IC = "AIC",
                                       method = "forward",
                                       family = binomial)



general_2016_elections <- cbind(voter_list[ ,"general_2008"],
                                voter_list[ ,"primaries_2008"],
                                voter_list[ ,"general_2009"],
                                voter_list[ ,"general_2010"],
                                voter_list[ ,"general_2011"],
                                voter_list[ ,"general_2012"],
                                voter_list[ ,"sum_8_years_2016"],
                                voter_list[ ,"primaries_2013"],
                                voter_list[ ,"general_2013"],
                                voter_list[ ,"primaries_2014"],
                                voter_list[ ,"general_2014"],
                                voter_list[ ,"primaries_2015"],
                                voter_list[ ,"general_2015"],
                                voter_list[ ,"primaries_2016"],
                                voter_list[ ,"general_2016"])
bestglm_2016_elections <- bestglm::bestglm(general_2016_elections, 
                                        IC = "AIC",
                                        method = "forward",
                                        family = binomial)

### This included: general_2008,general_2010, general_2011, general_2012, general_2013, general_2014, general_2015, primaries_2016, general_2016



general_2016_method <- cbind(voter_list[ , method_col_names],
                             voter_list[ ,"general_2008"],
                             voter_list[ ,"general_2013"],
                             voter_list[ ,"general_2014"],
                             voter_list[ ,"general_2015"],
                             voter_list[ ,"primaries_2016"],
                             voter_list[ ,"general_2016"])


bestglm_2016_method <- bestglm::bestglm(general_2016_method, 
                                        IC = "AIC",
                                        method = "forward",
                                        family = binomial)

### This did not include any of the voting methods, so we'll not worry about this in any future models


table(voter_list$Party) ##To find out the most common parties

general_2016_party <- data.frame(voter_list$party_Republican,
                            voter_list$party_Democratic,
                            voter_list$party_Libertarian, 
                            voter_list[["party_Independent American"]],
                            voter_list$party_Unaffiliated,
                            voter_list$party_Constitution,
                            voter_list$general_2008,
                            voter_list$general_2010,
                            voter_list$general_2011,
                            voter_list$general_2012,
                            voter_list$general_2013,
                            voter_list$general_2014,
                            voter_list$general_2015,
                            voter_list$primaries_2016,
                            voter_list$general_2016)

bestglm_2016_party <- bestglm::bestglm(general_2016_party, 
                                       IC = "AIC",
                                       method = "forward",
                                       family = binomial)



##Now, lets aggregate the 2016 and the 2012 general elections to estimate coefficients


general_2016_final <- data.frame(voter_list$party_Democratic,
                                 voter_list$party_Unaffiliated,
                                 voter_list$party_Constitution,
                                 voter_list$general_2008,
                                 voter_list$general_2010,
                                 voter_list$general_2011,
                                 voter_list$general_2012,
                                 voter_list$general_2013,
                                 voter_list$general_2014,
                                 voter_list$general_2015,
                                 voter_list$primaries_2016,
                                 voter_list$general_2016)



general_2012_to_16 <- data.frame(voter_list$party_Democratic,
                          voter_list$party_Unaffiliated,
                          voter_list$party_Constitution,
                          voter_list$general_2004,
                          voter_list$general_2006,
                          voter_list$general_2007,
                          voter_list$general_2008,
                          voter_list$general_2009,
                          voter_list$general_2010,
                          voter_list$general_2011,
                          voter_list$primaries_2012,
                          voter_list$general_2012)

colnames(general_2012_to_16) <- colnames(general_2016_final)

training_final <- rbind(general_2016_final, general_2012_to_16)

colnames(training_final) <- c("party_Democratic", 
                              "party_Unaffiliated", 
                              "party_Constitution", 
                              "general_2008", 
                              "general_2010",
                              "general_2011",
                              "general_2012",
                              "general_2013",
                              "general_2014",
                              "general_2015",
                              "primaries_2016",
                              "general_2016")



final_glm <- glm(general_2016 ~ ., family = binomial, data=training_final)



pred_df <-  data.frame(voter_list$party_Democratic,
                       voter_list$party_Unaffiliated,
                       voter_list$party_Constitution,
                       voter_list$general_2012,
                       voter_list$general_2014,
                       voter_list$general_2015,
                       voter_list$general_2016,
                       voter_list$general_2017,
                       voter_list$general_2018,
                       voter_list$general_2019,
                       voter_list$primaries_2020)

colnames(pred_df) <- colnames(training_final)[1:(length(colnames(training_final))-1)]


voter_list$voting_prob <- predict.glm(final_glm, 
                           newdata = pred_df, type = "response")







##Order the voter list by the voting probablity

ordered_df <- voter_list[order(voter_list[ ,"voting_prob"], decreasing = TRUE), c(1:26, length(voter_list))]
## write this to an excel sheet

hist(voter_list$voting_prob, main = "Voter Probability Histogram", xlab = "Probability", ylab = "Number of Voters")

sum(voter_list$voting_prob > .95)

write.xlsx(ordered_df, file = "/Volumes/CLEMENT/Data/sorted_by_probs.xlsx")

## Now that we have a probability for each voter, we want to be able 
## to only send one card to each family. 

household_voter_list <- ordered_df

household_voter_list[,"household_size"] <- c(1)
household_voter_list[,"expected_value"] <- household_voter_list[,"voting_prob"]
drop_list <- numeric(length = 100)
k <- 1

house_cols <- c("Zip", "House Number", "Street", "Direction Prefix", "Street", "Direction Suffix")

for (col in house_cols){
  household_voter_list[is.na(household_voter_list[ ,col]), col] <- "NONE"
}

household_voter_list <- household_voter_list[order(household_voter_list[,"Last Name"]), ]

for (i in 1:nrow(household_voter_list)) {
##  print(paste("i is ", i))
  j <- i + 1
  while (!(i %in% drop_list) & (household_voter_list[i, "Last Name"] == household_voter_list[j, "Last Name"])){
##    print(paste("Checking",household_voter_list[i, "First Name"], "and", household_voter_list[j, "First Name"]))
    if ((household_voter_list[i, "Zip"] == household_voter_list[j, "Zip"]) &
        (household_voter_list[i, "House Number"] == household_voter_list[j, "House Number"]) &
        (household_voter_list[i, "Direction Prefix"] == household_voter_list[j, "Direction Prefix"]) &
        (household_voter_list[i, "Street"] == household_voter_list[j, "Street"]) &
        (household_voter_list[i, "Direction Suffix"] == household_voter_list[j, "Direction Suffix"])) {
      household_voter_list[i, "First Name"] <- paste(household_voter_list[i, "First Name"], household_voter_list[j, "First Name"], sep = ", ")
      household_voter_list[i,"household_size"] <- household_voter_list[i,"household_size"] + 1
      household_voter_list[i,"expected_value"] <- household_voter_list[i,"expected_value"] + household_voter_list[j,"voting_prob"]
      drop_list[k] <- j
      k <- k + 1
    }
    j <- j + 1
  }
}

for (col in house_cols){
  household_voter_list[household_voter_list[ ,col] == "NONE", col] <- NA
}


household_voter_list <- household_voter_list[-drop_list, ]
sorted_by_expected_value <- household_voter_list[order(household_voter_list[, "expected_value"], decreasing = TRUE), ]

write.xlsx(sorted_by_expected_value, file = "/Volumes/CLEMENT/Data/sorted_by_expected_value_new.xlsx")


### optimize our sending
first_multiplier <- 2
second_multiplier <- 1

total_cards <- 9642


one <- round(total_cards /2, digits = 0)
two <- round(total_cards /2, digits = 0)
next_val <- 1 ## needs to enter the loop. The actual values should be much higher. 
prev_val <- 0

while(next_val > prev_val) {
  one <- one+1
  two <- two-1
  prev_val <- next_val
  next_val <- first_multiplier * sum(sorted_by_expected_value[1:one,"expected_value"]) + 
    second_multiplier * sum(sorted_by_expected_value[1:two,"expected_value"])
  
}


write.xlsx(sorted_by_expected_value[1:one, ], file = "/Volumes/CLEMENT/Data/send_one_card.xlsx")
write.xlsx(sorted_by_expected_value[1:two, ], file = "/Volumes/CLEMENT/Data/send_two_cards.xlsx")





