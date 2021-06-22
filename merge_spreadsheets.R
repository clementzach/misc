library(readxl)

library(dplyr)

library(lubridate)

library(tidyr)

library(comparator)
from_theranest = readxl::read_xlsx( '/Volumes/CLEMENT/CCC_conversion/spreadsheets/Client List from TheraNest 5-24-21.xlsx')

from_pdf = read.csv( '/Volumes/CLEMENT/CCC_conversion/spreadsheets/NEW_extracted_information_all.csv')



email_social <- from_theranest %>% mutate(
  only_first = sapply(`First Name`, function(x){strsplit(x, split = ' +')[[1]][1]}), 
only_last = sapply(`Last Name`, function(x){tail(strsplit(x, split = ' +')[[1]])}), 
dob = substr(`Date of Birth`, start = 1, stop = 10), 
combined_names = tolower(paste(only_first, only_last, dob))) %>% 
  rename(first_name_from_theranest = `First Name`, last_name_from_theranest = `Last Name`) %>% 
  select(c(combined_names, 
           Email, 
           Ssn, 
           first_name_from_theranest, 
           last_name_from_theranest, 
           'Primary Insurance: Insured Last Name',
           'Primary Insurance: Insured Middle Name',
           'Primary Insurance: Insured Phone',
           'Primary Insurance: Insured Birthdate',
           "Primary Insurance: Relationship To Insured"))


from_pdf_clean <- from_pdf %>% mutate(combined_names = 
                                        tolower(paste(first.name, 
                                              last.name, 
                                              substr(parse_date_time(client.date.of.birth, 
                                                                     orders = 'mdy' ), 
                                                     start = 1, 
                                                     stop = 10) , sep = " "))) %>% 
  filter(sapply(full.name, nchar) > 5, 
         sapply(Billable.party.1, nchar) > 10, 
         sapply(client.date.of.birth, nchar) > 1)

print(paste('data cleaning reduced the number of records from ', toString(nrow(from_pdf)), 'to', toString(nrow(from_pdf_clean)), 
            "\n records were removed if they didn't have a client date of birth, they didn't have a name, or didn't have a billable party", 
            sep = " "))



joined_df <- left_join(from_pdf_clean, email_social, by = 'combined_names', na_matches = 'never') 


duplicate_df <- as.data.frame(table(from_pdf_clean$combined_names))

#nrow(joined_df)

#nrow(email_social)

print('Individuals with duplicate records')
print(duplicate_df[duplicate_df$Freq > 1,])

print('Fuzzy matching for emails not matched to a client: ')

for (email in from_theranest$Email[! from_theranest$Email %in% joined_df$Email]){
  i <- which(email_social$Email == email)
  max_similarity = which.max(JaroWinkler()(email_social$combined_names[i], joined_df$combined_names))
  print(paste("Assuming", 
              email_social$first_name_from_theranest[i],
              email_social$last_name_from_theranest[i],
              from_theranest$`Date of Birth`[i],
              "is the same as", 
              joined_df$full.name[max_similarity], 
              substr(parse_date_time(joined_df$client.date.of.birth[max_similarity], 
                                     orders = 'mdy' ), start = 1, stop = 10)))
  joined_df[max_similarity, 'Email'] <- email
  joined_df[max_similarity, 'Ssn'] <- email_social$Ssn[i]
  joined_df[max_similarity, 'first_name_from_theranest'] <- email_social$first_name_from_theranest[i][1]
  joined_df[max_similarity, 'last_name_from_theranest'] <- email_social$last_name_from_theranest[i][1]
}

for (Ssn in from_theranest$Ssn[! from_theranest$Ssn %in% joined_df$Ssn]){
  i <- which(email_social$Ssn == Ssn)
  max_similarity = which.max(JaroWinkler()(email_social$combined_names[i], joined_df$combined_names))
  print(paste("Assuming", 
              email_social$first_name_from_theranest[i],
              email_social$last_name_from_theranest[i],
              from_theranest$`Date of Birth`[i],
              "is the same as", 
              joined_df$full.name[max_similarity], 
              substr(parse_date_time(joined_df$client.date.of.birth[max_similarity], 
                                     orders = 'mdy' ), start = 1, stop = 10)))
  #joined_df[max_similarity, 'Email'] <- email
  joined_df[max_similarity, 'Ssn'] <- email_social$Ssn[i][1]
  joined_df[max_similarity, 'first_name_from_theranest'] <- email_social$first_name_from_theranest[i][1]
  joined_df[max_similarity, 'last_name_from_theranest'] <- email_social$last_name_from_theranest[i][1]
  
  
}


for (l_name in from_theranest$`Last Name`[! from_theranest$`Last Name` %in% joined_df$last_name_from_theranest]){
  i <- which(email_social$last_name_from_theranest == l_name)
  max_similarity = which.max(JaroWinkler()(email_social$combined_names[i], joined_df$combined_names))
  print(paste("Assuming", 
              email_social$first_name_from_theranest[i],
              email_social$last_name_from_theranest[i],
              from_theranest$`Date of Birth`[i],
              "is the same as", 
              joined_df$full.name[max_similarity], 
              substr(parse_date_time(joined_df$client.date.of.birth[max_similarity], 
                                     orders = 'mdy' ), start = 1, stop = 10)))
  #joined_df[max_similarity, 'Email'] <- email
  joined_df[max_similarity, 'Ssn'] <- email_social$Ssn[i][1]
  joined_df[max_similarity, 'first_name_from_theranest'] <- email_social$first_name_from_theranest[i][1]
  joined_df[max_similarity, 'last_name_from_theranest'] <- email_social$last_name_from_theranest[i][1]
  
  
}


for (f_name in from_theranest$`First Name`[! from_theranest$`First Name` %in% joined_df$first_name_from_theranest]){
  i <- which(email_social$first_name_from_theranest == f_name)
  max_similarity = which.max(JaroWinkler()(email_social$combined_names[i], joined_df$combined_names))
  print(paste("Assuming", 
              email_social$first_name_from_theranest[i],
              email_social$last_name_from_theranest[i],
              from_theranest$`Date of Birth`[i],
              "is the same as", 
              joined_df$full.name[max_similarity], 
              substr(parse_date_time(joined_df$client.date.of.birth[max_similarity], 
                                     orders = 'mdy' ), start = 1, stop = 10)))
  #joined_df[max_similarity, 'Email'] <- email
  joined_df[max_similarity, 'Ssn'] <- email_social$Ssn[i][1]
  joined_df[max_similarity, 'first_name_from_theranest'] <- email_social$first_name_from_theranest[i][1]
  joined_df[max_similarity, 'last_name_from_theranest'] <- email_social$last_name_from_theranest[i][1]
  
  
}


joined_df <- joined_df %>% select(!c(combined_names))

write.csv(joined_df, 
          
          paste('/Volumes/CLEMENT/CCC_conversion/spreadsheets/final_joined_spreadsheet_',
          substr(now(), start = 1, stop = 10),
          '.csv', sep = ''), 
          na = '', row.names = F)

print('New spreadsheet successfully created and written. Thanks, McKenna :)')
rm(list = ls())
