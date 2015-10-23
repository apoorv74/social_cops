
# Social Cops Data Hero Project
# Assignment, Data, v2 - 1

# Setting the working directory
setwd('/Users/mac_warrior/Downloads/social_cops')

# Reading the files as CSV - all_names and missing_data
all_names <- read.csv('all_names.csv',stringsAsFactors = F)
names(all_names)[] <- all_names[1,]
all_names <- all_names[2:nrow(all_names),2:7]
all_names$`Tab No` <- as.numeric(all_names$`Tab No`)

# Converting dates to a common format for both the datasets
all_names$`Survey Start Date` <- as.Date(all_names$`Survey Start Date`,format = '%d-%b-%Y')
all_names$`Survey End Date` <- as.Date(all_names$`Survey End Date`,format = '%d-%b-%Y')

missing_data <- read.csv('missing_data.csv',stringsAsFactors = F)
missing_data$Survey.Date <- as.Date(missing_data$Survey.Date,format = '%m/%d/%y')

# Using dply for data frame operations
library(dplyr)

# Considering only relevant columns for calculation
tab_all_data <- all_names[,c(4:6)]

# Sorting the dataset
tab_all_data <- tab_all_data %>% arrange(`Tab No`)

#Filtering for only those tabs where the data is missing
tab_all_data <- tab_all_data[tab_all_data$`Tab No` %in% missing_data$Tab.No,]

# Joining both the datasets on Tab no - For every entry of Tab - there will be as many rows 
# as in missing data for same tab No. - this is essential to compare the Survey date with
# Survery Start Date and Survey End Date

tab_all_data <- left_join(tab_all_data,missing_data,by = c(`Tab No` = 'Tab.No'))
tab_all_data <- tab_all_data[,c(1:5)]

# Creating a flag where the date is in the range of start and end date
tab_all_data$flag <- 0
tab_all_data$flag[(tab_all_data$Survey.Date>=tab_all_data$`Survey Start Date`) & (tab_all_data$Survey.Date<=tab_all_data$`Survey End Date`)] <- 1

# Considering only those rows where the flag is 1 i.e. we found a match for a particular Tab
data_found <- tab_all_data[tab_all_data$flag ==1,c(1,4,2,3)]
names(data_found)[1] <- 'Tab.No'

# Joining with all_names to get the village level details
data_found <- left_join(data_found,all_names,by=NULL)

# Joining the missing data with the final created dataset 
missing_data <- left_join(missing_data,data_found,by=NULL)
missing_data <- missing_data[,c(1:3,7:11)]

# Removing teh duplicates if any
missing_data <- unique(missing_data)

# Hygeine changes in the final created dataset - For Tab's where the mapping is not done
# the values for columns are taken as 'Not Mapped'
missing_data$`AC Name`[is.na(missing_data$`AC Name`)] <- 'Not mapped'
missing_data$`Mandal Name`[is.na(missing_data$`Mandal Name`)] <- 'Not mapped'
missing_data$`Village Name`[is.na(missing_data$`Village Name`)] <- 'Not mapped'
missing_data$`Survey Start Date` <- as.character(missing_data$`Survey Start Date`)
missing_data$`Survey End Date` <- as.character(missing_data$`Survey End Date`)
missing_data$`Survey Start Date`[(is.na(missing_data$`Survey Start Date`))] <- 'Not mapped'
missing_data$`Survey End Date`[(is.na(missing_data$`Survey End Date`))] <- 'Not mapped'

missing_data$no_data_flag <- 0
missing_data$no_data_flag[missing_data$`AC Name` == 'Not mapped'] <- 1

#Checking the data if for same Tab No and survey data, we have more than one values
data_chk <- missing_data %>% filter(no_data_flag == 0) %>% group_by(Tab.No,Response.No) %>% summarise(count = length(Tab.No))

missing_data <- missing_data %>% arrange(Tab.No)

# Writing the final dataset created to the local
write.csv(missing_data,'missing_tab_details.csv',row.names=F)

# Summary of the data mapped
# Total unique combinations of Tab No and Response No where the data was missing - 21511
# Total unique combinations of Tab No and Response No which were mapped to a particular village - 19608 (91%)
# Tab No and Response No which were mapped to
# Single Village - 17022 (79%)
# Two Villages - 2287 (11%)
# Three Villages - 282 (1%)
# Four Villages - 17 - (0.07%)