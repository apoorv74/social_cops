
#Creation of Health Index from several health parameters for Bihar Districts
#Setting the working directory
setwd(paste0('/Users/apoorv.anand/Downloads','/bihar district data'))

#Declaring Libraries
library(rvest)
library(stringr)
library(dplyr)
library(reshape2)
# Creating a Health Index
# Reading Data form AHS Factsheet and converting to csv through Tabula
# Fertility
fertility <- read.csv('fertitlity_bihar_districts.csv')
fertility$row_id <- c(1:nrow(fertility))
fertility <- fertility[1:41,]
names(fertility)[] <- c('District','two_above_total','two_above_rural','two_above_urban',
                        'three_above_total','three_above_rural','three_above_urban','row_id')
fertility$District <- as.character(fertility$District)
fertility <- fertility[fertility$row_id >= 4,c(1:7)]
fertility$District[grepl(pattern = 'Kaimur',fertility$District)] <- 'Bhabua'

# Abortion
abortion <- read.csv('abortion_percent.csv')
abortion <- abortion[,c(1,3:5)]
names(abortion)[1] <- 'District'
abortion <- abortion[2:nrow(abortion),] 
names(abortion)[2:ncol(abortion)] <- paste0('abort_',c('Total','Rural','Urban'))
abortion$District <- str_trim(as.character(abortion$District))
abortion$District[grepl(pattern = 'Kaimur',abortion$District)] <- 'Bhabua'

# Institutional Deliveries
ins_delivery <- read.csv('institutional_delivery.csv')
ins_delivery <- ins_delivery[,c(1,3:5)]
names(ins_delivery)[1] <- 'District'
ins_delivery <- ins_delivery[2:nrow(ins_delivery),]
names(ins_delivery)[2:ncol(ins_delivery)] <- paste0('ins_',c('Total','Rural','Urban'))
ins_delivery$District <- str_trim(as.character(ins_delivery$District))
ins_delivery$District[grepl(pattern = 'Kaimur',ins_delivery$District)] <- 'Bhabua'

# Immunisation
immunisation <- read.csv('immunisation.csv')
immunisation <- immunisation[3:nrow(immunisation),c(1,8:10)]
names(immunisation)[1] <- 'District'
names(immunisation)[2:ncol(immunisation)] <- paste0('immune_',c('Total','Rural','Urban'))
immunisation$District <- as.character(immunisation$District)
immunisation$District[grepl(pattern = 'Kaimur',immunisation$District)] <- 'Bhabua'

# Infant Mortality Rate - IMR
imr <-read.csv('imr.csv',header=F)
imr <- imr[,c(1,2,5,8)]
names(imr)[1] <- 'District'
names(imr)[2:ncol(imr)] <- paste0('imr_',c('Total','Rural','Urban'))
imr$District <- as.character(imr$District)
imr$District[grepl(pattern = 'Kaimur',imr$District)] <- 'Bhabua'

#Combining all datasets
master_health_data <- left_join(fertility,abortion,by=NULL)
master_health_data <- left_join(master_health_data,ins_delivery,by=NULL)
master_health_data <- left_join(master_health_data,imr,by=NULL)
master_health_data <- left_join(master_health_data,immunisation,by=NULL)

#Converting factor columns to numeric
col_names <- names(master_health_data)[2:19]
master_health_data[col_names] <- sapply(master_health_data[col_names],as.character)
master_health_data[col_names] <- sapply(master_health_data[col_names],as.numeric)

#Converting NA's to 0
master_health_data[is.na(master_health_data)] <- 0

#Removing the first Bihar Row
master_health_data <- master_health_data[2:nrow(master_health_data),]

#Doing analysis on the overall - Total data
total_col <- c('District',col_names[grepl('total',col_names,ignore.case = T)])
master_health_data <- master_health_data[total_col]

#Removing threeabove and abortion
master_health_data <- master_health_data[,c(1,2,5:7)]

# Since our scale is higher the better - we take the inverse of imr(Infant Mortality Rate)
master_health_data$imr_Total <- 1/master_health_data$imr_Total

# Standardizing the variables - Custom function used to standarise the variables
standard <- function(x){
  x <- (x-mean(x))/sd(x)
}

col_names <- names(master_health_data)[2:ncol(master_health_data)]
master_health_data[col_names] <- sapply(master_health_data[col_names],standard)
master_health_data$state_id <- c(1:nrow(master_health_data))

# Applying PCA to the generated data
pca <- prcomp(master_health_data[,c(2:5)],scale. = T)
dtf <- data.frame(pca$x)

# Considering PC1 and PC2 - Total variability explained - 73%
dtf <- dtf[,c(1:2)]

# Calculating cumulative score based on components
dtf$score <- apply(dtf, 1, function(x) sum((x)))
dtf$state_id <- c(1:nrow(dtf))

# Sorting states based on score
dtf <- arrange(dtf,desc(score))

district_df <- data.frame(state_flag = c(0:37) , District = c('Araria','Arwal','Aurangabad','Banka','Begusarai',
                                                              'Bhagalpur','Bhojpur','Buxar','Darbhanga',
                                                              'Gaya','Gopalganj','Jamui','Jehanabad','Bhabua',
                                                              'Katihar','Khagaria','Kishanganj','Lakhisarai',
                                                              'Madhepura','Madhubani','Munger','Muzaffarpur',
                                                              'Nalanda','Nawada','Pashchim Champaran',
                                                              'Patna','Purba Champaran','Purnia','Rohtas','Saharsa',
                                                              'Samastipur','Saran','Sheikhpura','Sheohar',
                                                              'Sitamarhi','Siwan','Supaul','Vaishali'))


# List of Districts with population and Area 
xpath <- '//*[@id="mw-content-text"]/table[1]'
url <- 'https://en.wikipedia.org/wiki/List_of_districts_of_Bihar'

district_table <- url %>% html %>% html_nodes(xpath = xpath) %>% html_table()
district_table <- data.frame(district_table)
names(district_table)[] <- district_table[1,]
district_table <- district_table[2:nrow(district_table),]
#Custom function used to remove comma from numbers
rem_comma <- function(x){
  x <- str_replace_all(x,",","")
}

col_names <- names(district_table)[4:7]
district_table[col_names] <- sapply(district_table[col_names], rem_comma)
district_table[col_names] <- sapply(district_table[col_names], as.numeric)
district_table$density_2011 <- district_table$`Population (2011)`/district_table$`Area (km²)`
district_table <- district_table[,c(2,4,9)]
names(district_table)[] <- c('District','Population','Density')
district_table$percent_population <- district_table$Population/104096864 * 100
district_table <- arrange(district_table,(percent_population))
district_table$District <- factor(district_table$District,levels = district_table$District)
district_table$District <- as.character(district_table$District)
district_table$District[district_table$District == 'East Champaran'] <- 'Purba Champaran'
district_table$District[district_table$District == 'West Champaran'] <- 'Pashchim Champaran'

# Merging with master data to get the state_names
dtf <- left_join(dtf,master_health_data[,c(1,6)],by=NULL)
dtf <- left_join(dtf,district_table[,c(1,2)],by=NULL)

# Creating an Education Index
bihar_education <- read.csv('bihar_education_metrics.csv')
bihar_education$rowid <- c(1:nrow(bihar_education))
bihar_education$state_flag <- 0
bihar_education$state_flag[1] <- 1
bihar_education$state_flag[bihar_education$rowid %% 17 == 0] <- bihar_education$rowid[bihar_education$rowid %% 17 == 0]/17 + 1
bihar_education <- bihar_education[,c(1,4,5,6)]
bihar_education$X <- as.character(bihar_education$X)
bihar_education <- bihar_education[bihar_education$state_flag <= 1,]
bihar_education$rowid <- c(1:nrow(bihar_education))
bihar_education$state_flag <- (rep(1:37,each=16))
bihar_education <- bihar_education[,c(4,1,2)]

#Adding the remaining state to the main dataset
araria_data <- read.csv('araria_bihar_edu_stats.csv')
araria_data <- araria_data[,c(1,4)]
araria_data$state_flag <- 0
araria_data <- araria_data[,c(3,1,2)]


bihar_edu_cast <- dcast(bihar_education,state_flag ~ X)
araria_edu_cast <- dcast(araria_data,state_flag ~ X)

bihar_edu_metric <- rbind(araria_edu_cast,bihar_edu_cast)

bihar_edu_metric <- left_join(bihar_edu_metric,district_df,by=NULL)
bihar_edu_metric <- bihar_edu_metric[,c(18,2:17)]

colnames <- names(bihar_edu_metric)[2:ncol(bihar_edu_metric)]
bihar_edu_metric[colnames] <- sapply(bihar_edu_metric[colnames],as.numeric)

#Standardizing the data
bihar_edu_metric[colnames] <- sapply(bihar_edu_metric[colnames],standard)

bihar_edu_metric <- bihar_edu_metric[,c(3,4,7,9,10,11,12,13,14)]

#Using principal component to analyse the effective rank of districts based on the above parameters
pca_edu <- prcomp(bihar_edu_metric,scale. = T)
dtf_edu <- data.frame(pca_edu$x)

# Considering PC1 till PC4 - Total variability explained - 76%
dtf_edu <- dtf_edu[,c(1:4)]

# Calculating cumulative score based on components
dtf_edu$score <- apply(dtf_edu, 1, function(x) sum((x)))
dtf_edu$state_flag <- c(0:37)
dtf_edu <- arrange(dtf_edu,desc(score))

#Joining with district_df to get the district names
dtf_edu <- left_join(dtf_edu,district_df,by=NULL)
edu_rank <- dtf_edu[,c(7,5)]
edu_rank$rank <- c(1:nrow(edu_rank))
edu_rank$rank_bucket <- factor(findInterval(edu_rank$rank,c(4,8,15,25,Inf),rightmost.closed = T))

# Visualing the Results
# Declaring the visualisation Libraries
library(ggplot2)
library(ggthemes)
library(rCharts)
bihar_dist <- readShapeSpatial('bihar_all.shp')
bihar_dist_df <- fortify(bihar_dist,region = "NAME_2")
# bihar_dist_df$id <- toupper(bihar_dist_df$id)
cname <- aggregate(cbind(long,lat) ~ id, data=bihar_dist_df,FUN=function(x)mean(range(x)))
cname$angle <-0
cname$long[cname$id == 'Sitamarhi'] <- 85.533720
cname$lat[cname$id == 'Sitamarhi'] <- 26.587431
dtf$rank <- c(1:nrow(dtf))
dtf$rank_bucket <- factor(findInterval(dtf$rank,c(4,8,15,25,Inf),rightmost.closed = T))

# Health choropleth
ggplot() + geom_map(data = dtf, aes(map_id = District, fill = rank_bucket,color='black'), map = bihar_dist_df) + 
  expand_limits(x = bihar_dist_df$long, y = bihar_dist_df$lat) + 
  geom_text(data=cname, aes(x = long, lat, label = id,angle = angle, map_id =NULL), size=4,color='black',fontface = 'bold',alpha=0.7) + 
  scale_fill_manual(values = c("#ffffb2", "#fecc5c", "#fd8d3c",'#f03b20','#bd0026'),name= "Rank", guide = guide_legend(reverse = TRUE)) + theme_minimal()

# Education choropleth
ggplot() + geom_map(data = edu_rank, aes(map_id = District, fill = rank_bucket,color='black'), map = bihar_dist_df) + 
  expand_limits(x = bihar_dist_df$long, y = bihar_dist_df$lat) + 
  geom_text(data=cname, aes(x = long, lat, label = id,angle = angle, map_id =NULL), size=4,color='black',fontface = 'bold',alpha=0.7) + 
  scale_fill_manual(values = c("#ffffb2", "#fecc5c", "#fd8d3c",'#f03b20','#bd0026'),name= "Rank", guide = guide_legend(reverse = TRUE)) + theme_minimal()


# Social Cops Bihar Analysis - Plot
# logo color - #FED731
social_cops <- dtf[,c(4,5,3)]
social_cops$score <- 0
social_cops$score <- factor(social_cops$score)
ggplot() + geom_map(data = social_cops, aes(map_id = District, fill = score,color='#252525'), map = bihar_dist_df) + 
  expand_limits(x = bihar_dist_df$long, y = bihar_dist_df$lat) + 
  geom_text(data=cname, aes(x = long, lat, label = id,angle = angle, map_id =NULL), size=4,color='black',fontface = 'bold') + 
  scale_fill_manual(values = "#FED731",name= "BIHAR ANALYSIS", guide = guide_legend(reverse = TRUE)) + theme_minimal()


district_table$District <- factor(district_table$District,levels = district_table$District)

# Plot - Ranking states according to population
population_plot <- ggplot(data = district_table,aes(x=District,y=percent_population)) + 
  geom_bar(stat='identity') + coord_flip() + theme_economist_white() + xlab('') + ggtitle('District Population') +ylab('Percent Population')

# # Plotting Health and Education Indexes with District Population
# dtf$rank <- c(1:nrow(dtf))
# score_bucket <- c(-3,-1,0,1,3)
# dtf$score_bucket <- findInterval(dtf$score,score_bucket,rightmost.closed = T)
# health_plot <- ggplot(dtf,aes(x = score, y=Population)) + geom_point(aes(size = pop_bucket)) +
#   scale_size_continuous(range = c(5,15)) + geom_text(aes(label=District),vjust=0,hjust=1) +
#   theme_classic()
# 
# h1 <- hPlot(x = "score", y='Population' ,data = dtf, type = c("bubble"), 
#             group = "District", size = "Population")
# h1$xAxis(title =list (text = "District"))
# h1$print("chart5")



