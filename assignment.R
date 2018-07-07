
###################Random Forest Model for loan prediction################################
directory = "F:/rassignment"
setwd(directory)
FileList <- list.files(path = ".", pattern = "*.csv")
Data <- read.csv(FileList, header = TRUE)
library(DMwR)
Data$m <- scale(Data$loan_amount)
Data$m_ <- unscale(Data$m, Data$m)

########################outlier tratment################
duration_quantiles <- data.frame(quantile(Data$loan_amount,
                                          prob=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
                                                 0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,
                                                 0.90,0.95,0.99,1)))

#########################Scaling Output################


#####Find complete cases for Loan amount#############


SampleData <- subset(Data, Data$loan_amount <= 15000)

######################Gender #######################################################
SampleData$sex <- as.character(SampleData$sex)
SampleData$sex[(SampleData$sex == "F")] <- "1"
SampleData$sex[(SampleData$sex == "M")] <- "0"
SampleData$sex <- as.integer(SampleData$sex)

SampleData$sex[is.na(SampleData$sex)]<-3
#################Data cleaning and imputation########################################

SampleData$secondary_business <- as.character(SampleData$secondary_business)
SampleData$secondary_business[(SampleData$secondary_business == "NULL")] <- "NA"
SampleData$secondary_business[(SampleData$secondary_business == "Agriculture")] <- "1"
SampleData$secondary_business[(SampleData$secondary_business == "Daily wage labourer")] <- "2"
SampleData$secondary_business[(SampleData$secondary_business == "Livestock rearing")] <- "3"
SampleData$secondary_business[(SampleData$secondary_business == "none")] <- "0"
SampleData$secondary_business[(SampleData$secondary_business == "Others")] <- "4"
SampleData$secondary_business <- as.integer(SampleData$secondary_business)

SampleData$monthly_expenses <-as.numeric(as.character(SampleData$monthly_expenses)) #first convert each column into numeric if it is from factor
SampleData$monthly_expenses[is.na(SampleData$monthly_expenses)] = median(SampleData$monthly_expenses, na.rm=TRUE)


SampleData$annual_income <-as.numeric(as.character(SampleData$annual_income)) #first convert each column into numeric if it is from factor
SampleData$annual_income[is.na(SampleData$annual_income)] = median(SampleData$annual_income, na.rm=TRUE)

SampleData$occupants_count <-as.numeric(as.character(SampleData$occupants_count)) #first convert each column into numeric if it is from factor
SampleData$occupants_count[is.na(SampleData$occupants_count)] = median(SampleData$occupants_count, na.rm=TRUE)

SampleData$house_area <-as.numeric(as.character(SampleData$house_area)) #first convert each column into numeric if it is from factor
SampleData$house_area[is.na(SampleData$house_area)] = median(SampleData$house_area, na.rm=TRUE)

####Type of House#########
SampleData$type_of_house <- as.character(SampleData$type_of_house)
SampleData$type_of_house[SampleData$type_of_house == "R"] <- "0"
SampleData$type_of_house[SampleData$type_of_house == "T1"] <- "1"
SampleData$type_of_house[SampleData$type_of_house == "T2"] <- "2"
SampleData$type_of_house <- as.integer(SampleData$type_of_house)

##########HOME OWNERSHIP#######
SampleData$home_ownership <- as.integer(as.character(SampleData$home_ownership))

################Loan Purpose (converting from catagorical to numeric values and reducing the categories)#############

SampleData$loan_purpose <- as.character(SampleData$loan_purpose)

SampleData$loan_purpose[SampleData$loan_purpose %in% c('Tuition Centre', 'Food Items', 'Professional', 'Others', 'Vocational Loans', 'Training')] <- "1"
SampleData$loan_purpose[SampleData$loan_purpose %in% c('Agro Based Businesses', 'Tent Services', 'Tobacco Related Activities', 'Cyber Caf_','Transportation Services','Business Services - II', 'Retail Store', 'Eateries', 'Repair Services', 'Carpentery work', 'Laundry Services', 'Flower Business', 'Meat Businesses', 'Agarbatti Business', 'Business Services - I','Retail Sale', 'Utensil Selling', 'Recycling/ Waste Management', 'Cable TV Services', 'Animal husbandry', 'Apparels', 'Handicrafts', 'Sanitation', 'Artifical Jewellry Selling', 'Beauty Salon', 'Jewellry Shop', 'Manufacturing')] <- "2"
SampleData$loan_purpose[SampleData$loan_purpose %in% c('Education Loan')] <- "3"
SampleData$loan_purpose[SampleData$loan_purpose %in% c('Farming/ Agriculture')] <- "4"
SampleData$loan_purpose[SampleData$loan_purpose %in% c('Construction Related Activities')] <- "5"
SampleData$loan_purpose[SampleData$loan_purpose %in% c('#NA')] <- NA
SampleData$loan_purpose <- as.integer(SampleData$loan_purpose)


smp_size <- floor(0.75 * nrow(SampleData))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(SampleData)), size = smp_size)

#########################Splitting the given data into test and train datasets for initial validation####################################
TrainData <- SampleData[train_ind, ]
TestData <- SampleData[-train_ind, ]


############################Random Forest algorithm#########################################################################
library(randomForest)
fit <- randomForest(loan_amount ~   sex + annual_income + monthly_expenses + loan_tenure + loan_purpose, 
                    data=TrainData, 
                    importance=TRUE, 
                    ntree=500,
                    na.action=na.exclude)


TestData$pred <- predict(fit, TestData)
TestData$pred <- round(pred, digits = -2) #############Rounding the data to the nearest hundred#######################################

######################################RMSE CALCULATION####################################################
TestData$Error <- (abs(TestData$pred - TestData$loan_amount)/TestData$loan_amount) * 100

                     
