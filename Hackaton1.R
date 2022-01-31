library(readr)
library(ggplot2)
getwd()
setwd("C:\\Users\\juan_\\OneDrive\\Desktop")
diabetic_data <- read.csv("hult classes\\hackaton\\dataset_diabetes\\dataset_diabetes\\diabetic_data.csv",na.string ="?")
View(diabetic_data)
#create a DF out of the data
df_diab_data <- data.frame(diabetic_data)
df_diab_data

# Create the function mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# mode of race: Caucasian. The missing values were replaced by the mode
table(df_diab_data$race)
sum(is.na(df_diab_data$race)) #checking amount of missing values
getmode(df_diab_data$race)    #mode value for the variabe
df_diab_data$race[is.na(df_diab_data$race)] <- getmode(df_diab_data$race)
table(df_diab_data$race)      #amount count for each value
sum(is.na(df_diab_data$race)) #checking amount of missing values

# mode for gender: Female. The Unknown values were replaced by the mode
table(df_diab_data$gender)
sum(is.na(df_diab_data$gender))#checking amount of missing values
df_diab_data$gender[ df_diab_data$gender == "Unknown/Invalid" ] <- getmode(df_diab_data$gender)
table(df_diab_data$gender)     #checking amount of missing values

#special focus on this variable WE NEED TO DROP
# mode for weight: ? the rate of ? is 96% we dont have enough data to analyze. We have to drop
table(diabetic_data$weight)
sum(is.na(df_diab_data$weight))#checking amount of missing values
df_diab_data$weight[ is.na(df_diab_data$weight) ] <- "no weight"


# mode for payer_code: ?. Replace the missing values for MC, which is the second highest value. The difference between MC and ? was 2%
table(df_diab_data$payer_code)     #count each unique value
sum(is.na(df_diab_data$payer_code))#checking amount of missing values
df_diab_data$payer_code[ is.na(df_diab_data$payer_code) ] <- 0
table(df_diab_data$payer_code)     #count each unique value
sum(is.na(df_diab_data$payer_code))#checking amount of missing values

# mode for medical_specialty:?   will change the NA values for unknown. We could consider to drop the variable
table(df_diab_data$medical_specialty)     #count each unique value
sum(is.na(df_diab_data$medical_specialty))#checking amount of missing values
df_diab_data$medical_specialty[is.na(df_diab_data$medical_specialty)]<- "Unknown"


# 358 missing diagnosis. We discovered that there are several diagnosis codes. All the missing values will be changed for the mode 
# Research shows that diabetic related code are connected to 250 - 250.93
table(df_diab_data$diag_2)       #count each unique value
sum(is.na(df_diab_data$diag_2))  #checking amount of missing values
df_diab_data$diag_2[is.na(df_diab_data$diag_2)]<- 0

# 1423 missing diagnosis. We discovered that there are several diagnosis codes.All the missing values will be changed for the mode 
# Research shows that diabetic related code are connected to 250 - 250.93
table(df_diab_data$diag_3)       #count each unique value
df_diab_data$diag_3[is.na(df_diab_data$diag_3)]<- 0 
sum(is.na(df_diab_data$diag_3))  #checking amount of missing values

##############Watch out the fix weight
sum(is.na(df_diab_data$weight))

########Changing the race in to numeric variables###############
###########create new col_bin #org value#new_value #original col             
df_diab_data$race_bin <- gsub("Caucasian","1",df_diab_data$race)
###########new col_bin        #org value#new_value #new col
df_diab_data$race_bin <- gsub("Asian","3",df_diab_data$race_bin)
###########new col_bin        #org value#new_value #new col  just repeat
df_diab_data$race_bin <- gsub("AfricanAmerican","2",df_diab_data$race_bin)
df_diab_data$race_bin <- gsub("Hispanic","3",df_diab_data$race_bin)
df_diab_data$race_bin <- gsub("Other","3",df_diab_data$race_bin)
df_diab_data$race_bin <- as.numeric(df_diab_data$race_bin)
table(df_diab_data$race_bin)
hist(df_diab_data$race_bin)
?hist

##########admission_type###########
###################################
table(df_diab_data$admission_type_id)    # check the original count values of the variable
###########create new col_bin #org value#new_value #original col
df_diab_data$admission_type_bin <- gsub("1","1",df_diab_data$admission_type_id)
###########new col_bin        new bin 1 will be merging the old 1, 2 and 7 values due to they required treatment in less than 12 hours
df_diab_data$admission_type_bin <- gsub("2","1",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("7","1",df_diab_data$admission_type_bin)
###########new col_bin        new bin 2 will be merging the old 3 values due to they dont need treatment in less than 12 hours
df_diab_data$admission_type_bin <- gsub("3","2",df_diab_data$admission_type_bin)
###########new col_bin        new bin 3 will be merging the old 4 values due to is related to new born
df_diab_data$admission_type_bin <- gsub("4","3",df_diab_data$admission_type_bin)
###########new col_bin        new bin 4 will be merging the old 5, 6 and 8 values due to lack of information for their treatment
df_diab_data$admission_type_bin <- gsub("5","4",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("6","4",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("8","4",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- as.numeric(df_diab_data$admission_type_bin)
table(df_diab_data$admission_type_bin)
hist(df_diab_data$admission_type_bin)

###Check the information - discharge_disposition_id
table(df_diab_data$discharge_disposition_id)
df_diab_data$dis_dis_bin <-df_diab_data$discharge_disposition_id

#New bin 1 - Recover and only need daily care out of hosipital             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,8]== 1|df_diab_data[i,8]==3|df_diab_data[i,8]==4|
     df_diab_data[i,8]==6|df_diab_data[i,8]==8|df_diab_data[i,8]==24|
     df_diab_data[i,8]==27|df_diab_data[i,8]==30){
    df_diab_data[i,53]<- 1
  }else if(df_diab_data[i,8]== 2|df_diab_data[i,8]==5|df_diab_data[i,8]==9|
           df_diab_data[i,8]==10|df_diab_data[i,8]==12|df_diab_data[i,8]==15|
           df_diab_data[i,8]==16|df_diab_data[i,8]==17|df_diab_data[i,8]==22|
           df_diab_data[i,8]==23|df_diab_data[i,8]==28|df_diab_data[i,8]==29){
    df_diab_data[i,53] <-2
  }else if(df_diab_data[i,8]==11|df_diab_data[i,8]==13|df_diab_data[i,8]==14|
           df_diab_data[i,8]==19|df_diab_data[i,8]==20|df_diab_data[i,8]==21){
    df_diab_data[i,53] <-3
  }else{
    df_diab_data[i,53] <-4
  }
} 

#change the new value in new column as numeric variables
df_diab_data$dis_dis_bin <- as.numeric(df_diab_data$dis_dis_bin)
#Check the value in new column
table(df_diab_data$dis_dis_bin)
#visualization the frequency of the 
hist(df_diab_data$dis_dis_bin)

### Changing the payer_code in to numeric variables
###Check the information - payer_code
table(df_diab_data$payer_code)

###########create new col_bin for "Unknown" Payer Code                  
df_diab_data$payer_code_bin <- gsub("0","1",df_diab_data$payer_code) 

###########new col_bin for "Other Insurance" Payer Code     
df_diab_data$payer_code_bin <- gsub("BC","2",df_diab_data$payer_code_bin) 
df_diab_data$payer_code_bin <- gsub("CH","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("CM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("CP","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("DM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("FR","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("HM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("MD","2",df_diab_data$payer_code_bin) 
df_diab_data$payer_code_bin <- gsub("MP","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("OG","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("OT","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("PO","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("SI","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("UN","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("WC","2",df_diab_data$payer_code_bin)

###########new col_bin for "Medicare & Medicaid" Payer Code      
df_diab_data$payer_code_bin <- gsub("MC","3",df_diab_data$payer_code_bin) 

###########new col_bin for "Self-Pay" Payer Code                 
df_diab_data$payer_code_bin <- gsub("SP","4",df_diab_data$payer_code_bin) #Self-Pay

df_diab_data$payer_code_bin <- as.numeric(df_diab_data$payer_code_bin)
table(df_diab_data$payer_code_bin)
hist(df_diab_data$payer_code_bin)

###Changing the age in to numeric variables
table(df_diab_data$age)
#remove the "[" for age column to make sure the rest gsub code can work
df_diab_data$age <- gsub(pattern="\\[", replacement="", x= df_diab_data$age)
# This code us creating bins by 0 to 30 to be considered young 
df_diab_data$age_bin <- gsub("0-10)","1",df_diab_data$age)
#remove the "[" for age_bin column to make sure the rest gsub code can work
df_diab_data$age_bin <- gsub(pattern="\\[", replacement="", x= df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("10-20)","1",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("20-30)","1",df_diab_data$age_bin)
###########
# This code us creating bins by ages 30 to 60 to be considered middle aged
df_diab_data$age_bin <- gsub("30-40)","2",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("40-50)","2",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("50-60)","2",df_diab_data$age_bin)

# This code us creating bins by combining the ages 60 to 100 to be considered old aged 
df_diab_data$age_bin <- gsub("60-70)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("70-80)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("80-90)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("90-100)","3",df_diab_data$age_bin)

#change the new value in new column as numeric variables
df_diab_data$age_bin <- as.numeric(df_diab_data$age_bin)
#Check the value in new column
table(df_diab_data$age_bin)
#visualization the frequency of the
hist(df_diab_data$age_bin)


##########admission_referral###########

###Check the information - discharge_disposition_id
table(df_diab_data$admission_source_id)
df_diab_data$admission_referral_bin <-df_diab_data$admission_source_id
#New bin 1 - Recover and only need daily care out of hosipital             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,9]== 9|df_diab_data[i,9]==15|df_diab_data[i,9]==16|
     df_diab_data[i,9]==19|df_diab_data[i,9]==20){
    df_diab_data[i,56]<- 1
  }else if(df_diab_data[i,9]== 11|df_diab_data[i,9]==12|df_diab_data[i,9]==13|
           df_diab_data[i,9]==14|df_diab_data[i,9]==22|df_diab_data[i,9]==23){
    df_diab_data[i,56] <-2
  }else if(df_diab_data[i,9]== 1|df_diab_data[i,9]==2|df_diab_data[i,9]==3|
           df_diab_data[i,9]==17|df_diab_data[i,9]==18){
    df_diab_data[i,56] <-3
  }else if(df_diab_data[i,9]== 4|df_diab_data[i,9]==7|df_diab_data[i,9]==10|
           df_diab_data[i,9]==21|df_diab_data[i,9]==24|df_diab_data[i,9]==25){
    df_diab_data[i,56] <-4
  }else{
    df_diab_data[i,56] <-5
  }
} 

table(df_diab_data$admission_source_id)
#change the new value in new column as numeric variables
df_diab_data$admission_referral_bin <- as.numeric(df_diab_data$admission_referral_bin)
#Check the value in new column
table(df_diab_data$admission_referral_bin)
#visualization the frequency of the
hist(df_diab_data$admission_referral_bin)

table(df_diab_data$medical_specialty)
df_diab_data$medical_specialty_bin <-df_diab_data$medical_specialty
#New bin 1 - Recover and only need daily care out of hosipital             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,12]== "Emergency/Trauma"|df_diab_data[i,12]== "Family/General Practice"|
     df_diab_data[i,12]== "Cardiology"|df_diab_data[i,12]== "InternalMedicine"|
     df_diab_data[i,12]== "Surgery-Cardiovascular"|df_diab_data[i,12]== "Surgery-Cardiovascular/Thoracic"|
     df_diab_data[i,12]== "Surgery-Colon&Rectal"|df_diab_data[i,12]== "Surgery-General"|
     df_diab_data[i,12]== "Surgery-Maxillofacial"|df_diab_data[i,12]== "Surgery-Neuro"|
     df_diab_data[i,12]== "Surgery-Pediatric"|df_diab_data[i,12]== "Surgery-Plastic"|
     df_diab_data[i,12]== "Surgery-PlasticwithinHeadandNeck"|df_diab_data[i,12]== "Surgery-Thoracic"|
     df_diab_data[i,12]== "Surgery-Vascular"|df_diab_data[i,12]== "SurgicalSpecialty"|
     df_diab_data[i,12]== "Surgeon"|df_diab_data[i,12]== "Pediatrics"|
     df_diab_data[i,12]== "Pediatrics-AllergyandImmunology"|df_diab_data[i,12]== "Pediatrics-CriticalCare"|
     df_diab_data[i,12]== "Pediatrics-EmergencyMedicine"|df_diab_data[i,12]== "Pediatrics-Endocrinology"|
     df_diab_data[i,12]== "Pediatrics-Hematology-Oncology"|df_diab_data[i,12]== "Pediatrics-InfectiousDiseases"|
     df_diab_data[i,12]== "Pediatrics-Neurology"|df_diab_data[i,12]== "Pediatrics-Pulmonology"|
     df_diab_data[i,12]== "Anesthesiology-Pediatric"|df_diab_data[i,12]== "Cardiology-Pediatric"|
     df_diab_data[i,12]== "Orthopedics-Reconstructive"|df_diab_data[i,12]== "Orthopedics"|
     df_diab_data[i,12]== "Radiologist"|df_diab_data[i,12]== "Radiology"){
    df_diab_data[i,57]<- 1
  }else if(df_diab_data[i,12]=="Unknown"){
    df_diab_data[i,57] <-3
  }else{
    df_diab_data[i,57] <-2
  }
} 

table(df_diab_data$medical_specialty_bin)
#change the new value in new column as numeric variables
df_diab_data$medical_specialty_bin <- as.numeric(df_diab_data$medical_specialty_bin)
#Check the value in new column
table(df_diab_data$medical_specialty_bin)
#visualization the frequency of the
hist(df_diab_data$medical_specialty_bin)


new_df <- df_diab_data[,51:57]

install.packages("Hmisc")
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
cor(new_df)

num_df <- df_diab_data[,13:18]
cor(num_df)
table(df_diab_data$diabetesMed)


##########Changing medicines to numeric information############
##########0 is for No taking the medicine######################
##########1 is for Down, is reducing the dose of the medicine##
##########2 is for Steady, is taking the same dose of medicine#
##########3 is for Up, is increasing the dose of the medicine##

##########metformin############################################
table(df_diab_data$metformin)
df_diab_data$metformin_bin <- gsub("No","0",df_diab_data$metformin)
df_diab_data$metformin_bin <- gsub("Down","1",df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- gsub("Steady","2",df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- gsub("Up","3",df_diab_data$metformin_bin)
table(df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- as.numeric(df_diab_data$metformin_bin)
hist(df_diab_data$metformin_bin)

##########repaglinide############################################
table(df_diab_data$repaglinide)
df_diab_data$repaglinide_bin <- gsub("No","0",df_diab_data$repaglinide)
df_diab_data$repaglinide_bin <- gsub("Down","1",df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- gsub("Steady","2",df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- gsub("Up","3",df_diab_data$repaglinide_bin)
table(df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- as.numeric(df_diab_data$repaglinide_bin)
hist(df_diab_data$repaglinide_bin)

##########nateglinide############################################
table(df_diab_data$nateglinide)
df_diab_data$nateglinide_bin <- gsub("No","0",df_diab_data$nateglinide)
df_diab_data$nateglinide_bin <- gsub("Down","1",df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- gsub("Steady","2",df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- gsub("Up","3",df_diab_data$nateglinide_bin)
table(df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- as.numeric(df_diab_data$nateglinide_bin)
hist(df_diab_data$nateglinide_bin)

##########chlorpropamide############################################
table(df_diab_data$chlorpropamide)
df_diab_data$chlorpropamide_bin <- gsub("No","0",df_diab_data$chlorpropamide)
df_diab_data$chlorpropamide_bin <- gsub("Down","1",df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- gsub("Steady","2",df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- gsub("Up","3",df_diab_data$chlorpropamide_bin)
table(df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- as.numeric(df_diab_data$chlorpropamide_bin)
hist(df_diab_data$chlorpropamide_bin)

##########glimepirida############################################
table(df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("No","0",df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("Down","1",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Steady","2",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Up","3",df_diab_data$glimepiride_bin)
table(df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- as.numeric(df_diab_data$glimepiride_bin)
hist(df_diab_data$glimepiride_bin)

##########glimepirida############################################
table(df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("No","0",df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("Down","1",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Steady","2",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Up","3",df_diab_data$glimepiride_bin)
table(df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- as.numeric(df_diab_data$glimepiride_bin)
hist(df_diab_data$glimepiride_bin)

##########acetohexamide############################################
table(df_diab_data$acetohexamide)
df_diab_data$acetohexamide_bin <- gsub("No","0",df_diab_data$acetohexamide)
df_diab_data$acetohexamide_bin <- gsub("Down","1",df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- gsub("Steady","2",df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- gsub("Up","3",df_diab_data$acetohexamide_bin)
table(df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- as.numeric(df_diab_data$acetohexamide_bin)
hist(df_diab_data$acetohexamide_bin)

##########glipizide############################################
table(df_diab_data$glipizide)
df_diab_data$glipizide_bin <- gsub("No","0",df_diab_data$glipizide)
df_diab_data$glipizide_bin <- gsub("Down","1",df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- gsub("Steady","2",df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- gsub("Up","3",df_diab_data$glipizide_bin)
table(df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- as.numeric(df_diab_data$glipizide_bin)
hist(df_diab_data$glipizide_bin)

##########glyburide############################################
table(df_diab_data$glyburide)
df_diab_data$glyburide_bin <- gsub("No","0",df_diab_data$glyburide)
df_diab_data$glyburide_bin <- gsub("Down","1",df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- gsub("Steady","2",df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- gsub("Up","3",df_diab_data$glyburide_bin)
table(df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- as.numeric(df_diab_data$glyburide_bin)
hist(df_diab_data$glyburide_bin)

##########tolbutamide############################################
table(df_diab_data$tolbutamide)
df_diab_data$tolbutamide_bin <- gsub("No","0",df_diab_data$tolbutamide)
df_diab_data$tolbutamide_bin <- gsub("Down","1",df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- gsub("Steady","2",df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- gsub("Up","3",df_diab_data$tolbutamide_bin)
table(df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- as.numeric(df_diab_data$tolbutamide_bin)
hist(df_diab_data$tolbutamide_bin)

##########pioglitazone############################################
table(df_diab_data$pioglitazone)
df_diab_data$pioglitazone_bin <- gsub("No","0",df_diab_data$pioglitazone)
df_diab_data$pioglitazone_bin <- gsub("Down","1",df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- gsub("Steady","2",df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- gsub("Up","3",df_diab_data$pioglitazone_bin)
table(df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- as.numeric(df_diab_data$pioglitazone_bin)
hist(df_diab_data$pioglitazone_bin)

##########rosiglitazone############################################
table(df_diab_data$rosiglitazone)
df_diab_data$rosiglitazone_bin <- gsub("No","0",df_diab_data$rosiglitazone)
df_diab_data$rosiglitazone_bin <- gsub("Down","1",df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- gsub("Steady","2",df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- gsub("Up","3",df_diab_data$rosiglitazone_bin)
table(df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- as.numeric(df_diab_data$rosiglitazone_bin)
hist(df_diab_data$rosiglitazone_bin)

##########acarbose############################################
table(df_diab_data$acarbose)
df_diab_data$acarbose_bin <- gsub("No","0",df_diab_data$acarbose)
df_diab_data$acarbose_bin <- gsub("Down","1",df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- gsub("Steady","2",df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- gsub("Up","3",df_diab_data$acarbose_bin)
table(df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- as.numeric(df_diab_data$acarbose_bin)
hist(df_diab_data$acarbose_bin)

##########miglitol############################################
table(df_diab_data$miglitol)
df_diab_data$miglitol_bin <- gsub("No","0",df_diab_data$miglitol)
df_diab_data$miglitol_bin <- gsub("Down","1",df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- gsub("Steady","2",df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- gsub("Up","3",df_diab_data$miglitol_bin)
table(df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- as.numeric(df_diab_data$miglitol_bin)
hist(df_diab_data$miglitol_bin)

##########troglitazone############################################
table(df_diab_data$troglitazone)
df_diab_data$troglitazone_bin <- gsub("No","0",df_diab_data$troglitazone)
df_diab_data$troglitazone_bin <- gsub("Down","1",df_diab_data$troglitazone_bin)
df_diab_data$troglitazone_bin <- gsub("Steady","2",df_diab_data$troglitazone_bin )
df_diab_data$troglitazone_bin <- gsub("Up","3",df_diab_data$troglitazone_bin)
table(df_diab_data$troglitazone_bin)
df_diab_data$troglitazone_bin <- as.numeric(df_diab_data$troglitazone_bin)
hist(df_diab_data$troglitazone_bin)

##########tolazamide############################################
table(df_diab_data$tolazamide)
df_diab_data$tolazamide_bin <- gsub("No","0",df_diab_data$tolazamide)
df_diab_data$tolazamide_bin <- gsub("Down","1",df_diab_data$tolazamide_bin)
df_diab_data$tolazamide_bin <- gsub("Steady","2",df_diab_data$tolazamide_bin )
df_diab_data$tolazamide_bin <- gsub("Up","3",df_diab_data$tolazamide_bin)
table(df_diab_data$tolazamide_bin)
df_diab_data$tolazamide_bin <- as.numeric(df_diab_data$tolazamide_bin)
hist(df_diab_data$tolazamide_bin)

##########examide############################################
table(df_diab_data$examide)
df_diab_data$examide_bin <- gsub("No","0",df_diab_data$examide)
df_diab_data$examide_bin <- gsub("Down","1",df_diab_data$examide_bin)
df_diab_data$examide_bin <- gsub("Steady","2",df_diab_data$examide_bin )
df_diab_data$examide_bin <- gsub("Up","3",df_diab_data$examide_bin)
table(df_diab_data$examide_bin)
df_diab_data$examide_bin <- as.numeric(df_diab_data$examide_bin)
hist(df_diab_data$examide_bin)

##########citogliptone############################################
table(df_diab_data$citoglipton)
df_diab_data$citoglipton_bin <- gsub("No","0",df_diab_data$citoglipton)
df_diab_data$citoglipton_bin <- gsub("Down","1",df_diab_data$citoglipton_bin)
df_diab_data$citoglipton_bin <- gsub("Steady","2",df_diab_data$citoglipton_bin )
df_diab_data$citoglipton_bin <- gsub("Up","3",df_diab_data$citoglipton_bin)
table(df_diab_data$citoglipton_bin)
df_diab_data$citoglipton_bin <- as.numeric(df_diab_data$citoglipton_bin)
hist(df_diab_data$citoglipton_bin)

##########insuline############################################
table(df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("No","0",df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("Down","1",df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- gsub("Steady","2",df_diab_data$insulin_bin )
df_diab_data$insulin_bin <- gsub("Up","3",df_diab_data$insulin_bin)
table(df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- as.numeric(df_diab_data$insulin_bin)
hist(df_diab_data$insulin_bin)

##########insuline############################################
table(df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("No","0",df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("Down","1",df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- gsub("Steady","2",df_diab_data$insulin_bin )
df_diab_data$insulin_bin <- gsub("Up","3",df_diab_data$insulin_bin)
table(df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- as.numeric(df_diab_data$insulin_bin)
hist(df_diab_data$insulin_bin)

##########glyburide-metformin############################################
table(df_diab_data$glyburide.metformin)
df_diab_data$glyburide.metformin_bin <- gsub("No","0",df_diab_data$glyburide.metformin)
df_diab_data$glyburide.metformin_bin <- gsub("Down","1",df_diab_data$glyburide.metformin_bin)
df_diab_data$glyburide.metformin_bin <- gsub("Steady","2",df_diab_data$glyburide.metformin_bin )
df_diab_data$glyburide.metformin_bin <- gsub("Up","3",df_diab_data$glyburide.metformin_bin)
table(df_diab_data$glyburide.metformin_bin)
df_diab_data$glyburide.metformin_bin <- as.numeric(df_diab_data$glyburide.metformin_bin)
hist(df_diab_data$glyburide.metformin_bin)

##########blipizide-metformin############################################
table(df_diab_data$glipizide.metformin)
df_diab_data$glipizide.metformin_bin <- gsub("No","0",df_diab_data$glipizide.metformin)
df_diab_data$glipizide.metformin_bin <- gsub("Down","1",df_diab_data$glipizide.metformin_bin)
df_diab_data$glipizide.metformin_bin <- gsub("Steady","2",df_diab_data$glipizide.metformin_bin )
df_diab_data$glipizide.metformin_bin <- gsub("Up","3",df_diab_data$glipizide.metformin_bin)
table(df_diab_data$glipizide.metformin_bin)
df_diab_data$glipizide.metformin_bin <- as.numeric(df_diab_data$glipizide.metformin_bin)
hist(df_diab_data$glipizide.metformin_bin)

##########glimepiride-pioglitazone############################################
table(df_diab_data$glimepiride.pioglitazone)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("No","0",df_diab_data$glimepiride.pioglitazone)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Down","1",df_diab_data$glimepiride.pioglitazone_bin)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Steady","2",df_diab_data$glimepiride.pioglitazone_bin )
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Up","3",df_diab_data$glimepiride.pioglitazone_bin)
table(df_diab_data$glimepiride.pioglitazone_bin)
df_diab_data$glimepiride.pioglitazone_bin <- as.numeric(df_diab_data$glimepiride.pioglitazone_bin)
hist(df_diab_data$glimepiride.pioglitazone_bin)

##########metformin-rosiglitazone############################################
table(df_diab_data$metformin.rosiglitazone)
df_diab_data$metformin.rosiglitazone_bin <- gsub("No","0",df_diab_data$metformin.rosiglitazone)
df_diab_data$metformin.rosiglitazone_bin <- gsub("Down","1",df_diab_data$metformin.rosiglitazone_bin)
df_diab_data$metformin.rosiglitazone_bin <- gsub("Steady","2",df_diab_data$metformin.rosiglitazone_bin )
df_diab_data$metformin.rosiglitazone_bin <- gsub("Up","3",df_diab_data$metformin.rosiglitazone_bin)
table(df_diab_data$metformin.rosiglitazone_bin)
df_diab_data$metformin.rosiglitazone_bin <- as.numeric(df_diab_data$metformin.rosiglitazone_bin)
hist(df_diab_data$metformin.rosiglitazone_bin)

##########metformin-pioglitazone############################################
table(df_diab_data$metformin.pioglitazone)
df_diab_data$metformin.pioglitazone_bin <- gsub("No","0",df_diab_data$metformin.pioglitazone)
df_diab_data$metformin.pioglitazone_bin <- gsub("Down","1",df_diab_data$metformin.pioglitazone_bin)
df_diab_data$metformin.pioglitazone_bin <- gsub("Steady","2",df_diab_data$metformin.pioglitazone_bin )
df_diab_data$metformin.pioglitazone_bin <- gsub("Up","3",df_diab_data$metformin.pioglitazone_bin)
table(df_diab_data$metformin.pioglitazone_bin)
df_diab_data$metformin.pioglitazone_bin <- as.numeric(df_diab_data$metformin.pioglitazone_bin)
hist(df_diab_data$metformin.pioglitazone_bin)

my_scatter <- ggplot(data = df_diab_data, aes(x = age_bin , y = diabetesMed ))+
  geom_point()+ geom_smooth()+geom_jitter()

my_scatter
