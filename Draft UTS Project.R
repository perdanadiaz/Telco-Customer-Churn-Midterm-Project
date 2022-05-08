library(dplyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(stringr)
library(ggcorrplot)
library(GGally)
library(rlang)
library(caret)
library(MASS)
library(car)
library(InformationValue)
library(plyr)
library(float)
library(scales)
library(gridExtra)
library(grid)
library(tidyverse
        )

#1. Data Overview
datatelco = read.csv("D:/Data 1/Magister/Big Data Analytics/Tugas/UTS/Data 2-Big Data Project.csv",stringsAsFactors = TRUE)
str(datatelco)
#2. Pre-Processing Data
#Missing Cust ID
datatelco = datatelco[,-1]
#Additional Column ID
datatelco$id = c(1:nrow(datatelco))
datatelco = datatelco[,c(21,1:20)]
#Correction the unstructured names
datatelco = clean_names(datatelco)
head(datatelco)
summary(datatelco)
str(datatelco)
sapply(datatelco, typeof)
#Now cleaning the missing data, and there is some missing values from total charges
sapply(datatelco[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))
#Cleaning the missing data
datatelco = datatelco[!is.na(datatelco$total_charges),]
charges = datatelco
dim(charges)
#convert total_charges to float type (currently a num)
fl(datatelco$total_charges)
#replace 'No Internet Service' to 'No' for the following columns:
datatelco[datatelco == 'No internet service'] = 'No'
datatelco[datatelco == 'No phone service'] = 'No'
#replace values 1 and 0 with Yes and No for senior_citizen
datatelco$senior_citizen[datatelco$senior_citizen == 1] = 'Yes'
datatelco$senior_citizen[datatelco$senior_citizen == 0] = 'No'
head(datatelco)
#Change tenure to a categorical column
library(dplyr)
datatelco = (mutate(datatelco, tenure_group = ifelse(datatelco$tenure %in% 0:12, "Tenure(0-12)",
                                                  ifelse(datatelco$tenure %in% 13:24, "Tenure(13-24)",
                                                         ifelse(datatelco$tenure %in% 25:36, "Tenure(25-36)",
                                                                ifelse(datatelco$tenure %in% 37:48, "Tenure(37-48)",
                                                                       ifelse(datatelco$tenure %in% 49:60, "Tenure(49-60)","Tenure_gt_60")))))))
#convert new tenure_group from a character column to a factor column
datatelco$tenure_group = as.factor(datatelco$tenure_group)

#splitting churn and non-churn data 
churn = filter(datatelco, churn == "Yes")
non_churn = filter(datatelco, churn == "No")

### Exploratory Analysis 
library(ggplot2)
library(scales)
library(dplyr)
library(plyr)

#Before Analysis, Check Outlier for numeric data (monthly_charges and total_charges)
options(repr.plot.width=30, repr.plot.height=20)
hist(datatelco$monthly_charges,main="Monthly_Charges Histogram",col = "dimgray",xlab="Monthly_charges",ylab="Count")
hist(datatelco$total_charges,main="Total_Charges Histogram",col = "dimgray",xlab="Total_charges",ylab="Count")
hist(datatelco$tenure,main="Tenure Histogram",col = "dimgray",xlab="Tenure",ylab="Count") 

#churn V Non-churn Plot
churn = filter(datatelco, churn == "Yes")
non_churn = filter(datatelco, churn == "No")
churn_plot = ggplot(datatelco, aes(x=factor(churn))) +
  geom_bar(fill="dimgray", width = .75) +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Churn') +
  xlab('') +
  ylab('Clients')
churn_plot


# gender_plot
female = filter(datatelco, gender == "Female")
male = filter(datatelco, gender == "Male")
female_plot = ggplot(data=female, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Female Clients') +
  xlab('Churn') +
  ylab('Clients')

male_plot = ggplot(data=male, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Male Clients') +
  xlab('Churn') +
  ylab('Clients')

# senior_plot
senior = filter(datatelco, senior_citizen == "Yes")
non_senior = filter(datatelco, senior_citizen == "No")
senior_citizen_plot = ggplot(data=senior, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Senior Clients') +
  xlab('Churn') +
  ylab('Clients')

non_senior_plot = ggplot(data=non_senior, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Non Senior Clients') +
  xlab('Churn') +
  ylab('Clients')

#partner plot
partner = filter(datatelco, partner == "Yes")
single = filter(datatelco, partner == "No")
partnership_plot = ggplot(data=partner, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients with partner') +
  xlab('Churn') +
  ylab('Clients')

single_plot = ggplot(data=single, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Single Clients') +
  xlab('Churn') +
  ylab('Clients')

# dependents_plot
dependents = filter(datatelco, dependents  == "Yes")
no_dependents = filter(datatelco, dependents  == "No")
dep_plot = ggplot(data=dependents, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients with dependents ') +
  xlab('Churn') +
  ylab('Clients')

no_dep_plot = ggplot(data=no_dependents, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients without dependents ') +
  xlab('Churn') +
  ylab('Clients')

# phone_plot
phone = filter(datatelco, phone_service  == "Yes")
no_phone = filter(datatelco, phone_service  == "No")
phoneservice_plot = ggplot(data=phone, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Phone Service') +
  xlab('Churn') +
  ylab('Clients')

no_phoneservice_plot = ggplot(data=no_phone, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Phone Service') +
  xlab('Churn') +
  ylab('Clients')

#Multiple Lines 
mult_lines = filter(datatelco, multiple_lines == "Yes")
one_line = filter(datatelco, multiple_lines == "No")
mult_lines_plot = ggplot(data=mult_lines, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Multiple Phone Lines') +
  xlab('Churn') +
  ylab('Clients')

one_line_plot = ggplot(data=one_line, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With One Phone Line') +
  xlab('Churn') +
  ylab('Clients')

#Internet
fiber = filter(datatelco, internet_service == "Fiber optic")
dsl = filter(datatelco, internet_service == "DSL")
no_internet_service = filter(datatelco, internet_service == "No")
fiber_plot = ggplot(data=fiber, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Fiber Optic') +
  xlab('Churn') +
  ylab('Clients')
dsl_plot = ggplot(data=dsl, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With DSL') +
  xlab('Churn') +
  ylab('Clients')
no_internet_service_plot = ggplot(data=no_internet_service, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Internet Service') +
  xlab('Churn') +
  ylab('Clients')

#Online Security
online_security = filter(datatelco, online_security == "Yes")
no_online_security = filter(datatelco, online_security == "No")
online_backup = filter(datatelco, online_backup == "Yes")
no_online_backup = filter(datatelco, online_backup == "No")

online_security_plot = ggplot(data=online_security, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Online Security') +
  xlab('Churn') +
  ylab('Clients')
no_online_security_plot = ggplot(data=no_online_security, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Online Security') +
  xlab('Churn') +
  ylab('Clients')
online_backup_plot = ggplot(data=online_backup, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Online Backup') +
  xlab('Churn') +
  ylab('Clients')
no_online_backup_plot = ggplot(data=no_online_backup, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Online Backup') +
  xlab('Churn') +
  ylab('Clients')

#Device Protection
device_protection = filter(datatelco, device_protection == "Yes")
no_device_protection = filter(datatelco, device_protection == "No")
protection_plot = ggplot(data=device_protection, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Device Protection') +
  xlab('Churn') +
  ylab('Clients')

no_protection_plot = ggplot(data=no_device_protection, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Device Protection') +
  xlab('Churn') +
  ylab('Clients')

#Tech Support
tech_support = filter(datatelco, tech_support  == "Yes")
no_tech_support = filter(datatelco, tech_support  == "No")
tech_support_plot = ggplot(data=tech_support, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Tech Support') +
  xlab('Churn') +
  ylab('Clients')

no_tech_support_plot = ggplot(data=no_tech_support, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Tech Support') +
  xlab('Churn') +
  ylab('Clients')

#Streaming TV
stream_tv = filter(datatelco, streaming_tv  == "Yes")
no_stream_tv = filter(datatelco, streaming_tv  == "No")
stream_tv_plot = ggplot(data=stream_tv, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Streaming TV') +
  xlab('Churn') +
  ylab('Clients')

no_stream_tv_plot = ggplot(data=no_stream_tv, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Streaming TV') +
  xlab('Churn') +
  ylab('Clients')

#Streaming Movies 
stream_movies = filter(datatelco, streaming_movies == "Yes")
no_stream_movies = filter(datatelco, streaming_movies == "No")
stream_movies_plot = ggplot(data=stream_movies, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Streaming Movies') +
  xlab('Churn') +
  ylab('Clients')

no_stream_movies_plot = ggplot(data=no_stream_movies, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Streaming Movies') +
  xlab('Churn') +
  ylab('Clients')

#contract Distribution
month_contract = filter(datatelco, contract == "Month-to-month")
one_year_contract = filter(datatelco, contract == "One year")
two_year_contract = filter(datatelco, contract == "Two year")
month_contract_plot = ggplot(data=month_contract, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Month-to-Month contract') +
  xlab('Churn') +
  ylab('Clients')

one_year_contract_plot = ggplot(data=one_year_contract, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With One Year contract') +
  xlab('Churn') +
  ylab('Clients')

two_year_contract_plot = ggplot(data=two_year_contract, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Two Year contract') +
  xlab('Churn') +
  ylab('Clients')

#Paperless Billing
paperless_billing = filter(datatelco, paperless_billing == "Yes")
no_paperless_billing = filter(datatelco, paperless_billing == "No")

paperless_billing_plot = ggplot(data=paperless_billing, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients With Paperless Billing') +
  xlab('Churn') +
  ylab('Clients')
no_paperless_billing_plot = ggplot(data=no_paperless_billing, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Without Paperless Billing') +
  xlab('Churn') +
  ylab('Clients')

#Payment Method
bank_transfer = filter(datatelco, payment_method == "Bank transfer (automatic)")
credit_card = filter(datatelco, payment_method == "Credit card (automatic)")
electronic_check = filter(datatelco, payment_method == "Electronic check")
mailed_check = filter(datatelco, payment_method == "Mailed check")

bank_transfer_plot= ggplot(data=bank_transfer, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Paying by Bank Transfer (automatic)') +
  xlab('Churn') +
  ylab('Clients')
credit_card_plot = ggplot(data=credit_card, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Paying with Credit Card (automatic)') +
  xlab('Churn') +
  ylab('Clients')
electronic_check_plot = ggplot(data=electronic_check, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Paying with Electronic Check') +
  xlab('Churn') +
  ylab('Clients')
mailed_check_plot = ggplot(data=mailed_check, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients Paying with Mailed Check') +
  xlab('Churn') +
  ylab('Clients')

#Tenure Distribution
tenure_12 = filter(datatelco, tenure_group == "Tenure(0-12)")
tenure_24 = filter(datatelco, tenure_group == "Tenure(13-24)")
tenure_36 = filter(datatelco, tenure_group == "Tenure(25-36)")
tenure_48 = filter(datatelco, tenure_group == "Tenure(37-48)")
tenure_60 = filter(datatelco, tenure_group == "Tenure(49-60)")
tenure_over_60 = filter(datatelco, tenure_group == "Tenure_gt_60")

tenure_12_plot= ggplot(data=tenure_12, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('12 Months or Less') +
  xlab('Churn') +
  ylab('Clients')
tenure_24_plot= ggplot(data=tenure_24, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('12 to 24 Months') +
  xlab('Churn') +
  ylab('Clients')
tenure_36_plot= ggplot(data=tenure_36, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('24 to 36 Months') +
  xlab('Churn') +
  ylab('Clients')
tenure_48_plot = ggplot(data=tenure_48, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('36 to 48 Months') +
  xlab('Churn') +
  ylab('Clients')
tenure_60_plot = ggplot(data=tenure_60, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('48 to 60 Months') +
  xlab('Churn') +
  ylab('Clients')
tenure_over_60_plot = ggplot(data=tenure_over_60, aes(x=churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='dimgray') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('over 60 Months') +
  xlab('Churn') +
  ylab('Clients')

#Gender Distribution in Customer Behaviour
grid.arrange(female_plot, 
             male_plot,
             top=textGrob("Gender Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Marital Status Distribution in Customer Behaviour
grid.arrange(partnership_plot, 
             single_plot, 
             top=textGrob("Marital Status Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#dependents  Distribution in Customer Behaviour
grid.arrange(dep_plot, 
             no_dep_plot,
             top=textGrob("Dependents  Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Senior Citizen Distribution in Customer Behaviour
grid.arrange(senior_citizen_plot, 
             non_senior_plot,
             top=textGrob("Senior Citizen Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Phone Service Distribution in Customer Behaviour
grid.arrange(phoneservice_plot,
             no_phoneservice_plot,
             mult_lines_plot,
             one_line_plot,
             top=textGrob("Phone Service Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Internet Service Distribution in Customer Behaviour
grid.arrange(fiber_plot,
             dsl_plot,
             no_internet_service_plot,
             top=textGrob("Internet Service Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Online Security Distribution in Customer Behaviour
grid.arrange(online_security_plot,
             no_online_security_plot,
             top=textGrob("Online Security Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Online Backup Distribution in Customer Behaviour
grid.arrange(online_backup_plot,
             no_online_backup_plot, 
             top=textGrob("OnlineBackup Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Device Protection Distribution in Customer Behaviour
grid.arrange(protection_plot,
             no_protection_plot,
             top=textGrob("Device Protection Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Tech Support Distribution in Customer Behaviour
grid.arrange(tech_support_plot,
             no_tech_support_plot, 
             top=textGrob("Tech Support Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Streaming Distribution in Customer Behaviour
grid.arrange(stream_tv_plot,
             no_stream_tv_plot,
             stream_movies_plot,
             no_stream_movies_plot, 
             top=textGrob("Streaming Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#contract Distribution in Customer Behaviour
grid.arrange(month_contract_plot,
             one_year_contract_plot,
             two_year_contract_plot, 
             top=textGrob("Contract Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Billing Distribution in Customer Behaviour
grid.arrange(paperless_billing_plot,
             no_paperless_billing_plot,
             top=textGrob("Billing Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Payment Distribution in Customer Behaviour
grid.arrange(bank_transfer_plot,
             credit_card_plot,
             electronic_check_plot,
             mailed_check_plot,
             top=textGrob("Payment Distribution in Customer Behaviour", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Tenure Distribution in Customer Behaviour
grid.arrange(tenure_12_plot,
             tenure_24_plot,
             tenure_36_plot,
             tenure_48_plot,
             tenure_60_plot,
             tenure_over_60_plot, 
             top=textGrob("Tenure Distribution by Time Period", gp=gpar(fontsize=20,font=2)),
             ncol=2)
#Tenure Distribution Histogram
ggplot(datatelco, aes(x=tenure_group, fill=churn)) +
  geom_histogram(
    stat = 'count',
    position=position_dodge(1)) +
  ggtitle('Tenure Distribution Histogram') +
  xlab('Tenure (months)') +
  ylab('Clients') +
  theme_minimal()

#Monthly Charges Histogram
ggplot(datatelco, aes(x=round(monthly_charges, digits=0),
                      y = (..count..)/sum(..count..),
                      fill=churn))+
  geom_histogram(stat = 'bin',
                 bins = 30, 
                 position=position_dodge()) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('Montly Charges Distribution Histogram') +
  xlab('Charges (USD)') +
  ylab('Clients (percent)') +
  theme_minimal()

#Total Charges Histogram
ggplot(datatelco, aes(x=round(total_charges, digits=0),
                      y = (..count..)/sum(..count..),
                      fill=churn))+
  geom_histogram(stat = 'bin',
                 bins = 30, 
                 position=position_dodge()) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('Total Charges Distribution Histogram') +
  xlab('Charges (USD)') +
  ylab('Clients (percent)') +
  theme_minimal()




#Preparing Model
set.seed(1000)
rows <- sample(nrow(charges))
data_1 <- charges
data_shuffled <- charges [rows,]

#Clean Categorical Variables
data_shuffled <- data.frame(lapply(data_shuffled, function(x) {
  gsub("No internet service", "No", x)}))

data_shuffled <- data.frame(lapply(data_shuffled, function(x) {
  gsub("No phone service", "No", x)}))

data_shuffled[,c(1,6,19,20)] <- lapply(data_shuffled[,c(1,6,19,20)],as.numeric)


head(data_shuffled)

#Create Dummy Variables
d_int <-data_shuffled[,c(1,6,19,20)]
d <- data_shuffled[,-c(1,6,19,20)]

dummy <- data.frame(sapply(data_shuffled[,-c(6,19,20)],function(x) data.frame(model.matrix(~x-1,data =data_shuffled[,-c(6,19,20)]))[,-1]))
head(dummy)

#Standardize Continuous Variables.
#I will standardize tenure and total charges variables only as the only numeric variables to be i will use in the model.
d_int[,c(2,3,4)] <- scale(d_int[,c(2,3,4)]) 
str_glue("Mean of Tenure : {round(mean(d_int$tenure),2)} | Standard deviation of tenure : {round(sd(d_int$tenure),2)}")
str_glue("Mean of Total Charges : {round(mean(d_int$total_charges),2)} | Standard deviation of total_charges : {round(sd(d_int$total_charges),2)}")
str_glue("Mean of Monthly Charges : {round(mean(d_int$monthly_charges),2)} | Standard deviation of monthly charges : {round(sd(d_int$monthly_charges),2)}")

#Final Dataset
final_data <- cbind(d_int , dummy) # remove monthly charges continous variable
head(final_data)

#Split Data
set.seed(1000)
split <- createDataPartition(y = final_data$churn,p=0.70,list = FALSE)

training_set<-final_data[split,]
testing_set<-final_data[-split,]

# check data balance
table(training_set$churn)
table(testing_set$churn)

#Logistic Regression
model_1 <- glm(churn~.,data = training_set,family = binomial(link = "logit"))
summary(model_1)
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
formula(model_2)
(vif_vars <- as.data.frame(vif(model_2)))
model_3 =glm(churn ~ tenure + senior_citizen + 
               dependents + phone_service + multiple_lines + internet_service.xFiber.optic + 
               internet_service.xNo + device_protection + streaming_tv + 
               streaming_movies + contract.xOne.year + contract.xTwo.year + 
               paperless_billing + payment_method.xElectronic.check,data = training_set,family = "binomial")
summary(model_3)
model_4= stepAIC(model_3,direction = "both")
summary(model_4)
formula(model_4)
(vif_vars2 <- as.data.frame(vif(model_4)))

model_5 =glm(churn ~ tenure + senior_citizen + dependents + 
               phone_service + multiple_lines + internet_service.xFiber.optic + 
               internet_service.xNo + online_backup + streaming_tv + contract.xOne.year + 
               contract.xTwo.year + paperless_billing + payment_method.xElectronic.check,data = training_set,family = "binomial")
summary(model_5)
model_6= stepAIC(model_5,direction = "both")
summary(model_6)
formula(model_6)
(vif_vars3 <- as.data.frame(vif(model_6)))

summary(model_2)
summary(model_4)


pred_train <- predict(model_4,newdata = training_set[,which(colnames(training_set)!= "churn")],type = "response")
pred <- predict(model_4,newdata = testing_set[,which(colnames(testing_set)!= "churn")],type = "response")
summary(pred)

# Check model train data
pred_trainchurn <- factor(ifelse(pred_train >= 0.50, "Yes", "No"))
actual_trainchurn <- factor(ifelse(training_set$churn==1,"Yes","No"))

table(actual_trainchurn,pred_trainchurn)

# check model test
pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(testing_set$churn==1,"Yes","No"))

table(actual_churn,pred_churn)

caret::confusionMatrix(pred_trainchurn,actual_trainchurn,positive = "No")
caret::confusionMatrix(pred_churn,actual_churn,positive = "No")

(optimal_cutoff <- optimalCutoff(testing_set$churn , pred))
pred_churn_60 <- factor(ifelse(pred >= 0.6006725,'Yes','No'))

caret::confusionMatrix(actual_churn,pred_churn_60,positive = 'No')
