######################## ElecKart MMM #########################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#MMM model building 
#Recommendation with the model output
################################################################

### Business Understanding:

# ElecKart, a eCommerce company which sells variety of products online to the customers wants to do a tap on sales and revenue over marketing.
# Company had spent significant amount on marketing in the last few years and the management thinks its not effective enough.


## AIM:

# To propose the optimal budgeting on commercials, online campaigns, pricing & promotion strategies thru Market Mix Model for next year.
# To predict the most influencial variable in terms of investment for the sales & revenue for the company


################################################################


### Dataset Understanding:

# dataset#1: ConsumerElectronics.csv is the sales order dataset. on the outset, it has 1648824 rows and 20 columns. 
# dataset#2: tab#1: product list with the frequency and the percentage of the sales of a product
  # tab#2: investmentn details in terms of different channels and its monthly splits from july 2015 to june 2016
  # tab#3: A special sales calender denoting the special offer days
  # tab#4: NPS, voice of customer, provided for the same duration

################################################################

### Data Preparation and EDA:

#set directory
#setwd("C:\\Users\\Partha Vijayan\\Downloads")
#getwd()

#load libraries
library(DataCombine)
library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(glmnet)
library(DAAG)
library(caret)
library(GGally)
library(corrplot)
library(gdata)
library(reshape)
library(data.table)
library(MASS)
library(car)
library(readxl)

#load dataset
#### Read the ad spend details 
###########################################################################################
ad_dtls <- read_excel("Media data and other information.xlsx", sheet = 2, col_names = TRUE, skip = 2)
ad_dtls$year_month <- paste(ad_dtls$Year , ad_dtls$Month , sep = '-')
ad_dtls$Year <- as.numeric(ad_dtls$Year)
ad_dtls$Month <- as.numeric(ad_dtls$Month)

ad_dtls$year_month <- factor(ad_dtls$year_month, levels = ad_dtls$year_month[order(ad_dtls$Year , ad_dtls$Month)])

#ad_dtls[order(ad_dtls$Year, ad_dtls$Month),] %>% ggplot(aes(x = year_month, y = Total.Investment)) + geom_bar(stat = "identity")
ggplot(ad_dtls, aes(x = year_month, y = `Total Investment`)) + geom_bar(stat = "identity")

## Spend  is lowest for aug
## Spend is high in Sep,Oct,Dec,Mar

#Investments Radio and others is assigned 0 for NA values
sapply(ad_dtls , function(x) sum(is.na(x)))
ad_dtls[which(is.na(ad_dtls$Radio)), "Radio"] <- 0 
ad_dtls[which(is.na(ad_dtls$Other)), "Other"] <- 0

ad_dtls_long <- gather(ad_dtls, Medium, Spend, 3:12)

ggplot(ad_dtls_long, aes (x = Month, y = Spend, colour = Medium)) + geom_line() +
  scale_x_discrete(name = "Months since May 2015", limits = seq(1, 12, 1))

#Removing Total investments from data, as it is the sum of all the mediums and converted to crore value
ad_dtls$Total.Investment <- NULL
ad_dtls[,3:11] <- ad_dtls[,3:11] * 10000000

##########################################################################################
# Import the order data set
##########################################################################################
order_rawdata <- read.csv ( "ConsumerElectronics.csv" , header = T , stringsAsFactors = F)
nrow(order_rawdata)  ##1648824
str(order_rawdata)

######################################################################################
## Data related checks 
#####################################################################################

## No issue with case sensitivity
sapply(order_rawdata, function(x) length(unique(toupper(x)))-length(unique(tolower(x)))) 

## NA values are there in gmv, cust_id, pincode. 4904 missing values in each column
sapply(order_rawdata, function(x){sum(is.na(x))})   

filter(order_rawdata , order_rawdata$gmv < 0 )  ## 0 row
filter(order_rawdata , order_rawdata$gmv == 0 ) ## 1349 rows 

filter(order_rawdata , order_rawdata$units < 0 )  ## 0 row
filter(order_rawdata , order_rawdata$units == 0 ) ## 0 row

table(order_rawdata$deliverybdays)  ## rows with negative as well as very high deliverybdays 
table(order_rawdata$deliverycdays)  ## rows with negative as well as very high deliverycdasy


unique(order_rawdata$s1_fact.order_payment_type)  ## Two payment type available - COD , Prepaid 
table(order_rawdata$s1_fact.order_payment_type)  ## COD is preferred one 

table(order_rawdata$sla)  ## rows with 0 sla. They are same day delivery. Few rows with high sla

length(unique(order_rawdata$pincode)) ## 7565 unique values 
length(unique(order_rawdata$cust_id)) ## 1201090 distinct customers 

unique(order_rawdata$product_analytic_super_category) ## Single value CE 
unique(order_rawdata$product_analytic_sub_category)   ## 14 distinct values 
unique(order_rawdata$product_analytic_category)       ## 5 distinct values 
unique(order_rawdata$product_analytic_vertical)       ## 74 distinct values 

## 51 product_analytic_vertical under CameraAccessory", "GamingAccessory", "HomeAudio"
filter (
  order_rawdata ,
  order_rawdata$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")
) %>% group_by(product_analytic_sub_category , product_analytic_vertical) %>% summarise(count = n()) 

filter(order_rawdata , order_rawdata$product_mrp < 0 )  ## 0 row
nrow(filter(order_rawdata , order_rawdata$product_mrp == 0 )) ## 5308 row


min(order_rawdata$order_date)   ## "2015-05-19 13:42:09"
max (order_rawdata$order_date ) ## "2016-07-25 01:19:45"

## Create a year-month variable 
order_rawdata$year_month <- paste(order_rawdata$Year, order_rawdata$Month , sep = '-')
order_rawdata$order_date <- as.Date(order_rawdata$order_date)
order_rawdata$start_week_date <-
  floor_date(
    as.Date(order_rawdata$order_date),
    unit = "week" ,
    week_start = getOption("lubridate.week.start", 1)
  )
order_rawdata[which(order_rawdata$start_week_date < '2015-07-01'),"start_week_date"] <- '2015-07-01'


## Find out the week number from a date
order_rawdata$week_no <-   strftime( order_rawdata$start_week_date ,format="%V")
## Create a year-month variable 
order_rawdata$year_month <- paste(order_rawdata$Year, order_rawdata$Month , sep = '-')
## Filter for date range July 2015 to June 2016
order_rawdata <- subset(order_rawdata, order_rawdata$order_date >= '2015-07-01' & order_rawdata$order_date < '2016-07-01')
## Filter out the rows  having missing values 
row.has.na <- apply(order_rawdata, 1, function(x){any(is.na(x))})
sum(row.has.na) #4904
## Remove the missing values 
order_rawdata <- order_rawdata[!row.has.na,]
order_rawdata$week_no <- as.numeric(order_rawdata$week_no)
## Add a varaiable to specify month number as per week start date 
order_rawdata$month_asper_week_startdate <- format(order_rawdata$start_week_date , "%m")

order_rawdata$month_asper_week_startdate <- as.numeric(order_rawdata$month_asper_week_startdate)
## Number the week from 1 to 53
## July 1st week will be 1 and june last week will be 53 


order_rawdata$week_no[order_rawdata$Year == 2015 ] <- (order_rawdata$week_no[order_rawdata$Year == 2015 ]) -26
order_rawdata$week_no[order_rawdata$Year == 2016 &
                        order_rawdata$week_no != 53] <-
  (order_rawdata$week_no[order_rawdata$Year == 2016 &
                           order_rawdata$week_no != 53]) + 27

order_rawdata[which(order_rawdata$Year == 2016 &
                      order_rawdata$Month == 1 &
                      order_rawdata$week_no == 53), "week_no"] <-
  order_rawdata[which(order_rawdata$Year == 2016 &
                        order_rawdata$Month == 1 &
                        order_rawdata$week_no == 53), "week_no"] - 26



## Filter out the rows having mrp value 0 
order_rawdata <- order_rawdata[!order_rawdata$product_mrp == 0,]

order_rawdata$deliverybdays[order_rawdata$deliverybdays < 0] = 0
order_rawdata$deliverycdays[order_rawdata$deliverycdays < 0] = 0
order_rawdata$product_procurement_sla [order_rawdata$product_procurement_sla <0 ] =0

order_rawdata$deliverybdays <- as.numeric(order_rawdata$deliverybdays)
order_rawdata$deliverycdays <- as.numeric(order_rawdata$deliverycdays)
order_rawdata$sla <- as.numeric(order_rawdata$sla)
order_rawdata$delivery_on_time <-
  order_rawdata$sla - (
    order_rawdata$deliverybdays + order_rawdata$deliverycdays + order_rawdata$product_procurement_sla
  )
order_rawdata$delivery_status[order_rawdata$delivery_on_time < 0] <- 'Delayed'
order_rawdata$delivery_status[order_rawdata$delivery_on_time == 0] <- 'On time'
order_rawdata$delivery_status[order_rawdata$delivery_on_time > 0] <- 'Early'

###########################################################################
##  Read the promotional details 
##########################################################################
special_sale <- read_excel("Media data and other information.xlsx", sheet = 3, col_names = TRUE)
special_sale$Year[1:6] <- 2015
special_sale$Year[7:12] <- 2016
special_sale$X <- NULL
#Derived manually from holidays
special_sale$start_week_no <- c(3,7,9,16,19,26,30,32,34,33,37,48)
special_sale$end_week_no <- c(3,8,9,16,20,27,30,32,34,34,37,48)
special_sale$promotion_type <- trim(sapply(special_sale$`Sales Calendar`, function(x) substr(x , 1,  (regexpr("\\(", x[1])-1 ))))
special_sale$Sales.Calendar <- NULL
special_sale_long <- gather ( special_sale , week_type , week_no , 2:3)
special_sale_long$week_type <- NULL

##############################################################################
## Read the satisfaction score 
#############################################################################
month_np_score <- read_excel("Media data and other information.xlsx", sheet = 4, col_names = TRUE)
str(month_np_score)
month_np_score <- month_np_score[2:13]
t_month_np_score <- transpose(month_np_score)
t_month_np_score$Month <- c(seq(7,12,1),seq(1,6,1))
colnames(t_month_np_score)[1] <- "NPS"

#############################################################################
### Aggragate the multiple dataset to create a master dataset at weekly level 
#############################################################################

## Group the data at weekly level 
weekly_order_data <- order_rawdata %>% group_by ( Year, month_asper_week_startdate,  product_analytic_category,product_analytic_sub_category, product_analytic_vertical,year_month , week_no)%>% summarise( prepaid_cnt =  sum(ifelse (s1_fact.order_payment_type =='Prepaid' , 1 , 0)) ,cod_cnt =  sum(ifelse (s1_fact.order_payment_type =='COD' , 1,0)) ,delayed_delivery_cnt =sum(ifelse (delivery_status =='Delayed' , 1 , 0)), early_delivery_cnt =sum(ifelse (delivery_status =='Early' , 1 , 0)), onetime_delivery_cnt =sum(ifelse (delivery_status =='On time' , 1 , 0)), tot_gmv = sum(gmv) , tot_units = sum(units) , tot_product_mrp = sum( as.numeric (product_mrp)), avg_gmv = mean(gmv) , avg_mrp = mean(product_mrp) , no_of_customer = length(unique(cust_id)), no_of_orders = length(unique(order_id)) , list_price = (tot_gmv/tot_units) , avg_price = mean(list_price) )

colnames(weekly_order_data)[2] <- "Month"

## Merge the ad data with weekly data 
weekly_order_ad_data <- merge(weekly_order_data ,ad_dtls , by=c("Year" , "Month"))

## dont need this variable any more
weekly_order_ad_data$year_month.y <- NULL 

## Merge the NPS data  
weekly_order_ad_data <- merge(weekly_order_ad_data ,t_month_np_score , by=c(  "Month"))

## Find out how many entries are there in a month  
week_in_a_month <- weekly_order_ad_data %>% group_by( Month ) %>% summarize (  tot_week = length(unique(week_no)) )

weekly_order_ad_data <- merge(weekly_order_ad_data ,week_in_a_month, by = c ( "Month") )

rows_ina_week <- weekly_order_ad_data %>% group_by( week_no ) %>% summarize ( total_row = n())

weekly_order_ad_data <- merge(weekly_order_ad_data ,rows_ina_week, by = c ( "week_no") )

## Convert monthly ad spend into weekly ad spend 
weekly_order_ad_data[,c(22:30)] <- weekly_order_ad_data[,c(22:30)]/(weekly_order_ad_data$tot_week*weekly_order_ad_data$total_row)

## Add the promotional sale name in dataset
##weekly_order_ad_data <- merge(weekly_order_ad_data ,special_sale_long, by.x = c ( "Year" , "week_no")  , by.y   = c ( "Year" , "week_no") , all.x=TRUE )

weekly_order_ad_data$promotion_type <- NULL
for (row_no  in 1:nrow(special_sale) ) {
  for (week in special_sale_long[[row_no,2]] : special_sale_long[[row_no,3]] ){
    print(paste("The week is", week))
    weekly_order_ad_data[which(weekly_order_ad_data$week_no==week),"promotion_type"]  <-   special_sale_long[[row_no,4]]
  }
}

#############################################################################################
## Engineered variables 
###############################################################################################
weekly_order_ad_data$discount_over_mrp <-
  (weekly_order_ad_data$tot_product_mrp - weekly_order_ad_data$tot_gmv) /
  weekly_order_ad_data$tot_product_mrp

weekly_order_ad_data$Holiday_week <- ifelse (is.na(weekly_order_ad_data$promotion_type) , 'N','Y' )
weekly_order_ad_data[which(is.na(weekly_order_ad_data$promotion_type)), "promotion_type"] <- "No_promotion"
weekly_order_ad_data$value_per_visitor <- weekly_order_ad_data$tot_gmv/weekly_order_ad_data$no_of_customer

#############################################################################
### Perform EDA analysis on weekly_order_ad_data and order_rawdata
## EDA will show we need different ad spend for each product category 
#############################################################################

## Month wise ad spend details 
ggplot(ad_dtls_long, aes (x = Month, y = Spend, colour = Medium)) + geom_line() + 
  scale_x_discrete(name="Months since May 2015", limits=seq(1,12,1))

## Create week wise sale and ad spend details for various sub category level 
ad_sale_dtls <- weekly_order_ad_data %>% group_by (product_analytic_sub_category, week_no)%>% 
  summarise(tot_sales = sum(tot_gmv) ,
            tot_tv_spend = sum (TV), tot_dig_spend = sum (Digital), 
            tot_spon_spend = sum(Sponsorship) , tot_content_spend = sum(`Content Marketing`),
            tot_online_spend = sum(`Online marketing`) ,tot_aff_spend = sum(Affiliates),
            tot_sem_spend = sum(SEM) ,tot_radio_spend = sum(Radio), 
            tot_oter_spend = sum(Other))


## weekly sale details for different sub category 
p <- ggplot(ad_sale_dtls , aes ( x = week_no , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "week", y = "Sales " ) + ggtitle ( " Sales  vs Total Ad spend")

## Total TV ad spend vs sales details
p <- ggplot(ad_sale_dtls , aes ( x = tot_tv_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs TV Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_dig_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Digital Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_spon_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Sponsor Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_content_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Content Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_online_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Online Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_aff_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Affiliate Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_sem_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs SEM Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_radio_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Radio Ad")

p <- ggplot(ad_sale_dtls , aes ( x = tot_oter_spend , y = tot_sales))+ geom_line()
p + facet_wrap( ~ ad_sale_dtls$product_analytic_sub_category, nrow =2, ncol = 7) + labs(x = "Ad spend", y = "Sales ") + ggtitle ( " Sale vs Other Ad")

## Sale at different promotional and non promotional weeks
weekly_order_ad_data %>% group_by ( promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity")

## Sale at different promotional weeks for different sub categories 
weekly_order_ad_data %>% group_by ( product_analytic_sub_category ,promotion_type) %>% summarise( Avg_sale = mean(tot_gmv)) %>% ggplot( aes ( x=promotion_type, y =Avg_sale  )) + geom_bar(stat = "identity") +facet_wrap( ~ product_analytic_sub_category, nrow =2, ncol = 7)+theme(axis.text.x=element_text(angle = -90, hjust = 0))

## weekly ad spend vs sales 
sale_vs_week_ad <- weekly_order_ad_data %>% group_by ( week_no) %>% summarise(tot_sales = sum(tot_gmv) , ad_spend = sum(TV+Digital+Sponsorship+`Content Marketing`+`Online marketing`+`Affiliates`+SEM+Radio+Other))
sale_vs_week_ad_long <- gather(sale_vs_week_ad, Type, Spend, 2:3)
ggplot(sale_vs_week_ad_long, aes ( x= week_no , y = Spend , color = Type))+geom_line()

### Disocunt percentage vs average sales

discount_vs_sales <- weekly_order_ad_data[,c("tot_gmv" , "discount_over_mrp")]
discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_over_mrp <= .1 , 'up to 10', ifelse ( discount_vs_sales$discount_over_mrp > .1 & discount_vs_sales$discount_over_mrp <= .3 , 'up to 30', ifelse(discount_vs_sales$discount_over_mrp > .3 & discount_vs_sales$discount_over_mrp <= .5 , 'up to 50','>50') ))
discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(tot_gmv)) %>% ggplot(aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity")

## Avg discount at different promotional and non promotional week
weekly_order_ad_data %>% group_by(promotion_type) %>% summarise(avg_disc = mean(discount_over_mrp)) %>% ggplot(aes(x= promotion_type, y =avg_disc )) + geom_bar(stat = "identity")

## Nps vs week
weekly_order_ad_data %>% group_by(week_no) %>% summarise(nps = mean(NPS)) %>% ggplot(aes(x= week_no, y =nps )) + geom_bar(stat = "identity")

## payment type vs number of orders
order_rawdata %>% group_by ( s1_fact.order_payment_type) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= s1_fact.order_payment_type, y =order_cnt )) + geom_bar(stat = "identity")

## delivery_status  vs number of orders
order_rawdata %>% group_by ( delivery_status) %>% summarise(order_cnt = n()) %>% ggplot(aes(x= delivery_status, y =order_cnt )) + geom_bar(stat = "identity")


############################################################################
###create 3 different data set & add engineered kpis
#############################################################################
unique(weekly_order_ad_data$product_analytic_category)
## Since model needs to be built at sub category level, this varaiable is needed 
weekly_order_ad_data$product_analytic_category <- NULL 
weekly_order_ad_data$year_month.x <- NULL 

## Create a dataset only for Home audio , camera accessory and gaming accessories 
weekly_order_ad_data <- filter ( weekly_order_ad_data ,weekly_order_ad_data$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")) 

## Dummy variable creation for character data types 
weekly_order_ad_data_chr <- weekly_order_ad_data[,c(5,32,34)]
weekly_order_ad_data_fact <- data.frame(sapply(weekly_order_ad_data_chr, function(x) factor(x)))
str(weekly_order_ad_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(weekly_order_ad_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =weekly_order_ad_data_fact))[,-1]))

## Create master data set by appending dummies with main data set 
weekly_order_ad_data_overall <- cbind(weekly_order_ad_data[,c(1:4,6:31,33,35)],dummies) 
#View(weekly_order_ad_data_overall) 

###############################################################################################
### Outlier treatment 
###############################################################################################


boxplot(weekly_order_ad_data_overall$tot_gmv )
boxplot(weekly_order_ad_data_overall$tot_units)
boxplot(weekly_order_ad_data_overall$tot_product_mrp)
boxplot(weekly_order_ad_data_overall$TV)
boxplot(weekly_order_ad_data_overall$Digital)
boxplot(weekly_order_ad_data_overall$Sponsorship)
boxplot(weekly_order_ad_data_overall$`Content Marketing`)
boxplot(weekly_order_ad_data_overall$`Online marketing`)
boxplot(weekly_order_ad_data_overall$Affiliates)
boxplot(weekly_order_ad_data_overall$SEM)
boxplot(weekly_order_ad_data_overall$Radio)
boxplot(weekly_order_ad_data_overall$Other)


## Since there are lots of outliers  in dataset, they cant be removed. 
## So they have been capped by appropiate quantile decided by looking at data spread

overall_quantile <- sapply(weekly_order_ad_data_overall[,c("tot_gmv","tot_units", "tot_product_mrp" , "TV" ,"Digital",
                                                           "Sponsorship", "Content Marketing", "Online marketing" ,"Affiliates", "SEM" ,"Radio" , "Other" )], 
                           function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

## remove_outliers function for capping the value to specific quantile

remove_outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}

weekly_order_ad_data_overall$tot_gmv <- remove_outliers (weekly_order_ad_data_overall$tot_gmv,0, .97 ) 
weekly_order_ad_data_overall$tot_units <- remove_outliers (weekly_order_ad_data_overall$tot_units,0, .97 ) 
weekly_order_ad_data_overall$tot_product_mrp <- remove_outliers (weekly_order_ad_data_overall$tot_product_mrp,0, .97 ) 
weekly_order_ad_data_overall$TV <- remove_outliers (weekly_order_ad_data_overall$TV,0, .98 ) 
weekly_order_ad_data_overall$Digital <- remove_outliers (weekly_order_ad_data_overall$Digital,0, .95 ) 
weekly_order_ad_data_overall$Sponsorship <- remove_outliers (weekly_order_ad_data_overall$Sponsorship,0, .95 ) 
weekly_order_ad_data_overall$Content.Marketing <- remove_outliers (weekly_order_ad_data_overall$`Content Marketing`,0, .95 ) 
weekly_order_ad_data_overall$SEM <- remove_outliers (weekly_order_ad_data_overall$SEM,0, .95 ) 
weekly_order_ad_data_overall$Radio <- remove_outliers (weekly_order_ad_data_overall$Radio,0, .95 ) 
weekly_order_ad_data_overall$Other <- remove_outliers (weekly_order_ad_data_overall$Other,0, .95 )

## Find out  how many distinct values are there are for different columns
sapply(weekly_order_ad_data_overall, function(x) length(unique(x)))

weekly_order_ad_data_overall$total_row <- NULL
weekly_order_ad_data_overall$tot_week <- NULL

##  Take back up of master dataset weekly_order_ad_data_overall

weekly_order_ad_data_overall2 <- weekly_order_ad_data_overall

## Check the correlation among multiple varaiables to decide which varaiables are highly corelated with each other
## Column 4 has been excluded as it contains sub category 

#corr <- cor(weekly_order_ad_data_overall2[,-c (4)])

##Depending on higher corelation or since there vars are direct proxy to sales , so taking them out
weekly_order_ad_data_overall$avg_mrp <- NULL
weekly_order_ad_data_overall$avg_price <- NULL
weekly_order_ad_data_overall$tot_units <- NULL
weekly_order_ad_data_overall$no_of_orders <- NULL
weekly_order_ad_data_overall$tot_product_mrp <- NULL
weekly_order_ad_data_overall$avg_gmv <- NULL
weekly_order_ad_data_overall$value_per_visitor <- NULL
weekly_order_ad_data_overall$Year <- NULL
weekly_order_ad_data_overall$no_of_customer <- NULL
weekly_order_ad_data_overall$delayed_delivery_cnt <- NULL
weekly_order_ad_data_overall$early_delivery_cnt <- NULL
weekly_order_ad_data_overall$onetime_delivery_cnt <- NULL
weekly_order_ad_data_overall$cod_cnt <- NULL
weekly_order_ad_data_overall$prepaid_cnt <- NULL

## Create 3 data set HomeAudio, GamingAccessory and CameraAccessory  for model building. 
list2env(split( weekly_order_ad_data_overall[,-3], weekly_order_ad_data_overall$product_analytic_sub_category), envir = .GlobalEnv)
str(HomeAudio)
str(GamingAccessory)
str(CameraAccessory)

nrow(HomeAudio)
nrow(GamingAccessory)
nrow(CameraAccessory)

######################### MMM Model Building############################################

########################################################################################
## Linear Regression - Camera
########################################################################################
linear_camera <- CameraAccessory
linear_camera$week_no <- NULL
linear_camera$Month <- NULL

#Scaling the dataset
linear_camera[,1:13] <- data.frame(scale(linear_camera[,1:13], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_camera), 0.7*nrow(linear_camera))
#Generate the train data set
train = linear_camera[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = linear_camera[-trainindices,]

cam_model1 <- lm(tot_gmv~. , data = train)
summary(cam_model1)
alias(cam_model1)

step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif(step_cam_model1)
alias(step_cam_model1)

#Content.Marketing
step_cam_model2 <- lm(tot_gmv ~ list_price + TV + Sponsorship + 
                        X.Affiliates + SEM + NPS + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBag + product_analytic_vertical.xCameraBattery + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xEid...Rathayatra.sale + promotion_type.xNo_promotion + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model2)
vif(step_cam_model2)

#Low significance and high VIF, all the variables have been removed step by step
#list_price, NPS, promotion_type.xEid...Rathayatra.sale, CameraBag, xCameraTripod, CameraBattery
step_cam_model3 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraLEDLight + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                        product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                        promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model3)
vif(step_cam_model3)

#Low significance and high VIF, all the variables have been removed step by step
#Flash, Teleconverter, xCameraLEDLight, xFlashShoeAdapter,xReflectorUmbrella,xNo_promotion,New.Year.Sale
step_cam_model4 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + discount_over_mrp + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model4)
vif(step_cam_model4)


#discount_over_mrp
step_cam_model5 <- lm(tot_gmv ~ TV + Sponsorship + 
                        X.Affiliates + SEM + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                        promotion_type.xRakshabandhan.Sale , data = train)
summary(step_cam_model5)
vif(step_cam_model5)

########################################################
# Validation

gmv_prediction <- predict(step_cam_model5, test)
test$predicted_gmv <- gmv_prediction

cam_r <- cor(test$tot_gmv, test$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test 0.839
###################################
#Estimating elasticity 

cam_final_model <- step_cam_model5

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var])
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")
############################################################################################
#Cross validation

cam_lm_cv <- cv.lm(data = train, form.lm = formula(tot_gmv ~ TV + Sponsorship + 
                                                     X.Affiliates + SEM + product_analytic_vertical.xCameraAccessory + 
                                                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                                                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                                                     product_analytic_vertical.xCameraHousing + 
                                                     product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                                                     product_analytic_vertical.xCameraRemoteControl + 
                                                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                                                     product_analytic_vertical.xLens + 
                                                     product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                     product_analytic_vertical.xTelescope + promotion_type.xDaussera.sale + 
                                                     promotion_type.xRakshabandhan.Sale), m =10)

#ms = 0.236
#########################################################################################
## Linear Regression - GamingAccessory
########################################################################################
linear_game <- GamingAccessory
linear_game$week_no <- NULL
linear_game$Month <- NULL

linear_game[,1:13] <- data.frame(scale(linear_game[,1:13], center = TRUE))
set.seed(100)

trainindices = sample(1:nrow(linear_game), 0.7*nrow(linear_game))
#Generate the train data set
train_game = linear_game[trainindices,]
#Similarly store the rest of the observations into an object "test".
test_game = linear_game[-trainindices,]

game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)
alias(game_model1)

step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)
alias(step_game_model1)

#Online.marketing
step_game_model2 <- lm(tot_gmv ~ TV + Digital + Sponsorship + Content.Marketing + 
                         X.Affiliates + Radio + Other + NPS + product_analytic_vertical.xCoolingPad + 
                         product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingChargingStation + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xEid...Rathayatra.sale + promotion_type.xPacman + 
                         promotion_type.xRepublic.Day , data = train_game)
summary(step_game_model2)
vif(step_game_model2)

#Low significance and high VIF, all the variables have been removed step by step
#Other, X.Affiliates, Big.Diwali.Sale, Rathayatra, TV
step_game_model3 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         Radio + NPS + product_analytic_vertical.xCoolingPad + 
                         product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingChargingStation + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xPacman + 
                         promotion_type.xRepublic.Day , data = train_game)
summary(step_game_model3)
vif(step_game_model3)


#Low significance and high VIF, all the variables have been removed step by step
#Radio, xPacman, Republic.Day, xGamingChargingStation, xCoolingPad
step_game_model4 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         NPS + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xDaussera.sale, data = train_game)
summary(step_game_model4)
vif(step_game_model4)

#Low significance and high VIF, all the variables have been removed step by step
#xGamingSpeaker, Content.Marketing, NPS
step_game_model5 <- lm(tot_gmv ~ Digital + Sponsorship + 
                         product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + 
                         promotion_type.xDaussera.sale, data = train_game)
summary(step_game_model5)
vif(step_game_model5)

########################################################
# Validation

gmv_game_prediction <- predict(step_game_model5, test_game)
test_game$predicted_gmv <- gmv_game_prediction

cam_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#R squared on test 0.544
###################################
#Estimating elasticity 

game_final_model <- step_game_model5

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var])
  return(x)
}

var_list_game <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list_game[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs.game <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs.game <- cbind(elasticity.outputs.game,do.call(rbind.data.frame, var_list_game))
colnames(elasticity.outputs.game) <- c("Variable","Elasticity")

elasticity.outputs.game$direction <- ifelse(elasticity.outputs.game$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.game, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

############################################################################################
#Cross validation

game_lm_cv <- cv.lm(data = train_game , form.lm = formula(tot_gmv ~ Digital + Sponsorship + 
                                                            product_analytic_vertical.xGamePad + 
                                                            product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                                                            product_analytic_vertical.xGamingMouse + 
                                                            promotion_type.xDaussera.sale), m =10)


#ms 0.297
#########################################################################################
## Linear Regression - HomeAudio
########################################################################################
linear_home <- HomeAudio
linear_home$week_no <- NULL
linear_home$Month <- NULL

linear_home[,1:13] <- data.frame(scale(linear_home[,1:13], center = TRUE))

set.seed(100)
trainindices= sample(1:nrow(linear_home), 0.7*nrow(linear_home))
#Generate the train data set
train_home = linear_home[trainindices,]
#Similarly store the rest of the observations into an object "test".
test_home = linear_home[-trainindices,]

home_model <- lm(tot_gmv~. , data = train_home)
summary(home_model)
alias(home_model)

step_home_model1 <- stepAIC(home_model, direction = "both")
summary(step_home_model1)
vif(step_home_model1)
alias(step_home_model1)

#Low significance and high VIF, all the variables have been removed step by step
#xHiFiSystem,xSoundMixer,xBSD
step_home_model2 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + Online.marketing + 
                         X.Affiliates + Other + NPS + product_analytic_vertical.xDJController + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model2)
vif(step_home_model2)

#Low significance and high VIF, all the variables have been removed step by step
#Online.marketing,Affiliates, Other,xDJController,NPS
step_home_model3 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model3)
vif(step_home_model3)


#Low significance and high VIF, all the variables have been removed step by step
#xDockingStation,xDock
step_home_model4 <- lm(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                         product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker + 
                         promotion_type.xBig.Diwali.Sale + 
                         promotion_type.xEid...Rathayatra.sale , data = train_home)
summary(step_home_model4)
vif(step_home_model4)

########################################################
# Validation

gmv_home_prediction <- predict(step_home_model4, test_home)
test_home$predicted_gmv <- gmv_home_prediction

cam_r <- cor(test_home$tot_gmv, test_home$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared
#Accuracy on test 0.813
###################################
#Estimating elasticity 

home_final_model <- step_home_model4

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var])
  return(x)
}

var_list_home <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list_home[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs.home <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs.home <- cbind(elasticity.outputs.home,do.call(rbind.data.frame, var_list_home))
colnames(elasticity.outputs.home) <- c("Variable","Elasticity")

elasticity.outputs.home$direction <- ifelse(elasticity.outputs.home$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs.home, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Linear Model") +xlab("Variables")

############################################################################################
#Cross validation

home_lm_cv <- cv.lm(data = train_home , form.lm = formula(tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                                                            product_analytic_vertical.xFMRadio + 
                                                            product_analytic_vertical.xHomeAudioSpeaker + 
                                                            promotion_type.xBig.Diwali.Sale + 
                                                            promotion_type.xEid...Rathayatra.sale), m =10)




#########################################################################################
## Multiplicative model - Camera
########################################################################################

multi_camera <- CameraAccessory
multi_camera$week_no <- NULL
multi_camera$Month <- NULL


#Treatment of zero values
summary(multi_camera)
multi_camera$TV[which(multi_camera$TV == 0)] <- 0.01
multi_camera$Radio[which(multi_camera$Radio == 0)] <- 0.01
multi_camera$Other[which(multi_camera$Other == 0)] <- 0.01
multi_camera$Content.Marketing[which(multi_camera$Content.Marketing == 0)] <- 0.01

#Log of the numerical variables
multi_camera[,1:13] <- data.frame(sign(multi_camera[,1:13])*log(abs(multi_camera[,1:13])))

trainindices.m= sample(1:nrow(multi_camera), 0.6*nrow(multi_camera))
#Generate the train data set
train.m = multi_camera[trainindices.m,]
#Similarly store the rest of the observations into an object "test".
test.m = multi_camera[-trainindices.m,]

model.mul1 <- lm(tot_gmv~., data = train.m)
summary(model.mul1)
alias(model.mul1)

model.mul2 <- stepAIC(model.mul1, direction = "both")
summary(model.mul2)
vif(model.mul2)
alias(model.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#xRepublic, xValentine, Radio, Online.marketing, other
model.mul3 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                   X.Affiliates + SEM + NPS + 
                   product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale + 
                   promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale, data = train.m)
summary(model.mul3)
vif(model.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#Rathayatra, TV, xDaussera, SEM, CameraLEDLight,xReflectorUmbrella,xSoftbox
model.mul4 <- lm(tot_gmv ~ list_price + Content.Marketing + 
                   X.Affiliates + NPS + 
                   product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                   product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                   product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                   product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                   product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                   product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                   product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                   product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                   product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                   product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                   promotion_type.xDaussera.sale + 
                   promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale, data = train.m)
summary(model.mul4)
vif(model.mul4)

############################################################
#Validating on the test data set
predict_cam <- predict(model.mul4, test.m)
test.m$predicted_gmv <- predict_cam

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.m$tot_gmv, test.m$predicted_gmv)
cam_rsquared <- cor(test.m$tot_gmv, test.m$predicted_gmv)^2
cam_rsquared
#Rsquared: 0.789 on test dataset
############################################################
cam.mul.model <- model.mul4

elasticity <- function(var) {
  x <- as.numeric(cam.mul.model$coefficients[var])
  return(x)
}

varlist.mul.cam <- list()

for(i in 2:length(cam.mul.model$coefficients)) {
  varlist.mul.cam[i-1] <- elasticity(names(cam.mul.model$coefficients)[i])
}

elasticity.cam.mul <- data.frame(names(cam.mul.model$coefficients[2:length(cam.mul.model$coefficients)]))
elasticity.cam.mul <- cbind(elasticity.cam.mul,do.call(rbind.data.frame, varlist.mul.cam))
colnames(elasticity.cam.mul) <- c("Variable","Elasticity")

elasticity.cam.mul$direction <- ifelse(elasticity.cam.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.cam.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

cam.mul.cv <- cv.lm(data = train.m , form.lm = formula(tot_gmv ~ list_price + Content.Marketing + 
                                                         X.Affiliates + NPS + 
                                                         product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBattery + 
                                                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                                                         product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                                                         product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                                                         product_analytic_vertical.xCameraTripod + product_analytic_vertical.xExtensionTube + 
                                                         product_analytic_vertical.xFlash + product_analytic_vertical.xFlashShoeAdapter + 
                                                         product_analytic_vertical.xLens + product_analytic_vertical.xReflectorUmbrella + 
                                                         product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                                                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope + 
                                                         promotion_type.xDaussera.sale + 
                                                         promotion_type.xIndependence.Sale + promotion_type.xRakshabandhan.Sale), m =10)
#ms 1.14
#########################################################################################
## Multiplicative model - GamingAccessory
########################################################################################

multi_gaming <- GamingAccessory
multi_gaming$week_no <- NULL
multi_gaming$Month <- NULL

#Treatment of NULL values
summary(multi_gaming)
multi_gaming$TV[which(multi_gaming$TV == 0)] <- 0.01
multi_gaming$Radio[which(multi_gaming$Radio == 0)] <- 0.01
multi_gaming$Other[which(multi_gaming$Other == 0 )] <- 0.01
multi_gaming$Content.Marketing[which(multi_gaming$Content.Marketing == 0)] <- 0.01

#Taking log of the data for multiplicative behaviour
#Log of the numerical variables
multi_gaming[,1:13] <- data.frame(sign(multi_gaming[,1:13])*log(abs(multi_gaming[,1:13])))

trainindices.game.m= sample(1:nrow(multi_gaming), 0.6*nrow(multi_gaming))
#Generate the train data set
train.game.m = multi_gaming[trainindices.game.m,]
#Similarly store the rest of the observations into an object "test".
test.game.m = multi_gaming[-trainindices.game.m,]

model.game.mul1 <- lm(tot_gmv~., data = train.game.m)
summary(model.game.mul1)
alias(model.game.mul1)

#Sponsorship -  high VIF and low significance
model.game.mul2 <- stepAIC(model.game.mul1, direction = "both")
summary(model.game.mul2)
vif(model.game.mul2)
alias(model.game.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#xBSD, NPS,xGamingAdapter, Digital, Big.Diwali.Sale
model.game.mul3 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + SEM + Radio + Other + 
                        product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGameControlMount + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                        promotion_type.xNo_promotion + promotion_type.xRakshabandhan.Sale, data = train.game.m)
summary(model.game.mul3)
vif(model.game.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#promotion_type.xNo_promotion, Independence.Sale,xRakshabandhan.sale,xGameControlMount,Other
model.game.mul4 <- lm(tot_gmv ~ list_price + TV + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + SEM + Radio + 
                        product_analytic_vertical.xCoolingPad + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale , data = train.game.m)
summary(model.game.mul4)
vif(model.game.mul4)

#Low significance and high VIF, all the variables have been removed step by step
#Online.marketing,X.Affiliates,Sponsorship, Content.Marketing
model.game.mul5 <- lm(tot_gmv ~ list_price + TV + SEM + Radio + 
                        product_analytic_vertical.xCoolingPad + 
                        product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                        product_analytic_vertical.xGamingChargingStation + 
                        product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                        product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                        product_analytic_vertical.xGamingSpeaker + 
                        promotion_type.xDaussera.sale , data = train.game.m)
summary(model.game.mul5)
vif(model.game.mul5)


############################################################
#Validating on the test data set
predict_game_mul <- predict(model.game.mul5, test.game.m)
test.game.m$predicted_gmv <- predict_game_mul

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)
cam_rsquared <- cor(test.game.m$tot_gmv, test.game.m$predicted_gmv)^2
cam_rsquared
#Rsquare: 0.69 
############################################################
game.mul.model <- model.game.mul5

elasticity <- function(var) {
  x <- as.numeric(game.mul.model$coefficients[var] * mean(train.game.m[,var])/mean(train.game.m$tot_gmv))
  return(x)
}

varlist.mul.game <- list()

for(i in 2:length(game.mul.model$coefficients)) {
  varlist.mul.game[i-1] <- elasticity(names(game.mul.model$coefficients)[i])
}

elasticity.game.mul <- data.frame(names(game.mul.model$coefficients[2:length(game.mul.model$coefficients)]))
elasticity.game.mul <- cbind(elasticity.game.mul,do.call(rbind.data.frame, varlist.mul.game))
colnames(elasticity.game.mul) <- c("Variable","Elasticity")

elasticity.game.mul$direction <- ifelse(elasticity.game.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.game.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

game.mul.cv <- cv.lm(data = train.game.m , form.lm = formula(tot_gmv ~ list_price + TV + SEM + Radio + 
                                                               product_analytic_vertical.xCoolingPad + 
                                                               product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingAccessoryKit + 
                                                               product_analytic_vertical.xGamingChargingStation + 
                                                               product_analytic_vertical.xGamingGun + product_analytic_vertical.xGamingHeadset + 
                                                               product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                                                               product_analytic_vertical.xGamingSpeaker + 
                                                               promotion_type.xDaussera.sale), m =10)

#ms 1.24
#########################################################################################
## Multiplicative model - HomeAudio
########################################################################################
multi_home <- HomeAudio
multi_home$week_no <- NULL
multi_home$Month <- NULL

#Treatment of NULL values
summary(multi_home)
multi_home$TV[which(multi_home$TV == 0)] <- 0.01
multi_home$Radio[which(multi_home$Radio == 0)] <- 0.01
multi_home$Other[which(multi_home$Other == 0 )] <- 0.01
multi_home$Content.Marketing[which(multi_home$Content.Marketing == 0)] <- 0.01

#Taking log of the data for multiplicative behaviour
#Log of the numerical variables
multi_home[,1:13] <- data.frame(sign(multi_home[,1:13])*log(abs(multi_home[,1:13])))

trainindices.home.m= sample(1:nrow(multi_home), 0.6*nrow(multi_home))
#Generate the train data set
train.home.m = multi_home[trainindices.home.m,]
#Similarly store the rest of the observations into an object "test".
test.home.m = multi_home[-trainindices.home.m,]

model.home.mul1 <- lm(tot_gmv~., data = train.home.m)
summary(model.home.mul1)
alias(model.home.mul1)


model.home.mul2 <- stepAIC(model.home.mul1, direction = "both")
summary(model.home.mul2)
vif(model.home.mul2)
alias(model.home.mul2)

#Low significance and high VIF, all the variables have been removed step by step
#Rathayatra.sale, New.Year.Sale , NPS, Dock,xHiFiSystem
model.home.mul3 <- lm(tot_gmv ~ list_price + TV + Digital + Sponsorship + Content.Marketing + 
                        Online.marketing + X.Affiliates + Radio + Other + product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + 
                        product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer + 
                        promotion_type.xDaussera.sale, data = train.home.m)
summary(model.home.mul3)
vif(model.home.mul3)

#Low significance and high VIF, all the variables have been removed step by step
#Radio, Online.marketing, X.Affiliates,Other,Content.Marketing, TV
model.home.mul4 <- lm(tot_gmv ~ list_price + Digital + Sponsorship + 
                        product_analytic_vertical.xDJController + 
                        product_analytic_vertical.xDockingStation + 
                        product_analytic_vertical.xFMRadio + 
                        product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                        product_analytic_vertical.xSoundMixer + 
                        promotion_type.xDaussera.sale, data = train.home.m)
summary(model.home.mul4)
vif(model.home.mul4)

############################################################
#Validating on the test data set
predict_home_mul <- predict(model.home.mul4, test.home.m)
test.home.m$predicted_gmv <- predict_home_mul

# Now, we need to test the r square between actual and predicted sales.
cam_r <- cor(test.home.m$tot_gmv, test.home.m$predicted_gmv)
cam_rsquared <- cor(test.home.m$tot_gmv, test.home.m$predicted_gmv)^2
cam_rsquared
#Accuracy: 0.679 ie. 67.9% on test dataset
############################################################
home.mul.model <- model.home.mul4

elasticity <- function(var) {
  x <- as.numeric(home.mul.model$coefficients[var] * mean(train.home.m[,var])/mean(train.home.m$tot_gmv))
  return(x)
}

varlist.mul.home <- list()

for(i in 2:length(home.mul.model$coefficients)) {
  varlist.mul.home[i-1] <- elasticity(names(home.mul.model$coefficients)[i])
}

elasticity.home.mul <- data.frame(names(home.mul.model$coefficients[2:length(home.mul.model$coefficients)]))
elasticity.home.mul <- cbind(elasticity.home.mul,do.call(rbind.data.frame, varlist.mul.home))
colnames(elasticity.home.mul) <- c("Variable","Elasticity")

elasticity.home.mul$direction <- ifelse(elasticity.home.mul$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.home.mul, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Multiplicative Model") +xlab("Variables")
##################################################################################
#Cross validation

home.mul.cv <- cv.lm(data = train.home.m , form.lm = formula(tot_gmv ~ list_price + Digital + Sponsorship + 
                                                               product_analytic_vertical.xDJController + 
                                                               product_analytic_vertical.xDockingStation + 
                                                               product_analytic_vertical.xFMRadio + 
                                                               product_analytic_vertical.xHomeAudioSpeaker + product_analytic_vertical.xSlingBox + 
                                                               product_analytic_vertical.xSoundMixer + 
                                                               promotion_type.xDaussera.sale), m =10)
ms = 0.863



## Base data set creation for lag model 

## Add sales for last 3 week

weekly_order_ad_data_with_lag <- weekly_order_ad_data %>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical) %>%  mutate(gmv_lag_1 = lag(tot_gmv, 1))  %>%  mutate(gmv_lag_2 = lag(tot_gmv, 2)) %>%  mutate(gmv_lag_3 = lag(tot_gmv, 3)) 
## add change in sales  last 3 weeks

weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category ,product_analytic_vertical ) %>%  mutate(gmv_change_from_w1 = (tot_gmv-lag(tot_gmv, 1))/tot_gmv) %>%  mutate(gmv_change_from_w2 = (tot_gmv-lag(tot_gmv, 2))/tot_gmv) %>%  mutate(gmv_change_from_w3 = (tot_gmv-lag(tot_gmv, 3))/tot_gmv) 

## add list price for   last 3 weeks
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(list_price_lag_1 = lag(list_price, 1))  %>%  mutate(list_price_lag_2 = lag(list_price, 2)) %>%  mutate(list_price_lag_3 = lag(list_price, 3)) 

#### add list price change  for   last 3 weeks
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(price_change_from_w1 = (list_price-lag(list_price, 1))/list_price) %>%  mutate(list_price_change_from_w2 = (list_price-lag(list_price, 2))/list_price) %>%  mutate(list_price_change_from_w3 = (list_price-lag(list_price, 3))/list_price)

#### add TV ad stcok. 60% effect of current week is propagating to next week
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(tv_ad_stock = TV+ if_else ( is.na (lag(TV, 1)*.6),0, lag(TV, 1)*.6) + if_else ( is.na (lag(TV, 2)*.36),0, lag(TV, 2)*.36) + if_else ( is.na (lag(TV, 3)*.22),0, lag(TV, 3)*.22) + if_else ( is.na (lag(TV, 4)*.13),0, lag(TV, 4)*.13)  + if_else ( is.na (lag(TV, 5)*.07),0, lag(TV, 5)*.07)  + if_else ( is.na (lag(TV, 6)*.05),0, lag(TV, 6)*.05) ) 

#### add Digital ad stcok. 20% effect of current week is propagating to next week

weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(dig_ad_stock = Digital+ if_else ( is.na (lag(Digital, 1)*.2),0, lag(Digital, 1)*.2) + if_else ( is.na (lag(Digital, 2)*.04),0, lag(Digital, 2)*.04)   )

#### add sponsorship ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(spon_ad_stock = Sponsorship+ if_else ( is.na (lag(Sponsorship, 1)*.2),0, lag(Sponsorship, 1)*.2) + if_else ( is.na (lag(Sponsorship, 2)*.04),0, lag(Sponsorship, 2)*.04)   )  

#### add content marketing ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(content_ad_stock = Content.Marketing+ if_else ( is.na (lag(Content.Marketing, 1)*.2),0, lag(Content.Marketing, 1)*.2) + if_else ( is.na (lag(Content.Marketing, 2)*.04),0, lag(Content.Marketing, 2)*.04)   ) 

#### add online marketing ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(online_ad_stock = Online.marketing+ if_else ( is.na (lag(Online.marketing, 1)*.2),0, lag(Online.marketing, 1)*.2) + if_else ( is.na (lag(Online.marketing, 2)*.04),0, lag(Online.marketing, 2)*.04)   )

#### add Radio ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(radio_ad_stock = Radio+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(Radio, 1)*.2) + if_else ( is.na (lag(Radio, 2)*.04),0, lag(Radio, 2)*.04)   )

#### add SEM ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(sem_ad_stock = SEM+ if_else ( is.na (lag(SEM, 1)*.2),0, lag(SEM, 1)*.2) + if_else ( is.na (lag(SEM, 2)*.04),0, lag(SEM, 2)*.04)   )

#### add Affiliate  ad stcok. 
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% arrange ( week_no) %>% group_by(product_analytic_sub_category,product_analytic_vertical ) %>%  mutate(affiliate_ad_stock = X.Affiliates+ if_else ( is.na (lag(X.Affiliates, 1)*.2),0, lag(X.Affiliates, 1)*.2) + if_else ( is.na (lag(X.Affiliates, 2)*.04),0, lag(X.Affiliates, 2)*.04)   )

## Filter out CameraAccessory", GamingAccessory and  HomeAudio sub category
weekly_order_ad_data_with_lag <- weekly_order_ad_data_with_lag %>% filter ( product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio"))

weekly_order_ad_data_with_lag <- as.data.frame(weekly_order_ad_data_with_lag)

## Since these dataframe has been created using weekly_order_ad_data, so we have character variables in  this dataset
## Convert char vars in factorial
weekly_order_ad_data_with_lag_chr <- weekly_order_ad_data_with_lag[,c(5,32,34)]
weekly_order_ad_data_with_lag_chr_fact <- data.frame(sapply(weekly_order_ad_data_with_lag_chr, function(x) factor(x)))
str(weekly_order_ad_data_with_lag_chr_fact)

# creating dummy variables for factor attributes
dummies1<- data.frame(sapply(weekly_order_ad_data_with_lag_chr_fact, 
                             function(x) data.frame(model.matrix(~x-1,data =weekly_order_ad_data_with_lag_chr_fact))[,-1]))


## Combine dummies with other varaiables to create master data set 
weekly_order_ad_data_with_lag_overall <- cbind ( weekly_order_ad_data_with_lag[, -c(5,32,34)], dummies1 ) 

cor_var <- cor ( weekly_order_ad_data_with_lag_overall[-4])

## Since these variables are highly corelated  or direct proxy to sales , so taking them out 
weekly_order_ad_data_with_lag_overall$tot_week <- NULL 
weekly_order_ad_data_with_lag_overall$total_row <- NULL
weekly_order_ad_data_with_lag_overall$avg_mrp <- NULL
weekly_order_ad_data_with_lag_overall$avg_price <- NULL
weekly_order_ad_data_with_lag_overall$tot_units <- NULL
weekly_order_ad_data_with_lag_overall$no_of_orders <- NULL
weekly_order_ad_data_with_lag_overall$tot_product_mrp <- NULL
weekly_order_ad_data_with_lag_overall$avg_gmv <- NULL
weekly_order_ad_data_with_lag_overall$value_per_visitor <- NULL
weekly_order_ad_data_with_lag_overall$Year <- NULL
weekly_order_ad_data_with_lag_overall$no_of_customer <- NULL
weekly_order_ad_data_with_lag_overall$delayed_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$early_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$onetime_delivery_cnt <- NULL
weekly_order_ad_data_with_lag_overall$cod_cnt <- NULL
weekly_order_ad_data_with_lag_overall$prepaid_cnt <- NULL


##weekly_order_ad_data_with_lag_overall [,c(1,2,4:36)] <- scale(weekly_order_ad_data_with_lag_overall [,c(1,2,4:36)])

# Create sub data set for each sub category
weekly_order_ad_data_lag_cam <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("CameraAccessory")) 
weekly_order_ad_data_lag_cam$product_analytic_sub_category <- NULL

weekly_order_ad_data_lag_game <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("GamingAccessory")) 
weekly_order_ad_data_lag_game$product_analytic_sub_category <- NULL

weekly_order_ad_data_lag_home <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("HomeAudio")) 
weekly_order_ad_data_lag_home$product_analytic_sub_category <- NULL

######################################################################################################
## Start building distributed lag model for camera sub category 
#################################################################################################

weekly_order_ad_data_lag_cam1 <- weekly_order_ad_data_lag_cam
weekly_order_ad_data_lag_cam <- na.omit(weekly_order_ad_data_lag_cam)

weekly_order_ad_data_lag_cam [,c(1:35)] <- scale(weekly_order_ad_data_lag_cam [,c(1:35)])


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_cam), 0.7*nrow(weekly_order_ad_data_lag_cam))
#Generate the train data set
train_cam = weekly_order_ad_data_lag_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_cam = weekly_order_ad_data_lag_cam[-trainindices,]



cam_model1 <- lm(tot_gmv~. , data = train_cam)
summary(cam_model1)


step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif(step_cam_model1)


## Removed content_ad_stock as high vif
step_cam_model2.1 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                          X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                          list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                          tv_ad_stock  + sem_ad_stock + product_analytic_vertical.xCameraAccessory + 
                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                          product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                          product_analytic_vertical.xCameraHousing  + 
                          product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                          product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                          product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                          product_analytic_vertical.xLens  + 
                          product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                          product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                          promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                          promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model2.1)
vif(step_cam_model2.1)



## removed sem_ad_stock as high vif
step_cam_model3 <- lm(tot_gmv ~ TV + Sponsorship + Content.Marketing + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model3)
vif(step_cam_model3)

## removed TV due to high vif

step_cam_model4 <- lm(tot_gmv ~   Sponsorship + Content.Marketing + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model4)
vif(step_cam_model4)



## removed Content.Marketing due to high vif

step_cam_model5 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates + SEM + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model5)
vif(step_cam_model5)

## Removed SEM as high vif


step_cam_model6 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates  + Other + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model6)
vif(step_cam_model6)

## Removed other  as p is high 

step_cam_model7 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model7)
vif(step_cam_model7)

## Removed product_analytic_vertical.xCameraEyeCup as p is high

step_cam_model8 <- lm(tot_gmv ~   Sponsorship  + 
                        X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                        list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                        tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                        product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                        product_analytic_vertical.xCameraFilmRolls + 
                        product_analytic_vertical.xCameraHousing  + 
                        product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                        product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                        product_analytic_vertical.xLens  + 
                        product_analytic_vertical.xSoftbox + product_analytic_vertical.xStrap + 
                        product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                        promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                        promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model8)
vif(step_cam_model8)



### Removed product_analytic_vertical.xSoftbox  , product_analytic_vertical.xCameraFilmRolls due to high p
step_cam_model10 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope + promotion_type.xChristmas...New.Year.Sale + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model10)
vif(step_cam_model10)


### Removed promotion_type.xChristmas...New.Year.Sale due to high p 
step_cam_model11 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale + 
                         promotion_type.xNo_promotion, data = train_cam)


summary(step_cam_model11)
vif(step_cam_model11)

### Removed promotion_type.xNo_promotion due to high p 
step_cam_model12 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3 + list_price_change_from_w2 + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model12)
vif(step_cam_model12)


### Removed list_price_change_from_w2 due to high vif 
step_cam_model13 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_lag_3  + list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model13)
vif(step_cam_model13)

## Removed list_price_lag_3 due to high vif 

step_cam_model14 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp + list_price_lag_2 + 
                         list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model14)
vif(step_cam_model14)

## Removed list_price_lag_2 due to high P 
step_cam_model15 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp  + 
                         list_price_change_from_w3 + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model15)
vif(step_cam_model15)

## Removed list_price_change_from_w3 due to high vif 

step_cam_model16 <- lm(tot_gmv ~   Sponsorship  + 
                         X.Affiliates   + NPS + discount_over_mrp   
                       + 
                         tv_ad_stock   + product_analytic_vertical.xCameraAccessory + 
                         product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip  
                       + 
                         product_analytic_vertical.xCameraHousing  + 
                         product_analytic_vertical.xCameraMicrophone + product_analytic_vertical.xCameraMount + 
                         product_analytic_vertical.xCameraRemoteControl + product_analytic_vertical.xCameraTripod + 
                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                         product_analytic_vertical.xLens  + 
                         product_analytic_vertical.xStrap + 
                         product_analytic_vertical.xTelescope  + 
                         promotion_type.xDaussera.sale + promotion_type.xRakshabandhan.Sale 
                       , data = train_cam)


summary(step_cam_model16)
vif(step_cam_model16)

#################################################################################
## Model evalution for test data set
################################################################################

gmv_cam_prediction <- predict(step_cam_model16, test_cam[,-3])
test_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_cam$tot_gmv, test_cam$predicted_gmv)
cam_rsquared <- cam_r^2
cam_rsquared   ####Rsquaredo on test = 0.798

## Cross validation 
cam_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_cam , form.lm = step_cam_model16, m =10)
###   ms 
###  0.317

################################################################
#### Plot elasticity for lag disttibuted model - Camera accessory sub category 
################################################################

cam_final_model <- step_cam_model16

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Lag Model") +xlab("Variables")



###################################################################################################
### Start building distirubuted lag model for game accessory category
##################################################################################################
weekly_order_ad_data_lag_game <- filter ( weekly_order_ad_data_with_lag_overall ,weekly_order_ad_data_with_lag_overall$product_analytic_sub_category %in% c("GamingAccessory")) 
weekly_order_ad_data_lag_game$product_analytic_sub_category <- NULL
weekly_order_ad_data_lag_game1 <- weekly_order_ad_data_lag_game
weekly_order_ad_data_lag_game <- na.omit(weekly_order_ad_data_lag_game)

weekly_order_ad_data_lag_game [,c(1:35)] <- scale(weekly_order_ad_data_lag_game [,c(1:35)])


set.seed(100)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_game), 0.7*nrow(weekly_order_ad_data_lag_game))
#Generate the train data set
train_game = weekly_order_ad_data_lag_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_game = weekly_order_ad_data_lag_game[-trainindices,]

##train[, 1:36] <- as.data.frame(scale(train[, 1:36]))

game_model1 <- lm(tot_gmv~. , data = train_game)
summary(game_model1)


step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif(step_game_model1)

## Removed online_ad_stock due to high vif 

step_game_model3 <- lm( tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                          Other + NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                          sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                          promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                          promotion_type.xDaussera.sale 
                        + promotion_type.xRepublic.Day, 
                        data = train_game)

summary(step_game_model3)
vif(step_game_model3)

##  sem_ad_stock due to high vif


step_game_model4 <- lm(tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                         Other + NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model4)
vif(step_game_model4)

## Removed Other due to high vif
step_game_model5 <- lm(tot_gmv ~ list_price + Sponsorship + SEM + Radio + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model5)
vif(step_game_model5)


## Removed Radio due to high vif
step_game_model6 <- lm(tot_gmv ~ list_price + Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2 + content_ad_stock + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model6)
vif(step_game_model6)


## Removed content_ad_stock as vif is high 
step_game_model7 <- lm(tot_gmv ~ list_price + Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model7)
vif(step_game_model7)



## Removed list_price as p is high 
step_game_model8 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model8)
vif(step_game_model8)

## Removed promotion_type.xBig.Diwali.Sale as p is high 
step_game_model9 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                         NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                         gmv_change_from_w3 + list_price_lag_2  + 
                         affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                         product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                         product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                         promotion_type.xBSD.5 + 
                         promotion_type.xDaussera.sale 
                       + promotion_type.xRepublic.Day, 
                       data = train_game)

summary(step_game_model9)
vif(step_game_model9)

## Removed promotion_type.xBSD.5 and promotion_type.xRepublic.Day as p is high 

step_game_model10 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3 + list_price_lag_2  + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model10)
vif(step_game_model10)

## Removed list_price_lag_2 as p is high
step_game_model11 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          NPS + discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 + 
                          gmv_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model11)
vif(step_game_model11)

## Removed gmv_change_from_w3 and NPS as p is high
step_game_model12 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          discount_over_mrp + gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model12)
vif(step_game_model12)




## Removed  discount_over_mrp as p is  is high
step_game_model13 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker  
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model13)
vif(step_game_model13)


## Removed product_analytic_vertical.xGamingSpeaker as P is  is high
step_game_model14 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_lag_1 + gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model14)
vif(step_game_model14)



## Removed gmv_lag_1 as P is  is high
step_game_model15 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          gmv_change_from_w2 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model15)
vif(step_game_model15)

## Removed gmv_change_from_w2 as P is  is high
step_game_model16 <- lm(tot_gmv ~   Sponsorship + SEM  + 
                          affiliate_ad_stock + product_analytic_vertical.xGamePad + 
                          product_analytic_vertical.xGamingHeadset + product_analytic_vertical.xGamingKeyboard + 
                          product_analytic_vertical.xGamingMouse   
                        + 
                          promotion_type.xDaussera.sale 
                        , 
                        data = train_game)

summary(step_game_model16)
vif(step_game_model16)

### Model evalution on test data set for game accessory

gmv_game_prediction <- predict(step_game_model16, test_game[,-3])
test_game$predicted_gmv <- gmv_game_prediction

game_r <- cor(test_game$tot_gmv, test_game$predicted_gmv)
game_rsquared <- game_r^2
game_rsquared   ##Rsquare on test = 0.68

## Cross validation
game_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_game , form.lm = step_game_model16, m =10)

####ms   
##0.322

##################################################################################

game_final_model <- step_game_model16

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  LagDistributed  Model") +xlab("Variables")


#######################################################################################

######################################################################################
weekly_order_ad_data_lag_home1 <- weekly_order_ad_data_lag_home
weekly_order_ad_data_lag_home <- na.omit(weekly_order_ad_data_lag_home)

weekly_order_ad_data_lag_home [,c(1:35)] <- scale(weekly_order_ad_data_lag_home [,c(1:35)])


set.seed(100)


trainindices= sample(1:nrow(weekly_order_ad_data_lag_home), 0.7*nrow(weekly_order_ad_data_lag_home))
#Generate the train data set
train_home = weekly_order_ad_data_lag_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_home = weekly_order_ad_data_lag_home[-trainindices,]


home_model1 <- lm(tot_gmv~. , data = train_home)
summary(home_model1)


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif(step_home_model1)

## Removed online_ad_stock due to high vif 
step_home_model2 <- lm( tot_gmv ~ week_no + Digital + Content.Marketing + 
                          Online.marketing + X.Affiliates + Radio + discount_over_mrp + 
                          gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock + content_ad_stock + 
                          radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model2)
vif(step_home_model2)


## Removed content_ad_stock due to high vif 
step_home_model3 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing + X.Affiliates + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock  + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model3)
vif(step_home_model3)


## Removed X.Affiliates due to high vif 
step_home_model4 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3 + dig_ad_stock  + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model4)
vif(step_home_model4)


## Removed dig_ad_stock due to high vif 
step_home_model5 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         radio_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model5)
vif(step_home_model5)



## Removed  radio_ad_stock due to high vif 
step_home_model6 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Online.marketing  + Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home) 


summary(step_home_model6)
vif(step_home_model6)


## Removed Online.marketing due to high vif 
step_home_model7 <- lm(tot_gmv ~ week_no + Digital + Content.Marketing + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model7)
vif(step_home_model7)


## Removed Content.Marketing due to high vif 
step_home_model8 <- lm(tot_gmv ~ week_no + Digital  + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model8)
vif(step_home_model8)


## Removed promotion_type.xChristmas...New.Year.Sale due to high P 
step_home_model9 <- lm(tot_gmv ~ week_no + Digital  + 
                         Radio + discount_over_mrp + 
                         gmv_lag_3 + list_price_change_from_w3   + 
                         affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                         product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                         product_analytic_vertical.xHomeAudioSpeaker  + 
                         promotion_type.xDaussera.sale + 
                         promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model9)
vif(step_home_model9)


## Removed gmv_lag_3 due to high P 
step_home_model10 <- lm(tot_gmv ~ week_no + Digital  + 
                          Radio + discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model10)
vif(step_home_model10)




## Removed Radio due to high P 
step_home_model11 <- lm(tot_gmv ~ week_no + Digital  + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale + 
                          promotion_type.xNo_promotion, data = train_home ) 


summary(step_home_model11)
vif(step_home_model11)

##Removed promotion_type.xNo_promotion due to high p 

step_home_model12 <- lm(tot_gmv ~ week_no + Digital  + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model12)
vif(step_home_model12)


##Removed Digital due to high p 

step_home_model13 <- lm(tot_gmv ~ week_no   + 
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home   ) 


summary(step_home_model13)
vif(step_home_model13)


##Removed week_no due to high p 

step_home_model14 <- lm(tot_gmv ~    
                          discount_over_mrp + 
                          list_price_change_from_w3   + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model14)
vif(step_home_model14)

## Removed list_price_change_from_w3 due to high p 
step_home_model15 <- lm(tot_gmv ~    
                          discount_over_mrp 
                        + 
                          affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                          product_analytic_vertical.xDock + product_analytic_vertical.xFMRadio + 
                          product_analytic_vertical.xHomeAudioSpeaker  + 
                          promotion_type.xDaussera.sale 
                        , data = train_home  ) 


summary(step_home_model15) 
vif(step_home_model15)

## Model evalution for distributed lag model - home audio 

gmv_home_prediction <- predict(step_home_model15, test_home[,-3])
test_home$predicted_gmv <- gmv_home_prediction

home_r <- cor(test_home$tot_gmv, test_home$predicted_gmv)
home_rsquared <- home_r^2
home_rsquared   ###Rsquare on test = 0.74

## 10 fold Cross validation 

home_lm_cv <- cv.lm(data = weekly_order_ad_data_lag_home , form.lm = step_home_model15, m =10)
###ms 
###0.42

##############################################################################################
## Elasticity plot for distributed lag model - Home audio 
#############################################################################################
home_final_model <- step_home_model15

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  LagDistributed  Model") +xlab("Variables")






##################Modeling - Camera Accessory#####################
str(CameraAccessory)
sum(is.na(CameraAccessory))
CameraAccessoryKoyck <- CameraAccessory
library(DataCombine)
# Creating Lag variable 
CameraAccessoryKoyck <- slide(CameraAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(CameraAccessoryKoyck)

Camera_koyck <- na.omit(CameraAccessoryKoyck)
Camera_koyck1 <- data.frame(scale(Camera_koyck[,c(1:15)]))
Camera_koyck2 <- data.frame(Camera_koyck[,16:77])
Camera_koyck3 <- data.frame(scale(Camera_koyck[,78]))

Camera_koyck <- cbind(Camera_koyck1, Camera_koyck2, Camera_koyck3)
str(Camera_koyck)
#Camera_koyck <- na.omit(CameraAccessoryKoyck)
str(Camera_koyck)
# removing month and year columns
Camera_koyck <- Camera_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(100)
trainindices= sample(1:nrow(Camera_koyck), 0.6*nrow(Camera_koyck))
train_c = Camera_koyck[trainindices,]
test_c = Camera_koyck[-trainindices,]
str(train_c)
names(train_c)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_c)
summary(Koyck_model1)



# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing Radio as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates + Radio + NPS + 
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_c)


summary(Koyck_model3)
vif(Koyck_model3)

# removing NPS as p-value is very high
Koyck_model4<- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + 
                    Content.Marketing + X.Affiliates + Radio + 
                    product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                    product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                    product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                    product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                    product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                    product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                    promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_c)

summary(Koyck_model4)
vif(Koyck_model4)

# removing Content.Marketing as vif is high
Koyck_model5 <- lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + X.Affiliates + Radio + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model5)
vif(Koyck_model5)

# removing promotion_type.xBig.Diwali.Sale as high p-value
Koyck_model6 <-  lm(formula = tot_gmv ~ list_price + TV + Digital + Sponsorship + X.Affiliates + Radio + 
                      product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                      product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                      product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                      product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                      product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xChristmas...New.Year.Sale + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data = train_c)

summary(Koyck_model6)
vif(Koyck_model6)

# removing TV as p-value is very high
Koyck_model7 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + Radio + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model7)
vif(Koyck_model7)

# removing radio as it has high p-value
Koyck_model8 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model8)
vif(Koyck_model8)

# removing promotion_type.xChristmas...New.Year.Sale as it has relatively high p-value
Koyck_model9 <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                     product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                     product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                     product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                     product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                     product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                   data = train_c)
summary(Koyck_model9)
vif(Koyck_model9)


# final model after AIC and VIF tuning
final_koyck_camera <- lm(formula = tot_gmv ~ list_price + Digital + Sponsorship + X.Affiliates + 
                           product_analytic_vertical.xCameraAccessory + product_analytic_vertical.xCameraBatteryCharger + 
                           product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xCameraEyeCup + 
                           product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraHousing + 
                           product_analytic_vertical.xCameraLEDLight + product_analytic_vertical.xCameraMicrophone + 
                           product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                           product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                           product_analytic_vertical.xFlashShoeAdapter + product_analytic_vertical.xLens + 
                           product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xSoftbox + 
                           product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                           promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                         data = train_c)
summary(final_koyck_camera)
vif(final_koyck_camera)
par(mfrow = c(2,2))
plot(final_koyck_camera, main = "Final Camera Accessory - Koyck Model")
dev.off()

## Cross validation
crossval <- cv.lm(data = train_c, form.lm = formula(final_koyck_camera),m = 10,
                  main = "Cross Validation Camera Accessory Model")
attr(crossval, "ms")
# Cross validation(ms) = 0.232

# Predicting Values
pred_koyck_cam <- predict(final_koyck_camera, test_c)
RMSE(test_c$tot_gmv,pred_koyck_cam)
cam_r_koyck <- cor(test_c$tot_gmv, pred_koyck_cam)
cam_rsquared <- cam_r_koyck^2
cam_rsquared
#Rsquared on test: 0.731
###################################
#Estimating elasticity 

cam_final_model <- final_koyck_camera

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] * mean(train_c[,var])/mean(train_c$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")
###########################################

##################Modeling - Gaming Accessory#####################
str(GamingAccessory)
sum(is.na(GamingAccessory))
GamingAccessoryKoyck <- GamingAccessory
library(DataCombine)
# Creating Lag variable 
GamingAccessoryKoyck <- slide(GamingAccessoryKoyck, Var = "tot_gmv",slideBy = -1)
str(GamingAccessoryKoyck)

Game_koyck <- na.omit(GamingAccessoryKoyck)
Game_koyck1 <- data.frame(scale(Game_koyck[,c(1:15)]))
Game_koyck2 <- data.frame(Game_koyck[,16:77])
Game_koyck3 <- data.frame(scale(Game_koyck[,78]))

Game_koyck <- cbind(Game_koyck1, Game_koyck2, Game_koyck3)
str(Game_koyck)
# removing month and year columns
Game_koyck <- Game_koyck[,-c(1,2)]
# splitting train & test sets
set.seed(100)
trainindices= sample(1:nrow(Game_koyck), 0.6*nrow(Game_koyck))
train_g = Game_koyck[trainindices,]
test_g = Game_koyck[-trainindices,]
str(train_g)
names(train_g)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_g)
summary(Koyck_model1)

# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing SEM as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + product_analytic_vertical.xJoystickGamingWheel + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_g)
summary(Koyck_model3)
vif(Koyck_model3)

# removing product_analytic_vertical.xJoystickGamingWheel as p-value is moderately high
Koyck_model4<- lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                    NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                    product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                    product_analytic_vertical.xGamingSpeaker + 
                    promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                    promotion_type.xRakshabandhan.Sale, data = train_g)

summary(Koyck_model4)
vif(Koyck_model4)



# final model after AIC and VIF tuning
final_koyck_gaming <-  lm(formula = tot_gmv ~ TV + Digital + Sponsorship + X.Affiliates + 
                            NPS + product_analytic_vertical.xGamePad + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMouse + 
                            product_analytic_vertical.xGamingSpeaker + 
                            promotion_type.xBSD.5 + promotion_type.xChristmas...New.Year.Sale + 
                            promotion_type.xDaussera.sale + promotion_type.xIndependence.Sale + 
                            promotion_type.xRakshabandhan.Sale, data = train_g)
summary(final_koyck_gaming)
vif(final_koyck_gaming)
par(mfrow = c(2,2))
plot(final_koyck_gaming, main = "Final Gaming Accessory - Koyck Model")
dev.off()
## Cross validation
crossval <- cv.lm(data = train_g, form.lm = formula(final_koyck_gaming),m = 10,
                  main = "Cross Validation Gaming Accessory Model")
attr(crossval, "ms")
# Cross validation (ms) = 0.319
# Predicting Values
pred_koyck_game <- predict(final_koyck_gaming, test_g)
RMSE(test_g$tot_gmv,pred_koyck_game)
game_r_koyck <- cor(test_g$tot_gmv, pred_koyck_game)
game_rsquared <- game_r_koyck^2
game_rsquared
#Rsquared on test: 0.669
###################################
#Estimating elasticity 

game_final_model <- final_koyck_gaming

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] * mean(train_g[,var])/mean(train_g$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming Accessory - Koyck Model") +xlab("Variables")
###########################################


##################Modeling - Home Audio#####################
str(HomeAudio)
sum(is.na(HomeAudio))
HomeAudioKoyck <- HomeAudio
library(DataCombine)
# Creating Lag variable 
HomeAudioKoyck <- slide(HomeAudioKoyck, Var = "tot_gmv",slideBy = -1)
str(HomeAudioKoyck)

HomeA_koyck <- na.omit(HomeAudioKoyck)
HomeA_koyck1 <- data.frame(scale(HomeA_koyck[,c(1:15)]))
HomeA_koyck2 <- data.frame(HomeA_koyck[,16:77])
HomeA_koyck3 <- data.frame(scale(HomeA_koyck[,78]))

HomeA_koyck <- cbind(HomeA_koyck1, HomeA_koyck2, HomeA_koyck3)
str(HomeA_koyck)
# removing month and year columns
HomeA_koyck <- HomeA_koyck[,-c(1,2)]
# splitting train & test sets
trainindices= sample(1:nrow(HomeA_koyck), 0.6*nrow(HomeA_koyck))
train_h = HomeA_koyck[trainindices,]
test_h = HomeA_koyck[-trainindices,]
str(train_h)
names(train_h)
## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_h)
summary(Koyck_model1)

# Evaluating the first models for significant predictors
Koyck_model2 <- stepAIC(Koyck_model1,direction = "both")
summary(Koyck_model2)
vif(Koyck_model2)

# removing Online.marketing  as vif is very high
Koyck_model3<- lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                    X.Affiliates + discount_over_mrp + product_analytic_vertical.xDJController + 
                    product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                    product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                    promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                  data = train_h)
summary(Koyck_model3)
vif(Koyck_model3)

# removing X.Affiliates as p-value is high
Koyck_model4 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale + promotion_type.xEid...Rathayatra.sale, 
                    data = train_h)
summary(Koyck_model4)
vif(Koyck_model4)

# removing promotion_type.xEid...Rathayatra.sale as high p-value
Koyck_model5 <-  lm(formula = tot_gmv ~ Digital + Sponsorship + Content.Marketing + 
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model5)
vif(Koyck_model5)

# removing promotion_type.xEid...Rathayatra.sale as high p-value
Koyck_model6 <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                      discount_over_mrp + product_analytic_vertical.xDJController + 
                      product_analytic_vertical.xDock + product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model6)
vif(Koyck_model6)

# removing product_analytic_vertical.xDJController as high p-value
Koyck_model7 <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                      discount_over_mrp + product_analytic_vertical.xDock +  
                      product_analytic_vertical.xDockingStation + 
                      product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                      promotion_type.xDaussera.sale, 
                    data = train_h)
summary(Koyck_model7)
vif(Koyck_model7)


# final model after AIC and VIF tuning
final_koyck_homeA <-  lm(formula = tot_gmv ~ Digital + Sponsorship +  
                           discount_over_mrp + product_analytic_vertical.xDock +  
                           product_analytic_vertical.xDockingStation + 
                           product_analytic_vertical.xFMRadio + product_analytic_vertical.xHomeAudioSpeaker + 
                           promotion_type.xDaussera.sale, 
                         data = train_h)
summary(final_koyck_homeA)
vif(final_koyck_homeA)
par(mfrow = c(2,2))
plot(final_koyck_gaming, main = "Final Home Audio - Koyck Model")
dev.off()
## Cross validation
crossval <- cv.lm(data = train_h, form.lm = formula(final_koyck_homeA),m = 10, 
                  main = "Cross Validation Home Audio Model")
attr(crossval, "ms")
#Cross validation(ms): 0.41

# Predicting Values
pred_koyck_homeA <- predict(final_koyck_homeA, test_h)
RMSE(test_h$tot_gmv,pred_koyck_homeA)
homeA_r_koyck <- cor(test_h$tot_gmv, pred_koyck_homeA)
homeA_rsquared <- homeA_r_koyck^2
homeA_rsquared
#Rsquared on test: 0.896
###################################
#Estimating elasticity 

homeA_final_model <- final_koyck_homeA

elasticity <- function(var) {
  x <- as.numeric(homeA_final_model$coefficients[var] * mean(train_h[,var])/mean(train_h$tot_gmv))
  return(x)
}

var_list <- list()

for(i in 2:length(homeA_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(homeA_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(homeA_final_model$coefficients[2:length(homeA_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")
###########################################






#######################################################################
### Start Building multiplicative lag model for camera accessory 
#######################################################################
weekly_order_ad_data_mullag_cam <- weekly_order_ad_data_lag_cam1 


weekly_order_ad_data_mullag_cam_tmp  <- lapply (weekly_order_ad_data_mullag_cam[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_cam <- data.frame( cbind( weekly_order_ad_data_mullag_cam_tmp ,weekly_order_ad_data_mullag_cam[,36:97] ))
weekly_order_ad_data_mullag_cam <- na.omit(weekly_order_ad_data_mullag_cam)

set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_cam), 0.7*nrow(weekly_order_ad_data_mullag_cam))
#Generate the train data set
train_lagmul_cam = weekly_order_ad_data_mullag_cam[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_cam = weekly_order_ad_data_mullag_cam[-trainindices,]



cam_model1 <- lm(tot_gmv~. , data = train_lagmul_cam)
summary(cam_model1)


step_cam_model1 <- stepAIC(cam_model1, direction = "both")
summary(step_cam_model1)
vif (step_cam_model1 )

## Removed online_ad_stock due to high vif 
cam_model2 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                     SEM + Radio + Other + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model2)
vif (cam_model2 )




## Removed    Other due to high vif 
cam_model3 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing + X.Affiliates + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model3)
vif (cam_model3 )



## Removed    X.Affiliates due to high vif 
cam_model4 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock + spon_ad_stock + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model4)
vif (cam_model4 )



## Removed    spon_ad_stock due to high vif 
cam_model5 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock  + content_ad_stock + 
                     sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model5)
vif (cam_model5 )



## Removed    sem_ad_stock due to high vif 
cam_model6 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock + dig_ad_stock  + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model6)
vif (cam_model6 )



## Removed    sem_ad_stock due to high vif 
cam_model7 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     Radio + SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model7)
vif (cam_model7 )



## Removed    Radio due to high vif 
cam_model8 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model8)
vif (cam_model8 )



## Removed    Month due to high vif 
cam_model9 <- lm ( tot_gmv ~ week_no  + list_price + TV + Digital + 
                     Sponsorship + Content.Marketing + Online.marketing  + 
                     SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                     gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                     tv_ad_stock   + content_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                     product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                     product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                     product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                     product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                     product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                     product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                     product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                     promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                     promotion_type.xChristmas...New.Year.Sale  + 
                     promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                     promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model9)
vif (cam_model9 )




## Removed    week_no due to high vif 
cam_model10 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship + Content.Marketing + Online.marketing  + 
                      SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model10)
vif (cam_model10 )



## Removed    Content.Marketing  due to high vif 
cam_model11 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      SEM + NPS + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model11)
vif (cam_model11 )



## Removed    NPS  due to high vif 
cam_model12 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      SEM +  + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model12)
vif (cam_model12 )



## Removed    SEM  due to high vif 
cam_model13 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock   + content_ad_stock + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model13)
vif (cam_model13 )



## Removed    content_ad_stock  due to high vif 
cam_model14 <- lm ( tot_gmv ~    list_price + TV + Digital + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model14)
vif (cam_model14 )


## Removed    Digital  due to high vif 
cam_model15 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale + promotion_type.xBSD.5 + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model15)
vif (cam_model15 )


## Remopved promotion_type.xBSD.5 due to high P 


cam_model16 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model16)
vif (cam_model16 )


## Removed gmv_lag_2 due to high vif 
cam_model17 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model17)
vif (cam_model17 )



## Removed gmv_lag_3 due to high vif 
cam_model18 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp + gmv_lag_1 + 
                      gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model18)
vif (cam_model18 )



## Removed gmv_lag_1 due to high vif 
cam_model19 <- lm ( tot_gmv ~    list_price + TV +  + 
                      Sponsorship  + Online.marketing  + 
                      + discount_over_mrp +   
                      gmv_change_from_w1 + gmv_change_from_w2 + 
                      gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 + 
                      tv_ad_stock     + 
                      affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                      product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                      product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                      product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                      product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                      product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                      product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                      product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                      promotion_type.xBig.Diwali.Sale  + 
                      promotion_type.xChristmas...New.Year.Sale  + 
                      promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                      promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model19)
vif (cam_model19 )



## Removed tv ad stock due to high vif 
cam_model20 <- lm (  tot_gmv ~    list_price + TV +  + 
                       Sponsorship  + Online.marketing  + 
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model20)
vif (cam_model20 )



## Removed tv  due to high vif 
cam_model21 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  + Online.marketing  + 
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model21)
vif (cam_model21 )




## Removed Online.marketing  due to high vif 
cam_model22 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3 + list_price_lag_2 + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model22)
vif (cam_model22 )




## Removed list_price_lag_2  due to high vif 
cam_model23 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3  + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope + 
                       promotion_type.xBig.Diwali.Sale  + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                       promotion_type.xValentine.s.Day, data = train_lagmul_cam)

summary(cam_model23)
vif (cam_model23 )




## Removed promotion_type.xPacman , promotion_type.xValentine.s.Day , promotion_type.xRepublic.Day , promotion_type.xBig.Diwali.Sale due to high vif 
cam_model24 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       + discount_over_mrp +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3  + price_change_from_w1 
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope 
                     + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion   
                     , data = train_lagmul_cam)

summary(cam_model24)
vif (cam_model24 )


## Removed price_change_from_w1  , discount_over_mrp due to high vif 
cam_model25 <- lm (  tot_gmv ~    list_price +  
                       Sponsorship  +   
                       +   
                       gmv_change_from_w1 + gmv_change_from_w2 + 
                       gmv_change_from_w3   
                     + 
                       affiliate_ad_stock + product_analytic_vertical.xCameraAccessory + 
                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraBatteryGrip + 
                       product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xCameraFilmRolls + 
                       product_analytic_vertical.xCameraHousing + product_analytic_vertical.xCameraMicrophone + 
                       product_analytic_vertical.xCameraMount + product_analytic_vertical.xCameraRemoteControl + 
                       product_analytic_vertical.xExtensionTube + product_analytic_vertical.xFilter + 
                       product_analytic_vertical.xLens + product_analytic_vertical.xSoftbox + 
                       product_analytic_vertical.xStrap + product_analytic_vertical.xTelescope 
                     + 
                       promotion_type.xChristmas...New.Year.Sale  + 
                       promotion_type.xNo_promotion   
                     , data = train_lagmul_cam)

summary(cam_model25)
vif (cam_model25 )


## Model evalution on test data set 

gmv_cam_prediction <- predict(cam_model25, test_lagmul_cam[,-3])
test_lagmul_cam$predicted_gmv <- gmv_cam_prediction

cam_r <- cor(test_lagmul_cam$tot_gmv, test_lagmul_cam$predicted_gmv  )
cam_rsquared <- cam_r^2
cam_rsquared   ####Rsquare on test = 0.871

## Model evalution using cross validation 
cam_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_cam , form.lm = cam_model25, m =10)
###   ms 
###  0.67

cam_final_model <- cam_model25

elasticity <- function(var) {
  x <- as.numeric(cam_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(cam_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(cam_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(cam_final_model$coefficients[2:length(cam_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)
ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")

###################################################################################################
####### Start Building multipiactive lag model for gaming accessory 
##################################################################################################

weekly_order_ad_data_mullag_game <- weekly_order_ad_data_lag_game1 
weekly_order_ad_data_mullag_game <-na.omit(weekly_order_ad_data_mullag_game)

weekly_order_ad_data_mullag_game_tmp  <- lapply (weekly_order_ad_data_mullag_game[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_game <- data.frame( cbind( weekly_order_ad_data_mullag_game_tmp ,weekly_order_ad_data_mullag_game[,36:97] ))


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_game), 0.7*nrow(weekly_order_ad_data_mullag_game))
#Generate the train data set
train_lagmul_game = weekly_order_ad_data_mullag_game[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_game = weekly_order_ad_data_mullag_game[-trainindices,]



game_model1 <- lm(tot_gmv~. , data = train_lagmul_game)
summary(game_model1)


step_game_model1 <- stepAIC(game_model1, direction = "both")
summary(step_game_model1)
vif (step_game_model1 )

## Removed content_ad_stock  due to high vif 
step_game_model2 <- lm ( tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2 + tv_ad_stock + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model2)
vif (step_game_model2 )

## Removed Month due to high vif 
step_game_model3 <- lm ( tot_gmv ~ week_no  + list_price + TV + Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2 + tv_ad_stock + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model3)
vif (step_game_model3 )

## Removed  TV ad stock due to high vif 
step_game_model4 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                           discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           dig_ad_stock + spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model4)
vif (step_game_model4 )

## Removed dig_ad_stock due to high vif 
step_game_model5 <- lm (  tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates + SEM + Radio + Other + NPS + 
                            discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model5)
vif (step_game_model5 )

## removed SEM due to high vif 

step_game_model6 <- lm (  tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates  + Radio + Other + NPS + 
                            discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model6)
vif (step_game_model6)


## removed NPS due to high v
step_game_model7 <- lm (   tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp + gmv_lag_1 + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                             price_change_from_w1 + list_price_change_from_w2  + 
                             spon_ad_stock  + affiliate_ad_stock + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model7)
vif (step_game_model7)

## removed gmv_lag_1 due to high vif 
step_game_model8 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates  + Radio + Other  + 
                           discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3 + list_price_lag_2 + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model8)
vif (step_game_model8)


## removed list_price_lag_2

step_game_model9 <- lm ( tot_gmv ~ week_no  + list_price  + TV +Content.Marketing + 
                           Online.marketing + X.Affiliates  + Radio + Other  + 
                           discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                           gmv_change_from_w2 + gmv_change_from_w3  + 
                           price_change_from_w1 + list_price_change_from_w2  + 
                           spon_ad_stock  + affiliate_ad_stock + 
                           product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                           product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                           product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                           product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                           product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                           promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                           promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                           promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model9)
vif (step_game_model9)

## removed week_no due to high vif 

step_game_model10 <- lm ( tot_gmv ~    list_price  + TV +Content.Marketing + 
                            Online.marketing + X.Affiliates  + Radio + Other  + 
                            discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                            gmv_change_from_w2 + gmv_change_from_w3  + 
                            price_change_from_w1 + list_price_change_from_w2  + 
                            spon_ad_stock  + affiliate_ad_stock + 
                            product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                            product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                            product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                            product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                            product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                            product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                            promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                            promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                            promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model10)
vif (step_game_model10)

## removed affiliate_ad_stock due to high vif 

step_game_model11 <- lm (  tot_gmv ~    list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  + 
                             spon_ad_stock   + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model11)
vif (step_game_model11)

## removed spon_ad_stock and affiliate ad stockdue to high vif 
step_game_model12 <- lm (  tot_gmv ~    list_price  + TV +Content.Marketing + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model12)
vif (step_game_model12)


## removed   Content.Marketing  due to high vif 
step_game_model13 <- lm (  tot_gmv ~    list_price  + TV  + 
                             Online.marketing + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model13)
vif (step_game_model13)


## removed   Online.marketing  due to high vif 
step_game_model14 <- lm (  tot_gmv ~    list_price  + TV   
                           + X.Affiliates  + Radio + Other  + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model14)
vif (step_game_model14)



## removed   Other  due to high vif 
step_game_model15 <- lm (  tot_gmv ~    list_price  + TV   
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model15)
vif (step_game_model15)


## removed   TV  due to high vif 
step_game_model16 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController + promotion_type.xBSD.5 + 
                             promotion_type.xChristmas...New.Year.Sale + promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman + promotion_type.xRepublic.Day + 
                             promotion_type.xValentine.s.Day, data = train_lagmul_game)
summary(step_game_model16)
vif (step_game_model16)


## removed motion_type.xChristmas...New.Year.Sale , promotion_type.xBSD.5  due to high P
step_game_model17 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             price_change_from_w1 + list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model17)
vif (step_game_model17)

## removed price_change_from_w1 due to high P 
step_game_model18 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates  + Radio   + 
                             discount_over_mrp  + gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard + product_analytic_vertical.xGamingMemoryCard + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model18)
vif (step_game_model18)


## removed gmv_lag_3 , Radio , product_analytic_vertical.xGamingMemoryCard due to high P 
step_game_model19 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates     + 
                             discount_over_mrp  + gmv_lag_2  + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  + 
                             list_price_change_from_w2  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamingAdapter + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard  + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xNo_promotion + promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model19)
vif (step_game_model19)



## removed promotion_type.xNo_promotion , list_price_change_from_w2 , product_analytic_vertical.xGamingAdapter due to high P 
step_game_model20 <- lm (  tot_gmv ~    list_price     
                           + X.Affiliates     + 
                             discount_over_mrp  + gmv_lag_2  + gmv_change_from_w1 + 
                             gmv_change_from_w2 + gmv_change_from_w3  
                           + 
                             product_analytic_vertical.xCoolingPad + product_analytic_vertical.xGamePad + 
                             product_analytic_vertical.xGamingAccessoryKit  + 
                             product_analytic_vertical.xGamingChargingStation + product_analytic_vertical.xGamingHeadset + 
                             product_analytic_vertical.xGamingKeyboard  + 
                             product_analytic_vertical.xGamingMouse + product_analytic_vertical.xGamingSpeaker + 
                             product_analytic_vertical.xMotionController  + 
                             promotion_type.xDaussera.sale + 
                             promotion_type.xPacman  
                           , data = train_lagmul_game)
summary(step_game_model20)
vif (step_game_model20)

## Model evalution on test dataset
gmv_cam_prediction <- predict(step_game_model20, test_lagmul_game[,-3])
test_lagmul_game$predicted_gmv <- gmv_cam_prediction

game_r <- cor(test_lagmul_game$tot_gmv, test_lagmul_game$predicted_gmv  )
game_rsquared <- game_r^2
game_rsquared   ####Rsquare on test = 0.763

## Model evalution using cross validation 
game_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_game , form.lm = step_game_model20, m =10)

##ms - 0.91

##########################################################################################
###Elasticity plot for multipicative lag model for gaming accessories 
########################################################################################
game_final_model <- step_game_model20

elasticity <- function(var) {
  x <- as.numeric(game_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(game_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(game_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(game_final_model$coefficients[2:length(game_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Gaming  Accessory -  Lag Distributed Multipicative  Model") +xlab("Variables")



#################################################################################################
### Start building distributed lag multipicative model for home audio
################################################################################################
weekly_order_ad_data_mullag_home <- weekly_order_ad_data_lag_home1
weekly_order_ad_data_mullag_home <-na.omit(weekly_order_ad_data_mullag_home)

weekly_order_ad_data_mullag_home_tmp  <- lapply (weekly_order_ad_data_mullag_home[,1:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
weekly_order_ad_data_mullag_home <- data.frame( cbind( weekly_order_ad_data_mullag_home_tmp ,weekly_order_ad_data_mullag_home[,36:97] ))


set.seed(200)


trainindices= sample(1:nrow(weekly_order_ad_data_mullag_home), 0.7*nrow(weekly_order_ad_data_mullag_home))
#Generate the train data set
train_lagmul_home = weekly_order_ad_data_mullag_home[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_lagmul_home = weekly_order_ad_data_mullag_home[-trainindices,]



home_model1 <- lm(tot_gmv~. , data = train_lagmul_home)
summary(home_model1)


step_home_model1 <- stepAIC(home_model1, direction = "both")
summary(step_home_model1)
vif (step_home_model1 )

## Removed spon_ad_stock very high vif 
home_model2 <- lm(tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates + SEM + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model2)
vif(home_model2)


## Removed SEM very high vif 
home_model3 <- lm(tot_gmv ~ week_no + Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model3)
vif(home_model3)

## Removed week_no very high vif 
home_model4 <- lm(tot_gmv ~   Month + list_price + TV + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model4)
vif(home_model4)


## Removed TV very high vif 
home_model5 <- lm(tot_gmv ~   Month + list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model5)
vif(home_model5)


## Removed Month very high vif 
home_model6 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    dig_ad_stock  + online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model6)
vif(home_model6)


## Removed dig_ad_stock very high vif 
home_model7 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    sem_ad_stock + affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model7)
vif(home_model7)



## Removed sem_ad_stock very high vif 
home_model8 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates  + Other + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model8)
vif(home_model8)


## Removed Other very high vif 
home_model9 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                    Online.marketing + X.Affiliates   + gmv_lag_1 + 
                    gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                    list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                    online_ad_stock + radio_ad_stock + 
                    affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                    product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                    product_analytic_vertical.xDockingStation + product_analytic_vertical.xFMRadio + 
                    product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                    product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                    promotion_type.xDaussera.sale + promotion_type.xNo_promotion + 
                    promotion_type.xRepublic.Day + promotion_type.xValentine.s.Day, 
                  data = train_lagmul_home)
summary(home_model9)
vif(home_model9)


## Removed promotion_type.xValentine.s.Day ,promotion_type.xRepublic.Day ,product_analytic_vertical.xFMRadio  very high P 
home_model10 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale + promotion_type.xNo_promotion 
                   , 
                   data = train_lagmul_home)
summary(home_model10)
vif(home_model10)



## Removed promotion_type.xNo_promotion   very high P 
home_model11 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     list_price_lag_1 + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model11)
vif(home_model11)


## Removed list_price_lag_1   very high vif 
home_model12 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_lag_2 + gmv_lag_3 + gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model12)
vif(home_model12)


## Removed gmv_lag_2 ,gmv_lag_3  very high P 
home_model13 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2 + price_change_from_w1 + 
                     online_ad_stock + radio_ad_stock + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model13)
vif(home_model13)


## Removed radio_ad_stock ,price_change_from_w1  very high P 
home_model14 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  + 
                     online_ad_stock  + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model14)
vif(home_model14)


## Removed online_ad_stock  very high P 
home_model15 <- lm(tot_gmv ~     list_price  + Content.Marketing + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model15)
vif(home_model15)


## Removed Content.Marketing  very high P 
home_model16 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w1 + gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model16)
vif(home_model16)


## Removed gmv_change_from_w1  very high P 
home_model17 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 + 
                     + list_price_lag_2  
                   + 
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model17)
vif(home_model17)


## Removed list_price_lag_2  very high P 
home_model18 <- lm(tot_gmv ~     list_price   + 
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model18)
vif(home_model18)


## Removed list_price  very high P 
home_model19 <- lm(tot_gmv ~        
                     Online.marketing + X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     affiliate_ad_stock + product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model19)
vif(home_model19)


## Removed Online.marketing ,affiliate_ad_stock very high vif 
home_model20 <- lm(tot_gmv ~        
                     X.Affiliates   + gmv_lag_1 + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model20)
vif(home_model20)


## Removed gmv_lag_1 due to very high vif 
home_model21 <- lm(tot_gmv ~        
                     X.Affiliates    + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer + promotion_type.xChristmas...New.Year.Sale + 
                     promotion_type.xDaussera.sale  
                   , 
                   data = train_lagmul_home)
summary(home_model21)
vif(home_model21)


## Removed promotion_type.xChristmas...New.Year.Sale , promotion_type.xDaussera.sale due to very high P 
home_model22 <- lm(tot_gmv ~        
                     X.Affiliates    + 
                     gmv_change_from_w3 
                   +   
                     product_analytic_vertical.xBoomBox + 
                     product_analytic_vertical.xDJController + product_analytic_vertical.xDock + 
                     product_analytic_vertical.xDockingStation  + 
                     product_analytic_vertical.xHiFiSystem + product_analytic_vertical.xHomeAudioSpeaker + 
                     product_analytic_vertical.xSoundMixer  
                   , 
                   data = train_lagmul_home)
summary(home_model22)
vif(home_model22)




home_cam_prediction <- predict(home_model22, test_lagmul_home[,-3])
test_lagmul_home$predicted_gmv <- home_cam_prediction

home_r <- cor(test_lagmul_home$tot_gmv, test_lagmul_home$predicted_gmv  )
home_rsquared <- home_r^2
home_rsquared   #### Rsquare on test =  0.782

game_lm_cv <- cv.lm(data = weekly_order_ad_data_mullag_home , form.lm = home_model22, m =10)

##ms - 0.89

home_final_model <- home_model22

elasticity <- function(var) {
  x <- as.numeric(home_final_model$coefficients[var] )
  return(x)
}

var_list <- list()

for(i in 2:length(home_final_model$coefficients)) {
  var_list[i-1] <- elasticity(names(home_final_model$coefficients)[i])
}

elasticity.outputs <- data.frame(names(home_final_model$coefficients[2:length(home_final_model$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")
elasticity.outputs <- arrange( elasticity.outputs , -Elasticity)

ggplot(data=elasticity.outputs[, ], aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home  Audio -  Lag Distributed Multipicative  Model") +xlab("Variables")


################################################################
#Best Fit Model for each sub_category product
################################################################
# Camera accessories
# Simple linear regression model - Adj.R = 0.84 and MSE = 0.24

# Home Audioo
# Simple linear regression model - Adj.R = 0.81 and MSE = 0.16

# Game accessories
# Distributed lag model - Adj.R = 0.67 and MSE 0.32




################################################################
#Recommendation with the model output
################################################################
# Camera accessories
# The adjoining figure represent the elasticity of different variables w.r.t the overall sales figure.
# Positive elasticity means, increasing the value of the KPI would lead to increase in the sales.
# Affiliate spend has positive impact on sales. One unit of this ad spend will increase the sale by 0.17 units. 
# Sponsorship spend has positive impact  on sale and one unit of this ad spend  will increase sale by 0.16 units.  
# Dussehra sale has very positive  impact. It will increase the sale by .65 unit. 
#Company should promote Lens product as it has very positive effect on Revenue. 


# Home audio
# The adjoining figure represent the elasticity of different variables w.r.t the overall sales.
# Positive elasticity means increasing the value of the KPI would lead to increase in the sale.
# Content Marketing spend has positive impact on sales. One unit of this ad spend will increase the sale by 0.16 unit. 
# Sponsorship spend has positive impact  on sale and one unit of this ad spend  will increase sale by 0.10 unit.  
# Diwali sale has positive  impact. It will increase the sale by 0.24 unit. Same for Eid,Rathayatra sale. It will increase the sale by 0.44 unit. 
# Company should promote FM Radio and Home audio speaker as these have positive effect on  revenue. 


# Game accessories
# The adjoining figure represent the elasticity of different variables w.r.t the overall sales.
# Positive elasticity means increasing the value of the KPI would lead to increase in the sales.
# Affiliate spend has positive impact on sales. One unit of this ad spend will increase the sale by  0.13 unit. 
# Sponsorship spend has positive impact  on sale and one unit of this ad spend  will increase sale by 0.23 unit.  
# Dussehra sale has very positive  impact. It will increase the sale by 1.4 unit. 
# Company should promote Gamepad , Gaming Headset ,  Gaming Keyboard  and  Gaming Mouse products as these have positive effect on revenue. 

