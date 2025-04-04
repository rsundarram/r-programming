# import the walmart stores dataset


df <- read.csv('/home/labsuser/July 23 Batch/Walmart_Store_sales.csv')


View(df)


dim(df)

read.csv(file.choose())

# data types of the columns

str(df)


# date- sales in that week

# factor format we need to change to date


library('lubridate')


# format -   dd-mm-yyyy





as.Date(df$Date,format='%d-%m-%Y')


# here we need to pass the format explicitly


# if we want R to do all of this

date-+month-+year 

df$Date <- dmy(df$Date)

str(df)



# Which store has maximum sales

# bring the dataset into 
store no - aggregated sales

library('dplyr')

ctrl+shft+ m = pipe 

agg_store <- df %>% group_by(Store) %>% summarise(agg_sales=sum(Weekly_Sales)) %>% arrange(desc(agg_sales)) 

agg_store


Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation



# max sd?


df %>% group_by(Store) %>% summarise(stand_dev=sd(Weekly_Sales)) %>% arrange(desc(stand_dev))

# Also, find out the coefficient of mean to standard deviation


df_1 <- df %>% group_by(Store) %>% summarise(avg_sal=mean(Weekly_Sales),
                                     std_dev=sd(Weekly_Sales))


df_1 %>% mutate(coef=avg_sal/std_dev)


Which store/s has good quarterly growth rate in Q3’2012

# add a column for quarter for that date

df$qtr <- quarter(df$Date,with_year = TRUE)


View(df)


# qtr sales by store


df_2 <- df %>% group_by(Store,qtr) %>% summarise(qtr_sales=sum(Weekly_Sales))

View(df_2)

  
# use the lag function




df_2$lag_4 <- lag(df_2$qtr_sales,4)

# calculate the growth rate now


df_2$qtr_growth <- ((df_2$qtr_sales-df_2$lag_4)*100)/df_2$lag_4


# filter it for 2012 q3



df_3 <- df_2 %>% filter(qtr=='2012.3') %>% arrange(desc(qtr_growth))

View(df_3)


# store 4 has a good growth rate in 2012 q3


Some holidays have a negative impact on sales. 
Find out holidays which have higher sales
than the mean sales in non-holiday season for all stores together


# non holiday season for all stores 


df %>% filter(Holiday_Flag==0) %>% summarise(mean_sales_non=mean(Weekly_Sales))


# find those weeks where holidays were there and sales>1041256


df %>% filter(Weekly_Sales>1041256,Holiday_Flag==1) %>% select(Date)


Provide a monthly and semester view of sales in units and give insights


# add a monthly column


month(df$Date)


semester(df$Date)


# add these 2 columns in the df


df_3 <- df %>% mutate(sem=semester(Date),mon=month(Date))

View(df_3)

# aggregate at this level


df_3 <- df_3 %>% group_by(mon) %>% summarise(monthly_sales=sum(Weekly_Sales))



# barplot- +ggplot2


library('ggplot2')



ggplot(data=df_3,mapping=aes(x=mon,y=monthly_sales))+ geom_bar(stat='identity')


For Store 1 – Build  prediction models to forecast demand


# filter the data for store 1 


df_a <- df %>% filter(Store==1)

df_a

Linear Regression – Utilize variables like date and restructure dates as 
1 for 5 Feb 2010 
(starting from the earliest date in order).
Hypothesize if CPI, unemployment, and fuel price have any impact on sales.


df_a <- df_a %>% arrange(Date)


df_a$new_dt <- rownames(df_a)

View(df_a)

# now the dataset is ready for regression


sales_model <- lm(Weekly_Sales ~ Holiday_Flag+Temperature+CPI
                  , data=df_a)

# output of the model


summary(sales_model)

# look at the F stat

p value for F stat < alpha, the overall model is significant

check the individual variables for p value
if the p value < alpha, then that variable is significant.


