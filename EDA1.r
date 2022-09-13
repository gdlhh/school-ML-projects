# Lab 5 Logistic Regression

# Objective 1 - Perform Logistic Regression with and without preproccessing and compare the results.
# Objective 2 - Compute sensitivity, specificity and AUC.  draw ROC curves  
# Objective 3 - Perform various data cleaning and  normalization operations and compare results

library(dplyr)
library(ggplot2)
library(lubridate)
library(GGally)
library(arm)
library(mice)
library(caret)

df = read.csv(file = 'C:\\Users\\Bob\\Desktop\\AML\\archive\\hotel_booking.csv', header = T)
str(df) #For meal column, BB means bed and breakfast, SC means self catering, HB (half-board) means breakfast and dinner incl., FB (full-board) means breakfast, 
        #lunch and dinner are incl.
        #For market segment column, TA means travel agent, TO means Tour operator
        #For distribution channel column, these represent the booking distributioin channels, GDS means global distributed system (computerised)
        #For deposit_type column, No Deposit – no deposit was made; Non Refund – a deposit was made in the value of the total stay cost; Refundable
        #For customer type column, Group – when the booking is associated to a group; Transient – when the booking is not part of a group or contract, and is 
        #generally, they are walk-in guests, last minute or bookers or simply people that require a very short term stay in your facility, 
        #Contract - when the booking has an allotment or other type of contract associated to it;
        #adr is average daily rate
        #For reservation status column
        #Reservation last status, assuming one of three categories:	BO
        #Canceled – booking was canceled by the customer;
        #Check-Out – customer has checked in but already departed;
        #No-Show – customer did not check-in and did inform the hotel of the reason why
df = df[,-c(33,34,35,36,14,4,5,6,7,32,31)]

#To see if changing the room affects cancellation

df_room_types = df %>% dplyr::select(is_canceled, reserved_room_type, assigned_room_type) %>%
                        mutate(similar_room = if_else(assigned_room_type == reserved_room_type, 1, 0))

ggplot(df_room_types, aes(x = factor(similar_room), fill = factor(is_canceled))) + geom_bar() + theme_light() + ggtitle('Number of cancellations based on change of room')

#Determining which combination type is an upgrade or downgrade

cancelled_room_types = df_room_types[df_room_types$similar_room == 0,] #Subsetting all rooms that were changed
cancelled_room_types$assumption = if_else(cancelled_room_types$reserved_room_type > cancelled_room_types$assigned_room_type, 'Assumed downgrade', 'Assumed upgrade')

#Plotting based on assumption

ggplot(cancelled_room_types, aes(x = assumption, fill = factor(is_canceled))) + geom_bar() + theme_minimal()

sum(cancelled_room_types$assumption == 'Assumed downgrade')
sum(cancelled_room_types[cancelled_room_types$assumption == 'Assumed downgrade',1])

sum(cancelled_room_types$assumption == 'Assumed upgrade')
sum(cancelled_room_types[cancelled_room_types$assumption == 'Assumed upgrade',1])

# Factorise the character columns

df$hotel = as.factor(df$hotel)
df$meal = as.factor(df$meal)
df$market_segment = as.factor(df$market_segment)
df$distribution_channel = as.factor(df$distribution_channel)
df$reserved_room_type = as.factor(df$reserved_room_type)
df$assigned_room_type = as.factor(df$assigned_room_type)
df$deposit_type = as.factor(df$deposit_type)
df$customer_type = as.factor(df$customer_type)


md.pattern(df)


# arrival_date_month should be an ordinal encoding


# One hot encoding

#Step1: Dummify the variables

dmy = dummyVars(" ~ .", data = df)
df_dmy = data.frame(predict(dmy, newdata = df))


#Correlation plot

corr_simple <- function(data=df,sig=0.5){
        #convert data to numeric in order to run correlations
        #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
        df_cor <- data %>% mutate_if(is.character, as.factor)
        df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
        #run a correlation and drop the insignificant ones
        corr <- cor(df_cor)
        #prepare to drop duplicates and correlations of 1     
        corr[lower.tri(corr,diag=TRUE)] <- NA 
        #drop perfect correlations
        corr[corr == 1] <- NA 
        #turn into a 3-column table
        corr <- as.data.frame(as.table(corr))
        #remove the NA values from above 
        corr <- na.omit(corr) 
        #select significant values  
        corr <- subset(corr, abs(Freq) > sig) 
        #sort by highest correlation
        corr <- corr[order(-abs(corr$Freq)),] 
        #print table
        print(corr)
        #turn corr back into matrix in order to plot with corrplot
        mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

}

corr_simple(data = df_dmy)

#Removing redundant variables

df = df[,-c(9,10,11,15,16)]

dmy = dummyVars(" ~ .", data = df)
df_dmy = data.frame(predict(dmy, newdata = df))

ggcorr(df_dmy, size = 0.1, label = T, label_size = 2, label_alpha = T)


# Cleaned dataset

df = df[,c(1,2,6,9,10,11,12,13,16,17,19,20)]

write.csv(df,'C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned.csv')

library(ISLR)
library(dplyr)


df = read.csv(file ='C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned.csv', header = T)
df = df[,c(-1)]


df$hotel = as.factor(df$hotel)
df$deposit_type = as.factor(df$deposit_type)
df$customer_type = as.factor(df$customer_type)
df$is_repeated_guest = as.factor(df$is_repeated_guest)
df$is_canceled = as.factor(df$is_canceled)

par(mar=c(1,1,1,1)) # To set margins smaller
par(mfrow=c(1,7))
for(i in c(3,5,6,7,9,11,12)) {
        hist(df[,i], main=names(df)[i])
}

for(i in c(3,5,6,7,9,11,12)) {
        boxplot(df[,i], main=names(df)[i])
}


library(Amelia)
library(mlbench)
library(corrplot)

correlations = cor(df[,c(3,5,6,7,9,11,12)])
{plot.new(); dev.off()}
corrplot(correlations, method="circle", tl.cex = 0.5)

missmap(df, col=c("blue", "red"), legend=FALSE)

#imputing the values


df[is.na.data.frame(df)] = 0

write.csv(df,'C:\\Users\\Bob\\Desktop\\AML\\archive\\df_cleaned1.csv')
