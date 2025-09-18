###Data Cleaning & Descriptive Analytics - Data Analytics Training Sessions Lab-1

#Data Descriptions-Use GPAdata.csv
head(GPAdata_clean)
str(GPAdata_clean)

#To get the summary of each column in the dataset 
summary(GPAdata_clean)

table(GPAdata_clean$sex)
table(GPAdata_clean$athlete)

#Missing Data & Data Cleaning
GPAdata_clean$sex <- ifelse(GPAdata_clean$sex %in% c('1','male'),1,
                            ifelse(GPAdata_clean$sex == '2',2,NA))
table(GPAdata_clean$sex)

#Make a new dataframe 
df <- GPAdata_clean
df$athlete <- ifelse(df$athlete %in% c(1,'1'),1,
                     ifelse(df$athlete %in% c(0,'0'),0,NA))
table(df$athlete)

summary(df$satv)
df$satv[df$satv == 475000] <- 475
table(df$satv)

sum(is.na(df[,c('gpa','satv','satm')]))
sum(is.na(df$gpa))
sum(is.na(df$satv))
summary(df$gpa)

#Mean Imputation
gpa_mean <- mean(df$gpa, na.rm = TRUE)
df$gpa[is.na(df$gpa)] = gpa_mean

satv_mean <- mean(df$satv, na.rm = TRUE)
df$satv[is.na(df$satv)] = satv_mean

#Converting Categorical Variables
head(df)

#sex: recoding to "Male/Female"
df$sex <- ifelse(df$sex == 1, "Male",
                 ifelse(df$sex == 2, "Female", NA))

#athlete: recoding to "Athlete/Non-athlete"
df$athlete <- ifelse(df$athlete == 1, "Athlete",
                     ifelse(df$athlete == 0, "Non-athlete", NA))
head(df) 

#Exploratory Data Analysis (EDA) 
summary(df$gpa)
summary(df$satm)
summary(df$satv)

sd(df$gpa)
sd(df$satm)
sd(df$satv)

#Visualizations
#Histogram
hist(df$gpa,
     main = "Histogram of GPA",
     xlab = "GPA",
     ylab = "Frequency",
     col = "blue")

hist(df$satm,
     main = "Histogram of SAT Maths",
     xlab = "SAT MAth Score",
     ylab = "Frequency",
     col = "Green")

hist(df$satv,
     main = "Histogram of SAT Verbal",
     xlab = "SAT Verbal Score",
     ylab = "Frequency",
     col = "Red")

#Boxplots
#GPA by sex
boxplot(df$gpa~df$sex,
        main = "Boxplot of GPA by Sex",
        xlab = "Sex",
        ylab = 'GPA',
        col = c('Green', 'Red'))

#GPA by athlete status
boxplot(df$gpa~df$athlete,
        main = "Boxplot of GPA by Athlete",
        xlab = "Athlete Status",
        ylab = 'GPA',
        col = c('Blue', 'Yellow'))

#Correlation
df$gpa <- as.integer(round(df$gpa))
table(df$gpa)

#gpa and satm
cor(df$gpa,df$satm)
plot(df$gpa,df$satm,
     xlab = 'GPA',
     ylab = 'SATM',
     col =c('blue', 'red'))

#satm and satv
cor(df$satm, df$satv)
plot(df$satm, df$satv,
     xlab = 'SATM',
     ylab = 'SATV',
     col =c('blue', 'red'))

#gpa and satv
cor(df$gpa, df$satv)
plot(df$gpa, df$satv,
     xlab = 'GPA',
     ylab = 'SATV',
     col =c('blue', 'red'))
