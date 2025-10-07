########## Dataset: GPAdata_clean.csv ##########

# 1. Create a plot that shows the relationship between SAT Math (satm) and GPA
library(ggplot2)
df <- read.csv("GPAdata_clean.csv")
ggplot(df, aes(x = satm, y = gpa)) +
  geom_point(na.rm = TRUE) +
  labs(title = "SAT Math vs GPA", x = "SAT Math", y = "GPA")

# 2.Modify the plot so that sex categories are visually distinguished. What differences do you observe?
ggplot(df, aes(x = satm, y = gpa, color = sex)) +
  geom_point(na.rm = TRUE) +
  labs(title = "SAT Math vs GPA by Sex", x = "SAT Math", y = "GPA", color = "Sex")

# 3. Compare the GPA distribution between athletes and non-athletes using an appropriate plot.
ggplot(df, aes(x = athlete, y = gpa)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "GPA: Athletes (1) vs Non-Athletes (0)", x = "Athlete", y = "GPA")

# 4. Create a plot showing the relationship between SAT Verbal (satv) and GPA, separating the plots by sex
ggplot(df, aes(x = satv, y = gpa)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ sex) +
  labs(title = "SAT Verbal vs GPA by Sex", x = "SAT Verbal", y = "GPA")

# 5. Compute the average GPA grouped by sex. Which group has the higher GPA?
aggregate(gpa ~ sex, df, mean, na.rm = TRUE)

# 6. Compute the average SAT Math score for athletes vs non-athletes.
aggregate(satm ~ athlete, df, mean, na.rm = TRUE)



########## Dataset: Soft_drink-Example1.xlsx ##########

# 7. Create a frequency table of the different soft drink brands.
library(readxl)
df1 <- Soft_drink_Example1
table(trimws(df1$Soft_Drink))

# 8. Visualize the counts of each brand with a bar chart.
library(ggplot2)
df1$brand <- trimws(df1$Soft_Drink)
ggplot(df1, aes(x = brand)) +
  geom_bar() +
  labs(title = "Soft Drink Brand Counts", x = "Brand", y = "Count")

# 9. Which brand appears most frequently in the dataset?
freq <- table(trimws(df1$Soft_Drink))
names(freq)[which.max(freq)]

########## Datasets: donor_email.xlsx and Donnation_data.xlsx ########## 

# 10.Join the donor email list with the donation records so that each donation is linked to the donor’s state and email.
library(readxl)
library(dplyr)
donation <- read_excel("Donnation_data (1).xlsx")
donor <- read_excel("donor_email (1).xlsx")
# Join on donor name or ID (replace "Donor" with correct key)
merged_data <- left_join(donation, donor, by = "Donor")
head(merged_data)

# 11. After joining, calculate the total donation amount per donor.
total_per_donor <- merged_data %>%
  group_by(Donor) %>%
  summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Amount))

# 12. Create a plot showing the top 5 donors by total donation amount.
library(ggplot2)
top5 <- head(total_per_donor, 5)
ggplot(top5, aes(x = reorder(Donor, Total_Amount), y = Total_Amount, fill = Donor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 Donors by Total Donation",
       x = "Donor", y = "Total Donation Amount")

# 13. Compute the average donation amount by state. Which state has the highest average donation?
avg_state <- merged_data %>%
  group_by(State) %>%
  summarise(Avg_Donation = mean(Amount, na.rm = TRUE)) %>%
  arrange(desc(Avg_Donation))

# 14. Create a plot that shows donation amounts over time (date on the x-axis, amount on the y-axis).
merged_data$Date <- as.Date(merged_data$Date)

ggplot(merged_data, aes(x = Date, y = Amount)) +
  geom_line(color = "blue") +
  labs(title = "Donation Amounts Over Time",
       x = "Date", y = "Donation Amount")





















