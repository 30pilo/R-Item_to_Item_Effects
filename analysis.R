install.packages("readr")
library(readr)

install.packages("tidyverse")
library(tidyverse)


data <- read.csv("OneDrive/pfda.csv")
data

summary(data)

View(data)

summary(data$Mjob)

colSums(is.na(data))

names(data)

#data cleaning
nrow(data)

#pre-processing
dim(data)

str(data)

###################################

#analysis1.1
work_exp <- subset(data, workex == "Yes")
num_students_work_exp <- sum(data$workex == "Yes")
num_students_work_exp
work_ex <- subset(data, workex == "No")
num_students_work_ex <- sum(data$workex == "No")
num_students_work_ex
with_work <- sum(data$workex == "Yes")
without_work <- sum(data$workex == "No")
work_counts <- data.frame(
  Work_Experience = c("With Work Experience", "Without Work Experience"),
  Count = c(with_work, without_work)
)
pie(work_counts$Count, labels = work_counts$Work_Experience, 
    col = c("lightblue", "lightgreen"), main = "Work Experience of Students")

#analysis1.2
work_exp <- subset(data, workex == "Yes")

male_count <- sum(work_exp$gender == "M")
female_count <- sum(work_exp$gender == "F")

df <- data.frame(
  gender = c("Male", "Female"),
  count = c(male_count, female_count)
)
pie(df$count, labels = df$gender, main = "Gender Distribution of Work Experience")

#analysis1.3
work_exp <- subset(data, workex == "Yes")
work_exp_age <- subset(work_exp, age >= 18 & age <= 23)
age_counts <- table(work_exp_age$age)
barplot(age_counts, xlab = "Age", ylab = "Number of People",
        main = "Number of People with Work Experience (18 to 23) by Age")

#analysis1.4
etest_passed <- data$etest_p >= 80
worked <- data$workex == "Yes"
worked_etest_passed <- data[worked & etest_passed, ]
num_passed <- nrow(worked_etest_passed)
num_failed <- sum(worked) - num_passed
etest_counts <- data.frame(
  Etest_Pass = c("Passed", "Failed"),
  Count = c(num_passed, num_failed)
)
pie(etest_counts$Count, labels = etest_counts$Etest_Pass, 
    col = c("lightblue", "lightgreen"), main = "Etest Pass/Fail of Students with Work Experience")

#analysis1.5
etest_passed <- data$etest_p >= 80
noworked <- data$workex == "No"
noworked_etest_passed <- data[noworked & etest_passed, ]
num_passed <- nrow(noworked_etest_passed)
num_failed <- sum(noworked) - num_passed
etest_counts <- data.frame(
  Etest_Pass = c("Passed", "Failed"),
  Count = c(num_passed, num_failed)
)
pie(etest_counts$Count, labels = etest_counts$Etest_Pass, 
    col = c("lightblue", "lightgreen"), main = "Etest Pass/Fail of Students with Work Experience")

#analysis1.6
experienced_data <- subset(data, workex == "Yes")
experienced_data
max_score <- max(experienced_data$etest_p)
max_score
min_score <- min(experienced_data$etest_p)
min_score

worked <- data$workex == "Yes"
worked_data <- data[worked, ]
plot(worked_data$etest_p, type = "l", xlab = "Student Index", ylab = "Etest Score", 
     main = "Etest Scores of Students with Work Experience")

#analysis1.7
experienced_data <- subset(data, workex == "No")
experienced_data
max_score <- max(experienced_data$etest_p)
max_score
min_score <- min(experienced_data$etest_p)
min_score

noworked <- data$workex == "No"
noworked_data <- data[noworked, ]
plot(noworked_data$etest_p, type = "l", xlab = "Student Index", ylab = "Etest Score", 
     main = "Etest Scores of Students with Work Experience")

#analysis1.8
ggplot(data, aes(x=etest_p, y=workex, color=factor(workex))) +
  geom_point() +
  labs(x="Employment Exam Scores", y="Work Experience", color="Work Experience") +
  theme_minimal()


##############################################################

#analysis2.1
mjob_status <- subset(data, select=c("Mjob", "status"))
placed <- subset(mjob_status, status=="Placed")
freq_table <- table(placed$Mjob)
barplot(freq_table, main="Mjob of Placed Students", 
        xlab="Mjob", ylab="Number of Placed Students", 
        col=c("blue", "pink", "green", "orange", "purple"), 
        names.arg=c("Teacher", "Healthcare", "Civil Services", "Business", "Others"))

#analysis2.2
fjob_status <- subset(data, select=c("Fjob", "status"))
placed <- subset(fjob_status, status=="Placed")
freq_table <- table(placed$Fjob)
barplot(freq_table, main="Fjob of Placed Students", 
        xlab="Fjob", ylab="Number of Placed Students", 
        col=c("blue", "pink", "green", "orange", "purple"), 
        names.arg=c("Teacher", "Healthcare", "Civil Services", "Business", "Others"))

#analysis2.3
placement_data_job <- subset(data, status == "Placed")
table(placement_data_job$Medu, placement_data_job$Mjob)

work_by_mom <- aggregate(data$work, by = list(Mom_Ed = data$Fedu, Work = data$work), FUN = length)
library(ggplot2)
ggplot(work_by_mom, aes(x = Mom_Ed, y = x, fill = Work)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Type of Work by Mother Education", x = "Mother Education", y = "Count") +
  theme_classic()

#analysis2.4
placement_data_job <- subset(data, status == "Placed")
table(placement_data_job$Fedu, placement_data_job$Fjob)

work_by_dad <- aggregate(data$work, by = list(Dad_Ed = data$Fedu, Work = data$work), FUN = length)
library(ggplot2)
ggplot(work_by_dad, aes(x = Dad_Ed, y = x, fill = Work)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Type of Work by Father Education", x = "Father Education", y = "Count") +
  theme_classic()

#analysis2.5
placement_counts <- aggregate(status ~ gender + famsup + Fjob + Mjob, data = data, FUN = length)
ggplot(placement_counts, aes(x = famsup, y = status, fill = gender)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Parent Education Level", y = "Number of Placements", fill = "Gender") +
  scale_fill_manual(values = c("#FF9999", "#66CCFF")) + 
  theme_classic()

#analysis2.6
subset_data <- subset(data, (data$Mjob == "teacher" & data$Fjob == "teacher"))
placement_count <- table(subset_data$status)
pie(placement_count, main = "Placement Status of Students with Parents who have Master's Degrees")


#analysis2.7
ggplot(data, aes(x = factor(ssc_b), fill = status)) + 
  geom_bar(position = "dodge") +
  facet_grid(. ~ factor(famsup)) +
  xlab("Students with different levels of parental education") +
  ylab("Count") +
  ggtitle("Placement Status by Parental Education Level") +
  scale_fill_manual(values=c("#008080", "#FFA07A")) 

################################################################

#analysis3.1
central_count <- sum(data$ssc_b == "Central")
cat("The number of students who went to Central Board in their secondary education is:", central_count)
central_ssc_data <- data[data$ssc_b == "Central",]
central_hsc_count <- table(central_ssc_data$hsc_b)
cat("Number of students who went to Central, State, or Private Board for higher secondary education after Central Board in their secondary education:\n")
print(central_hsc_count)

central_students <- subset(data, ssc_b == "Central")
count_df <- data.frame(table(central_students$hsc_s))
names(count_df) <- c("Higher_Education_Institution", "Count")
hist(count_df$Count, breaks = seq(1000, max(count_df$Count) + 5, by = 5),
     col = "blue",
     xlab = "Number of Students",
     main = "Number of Students by Higher Education Institution After Going to Central in Secondary Education",
     ylab = "Frequency")
axis(side = 1, at = seq(1000, max(count_df$Count) + 5, by = 5), labels = seq(1000, max(count_df$Count) + 5, by = 5))

#analysis3.2
state_count <- sum(data$ssc_b == "State")
cat("The number of students who went to State Board in their secondary education is:", state_count)
state_ssc_data <- data[data$ssc_b == "State",]
state_hsc_count <- table(state_ssc_data$hsc_b)
cat("Number of students who went to Central, State, or Private Board for higher secondary education after State Board in their secondary education:\n")
print(state_hsc_count)

state_students <- subset(data, ssc_b == "State")
count_df <- data.frame(table(state_students$hsc_s))
names(count_df) <- c("Higher_Education_Institution", "Count")
hist(count_df$Count, breaks = seq(1000, max(count_df$Count) + 5, by = 5),
     col = "blue",
     xlab = "Number of Students",
     main = "Number of Students by Higher Education Institution After Going to Central in Secondary Education",
     ylab = "Frequency")
axis(side = 1, at = seq(1000, max(count_df$Count) + 5, by = 5), labels = seq(1000, max(count_df$Count) + 5, by = 5))

#analysis3.3
private_count <- sum(data$ssc_b == "Private")
cat("The number of students who went to Private Board in their secondary education is:", private_count)
private_ssc_data <- data[data$ssc_b == "Private",]
private_hsc_count <- table(private_ssc_data$hsc_b)
cat("Number of students who went to Central, State, or Private Board for higher secondary education after Private Board in their secondary education:\n")
print(private_hsc_count)

private_students <- subset(data, ssc_b == "Private")
count_df <- data.frame(table(private_students$hsc_s))
names(count_df) <- c("Higher_Education_Institution", "Count")
hist(count_df$Count, breaks = seq(1000, max(count_df$Count) + 5, by = 5),
     col = "blue",
     xlab = "Number of Students",
     main = "Number of Students by Higher Education Institution After Going to Central in Secondary Education",
     ylab = "Frequency")
axis(side = 1, at = seq(1000, max(count_df$Count) + 5, by = 5), labels = seq(1000, max(count_df$Count) + 5, by = 5))

#analysis3.4
private_ssc <- subset(data, ssc_b == "Private")
private_ssc_placed <- sum(private_ssc$status == "Placed")
private_ssc_not_placed <- sum(private_ssc$status == "Not Placed")
private_ssc_placed_percentage <- private_ssc_placed / (private_ssc_placed + private_ssc_not_placed) * 100
pie(c(private_ssc_placed_percentage, 100 - private_ssc_placed_percentage), labels = c("Placed", "Not Placed"), col = c("green", "red"), 
       main = "Placement among those who went to private secondary education")



#################################################################

#analysis4.1
high_salary <- subset(data, salary >= 350000)
num_high_salary <- nrow(high_salary)
cat("The number of people with a salary of 350000 or more is:", num_high_salary)

data$salary_above_350k <- ifelse(data$salary > 350000, "Yes", "No")
salary_counts <- table(data$salary_above_350k)
percentages <- round((salary_counts / sum(salary_counts)) * 100, 1)
pie(percentages, labels = c("Below 350k", "Above 350k"), col = c("red", "green"), main = "Salary Distribution")

#analysis4.2
high_salary_data <- subset(data, salary > 350000)
placed_count <- sum(high_salary_data$status == "Placed")
cat("Number of placed people among those who receive more than 350,000 in salary:", placed_count)

data$placed_or_no <- ifelse(data$salary > 350000, "Placed", "Not Placed")
num_placed <- table(data$placed_or_no)
percentages <- round((num_placed / sum(num_placed)) * 100, 1)
pie(percentages, labels = c("Placed", "Not Placed"), col = c("red", "green"), main = "Salary Distribution")

#analysis4.3
subset_data <- subset(data, salary > 350000)
ggplot(subset_data, aes(x = age)) + 
  geom_bar() +
  ggtitle("Number of People by Age with Salary > 350000") +
  xlab("Age") +
  ylab("Count") +
  theme_minimal()

#analysis4.4
subset_data <- subset(data, salary > 350000)
table_data <- table(subset_data$workex)
df_table <- as.data.frame(table_data)
colnames(df_table) <- c("Worked", "Count")
ggplot(df_table, aes(x = Worked, y = Count, fill = Worked)) +
  geom_bar(stat = "identity") +
  ggtitle("Salary Distribution by Work Experience") +
  xlab("Work Experience") +
  ylab("Count") +
  theme_minimal()

####################################################################

#analysis5.1
num_paid <- sum(data$paid == "yes")
cat("Number of students who paid class:", num_paid, "\n")

num_paid <- sum(data$paid == "yes")
num_not_paid <- sum(data$paid == "no")
perc_paid <- num_paid / nrow(df) * 100
perc_not_paid <- num_not_paid / nrow(df) * 100
labels <- c("Paid", "Not Paid")
values <- c(perc_paid, perc_not_paid)
colors <- c("blue", "red")
pie(values, labels = labels, col = colors, main = "Tuition Payment Percentage")

#analysis5.2
relevant_cols <- c("famsup", "paid")
filtered_data <- data[complete.cases(data[, relevant_cols]), ]
num_with_support_and_tuition <- nrow(filtered_data[filtered_data$famsup == "yes" & filtered_data$paid == "yes", ])
cat("Number of students with family support and paid tuition:", num_with_support_and_tuition, "\n")

paid_data <- subset(data, paid == "yes")
with_famsup <- subset(paid_data, famsup == "yes")
without_famsup <- subset(paid_data, famsup == "no")
with_famsup_pct <- nrow(with_famsup) / nrow(paid_data) * 100
without_famsup_pct <- nrow(without_famsup) / nrow(paid_data) * 100
pie_data <- data.frame(category = c("With Family Support", "Without Family Support"),
                       pct = c(with_famsup_pct, without_famsup_pct))
ggplot(pie_data, aes(x="", y=pct, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "Percentage of Students Who Paid Tuition and Received Family Support",
       fill = "Category",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(hjust = 0.5))

#analysis5.3
paid_placement <- subset(data, paid=="yes" & status=="Placed")
nrow(paid_placement)

paid_data <- subset(data, paid == "yes")
status_placed <- subset(paid_data, status == "Placed")
status_noplaced <- subset(paid_data, status == "Not Placed")
status_placed_pct <- nrow(status_placed) / nrow(paid_data) * 100
status_noplaced_pct <- nrow(status_noplaced) / nrow(paid_data) * 100
pla_data <- data.frame(category = c("Placed", "Not Placed"),
                       pct = c(status_placed_pct, status_noplaced_pct))
ggplot(pla_data, aes(x="", y=pct, fill=category)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(title = "people paid tuition who were Placed and those who did not",
       fill = "Category",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(hjust = 0.5))


#################################################################################

#analysis6.1
placed_students <- data[data$status == "Placed", ]
age_counts <- table(placed_students$age)
barplot(age_counts, 
        main = "Number of Placed Students by Age", 
        xlab = "Age", 
        ylab = "Number of Students")

#analysis6.2
placed_students <- data[data$status == "Placed", ]
gender_counts <- table(placed_students$gender)
barplot(gender_counts, 
        main = "Number of Placed Students by Gender", 
        xlab = "Gender", 
        ylab = "Number of Students")

#analysis6.3
placed_students <- data[data$status == "Placed", ]
famsup_counts <- table(placed_students$famsup)
barplot(famsup_counts, 
        main = "Number of Placed Students with/without Family Support", 
        xlab = "Family Support", 
        ylab = "Number of Students")

#analysis6.4
placed_data <- subset(data, status == "Placed")
location_counts <- table(placed_data$address)
barplot(location_counts, main = "Placed Participants by Location", xlab = "Location", ylab = "Number of Participants",
               col = c("blue", "green"))

############################################################

#analysis7.1
activity_data <- data[, c("gender", "activities")]
activity_table <- table(activity_data$gender, activity_data$activities)
barplot(activity_table, main = "Number of Men and Women in Activities", xlab = "Activity Participation", 
        ylab = "Number of Students", col = c("blue", "pink"),  legend.text = c("Male", "Female"), 
        cex.legend=0.1, beside = TRUE,
        )


#analysis7.2
subset_data <- data[, c("activities", "internet")]
subset_data$no_activities <- ifelse(subset_data$activities == "no", "no", "yes")
ggplot(subset_data, aes(x = internet, fill = activities)) +
  geom_bar() +
  xlab("Internet Usage") +
  ylab("Number of Participants") +
  ggtitle("Internet Usage vs Extracurricular Activities") +
  scale_fill_manual(values = c("#8DD3C7", "#FDB462"), labels = c("Participated", "Did not Participate")) +
  theme_minimal()


