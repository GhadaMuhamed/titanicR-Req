setwd("/home/ghada/Desktop/rReq")
data <- read.csv("titanic.csv")
print("*****-------------------  Dimensions of Data ------------------*****")
cat("Rows: ", nrow(data), "\tColumns: ", ncol(data), '\n')
print("*****-------------------  Structure of Data  ------------------*****")
print(str(data))
print("*****-------------------   Summary of Data   ------------------*****")
print(summary(data))
print("*****-------------------   Summary of Age   ------------------*****")
dataAge = data$Age
summ = summary(dataAge)
print(summ)
print("*****-------------------   First quartile   ------------------*****")
print(summ["1st Qu."])
print("*****-------------------   Third quartile   ------------------*****")
print(summ["3rd Qu."])
print("*****-------------------   First quartile means 25% observations are below this quantity(approx)   ------------------*****")
print("*****-------------------   Third quartile means 75% observations are below this quantity(approx)   ------------------*****")
print("*****-------------------   Does Age have any NA fileds   ------------------*****")
print(anyNA(dataAge, recursive = FALSE))
embark = data$Embarked
print("*****-------------------   Embarked type   ------------------*****")
print(class(embark))
print(typeof(embark))
print("*****-------------------   Embarked levels   ------------------*****")
print(levels(embark))
#the data needs to be cleaned and processed
print("*****-------------------   Removing NA rows   ------------------*****")

ommittedData = na.omit(data)
print("*****-------------------   Removing unexpected embarked values   ------------------*****")
ommittedCleanedData <- subset(ommittedData, Embarked == "C" | Embarked == "Q" | Embarked == "S")
print("*****-------------------   Is there still NA fields  ------------------*****")
ommittedCleanedDataAge = ommittedCleanedData$Age
print(anyNA(ommittedCleanedDataAge, recursive = FALSE))
print("*****-------------------   Factoring EMbarked variable  ------------------*****")
ommittedCleanedDataEmbarked = ommittedCleanedData$Embarked
ommittedCleanedDataEmbarked <- factor(ommittedCleanedDataEmbarked)
print(levels(ommittedCleanedDataEmbarked))
print("*****-------------------   Removing cabin and tickets columns  ------------------*****")
drops <- c("Ticket","Cabin")
finalData = ommittedCleanedData[ , !(names(ommittedCleanedData) %in% drops)]
print(str(finalData))


print("******-------------------   Females   ------------------******")
females_num = sum(data$Gender=='female')
print(females_num)
print("******-------------------   Males   ------------------******")
males_num = sum(data$Gender=='male')
print(males_num)


png(file = "gender_pie.jpg")
pie(c(females_num, males_num), c("Females", "Males"), main = "Gender pie chart", col = c('red', 'blue'))
# Save the file.
dev.off()


survive_females = data$Survived[data$Gender=='female']
survived_f = sum(survive_females)
unsurvived_f = length(survive_females) - survived_f

png(file = "female_survive_pie.jpg")
pie(c(survived_f, unsurvived_f), c("Survived", "Didn't Survived"), main = "Titanic Females Survive pie chart", col = c('red', 'blue'))
dev.off()


survive_males = data$Survived[data$Gender=='male']
survived_m = sum(survive_males)
unsurvived_m = length(survive_males) - survived_m

png(file = "male_survive_pie.jpg")
pie(c(survived_m, unsurvived_m), c("Survived", "Didn't Survived"), main = "Titanic Males Survive pie chart", col = c('red', 'blue'))
dev.off()

png(file = "male_female_survive_pie.jpg")
pie(c(survived_m, survived_f), c("Survived Males", "Survived Females"), main = "Survived Male and Female pie chart", col = c('red', 'blue'))
dev.off()


# Women are so lucky to be women :D :D 

# Class 1 survive
survive_1 = data$Survived[data$Pclass==1]
survived_1 = sum(survive_1)
unsurvived_1 = length(survive_1) - survived_1

png(file = "class_1_survive_pie.jpg")
pie(c(survived_1, unsurvived_1), c("Survived", "Didn't Survived"), main = "Class 1 Survive pie chart", col = c('blue', 'red'))
dev.off()

# Class 2 survive
survive_2 = data$Survived[data$Pclass==2]
survived_2 = sum(survive_2)
unsurvived_2 = length(survive_2) - survived_2

png(file = "class_2_survive_pie.jpg")
pie(c(survived_2, unsurvived_2), c("Survived", "Didn't Survived"), main = "Class 2 Survive pie chart", col = c('blue', 'red'))
dev.off()

# Class 3 survive
survive_3 = data$Survived[data$Pclass==3]
survived_3 = sum(survive_3)
unsurvived_3 = length(survive_3) - survived_3

png(file = "class_3_survive_pie.jpg")
pie(c(survived_3, unsurvived_3), c("Survived", "Didn't Survived"), main = "Class 3 Survive pie chart", col = c('blue', 'red'))
dev.off()


# Stacked Bar Plot with Colors and Legend
counts <- cbind(first = c(survived_1, unsurvived_1), second = c(survived_2, unsurvived_2), third = c(survived_3, unsurvived_3))
png(file = "class_relation_barplot.jpg")
barplot(counts, main="Class Survival Relation", xlab="Class Number",
        ylab="Number of Survivals", col=c("blue","red"))

legend("topleft",
       c("Survived","Not survived"),
       fill = c("blue","red")
)
dev.off()

# highest percentage of survivals is from the highest class (racism)


boxplot(data$Age)

# don't know

#y <- dnorm(data$Age)

png(file = "density.png")
hist(data$Age)
# Save the file.
dev.off()

new_data <- data.frame(
  Name = data$Name,
  Survived = data$Survived
)
print(str(new_data))

write.csv(new_data, file = "titanic_preprocessed.csv")

