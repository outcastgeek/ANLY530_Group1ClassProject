## @knitr installLibraries

install.packages("knitr")
#install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("Hmisc")
install.packages("cluster")
install.packages("rpart")
install.packages("rpart.plot") 
install.packages("RColorBrewer") 
#install.packages("rattle")
install.packages("NbClust")
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
install.packages("bindrcpp")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("naivebayes")

## @knitr loadLibraries

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(scales)
library(base)
library(kernlab)
library(gmodels)
library(corrplot)
library(Hmisc)
library(cluster)
library(rpart)
library(rpart.plot) 
library(RColorBrewer) 
#library(rattle)
library(NbClust)
library(ModelMetrics)
library(generics)
library(gower)
library(bindrcpp)
library(caret)
library(naivebayes)

## @knitr helperFunctions

# Obtains the full File Path
fullFilePath <- function(fileName)
{
  fileFolder <- "./data/"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  fileNamePath
}

# Converts column of Timestamps to Date
ttColToDate <- function(dFrame, colName) {
  dFrame[colName] <- as.POSIXct(dFrame[colName], origin="1970-01-01")
  dFrame
}

# Converts column to utf-8
toUtf8 <- function(column) {
  columnUtf8 <- iconv(enc2utf8(column), sub = "byte")
  columnUtf8
}

# Formats Data
fmt <- function(dt, caption = "") {
  fmt_dt <- dt %>%
    kable("latex", longtable = T, booktabs = T)
  fmt_dt
}

# Style Data
style <- function(dt, full_width = F, angle = 0) {
  style_dt <- dt %>%
    kable_styling(latex_options = "striped", full_width = full_width) %>%
    row_spec(0, angle = angle)
  style_dt
}

#Plot the within (cluster) sum of squares to determine the initial value for "k" 
wssplot <- function(data, nc=15, seed=1234){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")}

## @knitr prepData

twentyeight.reasons <- c(
  "I Certain infectious and parasitic diseases",
  "II Neoplasms",
  "III Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
  "IV Endocrine, nutritional and metabolic diseases",
  "V Mental and behavioural disorders",
  "VI Diseases of the nervous system",
  "VII Diseases of the eye and adnexa",
  "VIII Diseases of the ear and mastoid process",
  "IX Diseases of the circulatory system",
  "X Diseases of the respiratory system",
  "XI Diseases of the digestive system",
  "XII Diseases of the skin and subcutaneous tissue",
  "XIII Diseases of the musculoskeletal system and connective tissue",
  "XIV Diseases of the genitourinary system",
  "XV Pregnancy, childbirth and the puerperium",
  "XVI Certain conditions originating in the perinatal period",
  "XVII Congenital malformations, deformations and chromosomal abnormalities",
  "XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
  "XIX Injury, poisoning and certain other consequences of external causes",
  "XX External causes of morbidity and mortality",
  "XXI Factors influencing health status and contact with health services.",
  "patient follow-up",
  "medical consultation",
  " blood donation",
  "laboratory examination",
  "unjustified absence",
  "physiotherapy",
  "dental consultation"
)
four.seasons <- c("Summer", "Autumn", "Winter", "Spring")
twelve.months.and.none <- c(
  "None",
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

## @knitr loadSheets

#Set Data File Name:
Absenteeism_at_work_file <- "Absenteeism_at_work.csv"

# Absenteeism
Absenteeism_data <- Absenteeism_at_work_file %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE, sep = ";") %>%
  mutate(
    Month.of.absence = factor(Month.of.absence, labels = twelve.months.and.none),
    Reason.for.absence = factor(Reason.for.absence, labels = twentyeight.reasons),
    Seasons = factor(Seasons, labels = four.seasons),
    `Row#` = row_number(),
    Absence.levels = cut(
      Absenteeism.time.in.hours,
      breaks = c(0,1,2,3,4,8,9, max(Absenteeism.time.in.hours) + 1),
      labels = c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6"),
      right = FALSE
    )
  )

## @knitr dataExploration

Absenteeism_data %>% dim()

Absenteeism_data %>% colnames()

Absenteeism_data %>% summary()

Absenteeism_data_by_Reason <- Absenteeism_data %>%
  group_by(ID, Reason.for.absence) %>%
  mutate(AbsenteeismReasonHours = sum(Absenteeism.time.in.hours)) %>%
  filter(row_number()==1)

# Absenteeism Hours by Reason
ggplot(Absenteeism_data_by_Reason, aes(x = Reason.for.absence, y = AbsenteeismReasonHours)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  coord_flip() +
  xlab("Reason for Absence") +
  expand_limits(y=c(0.0, 50.0)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6, angle = 10, hjust = 1)) +
  ylab("Absenteeism time in hours") +
  ggtitle("Absenteeism Hours by Reason")

Absenteeism_data_by_Seasons <- Absenteeism_data %>%
  group_by(ID, Seasons) %>%
  mutate(AbsenteeismSeasonHours = sum(Absenteeism.time.in.hours)) %>%
  filter(row_number()==1)

# Absenteeism Hours by Reason per Season
ggplot(Absenteeism_data_by_Seasons, aes(x = Reason.for.absence, y = AbsenteeismSeasonHours, fill = Seasons)) +
  geom_bar(stat="identity") +
  xlab("Reason for absence") +
  expand_limits(y=c(0.0, 50.0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 65, hjust = 1)) +
  ylab("Absenteeism time in hours") +
  labs("Seasons") +
  ggtitle("Absenteeism Hours by Reason per Season")

# Absenteeism Hours by Season
ggplot(Absenteeism_data_by_Seasons, aes(x = Seasons, y = AbsenteeismSeasonHours)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  xlab("Seasons") +
  theme_bw() +
  ylab("Absenteeism time in hours") +
  ggtitle("Absenteeism Hours by Seasons")

Absenteeism_data_by_Month <- Absenteeism_data %>%
  group_by(ID, Month.of.absence) %>%
  mutate(AbsenteeismMonthHours = sum(Absenteeism.time.in.hours)) %>%
  filter(row_number()==1)

# Absenteeism Hours by Reason per Month
ggplot(Absenteeism_data_by_Month, aes(x = Reason.for.absence, y = AbsenteeismMonthHours, fill = Month.of.absence)) +
  geom_bar(stat="identity") +
  xlab("Reason for absence") +
  expand_limits(y=c(0.0, 50.0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 65, hjust = 1)) +
  ylab("Absenteeism time in hours") +
  labs("Month") +
  ggtitle("Absenteeism Hours by Reason per Month")

# Absenteeism Hours by Month
ggplot(Absenteeism_data_by_Month, aes(x = Month.of.absence, y = AbsenteeismMonthHours)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  xlab("Month of Absence") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 65, hjust = 1)) +
  ylab("Absenteeism time in hours") +
  ggtitle("Absenteeism Hours by Month")

Absenteeism_data_by_Groups <- Absenteeism_data %>%
  group_by(ID, Absence.levels) %>%
  mutate(AbsenteeismSeasonHours = sum(Absenteeism.time.in.hours)) %>%
  filter(row_number()==1)

# Absenteeism Hours by Reason per Group
ggplot(Absenteeism_data_by_Groups, aes(x = Reason.for.absence, y = AbsenteeismSeasonHours, fill = Absence.levels)) +
  geom_bar(stat="identity") +
  xlab("Reason for absence") +
  expand_limits(y=c(0.0, 50.0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 65, hjust = 1)) +
  ylab("Absenteeism time in hours") +
  labs("Groups") +
  ggtitle("Absenteeism Hours by Reason per Group")

## @knitr dataAnalysis

# Set Seed
set.seed(987654321)

# Obtain the column IDs
columnIDs <- which(!names(Absenteeism_data)%in%c())

# Convert all the dataframe to numeric
Absenteeism_data_numeric <- Absenteeism_data %>%
  select(-`Row#`) %>% # Ignore Row#
  mutate_all(funs(as.numeric), columnIDs)

sapply(Absenteeism_data_numeric, class) # Check that all columns were converted to numeric

wssplot(Absenteeism_data_numeric)

# Obtain the best number of Clusters
nc <- NbClust(Absenteeism_data_numeric, method = "kmeans")
nc

nc_tbl <- table(nc$Best.n[1,])

print(nc_tbl)

nc_df <- nc_tbl %>% as.data.frame()

ggplot(data=nc_df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("Number of Clusters") +
  theme_bw() +
  ylab("Number of Criteria") +
  ggtitle("Recommended number of clusters")

## @knitr semiSupervisedClassification

# The best number of Cluster nc according to the majority rule
K = 2

# Perform kmeans clustering
abs_km.out <- kmeans(Absenteeism_data_numeric, K)

# Plot K-means Clusters
autoplot(abs_km.out, data = Absenteeism_data_numeric, frame = TRUE, frame.type = 'norm', label = TRUE, label.size = K)

print(abs_km.out$size)
print(abs_km.out$centers)

Absenteeism_data_numeric %>%
  select(-Absence.levels) %>%
  aggregate(
    by = list(cluster=abs_km.out$cluster),
    mean
  ) %>%
  print()

# Use a confusion or truth table to evaluate how well the k-Means analysis performed
abs_km.ct <- table(Absenteeism_data_numeric$Absence.levels, abs_km.out$cluster)
print(abs_km.ct)


#Generate a plot of the clusters
clusplot(Absenteeism_data_numeric, abs_km.out$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Set up Train model for classification
Absenteeism_data_df <- data.frame(k=abs_km.out$cluster, Absenteeism_data_numeric %>% select(-Absence.levels))
Absenteeism_data_df %>% dim()
print(str(Absenteeism_data_df))

Absenteeism_data_rdf <- Absenteeism_data_df[sample(1:nrow(Absenteeism_data_df)), ]
Absenteeism_data_rdf %>% dim()
head(Absenteeism_data_rdf)

Absenteeism_data_train <- Absenteeism_data_rdf[1:(as.integer(.75*nrow(Absenteeism_data_rdf))-1), ]
Absenteeism_data_test <- Absenteeism_data_rdf[(as.integer(.75*nrow(Absenteeism_data_rdf))):nrow(Absenteeism_data_rdf), ]

# Train the classifier and plot the results
abs.rfit <- rpart(k ~ ., data=Absenteeism_data_train, method="class")

#fancyRpartPlot(abs.rfit) not useful
rpart.plot(abs.rfit)

# Evaluate the model
abs.rpred <- predict(abs.rfit, Absenteeism_data_test, type="class")
print(table(abs.rpred, Absenteeism_data_test$k))

## @knitr svmClassification

# Split the dataset into 80% training and 20% testing datasets
abs_traindata <- Absenteeism_data %>% sample_frac(0.75) #[1:592,]
abs_testdata <- Absenteeism_data %>% anti_join(abs_traindata, by="Row#") #[593:740,]

abs_traindata <- abs_traindata %>% select(-`Row#`) # Ignore Row#
abs_testdata <- abs_testdata %>% select(-`Row#`) # Ignore Row#

abs_traindata %>% dim()
abs_testdata %>% dim()

# Train an SVM classifier
abs_svm_classifier <- ksvm(Absence.levels ~ ., data = abs_traindata, kernel = "vanilladot")

abs_svm_classifier

# plot the locations of the support vectors
abs_svm_plot_data <- (Absenteeism_data  %>% select(-`Row#`))[alphaindex(abs_svm_classifier)[[1]],]
kernlab::plot(abs_svm_plot_data, pch=19, main="Support vectors - linear model")

# Evaluate the model
abs_svm_predictions <- predict(abs_svm_classifier, abs_testdata)
table(abs_svm_predictions, abs_testdata$Absence.levels)
abs_agreement <- abs_svm_predictions == abs_testdata$Absence.levels
table(abs_agreement)

## @knitr naiveBayesClassification


