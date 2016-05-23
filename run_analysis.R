#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed 
	#to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
	#This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Good luck!

##############################

### steps performed to complete the tasks ###

#clear existing variables
rm(list=ls())
ls()

#set working directory
setwd("C:/training/data_science_specialization/R")
getwd()

#download data (in windows)
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "my_dataset.zip"
if(!file.exists(file)){
	print("downloading a file")
	download.file(url, file)
}

#unzip the download data
dir <- "UCI HAR Dataset"
resdir <- "result"

if(!file.exists(dir)){
	print("unzipping a file")
	unzip(file, list = FALSE, overwrite = TRUE)
} 

#create result directory
if(!file.exists(resdir)){
	print("create result dir")
	dir.create(resdir)
}

#read the files to data.table objects
fileToDataTable  <- function (filename, cols = NULL){
	print(paste("data.table:", filename))
	f <- paste(dir,filename,sep="/")
	dt <- data.frame()
	if(is.null(cols)){
		dt <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		dt <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
	}
	dt
}

#run and check fileToDataTable
features <- fileToDataTable("features.txt")

#read the data and build database
dt <- function(type, features){
	print(paste("fetching data", type))
	subject <- fileToDataTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y <- fileToDataTable(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x <- fileToDataTable(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject,y,x))
}

#run and verify dt
test <- dt("test", features)
train <- dt("train", features)

#save the tidy data in result directory
rs <- function (data, name){
	print(paste("saving to result file ", name))
	file <- paste(resdir, "/", name,".csv" ,sep="")
	write.csv(data,file)
}

#merges the training and test data sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
rs(mean_std,"mean_std_dataset")

#uses descriptive activity names to name the activities in the data set
activity_labels <- fileToDataTable("activity_labels.txt")

#appropriately labels the data set with descriptive variable names 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject. 
tidy_dt <- ddply(mean_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dt)[-c(1:2)] <- paste(colnames(tidy_dt)[-c(1:2)], "_mean", sep="")
rs(tidy_dt,"tidy_dataset")