README for getdata-015 - Coursera Data Scientist Specialization - course 3 "Getting and Cleaning Data" Date: 06/21/2015

Data for this project: Website for additional information on the data http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data zip for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The general process followed for execution of this assignment was step by step execution of each course identified item as listed in the assignment instructions. I will identify the specific steps I went through for each of these items.

A decision was made to combine everything in a single script file (run_analysis.R) this script does intermediary creation of other datasets (see CodeBook) before creating the final Tidy data set and writing it to a file.
1.Merging training and testing data sets: The dataset.zip file was downloaded from the supplied link and unzipped in a common coursera folder used for this project. The directory for work was set to just above the unpacked "UCI HAR Dataset" The code is noted and executed from this folder location For this item, it was determined that the read.table method was the best suited for reading the .txt files. The test and train data files were read in separately creating 6 separate data objects: xtest - the X_test.txt file xtrain - the X_train.txt file ytestsub - the subject_test.txt file ytest - the y_test.txt ytrainsub the subject_train.txt file ytrain the y_train.txt file

Before the actual merging of the data objects I renamed the "Subject" and "Activity" lable names to make them easier to deal with The next step was to create two data.frame objects (testDF and trainDF) and using cbind add the other data objects listed above Finally rbind was used to combine these two data frames into one combined data frame object.

The next thing to do was to replace the meaningliess (V1,V2...) column names with the more meaningful feature names. To do this the list of features was taken from the "features.txt and read into a data object, the significant name values were extracted from column 2 and a vector of names was created. These feature names were added to a vector containing all the column names and this was applied to the combined data frame (combinedDT)

As a last step, the numeric values for Activity were replaced with the string values, for sake of time, and because this list is small (6 items) these values were hard coded. - The result is that "combinedDF" now contains all the data, with appropriate names for columns and rows values.

1.Extract only mean and std deviation values: For this I created two new data frames extracting out mean() and std() to the appropriate named data frame. This was done by creating a regular expression and using grep to look for "mean()-" and "std()-". The decision was made to use the axial variables (x,y,z) and their mean and std as the values to extract to the tidy data set. This seemed to provide the best granularity with which further analysis could be done.


2.Use descriptive activity names to describe activities in the data set: Already accomplished in step 1


4 Appropriately label the data set with descriptive variable names: Also done as part of step 1, and during step 5
1.from the data (mean and std deviation data frames) in step 4 create a tidy dataset with the averages of each variable To do this I first subset the data by Subject (1-30 numeric values), and then using that subset data with a split function I proceeded to break up the data into buckets by Activity type. I created a 1 row data.frame for holding the tidy data and labeled it as follows: sensor mean_std subject_id activity x_measure y_measure z_measure

I built the data.frame one row at a time calculate the average for the collected buckets (Subject,Activity,Measure). The sensor value was the variable name with the x,y,z and mean() stripped off. I used unique() on the resultant list to get a singluar value for each sensor. The Activity and Subject were extracted from the dataset, and the mean was calculated on each bucket with the results being placed int he appropriate column (x,y or z)

I did this process for both mean() and std() values then combined them in to one final tall tidy data set.

More detail on the specifics of the variables chosen and their meaning can be found in the CodeBook.md also in this location.
