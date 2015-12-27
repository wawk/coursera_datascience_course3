CodeBook for Assignment: getdata-015 - Coursera Data Scientist Specialization - course 3 "Getting and Cleaning Data" Course Project Date 06/21/2015

The following details the variables assigned and used with each data frame or data object used in the execution of this project


Tidy Data Set: File Name: tidy_human_activity_recognition.txt Code Name: finalDF Column Variables sensor - this value is the sensor base for each (x,y,z) measurement For example a variable of tBodyAcc-mean()-x would be stripped down to tBodyAcc, this represents the base of the sensor which then have their measurements reflected in the x_measure y_measure, and z_measure columns. It was decided to use this as a simple way to identify multiple measurements across rows instead of columns, thus making the data frame taller not wider.

mean_std - Since both the mean() and std() values are included in the same data frame I needed a way to identify the measuremnet (all other values are the same for both). passing in the character vector of "mean" or "std" this column is populated. The value here represents that this row contains either mean data or std deviation data.

subject_id - the subject_id is a numeric value between 1-30 and are represented in the data txt files as subject. There was no correlation (names) given for the subjects so I simply included them as they were listed, note that each column has 6 values of each of the 1- 30 for mean and then again for std. This allows for an easy identification at a glance the grouping of subject to activity.

activity - the activies are either "WALKING","SITTING","STANDING","WAKING_DOWNSTAIRS", or "WALKING_UPSTAITS". These values are the string representation of the numeric values found in the raw txt files, the activies txt file was used to make this replacement. A choice to keep the strings as they were (uppercase) rather then massaging to a more standard lowercase representation. Again these values help to easily identify the groupings of the data in the tidy data set.

x_measure - the variable names for each sensor had a axial component with (x,y,z) being listed individually for each. I made this a column and the actual sensor value a value in the sensor column. That is for each row in the tidy data set, a sensor will be associated with the x,y,z columns for that sense in the main data set. The average was take for each of these values broken down by subject and activity, and this is the value in this row for each measurement. y_measure - The same that is listed for the x_measure can be applied to this variable, only for the y component of the sensor z_measure - The same that is listed for the x_measure can be applied to this variable, only for the z component of the sensor


meanDT stdDT Data Sets

First the only difference in these data sets is that one is for the mean and the other for the std deviation. They were devided up based on the names of the variables:

meanDT These are the values listed in the features txt file, they represent the appropriate sensor readings (on a xyz axis) for each sensor these measurements represent the mean values for these sensors "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z"
 "tGravityAcc-mean()-X" "tGravityAcc-mean()-Y" "tGravityAcc-mean()-Z"
 "tBodyAccJerk-mean()-X" "tBodyAccJerk-mean()-Y" "tBodyAccJerk-mean()-Z"
 "tBodyGyro-mean()-X" "tBodyGyro-mean()-Y" "tBodyGyro-mean()-Z"
 "tBodyGyroJerk-mean()-X" "tBodyGyroJerk-mean()-Y" "tBodyGyroJerk-mean()-Z" "fBodyAcc-mean()-X" "fBodyAcc-mean()-Y" "fBodyAcc-mean()-Z"
 "fBodyAccJerk-mean()-X" "fBodyAccJerk-mean()-Y" "fBodyAccJerk-mean()-Z"
 "fBodyGyro-mean()-X" "fBodyGyro-mean()-Y" "fBodyGyro-mean()-Z"
 "Subject" - This contains a numeric value from 1-30 representing each participant in the experiment
 "Activity - This contains a character string value representing the type of activity being done when the measurement was taken

stdDT These are the std deviation measurement taken and separated out to my stdDT dataset. The represent the std deviation calculations on the measurements taken across (x,y,z) axis "tBodyAcc-std()-X" "tBodyAcc-std()-Y" "tBodyAcc-std()-Z"
 "tGravityAcc-std()-X" "tGravityAcc-std()-Y" "tGravityAcc-std()-Z"
 "tBodyAccJerk-std()-X" "tBodyAccJerk-std()-Y" "tBodyAccJerk-std()-Z"
 "tBodyGyro-std()-X" "tBodyGyro-std()-Y" "tBodyGyro-std()-Z"
 "tBodyGyroJerk-std()-X" "tBodyGyroJerk-std()-Y" "tBodyGyroJerk-std()-Z" "fBodyAcc-std()-X" "fBodyAcc-std()-Y" "fBodyAcc-std()-Z"
 "fBodyAccJerk-std()-X" "fBodyAccJerk-std()-Y" "fBodyAccJerk-std()-Z"
 "fBodyGyro-std()-X" "fBodyGyro-std()-Y" "fBodyGyro-std()-Z"
 "Subject" - This contains a numeric value from 1-30 representing each participant in the experiment
 "Activity" - This contains a character string value representing the type of activity being done when the measurement was taken
