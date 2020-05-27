
#Download and Read Data, checking if archieve and folder exists.
#Descargar y leer datos, chequea si el archivo y carpeta existe.

filename<-"GCD_Final.zip"

library(dplyr)

if (!file.exists(filename)){
    fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, mode="wb")
}  

if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}


#Assigning all data frames
#Asignación de todos los marcos de datos

features<-read.table("UCI HAR Dataset/features.txt", col.names= c("n","functions"))
activities<-read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test<-read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test<-read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train<-read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#1. Merges the training and the test sets to create one data set.
#1. Fusiona los conjuntos de entrenamiento y prueba para crear un conjunto de datos.

X<-rbind(x_train, x_test)
Y<-rbind(y_train, y_test)
subject<-rbind(subject_train, subject_test)
merged_data<-cbind(subject, Y, X)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#2. Extrae solo las mediciones de la media y la desviación estándar para cada medición.

tidyData<-merged_data %>% select(subject, code, contains("mean"), contains("std"))


#3. Uses descriptive activity names to name the activities in the data set.
#3. Utiliza nombres de actividades descriptivos para nombrar las actividades en el conjunto de datos.

tidyData$code<-activities[tidyData$code, 2]
names(tidyData)

#4. Appropriately labels the data set with descriptive variable names.
#4. Etiqueta adecuadamente el conjunto de datos con nombres de variables descriptivas.

names(tidyData)[2] = "activityName"
names(tidyData)<-gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData)<-gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData)<-gsub("BodyBody", "Body", names(tidyData))
names(tidyData)<-gsub("Mag", "Magnitude", names(tidyData))
names(tidyData)<-gsub("^t", "Time", names(tidyData))
names(tidyData)<-gsub("^f", "Frequency", names(tidyData))
names(tidyData)<-gsub("tBody", "TimeBody", names(tidyData))
names(tidyData)<-gsub("-mean()", "Mean", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-std()", "STD", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-freq()", "Frequency", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("angle", "Angle", names(tidyData))
names(tidyData)<-gsub("gravity", "Gravity", names(tidyData))
names(tidyData)     #verifica el cambio de etiquetas, check the label change


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject.
#5. A partir del conjunto de datos en el paso 4, crea un segundo conjunto de datos ordenado independiente con 
#el promedio de cada variable para cada actividad y cada sujeto.

FinalTidyData<-tidyData %>%
    group_by(subject, activityName) %>%
    summarise_all(funs(mean))
write.table(FinalTidyData, "TidyData.txt", row.name=FALSE)


#FINAL  CHECK
#VERIFICACIÓN FINAL

str(FinalTidyData)
FinalTidyData