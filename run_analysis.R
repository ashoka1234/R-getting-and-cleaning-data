# I need to iteratively read from files since 
# full file DO NOT fit in memory in my computer
numrows <- 200

# specify column widths
w <- rep(16,561)

# specify columns with mean and std of features
col<-c(1:6,41:46,81:86,121:126,161:166,201:206,
       214:215,227:228,240:241,253:254,266:271,
       345:350,424:429,503:504,516:517,529:530,542:543)

# file names to be used
xtestfile <- "UCI HAR Dataset\\test\\X_test.txt"
xtrainfile <- "UCI HAR Dataset\\train\\X_train.txt"
ytestfile <- "UCI HAR Dataset\\test\\y_test.txt"
ytrainfile <- "UCI HAR Dataset\\train\\y_train.txt"
stestfile <- "UCI HAR Dataset\\test\\subject_test.txt"
strainfile <- "UCI HAR Dataset\\train\\subject_train.txt"

# first work on traing data set
xfile <- xtrainfile
yfile <- ytrainfile
sfile <- strainfile

for (i in 1:2) {
    rawstoskip <- 0

    # read part of feature dataset
    x <- read.fwf(xfile, widths = w, n = numrows, skip = rawstoskip)
    while (length(x[,1]) > 0) { 
        # read part of activity label data
        y <- read.table(yfile, nrows = numrows, skip = rawstoskip)

        # 2. read means and stds 
        xmeanstd <- x[,col]

        # 3. use descriptive activity names
        y[y==1] <- "WALKING"
        y[y==2] <- "WALKING_UPSTAIRS"
        y[y==3] <- "WALKING_DOWNSTAIRS"
        y[y==4] <- "SITTING"
        y[y==5] <- "STANDING"
        y[y==6] <- "LAYING"

        # 4. Appropriately labels the data set with descriptive variable names.
        fnames <- read.table("UCI HAR Dataset\\features.txt")
        fnames <- as.vector(fnames[,2])
        names(x) <- fnames
        names(xmeanstd) <- fnames[col]

        # 5. Creates a second, independent tidy data set with the average of 
        #    each variable for each activity and each subject.
        subjects <- read.table(sfile,nrows = numrows, skip = rawstoskip)
        xSubjectsActivities <- cbind(subjects,y,x)

        # need to maintain sum and size (length) of data to calculate mean
        sumdata <- aggregate(xSubjectsActivities[,c(-1,-2)],
                     by=as.list(xSubjectsActivities[,1:2]),
                     FUN=sum)
        lengthdata <- aggregate(xSubjectsActivities[,c(-1,-2)],
                        by=as.list(xSubjectsActivities[,1:2]),
                        FUN=length)

        names(sumdata)[1:2] <- c("subject","activity")
        names(lengthdata)[1:2] <- c("subject","activity")

        if (exists("tsumdata")) {
            tsumdata <- rbind(tsumdata,sumdata)
            tsumdata <- aggregate(tsumdata[,c(-1,-2)],
                                  by=as.list(tsumdata[,1:2]),
                                  FUN=sum)                 
        } else {
            tsumdata <- sumdata
        }
        if (exists("tlengthdata")) {
            tlengthdata <- rbind(tlengthdata,lengthdata)
            tlengthdata <- aggregate(tlengthdata[,c(-1,-2)],
                                  by=as.list(tlengthdata[,1:2]),
                                  FUN=sum)   
        } else {
            tlengthdata <- lengthdata
        }

        # process next set of raws
        rawstoskip <- rawstoskip + numrows
        x <- read.fwf(xfile, widths = w, n = numrows, skip = rawstoskip)
    }

    # now work on the test dataset
    xfile <- xtestfile
    yfile <- ytestfile
    sfile <- stestfile
}
        
tsumdata <- tsumdata[order(tsumdata[,1]),]
tlengthdata <- tlengthdata[order(tlengthdata[,1]),]

# Calculate the averages
averagedata <- tsumdata[,c(-1,-2)]/tlengthdata[,c(-1,-2)]
averagedata <- cbind(tsumdata[,1:2],averagedata)

write.csv(tsumdata,file = "feature_sums.csv",row.names = FALSE)
write.csv(tlengthdata,file = "feature_lengths.csv",row.names = FALSE)
write.csv(averagedata,file = "feature_averages.csv",row.names = FALSE)