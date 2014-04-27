
## main function. It handles other functions..
main <- function()
{

    rawFeatureData <- makeFeaturesData()
    activityLabel <- makeActivityLabelData()
    #print(activityLabel)
  
    # vector for measurements on the mean and standard deviation for each measurement
    extMeasuredColVector <- extractMeasureCols(rawFeatureData)
  
    trainDs <- loadingDataSet(dir="train",
                            colLabels=rawFeatureData,
                            extMeasuredColVector,
                            activityLabel=activityLabel,
                            nrows=-1)
  
    testDs <- loadingDataSet(dir="test",
                           colLabels=rawFeatureData,
                           extMeasuredColVector,
                           activityLabel=activityLabel,
                           nrows=-1)
  
    myResult <- rbind(trainDs,testDs)
  
    names(myResult) <- cleanHeader(myResult)
   
  
    writeResult(df=myResult,destFile="tidyDataSet.txt",dirName="result")

    #str(myResult)
  
    sumData <- summarizeData(myResult)
    writeResult(df=sumData,destFile="SummaryDataSet.txt",dirName="result")  
}

## loading feature.txt 
makeFeaturesData <- function(){
    message("[makeFeaturesData] loading features.txt....")
    df <- read.table(file="./data//UCI HAR Dataset/features.txt",sep=" ",
             col.names=c("colIdx","featureName"),
             stringsAsFactors=F)
    return(df)
}

# loading Activity labels file
makeActivityLabelData <- function(){
    message("[makeActivityLabelData] loading features.txt....")
    df <- read.table(file="./data//UCI HAR Dataset/activity_labels.txt",sep=" ",
                     col.names=c("activity_id","activity_name"),
                     stringsAsFactors=F)
    return(df)
}

##Extracts only the measurements on the mean and standard deviation for each measurement.
extractMeasureCols <- function(df = data.frame()){
    library(stringr)
    matchedCols <- str_detect(df$featureName,pattern="*-mean\\(\\)*|-std\\(\\)*")
    message("[extractMeasureCols] complete extract only measurement columns...")
    #print(df[matchedCols,])
    df[matchedCols,c("colIdx")]
}

## loading main data file.
## This function have argument named dir which is for determine weather train or test..
loadingDataSet <- function(dir="train",colLabels=data.frame(),measuredCols,nrows=-1,activityLabel=data.frame())
{
    mDS <- NULL
    
    popMainDataSet <- function() {
        message(sprintf("[loadingDataSet] start loading %s data set. It'll be take a few minutes....",dir))
        #     print(colLabels[,2])
        
        rawDataSets <- read.fwf(file=sprintf("./data/UCI HAR Dataset//%s/X_%s.txt",dir,dir),
                                header=F,
                                widths=rep(16,561),
                                col.names=colLabels[,2],
                                stringsAsFactors=F,
                                n=nrows)
        
        
        mDS <<- rawDataSets[,measuredCols]
    }
    popMainDataSet()

    activity <- read.table(file=sprintf("./data/UCI HAR Dataset/%s/y_%s.txt",dir,dir),
                            header=F,
                            col.names=c("activity_id"),
                            nrows=nrows)
    activity$activity_id <- as.factor(activity$activity_id)
    activityNames <- factor(activity$activity_id,labels=activityLabel$activity_name)
    activity$activity_name <- activityNames
    mDS <- cbind(activity,mDS)

    subjects <- read.table(file=sprintf("./data/UCI HAR Dataset/%s/subject_%s.txt",dir,dir),
                       header=F,
                       col.names=c("subject_id"),
                       nrows=nrows)
    mDS <- cbind(subjects,mDS)
    
    message(sprintf("finished loading %s data set.",dir))
    return(mDS)

    
}

writeResult <- function(df=data.frame(),destFile="myresult.txt",dirName="result")
{
    dir.create(file.path(".",dirName))
    write.csv(df,file=file.path(".",dirName,destFile),col.names=T,row.names=F)
    
}

## get average of each variable for each activity and each subject
summarizeData <- function(df=data.frame())
{
    library(plyr)
    
    message("[summarizeData] Summarizing Data...")
    sumData <- ddply(df,c("SUBJECT_ID", "ACTIVITY_NAME"),numcolwise(mean))
    colNames <- names(sumData)
    colNames <- gsub("^(TIME|FREQ)_","AGV_\\1_",colNames)
    #print(colNames)
    names(sumData) <- colNames
    return(sumData)
}

## cleaning tidy data set's column
cleanHeader <- function(df)
{
    names <- names(df)
    names <- gsub("\\.+","_",names)
    names <- gsub("^t","TIME_",names)
    names <- gsub("^f","FREQ_",names)
    names <- gsub("_$","",names)
    names <- toupper(names)
    #print(names)
    
    names
    
}
## excute this program!
main()
    
