directory = "/Users/jc/Downloads/Data Analysis Exercise/"
setwd(directory)
FileList <- list.files(path = ".", pattern = "*.csv")
MyData <- read.csv(FileList, header = TRUE)