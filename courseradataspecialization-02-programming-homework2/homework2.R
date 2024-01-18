rm(list = ls())

hwzip <- "courseradataspecialization-02-programming-homework2/homework2_data.zip"
if(!file.exists(hwzip)){
  download.file(
    "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip", 
    destfile = hwzip)
}
unzip(hwzip)

readpollutantfiles <- function(directory){
  if (missing(directory) || !(dir.exists(directory))) {
    stop ("data dir source not found")
  }
  filelist <- dir(path = directory, pattern = "*csv")
  # Getting the structure from the first file speeds up the rest
  testdf <- read.csv(file.path(directory, filelist[1]), 
                     nrows = 100, 
                     stringsAsFactors = F)
  listofdfs <- lapply(filelist, 
                      function(myfile){
                        read.csv(file.path(directory, myfile), 
                                 col.names = colnames(testdf), 
                                 stringsAsFactors = F)
                      }
  )
  # Joins all data frames in one go 
  do.call(rbind, listofdfs)
}

# Write a function named 'pollutantmean' that calculates the mean of
# a pollutant (sulfate or nitrate) across a specified list of monitors.
pollutantmean <- function(directory, pollutant, id=1:332){
  if (missing(directory) || !(dir.exists(directory))) {
    stop ("data dir source not found")
  }
  if (missing(pollutant) || 
      ( (pollutant != "sulfate") &&
        (pollutant != "nitrate") )) {
    stop ("pollutant not found")
  }
  mydf <- readpollutantfiles(directory)
  mean(mydf[[pollutant]][mydf[["ID"]] %in% id], na.rm = T)
}

# Write a function that reads a directory full of files and reports the 
# number of completely observed cases in each data file. The function should 
# return a data frame where the first column is the name of the file and 
# the second column is the number of complete cases.
complete <- function(directory, id=1:332){
  if (missing(directory) || !(dir.exists(directory))) {
    stop ("data dir source not found")
  }
  mydf <- readpollutantfiles(directory)
  mydf <- mydf[complete.cases(mydf), ]
  returnlist <- lapply(as.list(id), function(someid){
    data.frame(
      id = someid,
      nobs = sum(mydf[["ID"]] == someid)
    )
  })
  do.call(rbind, returnlist)
}

# Write a function that takes a directory of data files and a threshold for complete 
# cases and calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the 
# threshold. The function should return a vector of correlations for the monitors that 
# meet the threshold requirement.
corr <- function(directory, threshold = 0){
  if (missing(directory) || !(dir.exists(directory))) {
    stop ("data dir source not found")
  }
  mydf <- readpollutantfiles(directory)
  mydf <- mydf[complete.cases(mydf), ] # purge incomplete cases
  uniqueids <- unique(mydf[["ID"]]) # collect ids that we have to loop through
  listofdfs <- sapply(
    as.list(uniqueids),
    function(someid){
      subdf <- mydf[(mydf[["ID"]] == someid), ]
      if (dim(subdf)[1] < threshold) {
        return(99)
      }
      cor(x = subdf[["nitrate"]], y = subdf[["sulfate"]])
    } 
  )
  return(listofdfs[listofdfs != 99])
}

# pollutantmean("specdata", "sulfate", 1:10)
# pollutantmean("specdata", "nitrate", 70:72)
# pollutantmean("specdata", "nitrate")
# cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310)); print(cc$nobs)
# cc <- complete("specdata", 54) ; print(cc$nobs)
# set.seed(42) ; cc <- complete("specdata", 332:1) ; use <- sample(332, 10) ; print(cc[use, "nobs"])
# cr <- corr("specdata") ; cr <- sort(cr) ; set.seed(868) ; out <- round(cr[sample(length(cr), 5)], 4) ; print(out)
# cr <- corr("specdata", 129) ; cr <- sort(cr) ; n <- length(cr) ; set.seed(197) ; out <- c(n, round(cr[sample(n, 5)], 4)) ; print(out)
# cr <- corr("specdata", 2000) ; n <- length(cr) ; cr <- corr("specdata", 1000) ; cr <- sort(cr) ; print(c(n, round(cr, 4)))
