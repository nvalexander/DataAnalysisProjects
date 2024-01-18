rm(list = ls())

hwzip <- "courseradataspecialization-02-programming-homework1/quiz1_data.zip"
if(!file.exists(hwzip)){
  download.file(
    "https://d396qusza40orc.cloudfront.net/rprog/data/quiz1_data.zip",
    destfile = hwzip)
}
hwunzipped <- as.character(unzip(hwzip, list = T)[1])
con <- unz(hwzip, hwunzipped)
df <- read.csv(con, header = T)

# In the dataset provided for this Quiz,
# what are the column names of the dataset?
print(summary(df))

# Extract the first 2 rows of the data frame and print them to
# the console. What does the output look like?
print(df[1:2,])

# How many observations (i.e. rows) are in this data frame?
print(dim(df))

# Extract the last 2 rows of the data frame and print them to the
# console. What does the output look like?
print(df[dim(df)[1] + ((-2+1):0),])

# What is the value of Ozone in the 47th row?
print(df$Ozone[47])

# How many missing values are in the Ozone column of this data frame?
print(sum(is.na(df$Ozone)))

# What is the mean of the Ozone column in this dataset?
# Exclude missing values (coded as NA) from this calculation.
print(mean(df$Ozone, na.rm = T))

# Extract the subset of rows of the data frame where Ozone values are
# above 31 and Temp values are above 90. What is the mean of Solar.R
# in this subset?
print(mean(df[df$Ozone > 31 & df$Temp > 90, ][["Solar.R"]], na.rm = T))

# What is the mean of "Temp" when "Month" is equal to 6?
print(mean(df$Temp[df$Month == 6], na.rm = T))

# What was the maximum ozone value in the month of May
# (i.e. Month is equal to 5)
print(max(df$Ozone[df$Month == 5], na.rm = T))

close(con)
# file.remove(hwzip)
