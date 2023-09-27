#hwzip <- "homework4_data.zip"
#download.file("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip", destfile = hwzip)
#unzip(hwzip)

# Exploratory code
# hospitaldf <- read.csv(file = "hospital-data.csv", header = T, stringsAsFactors = F)
# outcomedf <- read.csv(file = "outcome-of-care-measures.csv", header = T, stringsAsFactors = F)
# outcomedf[, 11] <- as.numeric(outcomedf[, 11])
# hist(outcomedf[, 11])

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  # Check the validity of outcome argument.
  if (length(outcome) != 1) {
    stop ("invalid outcome")
  }
  if ("heart attack" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  } else if ("heart failure" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  } else if ("pneumonia" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  } else {
    stop ("invalid outcome")
  }

  # Load data file
  outcomedf <- read.csv(file = "outcome-of-care-measures.csv", header = T, stringsAsFactors = F)
  
  # Check the validity of state argument.
  ValidStates <- unique(outcomedf[["State"]])
  if (! (state %in% ValidStates)) { stop ("invalid state") }

  # Filter data set.
  outcomedf <- outcomedf[outcomedf[["State"]] == state, ]
  outcomedf[[colname]] <- as.numeric(outcomedf[[colname]])
  outcomedf <- outcomedf[outcomedf[[colname]] == min(outcomedf[[colname]], na.rm = T), ]
  
  # Sort data set.
  outcomedf <- outcomedf[order(outcomedf[["Hospital.Name"]]), ]
  
  # Return first hospital name.
  return(outcomedf[["Hospital.Name"]][1])
  
}

# Recommended tests:
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"

# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"

# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"

# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state

# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  # Preliminary check of num argument.
  if (length(num) != 1) {
    stop ("invalid num")
  }

  # Check the validity of outcome argument.
  if (length(outcome) != 1) {
    stop ("invalid outcome")
  }
  if ("heart attack" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  } else if ("heart failure" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  } else if ("pneumonia" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  } else {
    stop ("invalid outcome")
  }
  
  # Load data file
  outcomedf <- read.csv(file = "outcome-of-care-measures.csv", header = T, stringsAsFactors = F)
  
  # Check the validity of state argument.
  ValidStates <- unique(outcomedf[["State"]])
  if (! (state %in% ValidStates)) { stop ("invalid state") }

  # Filter data set.
  outcomedf <- outcomedf[outcomedf[["State"]] == state, ]
  outcomedf[[colname]] <- as.numeric(outcomedf[[colname]])
  outcomedf <- outcomedf[!(is.na(outcomedf[[colname]])), ]
  
  # Sort data set, using alphabetical order of names as a tiebreaker (second sort criterion).
  outcomedf <- outcomedf[order(outcomedf[[colname]], outcomedf[["Hospital.Name"]]), ] 

  # Prepare for index checks.
  maxindex <- nrow(outcomedf)
  
  if ("best" == num) {
    return(outcomedf[["Hospital.Name"]][1])
  } else if ("worst" == num) {
    return(outcomedf[["Hospital.Name"]][maxindex])
  } else if (!(is.numeric(num))) {
    stop("invalid num")
  } else if ((num >= 1) && (num <= maxindex)) {
    return(outcomedf[["Hospital.Name"]][num])
  } else {
    return(NA)
  }
}

# Recommended tests
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  # Check the validity of the num argument.
  if (length(num) != 1) {
    stop ("invalid num")
  }
  if (is.character(num)) {
    if (("best" != num) && ("worst" != num)) {
      stop ("invalid num")
    }
  } else if ( abs(round(num) - num) > 1e-6 || (num < 1) ) {
    stop ("invalid num")
  }

  # Check the validity of outcome argument.
  if (length(outcome) != 1) {
    stop ("invalid outcome")
  }
  if ("heart attack" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  } else if ("heart failure" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  } else if ("pneumonia" == outcome) {
    colname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  } else {
    stop ("invalid outcome")
  }

  # Load data file.
  outcomedf <- read.csv(file = "outcome-of-care-measures.csv", header = T, stringsAsFactors = F)

  # Check the validity of state argument.
  states <- sort(unique(outcomedf[["State"]]))

  # Expunge incomplete records.
  outcomedf[[colname]] <- as.numeric(outcomedf[[colname]])
  outcomedf <- outcomedf[!(is.na(outcomedf[[colname]])), ]

  # Initiate the returned structure.
  retval <- data.frame(hospital = c(), state = c())

  for (state in states) {
    # Filter by state.
    subsetdf <- outcomedf[outcomedf[["State"]] == state, ]

    # Sort.
    subsetdf <- subsetdf[order(subsetdf[[colname]], subsetdf[["Hospital.Name"]]), ]

    # Extract
    if ("best" == num) {
      hospitalname <- subsetdf[["Hospital.Name"]][[1]]
    } else if ("worst" == num) {
      hospitalname <- subsetdf[["Hospital.Name"]][[nrow(subsetdf)]]
    } else if (num > nrow(subsetdf)) {
      hospitalname <- NA
    } else {
      hospitalname <- subsetdf[["Hospital.Name"]][[num]]
    }

    retval <- rbind(retval, data.frame(hospital = hospitalname, state = state))
  }

  row.names(retval) <- retval[["state"]]
  return(retval)
}

# Recommended tests
# >  head(rankall("heart attack", 20), 10)
#                               hospital state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

# > tail(rankall("pneumonia", "worst"), 3)
#                                     hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
# WV                     PLATEAU MEDICAL CENTER    WV
# WY           NORTH BIG HORN HOSPITAL DISTRICT    WY

# > tail(rankall("heart failure"), 10)
#                                                             hospital state
# TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# TX                                        FORT DUNCAN MEDICAL CENTER    TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# VA                                          SENTARA POTOMAC HOSPITAL    VA
# VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# VT                                              SPRINGFIELD HOSPITAL    VT
# WA                                         HARBORVIEW MEDICAL CENTER    WA
# WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# WV                                         FAIRMONT GENERAL HOSPITAL    WV
# WY                                        CHEYENNE VA MEDICAL CENTER    WY

# Quiz Qs:
# > best("SC", "heart attack")
# [1] "MUSC MEDICAL CENTER"

# > best("NY", "pneumonia")
# [1] "MAIMONIDES MEDICAL CENTER"

# > best("AK", "pneumonia")
# [1] "YUKON KUSKOKWIM DELTA REG HOSPITAL"

# > rankhospital("NC", "heart attack", "worst")
# [1] "WAYNE MEMORIAL HOSPITAL"

# > rankhospital("WA", "heart attack", 7)
# [1] "YAKIMA VALLEY MEMORIAL HOSPITAL"

# > rankhospital("TX", "pneumonia", 10)
# [1] "SETON SMITHVILLE REGIONAL HOSPITAL"

# > rankhospital("NY", "heart attack", 7)
# [1] "BELLEVUE HOSPITAL CENTER"

# > r <- rankall("heart attack", 4) ; as.character(subset(r, state == "HI")$hospital)
# [1] "CASTLE MEDICAL CENTER"

# r <- rankall("pneumonia", "worst") ; as.character(subset(r, state == "NJ")$hospital)
# [1] "BERGEN REGIONAL MEDICAL CENTER"

# r <- rankall("heart failure", 10) ; as.character(subset(r, state == "NV")$hospital)
# [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"
