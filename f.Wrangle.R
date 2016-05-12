#### Replace a set of data frame fields with new names ----
Renamer <- function(data, old, new) { #args(dataset, old column name(s), new column name)
  for (i in 1:max(length(old),1)) {
    try(
        colnames(data)[which(colnames(data)==old[i])] <- new,
        silent=T
    )
  }
  return(data)
}

#### Convert m-d-y formatted string to date ----
ToDate <- function(data, fields) {
  for (i in 1:length(fields)) {
    try({
         tmp <- c(unlist(data[fields[i]]))
         data[fields[i]] <- mdy(tmp)
        },
        silent=T
    )
  }
  return(data)
}

#### Convert the specified currency fields from string to numeric ----
ToNumeric <- function(data, fields) {
  for (i in 1:length(fields)) {
    try({
         tmp <- gsub("(\\$|,)", "", c(unlist(data[fields[i]]))) #get rid of dollar signs and whatnot
         data[fields[i]] <- as.numeric(tmp)
        },
        silent=T
    )
  }
  return(data)
}

#### Convert calendar to fiscal year ----
YToFY <- function(x) {
# Takes a date; if month is July through December add 1 to the year
  return(
    as.numeric(
      year(x) + ifelse(month(x)>=7, 1, 0)
    )
  )
}

#### Return end of the fiscal year ----
FYE <- function(date) {
  ymd(
    as.character(
      paste(round(month(date) / 13) + year(date), "06", "30", sep="-")
    )
  )
}

#### Converts month number to fiscal month number ----
MoToFiscalMo <- function(month.number, start.month=7) {
# Default fiscal year start is July (7)
  return((month.number - start.month) %% 12 + 1)
  # %% is the modulo operator in R
}