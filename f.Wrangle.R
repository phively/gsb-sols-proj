#### Replace a set of data frame fields with new names ----
renamer <- function(data, old, new){ #args(dataset, old column name(s), new column name)
  for (i in 1:max(length(old),1)){
    try(colnames(data)[which(colnames(data)==old[i])] <- new, silent=T)
  }
  return(data)
}

#### Convert m-d-y formatted string to date ----
toDate <- function(data, fields){
  for (i in 1:length(fields)){
    try({tmp <- c(unlist(data[fields[i]])); data[fields[i]] <- mdy(tmp)}, silent=T)
  }
  return(data)
}

#### Convert the specified currency fields from string to numeric ----
toNumeric <- function(data, fields){
  for (i in 1:length(fields)){
    try({
      tmp <- gsub("(\\$|,)", "", c(unlist(data[fields[i]]))) #get rid of dollar signs and whatnot;
      data[fields[i]] <- as.numeric(tmp)
    }, silent=T)
  }
  return(data)
}

#### Convert calendar to fiscal year ----
YtoFY <- function(x){return(as.numeric(ifelse(month(x)>=7,1,0) + year(x)))}

#### Return end of the fiscal year ----
fye <- function(date){ymd(as.character(paste(round(month(date)/13)+year(date),"06","30", sep="-")))}