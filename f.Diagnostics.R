#### Examine the deviances to assess goodness of fit ----
dev.comp <- function(models, modnames, debug=F){
  # Loop through the models passed to the function
  out <- NULL
  # Check if not actually passed a list of models
  is.list <- class(models)[1]=="list"
    if(debug){print(is.list)}
  if(is.list) {n=length(models)} else {n=1}
    if(debug){print(paste("n =",n))}
  # Loop through models
  for(i in 1:n){
    # Extract the current model
    if(is.list) {m <- summary(models[[i]])} else {m <- summary(models)}
    m$difference = m$null.deviance - m$deviance
    m$rel.difference = m$difference / m$null.deviance
    m$df.p = m$df.null
    m$df.q = m$df.residual
    m$p = with(m, pchisq(difference, df=df.p-df.q, lower.tail=F))
    out <- rbind(out, data.frame(null.dev=m$null.deviance, dev=m$deviance, diff=m$difference, rel.diff=m$rel.difference, df.p=m$df.p, df.q=m$df.q, p=m$p))
    if(debug){print(out)}
  }
  rownames(out) <- modnames
  return(out)
}

#### Generate confusion matrix from a list of models ----
confusion <- function(models, threshold, modnames, counts=T, digits=2, debug=F){
  out <- list()
  # Check if not actually passed a list of models
  is.list <- class(models)[1]=="list"
    if(debug){print(is.list)}
  if(is.list) {n=length(models)} else {n=1}
    if(debug){print(paste("n =",n))}
  # Loop through models
  for(i in 1:n){
    # Extract the current model
    if(is.list) {m <- models[[i]]} else {m <- models}
    tabl <- table(truth=model.frame(m)[,1], pred=fitted(m)>=threshold)
    if(counts) {out[[i]] <- addmargins(tabl)}
    if(!counts){out[[i]] <- signif(addmargins(prop.table(tabl)), digits)}
  }
  names(out) <- modnames
  return(out)
}

#### Weighted 2-class confusion matrix, from probability and truth vectors ----
confusion.wtd <- function(preds, truth, dimnames=NULL, digits=2){
  q <- 1-preds
  # Construct the data
  out <- round(matrix(c(sum(q[truth==F]), sum(q[truth==T]), sum(preds[truth==F]), sum(preds[truth==T])),
                nrow=2, dimnames=list(c("FALSE","TRUE"), c("FALSE","TRUE"))), digits)
  # Coerce to table
  out <- as.table(out)
  names(attributes(out)$dimnames) <- dimnames
  return(out)
}

#### Calculate Lp norm of a vector; defaults to L2 norm ----
lpnorm <- function(data, p=2){
  if(p==0){return(NA)}
  else return(sum(abs(data^p))^(1/p))
}