#### Examine the deviances to assess goodness of fit ----
CompareDeviances <- function(models, modnames, debug=F) {
# models = a list of one or more model objects
# modnames = vector of the names to apply to the model objects
  # Loop through the models passed to the function
  out <- NULL
  # Check if not actually passed a list of models
  is.list <- class(models)[1] == "list"
  if (debug) {print(is.list)}
  if (is.list) {
    n <- length(models)
  } else {
    n <- 1
  }
  if (debug) {print(paste("n =", n))}
  # Loop through models
  for (i in 1:n) {
    # Extract the current model
    if (is.list) {
      m <- summary(models[[i]])
    } else {
      m <- summary(models)
    }
    m$difference <- m$null.deviance - m$deviance
    m$rel.difference <- m$difference / m$null.deviance
    m$df.p <- m$df.null
    m$df.q <- m$df.residual
    m$p <- with(m, pchisq(difference, df=df.p-df.q, lower.tail=F))
    out <- rbind(out,
                 data.frame(null.dev = m$null.deviance,
                            dev = m$deviance,
                            diff = m$difference,
                            rel.diff = m$rel.difference,
                            df.p = m$df.p,
                            df.q = m$df.q,
                            p = m$p))
    if (debug) {print(out)}
  }
  rownames(out) <- modnames
  return(out)
}

#### Generate confusion matrix from a list of models ----
ConfusionMatrix <- function(models, testdata=NULL, modnames=NULL, threshold=.5, counts=T, digits=2, debug=F) {
# models = a list of one or more model objects
# modnames = vector of the names to apply to the model objects
# threshold = classification threshold for confusion matrix; usually would want to set to .5
# counts = if set to F, returns proportion table
# digits = significant digits (NOT rounded) to include
  out <- list()
  # Check if not actually passed a list of models
  if (class(models)[[1]] != "list") {models <- list(models)}
  n <- length(models)
  if (debug) {print(paste("n =", n))}
  # If not given test data, use the model data frames themselves
  if (is.null(testdata)) {
    for (i in 1:n) {testdata[[i]] <- model.frame(models[[i]])}
  }
  # Reshape test data
  if (class(testdata) != "list") {testdata <- list(testdata)}
  # Loop through models
  for (i in 1:n) {
    # Extract the current model
    m <- models[[i]]
    # Extract the current test data
    if (debug) {print(paste("length test data", length(testdata), "i", i))}
    if (length(testdata) >= i){
      t <- testdata[[i]]
    } else {
      # If there is no more test data, keep using the last element of the testdata list
      t <- testdata[[length(testdata)]]
    }
    if (debug) {
      print(paste("Test data dimensions", nrow(t), "*", ncol(t), sep=" "))
      print(paste("Test data type", class(t)))
    }
    tabl <- table(truth=t[, as.character(m$formula)[2]], pred=predict(m, newdata=t, type="response") >= threshold)
    if (counts) {out[[i]] <- addmargins(tabl)}
    if (!counts) {out[[i]] <- signif(addmargins(prop.table(tabl)), digits)}
  }
  names(out) <- modnames
  return(out)
}

#### Weighted 2-class confusion matrix, from probability and truth vectors ----
ConfusionMatrixWeighted <- function(preds, truth, dimnames=NULL, digits=2) {
# preds = predicted probabilities
# truth = true class membership
# dimnames = source for the class labels
  q <- 1 - preds
  # Construct the data
  out <- round(matrix(c(sum(q[truth == F]),
                        sum(q[truth == T]),
                        sum(preds[truth == F]),
                        sum(preds[truth == T])),
                      nrow=2, dimnames=list(c("FALSE","TRUE"), c("FALSE","TRUE"))), digits)
  # Coerce to table
  out <- as.table(out)
  names(attributes(out)$dimnames) <- dimnames
  return(out)
}

#### Calculate Lp norm of a vector; defaults to L2 norm ----
LpNorm <- function(data, p=2) {
# data = numeric vector
# p = norm to calculate
  if (p==0) {
    return(NA)
  } else {
    return(sum(abs(data^p))^(1/p))
  }
}

#### Make a random forest error plot with tidyr and ggplot2 ----
PlotForest <- function(forest, title=NULL) {
  # Reformat err.rate to include the count of number of trees, and gather from columns into rows
  err <- data.frame(trees=(1:forest$ntree), forest$err.rate)
  err <- gather(err, "Label", "Error", 2:dim(err)[2])
  # Pretty plot
  ggplot(err, aes(x=trees, y=Error, color=Label)) + geom_line() + labs(title=title) + scale_y_continuous(limits=c(0,max(err$Error)))
}

#### Function to calculate error rates per stage ----
CalcErrorRates <- function(confusion.matrices, model.name, modnames=c("Plan", "Clear", "Ask", "Oral")) {
# confusion.matrices = list of objects; output from ConfusionMatrix
# model.name = string; row name for current model
  output <- data.frame(
    Model = model.name,
    Plan = confusion.matrices[[modnames[1]]]["FALSE","TRUE"] + confusion.matrices[[modnames[1]]]["TRUE","FALSE"],
    Clear = confusion.matrices[[modnames[2]]]["FALSE","TRUE"] + confusion.matrices[[modnames[2]]]["TRUE","FALSE"],
    Ask = confusion.matrices[[modnames[3]]]["FALSE","TRUE"] + confusion.matrices[[modnames[3]]]["TRUE","FALSE"],
    # Oral includes a fallback condition for when there are no True-False predictions
    Oral = tryCatch(confuse[[modnames[4]]]["FALSE","TRUE"] + confusion.matrices[[modnames[4]]]["TRUE","FALSE"], error=function(.) sum(confusion.matrices[[modnames[4]]]["FALSE","TRUE"]))
  )
  return(output)
}