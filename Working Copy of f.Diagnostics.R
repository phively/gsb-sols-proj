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
ConfusionMatrix <- function(models, modnames=NULL, threshold=.5, counts=T, digits=2, debug=F) {
# models = a list of one or more model objects
# modnames = vector of the names to apply to the model objects
# threshold = classification threshold for confusion matrix; usually would want to set to .5
# counts = if set to F, returns proportion table
# digits = significant digits (NOT rounded) to include
  out <- list()
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
      m <- models[[i]]
    } else {
      m <- models
    }
    tabl <- table(truth=model.frame(m)[,1], pred=fitted(m)>=threshold)
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

#### Make a random forest error plot with tidyr and ggplot2
PlotForest <- function(forest, title=NULL) {
  # Reformat err.rate to include the count of number of trees, and gather from columns into rows
  err <- data.frame(trees=(1:forest$ntree), forest$err.rate)
  err <- gather(err, "Label", "Error", 2:dim(err)[2])
  # Pretty plot
  ggplot(err, aes(x=trees, y=Error, color=Label)) + geom_line() + labs(title=title) + scale_y_continuous(limits=c(0,max(err$Error)))
}