#### JMod takes current solicitation information and outputs probabilities, expected amounts, and errors ----
JMod <- function(curr.stage, curr.dt, expected.dt, expect.amt=0, act.amt=0, closed.in.fy=F){
  # If expected to close after EOFY, discount to 0
  prob <- ifelse(fye(curr.dt)!=fye(expected.dt), 0,
          # If expected to close July through Feb
          ifelse(month(curr.dt) %in% c(7,8,9,10,11,12,1,2),
                 ifelse(as.numeric(curr.stage)==1, 1/6, #planned
                 ifelse(as.numeric(curr.stage)==2, 1/3, #cleared
                 ifelse(as.numeric(curr.stage)==3, 2/3, 1))), #asked/oral, 1 = paperwork in house = fallback
          # If expected to close Mar or Apr
          ifelse(month(curr.dt) %in% c(3,4),
                 ifelse(as.numeric(curr.stage)==1, 1/8,
                 ifelse(as.numeric(curr.stage)==2, 1/6,
                 ifelse(as.numeric(curr.stage)==3, 1/3, 1))),
          ifelse(month(curr.dt)==5, #if expected to close May
                 ifelse(as.numeric(curr.stage)==1, 0,
                 ifelse(as.numeric(curr.stage)==2, 1/8,
                 ifelse(as.numeric(curr.stage)==3, 1/6, 1))),
          ifelse(month(curr.dt)==6, #if expected to close June
                 ifelse(as.numeric(curr.stage)==1, 0,
                 ifelse(as.numeric(curr.stage)==2, 0,
                 ifelse(as.numeric(curr.stage)==3, 1/8, 1))), 0))))) #final fallback of 0 (e.g. no stage)
  pred <- ifelse(is.na(expect.amt), 0, expect.amt) * prob
  err <- pred - ifelse(is.na(act.amt), 0, act.amt)*closed.in.fy
  out = list(probability=prob, prediction=pred, error=err)
  return(out)
}

#### Calculates the error for JMod given the probability vectors and actual outcome ----
JMod.err <- function(probs, truth){
  return(probs-truth)
}