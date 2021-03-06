#### JMod takes current solicitation information and outputs probabilities, expected amounts, and errors ----
JMod <- function(curr.stage, curr.dt, expected.dt, expect.amt=0, act.amt=0, closed.in.fy=F) {
# curr.stage = numeric, current solicitation stage
# curr.dt = date, date to evaluate for discounting
# expected.dt = date, date solicitation is expected to come in
# expect.amt = expected solicitation booked amount
# act.amt = actual solicitation booked amount
# closed.in.fy = whether or not the solicitation was booked in the curr.dt fiscal year
  # If expected to close after EOFY, discount to 0
  prob <- ifelse(FYE(curr.dt) < FYE(expected.dt), 0,
    # If expected to close July through Feb
    ifelse(month(curr.dt) %in% c(7,8,9,10,11,12,1,2),
             ifelse(as.numeric(curr.stage)==1, 1/6, #planned
             ifelse(as.numeric(curr.stage)==2, 1/3, #cleared
             ifelse(as.numeric(curr.stage) %in% c(3,4), 2/3, #asked/oral
             1))), #1 = paperwork in house = fallback
      # If expected to close Mar or Apr
      ifelse(month(curr.dt) %in% c(3,4),
               ifelse(as.numeric(curr.stage)==1, 1/8,
               ifelse(as.numeric(curr.stage)==2, 1/6,
               ifelse(as.numeric(curr.stage) %in% c(3,4), 1/3,
               1))),
        # If expected to close May
        ifelse(month(curr.dt)==5,
                 ifelse(as.numeric(curr.stage)==1, 0,
                 ifelse(as.numeric(curr.stage)==2, 1/8,
                 ifelse(as.numeric(curr.stage) %in% c(3,4), 1/6,
                 1))),
          # If expected to close June
          ifelse(month(curr.dt)==6,
                   ifelse(as.numeric(curr.stage)==1, 0,
                   ifelse(as.numeric(curr.stage)==2, 0,
                   ifelse(as.numeric(curr.stage) %in% c(3,4), 1/8,
                   1))),
                   0 #final fallback of 0 (e.g. no month)
          )
        )
      )
    )
  )
  pred <- ifelse(is.na(expect.amt), 0, expect.amt) * prob
  err <- pred - ifelse(is.na(act.amt), 0, act.amt) * closed.in.fy
  out <- list(probability=prob, prediction=pred, actual=act.amt, error=err)
  return(out)
}

#### Calculates the error for JMod given the probability vectors and actual outcome ----
JModError <- function(probs, truth) {
  return(probs - truth)
}

#### Fixed 2/3 discounting for Oral stage ----
JMod_Oral <- function(curr.stage, curr.dt, expected.dt, expect.amt=0, act.amt=0, closed.in.fy=F) {
# curr.stage = numeric, current solicitation stage
# curr.dt = date, date to evaluate for discounting
# expected.dt = date, date solicitation is expected to come in
# expect.amt = expected solicitation booked amount
# act.amt = actual solicitation booked amount
# closed.in.fy = whether or not the solicitation was booked in the curr.dt fiscal year
  # If expected to close after EOFY, discount to 0
  prob <- ifelse(FYE(curr.dt) < FYE(expected.dt), 0,
    # If expected to close July through Feb
    ifelse(month(curr.dt) %in% c(7,8,9,10,11,12,1,2),
             ifelse(as.numeric(curr.stage)==1, 1/6, #planned
             ifelse(as.numeric(curr.stage)==2, 1/3, #cleared
             ifelse(as.numeric(curr.stage)==3, 2/3, #asked
             ifelse(as.numeric(curr.stage)==4, 2/3, #oral
             1)))), #1 = paperwork in house = fallback
      # If expected to close Mar or Apr
      ifelse(month(curr.dt) %in% c(3,4),
               ifelse(as.numeric(curr.stage)==1, 1/8,
               ifelse(as.numeric(curr.stage)==2, 1/6,
               ifelse(as.numeric(curr.stage)==3, 1/3,
               ifelse(as.numeric(curr.stage)==4, 2/3,
               1)))),
        # If expected to close May
        ifelse(month(curr.dt)==5,
                 ifelse(as.numeric(curr.stage)==1, 0,
                 ifelse(as.numeric(curr.stage)==2, 1/8,
                 ifelse(as.numeric(curr.stage)==3, 1/6,
                 ifelse(as.numeric(curr.stage)==4, 2/3,
                 1)))),
          # If expected to close June
          ifelse(month(curr.dt)==6,
                   ifelse(as.numeric(curr.stage)==1, 0,
                   ifelse(as.numeric(curr.stage)==2, 0,
                   ifelse(as.numeric(curr.stage)==3, 1/8,
                   ifelse(as.numeric(curr.stage)==4, 2/3,
                   1)))),
                   0 #final fallback of 0 (e.g. no month)
          )
        )
      )
    )
  )
  pred <- ifelse(is.na(expect.amt), 0, expect.amt) * prob
  err <- pred - ifelse(is.na(act.amt), 0, act.amt) * closed.in.fy
  out <- list(probability=prob, prediction=pred, actual=act.amt, error=err)
  return(out)
}

#### Fixed 2/3 discounting for Oral stage, plus 2-step discounting, July-Feb and Mar-June ----
JMod_Oral_2step <- function(curr.stage, curr.dt, expected.dt, expect.amt=0, act.amt=0, closed.in.fy=F) {
# curr.stage = numeric, current solicitation stage
# curr.dt = date, date to evaluate for discounting
# expected.dt = date, date solicitation is expected to come in
# expect.amt = expected solicitation booked amount
# act.amt = actual solicitation booked amount
# closed.in.fy = whether or not the solicitation was booked in the curr.dt fiscal year
  # If expected to close after EOFY, discount to 0
  prob <- ifelse(FYE(curr.dt) < FYE(expected.dt), 0,
    # If expected to close July through Feb
    ifelse(month(curr.dt) %in% c(7,8,9,10,11,12,1,2),
             ifelse(as.numeric(curr.stage)==1, 1/6, #planned
             ifelse(as.numeric(curr.stage)==2, 1/3, #cleared
             ifelse(as.numeric(curr.stage)==3, 2/3, #asked
             ifelse(as.numeric(curr.stage)==4, 2/3, #oral
             1)))), #1 = paperwork in house = fallback
      # If expected to close Mar or Apr
      ifelse(month(curr.dt) %in% c(3,4,5,6),
               ifelse(as.numeric(curr.stage)==1, 1/8,
               ifelse(as.numeric(curr.stage)==2, 1/6,
               ifelse(as.numeric(curr.stage)==3, 1/3,
               ifelse(as.numeric(curr.stage)==4, 2/3,
               1)))),
               0 #final fallback of 0 (e.g. no month)
      )
    )
  )
  pred <- ifelse(is.na(expect.amt), 0, expect.amt) * prob
  err <- pred - ifelse(is.na(act.amt), 0, act.amt) * closed.in.fy
  out <- list(probability=prob, prediction=pred, actual=act.amt, error=err)
  return(out)
}

#### Fixed 3/4 discounting for Oral stage, plus 2-step discounting, July-Feb and Mar-June ----
JMod_Oral_2step_75 <- function(curr.stage, curr.dt, expected.dt, expect.amt=0, act.amt=0, closed.in.fy=F) {
# curr.stage = numeric, current solicitation stage
# curr.dt = date, date to evaluate for discounting
# expected.dt = date, date solicitation is expected to come in
# expect.amt = expected solicitation booked amount
# act.amt = actual solicitation booked amount
# closed.in.fy = whether or not the solicitation was booked in the curr.dt fiscal year
  # If expected to close after EOFY, discount to 0
  prob <- ifelse(FYE(curr.dt) < FYE(expected.dt), 0,
    # If expected to close July through Feb
    ifelse(month(curr.dt) %in% c(7,8,9,10,11,12,1,2),
             ifelse(as.numeric(curr.stage)==1, 1/6, #planned
             ifelse(as.numeric(curr.stage)==2, 1/3, #cleared
             ifelse(as.numeric(curr.stage)==3, 2/3, #asked
             ifelse(as.numeric(curr.stage)==4, 3/4, #oral
             1)))), #1 = paperwork in house = fallback
      # If expected to close Mar or Apr
      ifelse(month(curr.dt) %in% c(3,4,5,6),
               ifelse(as.numeric(curr.stage)==1, 1/8,
               ifelse(as.numeric(curr.stage)==2, 1/6,
               ifelse(as.numeric(curr.stage)==3, 1/3,
               ifelse(as.numeric(curr.stage)==4, 3/4,
               1)))),
               0 #final fallback of 0 (e.g. no month)
      )
    )
  )
  pred <- ifelse(is.na(expect.amt), 0, expect.amt) * prob
  err <- pred - ifelse(is.na(act.amt), 0, act.amt) * closed.in.fy
  out <- list(probability=prob, prediction=pred, actual=act.amt, error=err)
  return(out)
}