#### Data file preprocessing ----

source("Rscript1a - modeling data appends.R")

#### Derived variables ----

mderived <- mdat %>% mutate(
  # Ask and planned ask giving amounts
  Ask.Amt.Over.Planned.Amt = Ask.Amt/Planned.Amt,
  Ask.Band = factor(
    ifelse(Ask.Amt>=5000000, "PG",
    ifelse(Ask.Amt>=25000, "MG",
    ifelse(Ask.Amt>0, "AF",
    NA)))
  ),
  Planned.Ask.Band = factor(
    ifelse(Planned.Amt>=5000000, "PG",
    ifelse(Planned.Amt>=25000, "MG",
    ifelse(Planned.Amt>0, "AF",
    NA)))
  ),
  # Boolean indicators - is the expected date in a different fiscal year?
  Plan.Future = FYE(Planning.Dt) < FYE(Expected.Dt),
  Clear.Future = FYE(Clear.Dt) < FYE(Expected.Dt),
  Ask.Future = FYE(Ask.Dt) < FYE(Expected.Dt),
  Oral.Future = FYE(Oral.Dt) < FYE(Expected.Dt),
  # Days between stages
  plan2clear = as.numeric(difftime(Clear.Dt, Planning.Dt, units="days")),
  clear2ask = as.numeric(difftime(Ask.Dt, Clear.Dt, units="days")),
  ask2oral = as.numeric(difftime(Oral.Dt, Ask.Dt, units="days")),
  # Days until important dates
	plan2EOFY = as.numeric(difftime(FYE(Planning.Dt), Planning.Dt, units="days")),
	clear2EOFY = as.numeric(difftime(FYE(Clear.Dt), Clear.Dt, units="days")),
	ask2EOFY = as.numeric(difftime(FYE(Ask.Dt), Ask.Dt, units="days")),
	oral2EOFY = as.numeric(difftime(FYE(Oral.Dt), Oral.Dt, units="days")),
	plan2expect = as.numeric(difftime(Expected.Dt, Planning.Dt, units="days")),
	clear2expect = as.numeric(difftime(Expected.Dt, Clear.Dt, units="days")),
	ask2expect = as.numeric(difftime(Expected.Dt, Ask.Dt, units="days")),
	oral2expect = as.numeric(difftime(Expected.Dt, Oral.Dt, units="days"))
)
		
#### Save data to disk ----

write.table(mderived, file="mderived.txt", sep="\t", row.names=F)