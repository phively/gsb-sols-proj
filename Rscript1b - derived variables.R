#### Data file preprocessing ----

source("Rscript1a - modeling data appends.R")

#### Derived variables ----

mderived <- mdat %>% mutate(
  # Character to factor
  Solicitation.Type.Desc = factor(Solicitation.Type.Desc),
  Final.Sol.Mgr = factor(Final.Sol.Mgr),
  # Ask and planned ask giving amounts
  Ask.Amt.Over.Pln.Amt = Ask.Amt/Planned.Amt,
  Ask.Amt.Over.Pln.Fac = factor(
    ifelse(Ask.Amt.Over.Pln.Amt < 1, "Down",
    ifelse(Ask.Amt.Over.Pln.Amt == 1, "Same",
    ifelse(Ask.Amt.Over.Pln.Amt > 1, "Up",
    NA)))
  ),
  Ask.Band = factor(
    ifelse(Ask.Amt >= 5000000, "PG",
    ifelse(Ask.Amt >= 25000, "MG",
    ifelse(Ask.Amt > 0, "AF",
    NA)))
  ),
  Planned.Ask.Band = factor(
    ifelse(Planned.Amt >= 5000000, "PG",
    ifelse(Planned.Amt >= 25000, "MG",
    ifelse(Planned.Amt > 0, "AF",
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
	oral2expect = as.numeric(difftime(Expected.Dt, Oral.Dt, units="days")),
	# Month of milestones, transformed so July is month 1
	Planned.Fiscal.Mo = MoToFiscalMo(month(Planned.Dt)),
	Expected.Fiscal.Mo = MoToFiscalMo(month(Expected.Dt)),
	planning.fiscal.mo = MoToFiscalMo(month(Planning.Dt)),
	clear.fiscal.mo = MoToFiscalMo(month(Clear.Dt)),
	ask.fiscal.mo = MoToFiscalMo(month(Ask.Dt)),
	oral.fiscal.mo = MoToFiscalMo(month(Oral.Dt)),
	actual.fiscal.mo = MoToFiscalMo(month(Actual.Dt)),
	# Aggregated variables
	Sol.Type.Agg = factor(
	  ifelse(Solicitation.Type.Desc == "Standard Pledge", "Standard Pledge",
	  ifelse(Solicitation.Type.Desc == "Outright Gift", "Outright Gift",
	  "Other"))
	),
	# Remove variables not needed for modeling or ID
	Final.Sol.Stage.Dt = NULL,
	Final.Sol.Stage = NULL,
	Booked = NULL,
	Solicit.Dt.Added = NULL,
	Planned.Dt = NULL,
	Expected.Dt = NULL,
	Planning.Dt = NULL,
	Clear.Dt = NULL,
	Ask.Dt = NULL,
	Oral.Dt = NULL,
	Actual.Dt = NULL,
	plan2actual = NULL,
	clear2actual = NULL,
	ask2actual = NULL,
	oral2actual=NULL,
	FY.plan = NULL,
	FY.clear = NULL,
	FY.ask = NULL,
	FY.oral = NULL	
)
		
#### Save data to disk ----

write.table(mderived, file="mderived.txt", sep="\t", row.names=F)