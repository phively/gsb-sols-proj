#### RScript1 ----

#### Requires the functions in f.Wrangle.R to be loaded

#### Today's data ----

Joe.dat <- read.csv(datafile, header=T, stringsAsFactors=F) %>%
  filter(Joe.Sol==1)
Joe.dat <- ToDate(Joe.dat, c("Current.Solicitation.Stage.Date","Planned.Ask.Dt","Solicit.Dt.Added",
                             "Ask.Dt","Expected.Dt","Actual.Dt","Solicit.Start.Dt","Most.Recent.Planning",
                             "Most.Recent.Clearance","Most.Recent.Oral.Pledge","Solicit.Stop.Dt"))
Joe.dat <- ToNumeric(Joe.dat, c("Aggregate.Planned.Ask.Amt", "Aggregate.Ask.Amt", "Aggregate.Expected.Amt",
                                "Aggregate.Actual.Amt"))

## Clean up stage dates based on what's available

## Fields that may be useful in modeling
origin="1970-01-02"
Joe.dat2 <- Joe.dat %>% mutate(Solicitation.ID,
                               Solicitation.Type.Desc,
                               Final.Sol.Stage.Dt = Current.Solicitation.Stage.Date,
                               Final.Sol.Stage = Solicitation.Stage.Desc,
                               Final.Sol.Mgr = Solicit.Mgr.Entity.Name,
                               Solicit.Dt.Added,
                               Planned.Dt = Planned.Ask.Dt,
                               Planned.Amt = Aggregate.Planned.Ask.Amt,
                               Expected.Dt = Expected.Dt,
                               Expected.Amt = Aggregate.Expected.Amt,
                               Start.Dt = Solicit.Start.Dt,##
                               Plan.Dt = Most.Recent.Planning,##
                               Planning.Dt = as.Date(as.POSIXct.Date(ifelse(is.na(Plan.Dt), Start.Dt, Plan.Dt), origin=origin)),
                               Clear.Dt = Most.Recent.Clearance,
                               Ask.Dt = Ask.Dt,
                               Ask.Amt = Aggregate.Ask.Amt,
                               Oral.Dt = Most.Recent.Oral.Pledge,
                               Stop.Dt = Solicit.Stop.Dt,##
                               Actual.Dt = as.Date(as.POSIXct.Date(ifelse(is.na(Actual.Dt), Stop.Dt, Actual.Dt), origin=origin)),
                               Actual.Amt = Aggregate.Actual.Amt,
                               Booked = substr(Final.Sol.Stage,1,2) %in% c("10","11") ) %>%
  select(Solicitation.ID, Solicitation.Type.Desc, Final.Sol.Stage.Dt, Final.Sol.Stage, Booked, Final.Sol.Mgr,
         Solicit.Dt.Added, Planned.Dt, Planned.Amt, Expected.Dt, Expected.Amt, Planning.Dt, Clear.Dt,
         Ask.Dt, Ask.Amt, Oral.Dt, Actual.Dt, Actual.Amt)

remove(origin)

#### NA dates cleanup ----
Joe.dat2$Expected.Amt[is.na(Joe.dat2$Expected.Dt) & (Joe.dat2$Expected.Amt %in% c(0,NA))] <- NA
Joe.dat2$Planned.Amt[is.na(Joe.dat2$Planned.Dt) & (Joe.dat2$Planned.Amt %in% c(0,NA))] <- NA
Joe.dat2$Ask.Amt[is.na(Joe.dat2$Ask.Dt) & (Joe.dat2$Ask.Amt %in% c(0,NA))] <- NA
Joe.dat2$Actual.Amt[is.na(Joe.dat2$Actual.Dt) & (Joe.dat2$Actual.Amt %in% c(0,NA))] <- NA

#### Rscript1a ----
mdat <- Joe.dat2 %>% filter(as.numeric(substr(Final.Sol.Stage, 1, 2)) >= 10 & Expected.Amt < 5000000) %>%
  # Drop any solicitations where the actual date is before the start date
  mutate(plan2actual = as.numeric(difftime(Actual.Dt, Planning.Dt, units="days")),
         clear2actual = as.numeric(difftime(Actual.Dt, Clear.Dt, units="days")),
         ask2actual = as.numeric(difftime(Actual.Dt, Ask.Dt, units="days")),
         oral2actual = as.numeric(difftime(Actual.Dt, Oral.Dt, units="days"))) %>%
  filter(plan2actual > 0) %>%
  # Determine the EOFY at the start of each stage
  mutate(planFYE = FYE(Planning.Dt),
         clearFYE = FYE(Clear.Dt),
         askFYE = FYE(Ask.Dt),
         oralFYE = FYE(Oral.Dt),
         # Did the solicitation close in the same FY as the stage
         FY.plan = Actual.Dt <= planFYE,
         FY.clear = Actual.Dt <= clearFYE,
         FY.ask = Actual.Dt <= askFYE,
         FY.oral = Actual.Dt <= oralFYE,
         planFYE = NULL, clearFYE = NULL, askFYE = NULL, oralFYE = NULL) %>%
  # Add indicators for solicitations that booked in the same year they hit the stage
  mutate(FY.Plan.Book = FY.plan*Booked==1,
         FY.Clear.Book = FY.clear*Booked==1,
         FY.Ask.Book = FY.ask*Booked==1,
         FY.Oral.Book = FY.oral*Booked==1)
         
#### RScript1b ----

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
#	Final.Sol.Stage.Dt = NULL,
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

## Save modeling file to disk
write.table(Joe.dat2, file="latest.modeling.txt", sep="\t", row.names=F)
write.table(mderived, file="latest.mderived.txt", sep="\t", row.names=F)