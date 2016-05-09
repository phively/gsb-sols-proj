#### Model data cleanup ----

# Load data saved in Rscript1

mdat <- read.csv("modeling.txt", sep="\t", stringsAsFactors=F)

# Include only closed solicitations with Expected under $5M
mdat <- mdat %>% filter(as.numeric(substr(Final.Sol.Stage,1,2)) >= 10 & Expected.Amt<5000000) %>%
  # Drop any solicitations where the actual date is before the start date
  mutate(plan2actual = as.numeric(difftime(Actual.Dt, Planning.Dt, units="days")),
         clear2actual = as.numeric(difftime(Actual.Dt, Clear.Dt, units="days")),
         ask2actual = as.numeric(difftime(Actual.Dt, Ask.Dt, units="days")),
         oral2actual = as.numeric(difftime(Actual.Dt, Oral.Dt, units="days"))) %>%
  filter(plan2actual>0) %>%
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