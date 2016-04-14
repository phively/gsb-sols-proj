#### Model data cleanup ----

# Function to return end of the fiscal year
fye <- function(date){ymd(as.character(paste(round(month(date)/13)+year(date),"06","30", sep="-")))}

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
  mutate(planFYE = fye(Planning.Dt),
         clearFYE = fye(Clear.Dt),
         askFYE = fye(Ask.Dt),
         oralFYE = fye(Oral.Dt),
         # Did the solicitation close in the same FY as the stage
         FY.plan = Actual.Dt <= planFYE,
         FY.clear = Actual.Dt <= clearFYE,
         FY.ask = Actual.Dt <= askFYE,
         FY.oral = Actual.Dt <= oralFYE,
         planFYE = NULL, clearFYE = NULL, askFYE = NULL, oralFYE = NULL) %>%
  # Add indicators for solicitations that booked in the same year they hit the stage
  mutate(FY.Plan.Book = FY.plan & Booked,
         FY.Clear.Book = FY.clear & Booked,
         FY.Ask.Book = FY.ask & Booked,
         FY.Oral.Book = FY.oral & Booked)