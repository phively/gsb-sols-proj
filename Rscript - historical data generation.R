#### Iterate through the sub-folder, loading files as data frames ----

## List of filenames in CSV directory
files <- list.files(path="CSV data", pattern="*.csv")

## Each list item contains one file's contents
contents <- list()
for(f in files){
  test <- read.csv(file=paste("CSV data\\",f, sep=""), header=T, stringsAsFactors=F)
  ## Drop NA Sol ID rows
  test <- Renamer(test, "SOL.ID", "Solicitation.ID")
  test <- filter(test, !is.na(Solicitation.ID))
  ## Rename duplicate column headers
  test <- Renamer(test, c("Solicitation.Stage","Stage"), "Solicitation.Stage.Desc")
  test <- Renamer(test, "Expected.Date", "Expected.Dt")
  test <- Renamer(test, "Expected.Amount", "Expected.Amt")
  test <- Renamer(test, c("Prospect.Name","Prospect.Name.1","Prospect.Sort.Name"), "Prospect.Name.Sort")
  test <- Renamer(test, "Ask.Date", "Ask.Dt")
  test <- Renamer(test, "Ask.Amount", "Ask.Amt")
  test <- Renamer(test, "Actual.Date", "Actual.Dt")
  test <- Renamer(test, c("Planned.Date","Planned..Asked.Date","Planned.Ask.Date","Planned.Asked.Date"), "Planned.Ask.Dt")
  test <- Renamer(test, c("Planned.Amount","Planned.Ask.Amount"), "Planned.Ask.Amt")
  test <- Renamer(test, c("Solicitation.Mgr","Active.Solicitation.Mgr","Solicitation.Manager"), "Solicit.Mgr.Entity.Name")
  test <- Renamer(test, c("Solicitation.Team","Solicitation.Team.Name"), "Solicit.Team.Desc")
  test <- Renamer(test, "Solicitor.1", "Solicitor.1.Entity.Name")
  test <- Renamer(test, "Solicitor.2", "Solicitor.2.Entity.Name")
  test <- Renamer(test, "Solicitor.3", "Solicitor.3.Entity.Name")
  test <- Renamer(test, "Title", "Solicitation.Title")
  test <- Renamer(test, c("UC.Degrees.Terse","UC.Degrees"), "UC.DegreesTerse.Program")
  test <- Renamer(test, c("URM","Univ.Rel.Mgr"), "Univ.Rel.Mgr.Entity.Name")
  test <- Renamer(test, c("Solicit.Mgr.Office","Solicitation.Manager.Office"), "Solicit.Mgr.Office.Desc")
  ## Data type cleanup
  test <- ToDate(test, c("Expected.Dt","Ask.Dt","Planned.Ask.Dt"))
  test <- ToNumeric(test, c("Ask.Amt", "Expected.Amt", "Planned.Ask.Amt"))
  ## Append filename and suffix
  test$filename <- gsub(".xlsx.csv", "", gsub(".xls.csv", "", f)) #get rid of .xls.csv
  test$suffix <- substr(test$filename, start=max(gregexpr(" ",test$filename)[[1]])+1, stop=nchar(test$filename))
  ## Extract date report was run
  test$rpt.date <- mdy(test$suffix)
  ## Write data frame to file contents
  contents[[f]] <- test
}

## Use dplyr to iteratively combine into a big data file
sols <- data.frame(NULL)
for(f in files){
  sols <- bind_rows(sols, contents[[f]])
}

#### Cleanup ----

sols2 <- sols %>% mutate(Planned.Ask.Dt = NA, Planned.Ask.Amt = NA, Ask.Dt = NA, Ask.Amt = NA)

## Import today's data

Joe.dat <- read.csv("2016-07-12 Booth solicitation history.csv", header=T, stringsAsFactors=F)
Joe.dat <- ToDate(Joe.dat, c("Current.Solicitation.Stage.Date","Planned.Ask.Dt","Solicit.Dt.Added","Ask.Dt","Expected.Dt","Actual.Dt"))
Joe.dat <- ToNumeric(Joe.dat, c("Final.Planned.Amt", "Final.Ask.Amt", "Final.Expected.Amt", "Actual.Amt", "Aggregate.Ask.Amt",
                                "Aggregate.Planned.Ask.Amt", "Aggregate.Actual.Amt", "Aggregate.Expected.Amt"))

## Fields that may be useful in modeling
Joe.dat2 <- Joe.dat %>% select(Solicitation.ID,
                               Solicitation.Type.Desc,
                               Final.Sol.Stage.Dt = Current.Solicitation.Stage.Date,
                               Final.Sol.Stage = Solicitation.Stage.Desc,
                               Final.Sol.Mgr = Solicit.Mgr.Entity.Name,
                               Solicit.Dt.Added,
                               Final.Planned.Dt = Planned.Ask.Dt,
                               Final.Planned.Amt = Aggregate.Planned.Ask.Amt,
                               Final.Ask.Dt = Ask.Dt,
                               Final.Ask.Amt = Aggregate.Ask.Amt,
                               Final.Expected.Dt = Expected.Dt,
                               Final.Expected.Amt = Aggregate.Expected.Amt,
                               Actual.Dt,
                               Actual.Amt = Aggregate.Actual.Amt)

#### Merge data files ----

## Remove unneded fields
merged.dat <- sols2 %>% select(Solicitation.ID, Solicitation.Stage.Desc, Solicitation.Title, Solicit.Mgr.Entity.Name,
                         Expected.Dt, Expected.Amt, Planned.Ask.Dt, Planned.Ask.Amt, Ask.Dt, Ask.Amt, filename, rpt.date)
## Keep only Joe-era solicitations
merged.dat <- merged.dat[merged.dat$Solicitation.ID %in% Joe.dat$Solicitation.ID,]
## Append final fields to end of merged.dat
merged.dat <- left_join(merged.dat, Joe.dat2, by="Solicitation.ID")

#### NA dates cleanup ----
merged.dat$Expected.Amt[is.na(merged.dat$Expected.Dt) & (merged.dat$Expected.Amt %in% c(0,NA))] <- NA
merged.dat$Planned.Ask.Amt[is.na(merged.dat$Planned.Ask.Dt) & (merged.dat$Planned.Ask.Amt %in% c(0,NA))] <- NA
merged.dat$Ask.Amt[is.na(merged.dat$Ask.Dt) & (merged.dat$Ask.Amt %in% c(0,NA))] <- NA
merged.dat$Final.Planned.Amt[is.na(merged.dat$Final.Planned.Dt) & (merged.dat$Final.Planned.Amt %in% c(0,NA))] <- NA
merged.dat$Final.Ask.Amt[is.na(merged.dat$Final.Ask.Dt) & (merged.dat$Final.Ask.Amt %in% c(0,NA))] <- NA
merged.dat$Final.Expected.Amt[is.na(merged.dat$Final.Expected.Dt) & (merged.dat$Final.Expected.Amt %in% c(0,NA))] <- NA
merged.dat$Actual.Amt[is.na(merged.dat$Actual.Dt) & (merged.dat$Actual.Amt %in% c(0,NA))] <- NA

#### sols2 selection

merged.dat <- merged.dat %>% mutate(
  FYE.Dt = FYE(rpt.date),
  Final.Dt = Actual.Dt
)
# Choose either actual date if available, or final sol stage date if not
merged.dat$Final.Dt[is.na(merged.dat$Final.Dt)] <- merged.dat$Final.Sol.Stage.Dt[is.na(merged.dat$Final.Dt)]
merged.dat <- merged.dat %>% mutate(
  diff = difftime(Final.Dt, FYE.Dt, units="days"),
  Resolved.In.FY = diff <= 0,
  Closed.In.FY = Resolved.In.FY & !(Actual.Amt %in% c(0,NA))
)

sols2 <- merged.dat %>% select(Solicitation.ID, Final.Sol.Stage, Solicitation.Stage.Desc, filename, rpt.date, Expected.Dt, Expected.Amt, Actual.Amt, Resolved.In.FY, Closed.In.FY, Solicitation.Type.Desc, Planned.Ask.Amt, Ask.Amt)

## Impute any missing values from mdat
# Expected
sols2$Expected.Amt[is.na(sols2$Expected.Amt)] <- left_join(sols2[is.na(sols2$Expected.Amt), c("Solicitation.ID", "Expected.Amt")], mdat[, c("Solicitation.ID", "Expected.Amt")], by="Solicitation.ID")[, 3]
# Actual
sols2$Actual.Amt[is.na(sols2$Actual.Amt)] <- left_join(sols2[is.na(sols2$Actual.Amt), c("Solicitation.ID", "Actual.Amt")], mdat[, c("Solicitation.ID", "Actual.Amt")], by="Solicitation.ID")[, 3]
# Planned
sols2$Planned.Ask.Amt[is.na(sols2$Planned.Ask.Amt)] <- left_join(sols2[is.na(sols2$Planned.Ask.Amt), c("Solicitation.ID", "Planned.Ask.Amt")], mdat[, c("Solicitation.ID", "Planned.Amt")], by="Solicitation.ID")[, 3]
# Ask
sols2$Ask.Amt[is.na(sols2$Ask.Amt)] <- left_join(sols2[is.na(sols2$Ask.Amt), c("Solicitation.ID", "Ask.Amt")], mdat[, c("Solicitation.ID", "Ask.Amt")], by="Solicitation.ID")[, 3]
## Create derived fields
sol.stage <- substr(sols2$Solicitation.Stage.Desc, 1, 2)
sols2 <- sols2 %>% mutate(
  Curr.Stage = ifelse(sol.stage %in% c("1.", "2.", "3.", "3A", "3B", "3C", "4."), 1,
               ifelse(sol.stage %in% c("5.", "6."), 2,
               ifelse(sol.stage %in% c("7."), 3,
               ifelse(sol.stage %in% c("8."), 4,
               ifelse(sol.stage %in% c("9."), 5,
               NA)
  )))),
  FY.Plan.Book = Curr.Stage == 1 & Closed.In.FY,
  FY.Clear.Book = Curr.Stage == 2 & Closed.In.FY,
  FY.Ask.Book = Curr.Stage == 3 & Closed.In.FY,
  FY.Oral.Book = Curr.Stage == 4 & Closed.In.FY,
  Sol.Type.Agg = factor(
    ifelse(Solicitation.Type.Desc == "Standard Pledge", "Standard Pledge",
    ifelse(Solicitation.Type.Desc == "Outright Gift", "Outright Gift",
    "Other")
  )),
  lt.Planned.Amt = LogTrans(Planned.Ask.Amt),
  lt.Expected.Amt = LogTrans(Expected.Amt),
  lt.Ask.Amt = LogTrans(Ask.Amt),
  Actual.Amt.In.FY = Actual.Amt * Closed.In.FY,
  Expect.In.Future.FY = Expected.Dt > FYE(rpt.date)
)
## Select needed fields
sols2 <- sols2 %>% select(Solicitation.ID, Final.Sol.Stage, Solicitation.Stage.Desc, Curr.Stage, filename, rpt.date, Expected.Dt, Planned.Ask.Amt, Ask.Amt, Expected.Amt, Actual.Amt, Actual.Amt.In.FY, Closed.In.FY, Sol.Type.Agg, lt.Planned.Amt, lt.Expected.Amt, lt.Ask.Amt, Expected.Dt, Expect.In.Future.FY)

## Save modeling file to disk
