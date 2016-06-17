#### Requires the functions in f.Wrangle.R to be loaded

#### Today's data ----

Joe.dat <- read.csv("2016-03-21 Booth solicitation history.csv", header=T, stringsAsFactors=F) %>%
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

## Save modeling file to disk
write.table(Joe.dat2, file="modeling.txt", sep="\t", row.names=F)
save(Joe.dat2, file="modeling.Rdata")
