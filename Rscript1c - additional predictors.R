#### Derived variables ----

## Needed constants
alpha <- 1
fiscal.month.name <- strtrim(month.name, 3)[c(7:12, 1:6)]

mderived <- mderived %>% mutate(
  lt.Planned.Amt = LogTrans(Planned.Amt),
  lt.Expected.Amt = LogTrans(Expected.Amt),
  lt.Ask.Amt = LogTrans(Ask.Amt),
  fm.planning = FacMonth(planning.fiscal.mo),
  fm.clear = FacMonth(clear.fiscal.mo),
  fm.ask = FacMonth(ask.fiscal.mo),
  fm.oral = FacMonth(oral.fiscal.mo)
)