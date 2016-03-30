#### Useful packages for data manipulation ----
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

#### Days between stages ----
ggdat <- Joe.dat2 %>%  mutate(plan2clear = as.numeric(difftime(Clear.Dt, Planning.Dt, units="days")),
                             clear2ask = as.numeric(difftime(Ask.Dt, Clear.Dt, units="days")),
                             ask2oral = as.numeric(difftime(Oral.Dt, Ask.Dt, units="days")),
                             ask2actual = as.numeric(difftime(Actual.Dt, Ask.Dt, units="days")),
                             oral2actual = as.numeric(difftime(Actual.Dt, Oral.Dt, units="days")),
                             plan2actual = as.numeric(difftime(Actual.Dt, Planning.Dt, units="days")),
                             giftsize = factor(ifelse(Expected.Amt>=5000000, 5000, ifelse(Expected.Amt>=1000000,
                                          1000, ifelse(Expected.Amt>=100000,100,0))),
                                          labels=c("Under $100k","$100k+","$1M+","$5M+"))) %>%
  # Only use closed solicitations; Stage 10=Booked, 12=Refused, 14=Cancelled, etc.
  filter(as.numeric(substr(Final.Sol.Stage,1,2)) >= 10)

g1 <- ggplot(ggdat, aes(x=plan2clear, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from planning to clearance") + xlim(c(0,1000))
g2 <- ggplot(ggdat, aes(x=clear2ask, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from clearance to ask")
g3 <- ggplot(ggdat, aes(x=ask2actual, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from ask to actual")
g4 <- ggplot(ggdat, aes(x=plan2actual, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from planning to actual")

grid.arrange(g1, g2, g3, g4, ncol=1)

g1 <- ggplot(ggdat[ggdat$giftsize!="$5M+",], aes(x=ask2oral, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from ask to oral")
g2 <- ggplot(ggdat[ggdat$giftsize!="$5M+",], aes(x=oral2actual, fill=giftsize)) + geom_density(alpha=.5) +
  facet_grid(giftsize~.) + labs(title="Days from oral to actual")

grid.arrange(g1, g2, ncol=1)

remove(g1, g2, g3, g4)

#### What does a start to end time model look like ----
# Filter out $5M+ expected amounts
mdat <- ggdat[ggdat$giftsize!="$5M+",]

mdat$y <- sqrt(abs(mdat$plan2actual))*sign(mdat$plan2actual)
hist(mdat$y) #beautifully normal...!

lmod <- lm(y ~ sqrt(Expected.Amt) + as.factor(Solicitation.Type.Desc) + as.factor(year(Ask.Dt)), mdat)
summary(lmod)
anova(lmod)
plot(lmod)

# Sans outliers
lmod <- lm(y ~ sqrt(Expected.Amt) + as.factor(year(Ask.Dt)), mdat[-c(992,342),])
anova(lmod)
summary(lmod)
plot(lmod)

summary(lmod)$coef[2,1]^2 * 24 * 60 * 60
# 2.659 seconds longer to close for each additional $1 expected, heh

#### Planning to clear time ----
mdat$y <- log(mdat$plan2clear)
hist(mdat$y)

lmod <- lm(y ~ sqrt(Expected.Amt) + as.factor(Solicitation.Type.Desc) + as.factor(year(Planning.Dt)), mdat[-c(178,230),])
anova(lmod)
summary(lmod)
plot(lmod)

#### Ask to Actual time ----
hist(mdat$ask2actual)
mdat$y <- sqrt(abs(mdat$ask2actual))*sign(mdat$ask2actual)

lmod <- lm(y ~ sqrt(Expected.Amt) + as.factor(Solicitation.Type.Desc) + as.factor(year(Ask.Dt)), mdat)
anova(lmod)

lmod <- lm(y ~ sqrt(Expected.Amt) + as.factor(year(Ask.Dt)), mdat)
summary(lmod)
plot(lmod)

summary(lmod)$coef[2,1]^2 * 24 * 60 * 60
# .44 seconds longer to close for each additional $1 expected, heh

remove(mdat, ggdat, lmod)

#### Model data cleanup project ----

# Function to return end of the fiscal year
fye <- function(date){ymd(as.character(paste(round(month(date)/13)+year(date),"06","30", sep="-")))}

# Include only closed solicitations with Expected under $5M
mdat <- Joe.dat2 %>% filter(as.numeric(substr(Final.Sol.Stage,1,2)) >= 10 & Expected.Amt<5000000) %>%
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
         planFYE = NULL, clearFYE = NULL, askFYE = NULL, oralFYE = NULL)

# For proofing
# write.table(mdat, file="test.txt", sep="\t", col.names=T, row.names=F)

#### Variable exploration ----

## Dependent variables
with(mdat, boxplot(plan2actual, clear2actual, ask2actual, oral2actual))
# See decided right-skewedness; try a transformation
with(mdat, boxplot(sqrt(plan2actual), sqrt(clear2actual), sqrt(ask2actual), sqrt(oral2actual)))
# Not half bad; what about log?
with(mdat, boxplot(log(plan2actual), log(clear2actual), log(ask2actual), log(oral2actual)))
# Probably should try a Box-Cox analysis when doing this for real

## Check the counts
with(mdat, length(na.omit(clear2actual)))
with(mdat, length(na.omit(ask2actual)))
with(mdat, length(na.omit(oral2actual)))
# Don't think I have enough data for an oral model
tmp <- mdat %>% mutate(Plan=as.numeric(month(Planned.Dt)), Clear=as.numeric(month(Clear.Dt)), Ask=as.numeric(month(Ask.Dt)),
                       Oral=as.numeric(month(Oral.Dt)), Count=1) %>% select(Count, Plan, Clear, Ask, Oral)

tmp <- rbind(xtabs(Count ~ as.numeric(month(Plan)), data=tmp, na.action="na.omit"),
      xtabs(Count ~ as.numeric(month(Clear)), data=tmp, na.action="na.omit"),
      xtabs(Count ~ as.numeric(month(Ask)), data=tmp, na.action="na.omit"),
      xtabs(Count ~ as.numeric(month(Oral)), data=tmp, na.action="na.omit"))
rownames(tmp) <- c("Plan","Clear","Ask","Oral")
colnames(tmp) <- month.name
# Table
(tmp[,c(7:12,1:6)])

## Explanatory variables

#### Logistic regression model for each stage ----
mod.plan <- glm(Y.plan ~ 1, mdat, family=binomial())
