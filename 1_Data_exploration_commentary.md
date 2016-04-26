Source can be viewed [on GitHub](https://github.com/phively/gsb-sols-proj/blob/master/1%20Data%20exploration%20commentary.Rmd).

Problem statement
=================

My goal is to find a way to accurately forecast fundraising revenue.

The existing model ("JMod") has two components. First, the probability a solicitation \(S_{i}\) comes in before the end of the fiscal year is a function of the current stage progress \(c\) and time until the end of the year \(t\), \(P(S_{i}=1)=f(c_{i},t)\). The expected FRP in the current fiscal year is this probability times the expected amount, \(E(FRP)=P(S_{i}=1)E(S_{i})\).

Focus on the probability model. I assume that there should also be some effect due to the average close rate, season (e.g. people give more in December), planned ask amount, actual ask amount, and potentially interactions. Something like this:

\[P(S_{ij}=1)=f(c_{i},t,\mu,t_{i}^{*},a_{j}^{*},a_{j})\]

It makes sense to start with a few straightforward classifiers, but I'd also like to look into nesting models.

Visualizations
==============

The dataset includes all closed (successful = booked, or unsuccessful = not booked) solicitations with an add date on or after 7/1/2011. Data is obtained from the first tab of the "Booth solicitation history" report (saved as a .csv).

``` r
#### Run Rscript0 to load useful packages and functions ----
source("Rscript0 - libraries.R")
source("f.Wrangle.R")
#### Run Rscript1 to load data ----
source("Rscript1 - data load and transform.R")
```

-   [Rscript0 - libraries.R](https://github.com/phively/gsb-sols-proj/blob/master/Rscript0%20-%20libraries.R)
-   [f.Wrangle.R](https://github.com/phively/gsb-sols-proj/blob/master/f.Wrangle.R)
-   [Rscript1 - data load and transform.R](https://github.com/phively/gsb-sols-proj/blob/master/Rscript1%20-%20data%20load%20and%20transform.R)

Begin with some visualizations of time in each stage:

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-2-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-2-2.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-2-3.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-2-4.png)<!-- -->

There's extremely obvious bimodality in most cases. Note in particular that while PG (defined as $5M+) is skewed positive overall, there seem to be two distinct groups: takes a while and takes a *long* while. The purple "Days from ask to actual" is about as perfect a mixture of two \(N(\mu_{k},\sigma^{2}_{k})\) as I've ever seen. Might be interesting to see if a certain population just takes longer in general.

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-3-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-3-2.png)<!-- -->

There isn't much data in the oral pledge stage so things are a bit bumpier here.

Here's the cleaned up data file:

``` r
#### Model data cleanup ----
source("Rscript1a - modeling data appends.R")
colnames(mdat)
```

    ##  [1] "Solicitation.ID"        "Solicitation.Type.Desc"
    ##  [3] "Final.Sol.Stage.Dt"     "Final.Sol.Stage"       
    ##  [5] "Booked"                 "Final.Sol.Mgr"         
    ##  [7] "Solicit.Dt.Added"       "Planned.Dt"            
    ##  [9] "Planned.Amt"            "Expected.Dt"           
    ## [11] "Expected.Amt"           "Planning.Dt"           
    ## [13] "Clear.Dt"               "Ask.Dt"                
    ## [15] "Ask.Amt"                "Oral.Dt"               
    ## [17] "Actual.Dt"              "Actual.Amt"            
    ## [19] "plan2actual"            "clear2actual"          
    ## [21] "ask2actual"             "oral2actual"           
    ## [23] "FY.plan"                "FY.clear"              
    ## [25] "FY.ask"                 "FY.oral"               
    ## [27] "FY.Plan.Book"           "FY.Clear.Book"         
    ## [29] "FY.Ask.Book"            "FY.Oral.Book"

-   [Rscript1a - modeling data appends.R](https://github.com/phively/gsb-sols-proj/blob/master/Rscript1a%20-%20modeling%20data%20appends.R)

**Booked** indicates whether the solicitation came in at \(>\)$0, and **FY.plan, FY.clear, FY.ask, FY.oral** indicate whether the solicitation closed in the same fiscal year as that stage was reached, while the corresponding **FY.[Stage].Book** indicates that a solicitation was booked in the same fiscal year that it reached that stage. Combinations of these indicators will be the dependent variables. **plan2actual, clear2actual, ask2actual, oral2actual** are measured in days. **Planned.Dt** is the planned *ask* date, while **Planning.Dt** is the date the solicitation entered the planning stage. The other covariates should be self-explanatory.

Variable exploration
====================

Dependent variables
-------------------

Do I want to model time to close from each current stage \(c_{i}\) as opposed to \(P(S_{ij}=1)\)?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-5-1.png)<!-- -->

|     |   plan2actual  |   clear2actual  |   ask2actual   |   oral2actual  |
|-----|:--------------:|:---------------:|:--------------:|:--------------:|
|     |   Min. : 1.0   |   Min. : -8.25  |  Min. :-35.21  |  Min. :-33.25  |
|     |  1st Qu.: 45.0 |  1st Qu.: 28.75 | 1st Qu.: 22.75 | 1st Qu.: 12.75 |
|     | Median : 126.0 |  Median : 91.75 | Median : 66.75 | Median : 30.79 |
|     |  Mean : 195.8  |  Mean : 142.96  |  Mean :114.12  |  Mean : 64.33  |
|     | 3rd Qu.: 283.0 | 3rd Qu.: 209.79 | 3rd Qu.:162.79 | 3rd Qu.: 75.75 |
|     |  Max. :1426.0  |  Max. :1426.75  |  Max. :919.75  |  Max. :919.75  |
|     |       NA       |    NA's :116    |    NA's :172   |    NA's :947   |

Negative values indicate a data issue or update. Decidedly right-skewed. Try a square root transformation:

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-6-1.png)<!-- -->

Not bad. How's log look?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-7-1.png)<!-- -->

Might be a good idea to try e.g. Box-Cox transformations to see how they compare.

Data counts - do I have enough to do what I want? Is it remotely reasonable to split by month or quarter?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-8-1.png)<!-- -->

|       |  Jul|  Aug|  Sep|  Oct|  Nov|  Dec|  Jan|  Feb|  Mar|  Apr|  May|  Jun|
|-------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| Plan  |   83|   98|  115|  159|  106|  125|   82|   65|  100|  133|  137|  106|
| Clear |   64|   82|  142|  164|  139|  145|   92|   76|   79|   79|   72|   59|
| Ask   |   50|   65|   90|  133|  163|  156|   74|   71|  118|   67|   78|   72|
| Oral  |   28|   11|   21|   34|   47|   37|   33|   25|   18|   33|   37|   38|

Peak ask occurs around Nov-Dec and Mar each year. Not nearly as much oral pledge data; usually oral to booked is fast enough that the date isn't captured in the system. (JMod rolls ask and oral together.)

Finally, let's take a look at the booked versus declined solicitations by stage by fiscal year closed.

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-9-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-9-2.png)<!-- -->

Confidence bands are based on the variance for a binomial random variable, \(\sqrt{\frac{p(1-p)}{n}}\) (assuming independence).

This is fascinating; a solicitation entering plan or clear in January or plan in April-July is likely to be refused rather than booked. Is there a specific year driving that?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-10-1.png)<!-- -->

Note that this is fiscal, not calendar, year. FY2011 didn't have much stage progress, as it is the first year of data.

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-11-1.png)<!-- -->

Looks like 2013 was a particularly good year, but it's normal to have lots of duds in those winter months

Discrete covariates
-------------------

I'm thinking time to close is not the natural outcome measure; a model predicting time to close can't be swapped out directly with the corresponding terms in JMod. Recall from above:

> For the probability model, I assume that there should also be some effect due to the average close rate, season (e.g. people give more in December), planned ask amount, actual ask amount, and potentially interactions. Something like this:
>
> \[P(S_{ij}=1)=f(c_{i},t,\mu,t_{i}^{*},a_{j}^{*},a_{j})\]

Take a look at each of these additional variables against Booked.

### Ask Amount

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-12-1.png)<!-- -->

Histogram and smoothed density estimate. It's above 50% close rate at every level, but the poorest close rate is between the $100k to $1M levels. There's obviously nonlinearity; a cubic polynomial might be a good approximation?

### Ask Amount/Planned Amount

I think differences in planned ask versus actual ask amounts might indicate trouble. How does the ratio of actual ask over planned ask look?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-13-1.png)<!-- -->

Too choppy to really see much of anything; most solicitations are asked at the planned amount and there are so few in the tails that there isn't much interesting to say. What if we just look at a few binned thresholds?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-14-1.png)<!-- -->

If someone asks slightly more than the planned ask amount it's a good sign that the talks were going well; otherwise there doesn't seem to be much to see here. The 25%-50% bin might also be statistically significant; don't know about the others given the small sample sizes.

Stage progression within a single year
--------------------------------------

Given that a solicitation reaches a certain stage, how likely is it to close that year, whether or not it's booked?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-15-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-15-2.png)<!-- -->

| Stage | Closed |  Count|  Proportion|
|:------|:-------|------:|-----------:|
| Plan  | FALSE  |    553|        0.42|
| Plan  | TRUE   |    756|        0.58|
| Clear | FALSE  |    331|        0.28|
| Clear | TRUE   |    862|        0.72|
| Ask   | FALSE  |    284|        0.25|
| Ask   | TRUE   |    853|        0.75|
| Oral  | FALSE  |     60|        0.17|
| Oral  | TRUE   |    302|        0.83|

Ask and Clear are closer than I would have thought. What about if we look at whether they were both closed *and* booked that year?

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-16-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-16-2.png)<!-- -->

| Stage | Booked.In.FY |  Count|  Proportion|
|:------|:-------------|------:|-----------:|
| Plan  | FALSE        |    767|        0.59|
| Plan  | TRUE         |    542|        0.41|
| Clear | FALSE        |    580|        0.49|
| Clear | TRUE         |    613|        0.51|
| Ask   | FALSE        |    516|        0.45|
| Ask   | TRUE         |    621|        0.55|
| Oral  | FALSE        |     71|        0.20|
| Oral  | TRUE         |    291|        0.80|

Curious; there really doesn't seem to be a discernible difference between clear and ask. The last thing to check is these hybrid "reached stage and closed" indicators by month.

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-17-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-17-2.png)<!-- -->

There are a couple of crossover points; January for Plan and Clear; Jan for ask (though it crosses back in March interestingly). Can't count on oral pledges made in May or June coming in before the end of the year. Let's focus on asks in the MG range, \(x\in[\$25\text{k},\$5\text{M})\):

![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-18-1.png)<!-- -->![](1_Data_exploration_commentary_files/figure-markdown_github/unnamed-chunk-18-2.png)<!-- -->

Ha, that's more like it. Plan has a negative trend after Feb; Clear is surprisingly close to flat; Ask has a negative trend after March; Oral can't be counted on in May and June.

Packages used
=============

``` r
session_info()
```

    ## Session info --------------------------------------------------------------

    ##  setting  value                                      
    ##  version  R version 3.2.4 Revised (2016-03-16 r70336)
    ##  system   x86_64, mingw32                            
    ##  ui       RTerm                                      
    ##  language (EN)                                       
    ##  collate  English_United States.1252                 
    ##  tz       America/Chicago                            
    ##  date     2016-04-26

    ## Packages ------------------------------------------------------------------

    ##  package    * version date       source        
    ##  assertthat   0.1     2013-12-06 CRAN (R 3.2.2)
    ##  colorspace   1.2-6   2015-03-11 CRAN (R 3.2.2)
    ##  DBI          0.3.1   2014-09-24 CRAN (R 3.2.2)
    ##  devtools   * 1.11.0  2016-04-12 CRAN (R 3.2.4)
    ##  digest       0.6.8   2014-12-31 CRAN (R 3.2.2)
    ##  dplyr      * 0.4.3   2015-09-01 CRAN (R 3.2.3)
    ##  evaluate     0.8.3   2016-03-05 CRAN (R 3.2.4)
    ##  formatR      1.3     2016-03-05 CRAN (R 3.2.4)
    ##  ggplot2    * 2.0.0   2015-12-18 CRAN (R 3.2.3)
    ##  gridExtra  * 2.0.0   2015-07-14 CRAN (R 3.2.3)
    ##  gtable       0.1.2   2012-12-05 CRAN (R 3.2.2)
    ##  highr        0.5.1   2015-09-18 CRAN (R 3.2.4)
    ##  htmltools    0.3     2015-12-29 CRAN (R 3.2.3)
    ##  knitr      * 1.12.3  2016-01-22 CRAN (R 3.2.4)
    ##  labeling     0.3     2014-08-23 CRAN (R 3.2.2)
    ##  lazyeval     0.1.10  2015-01-02 CRAN (R 3.2.2)
    ##  lubridate  * 1.5.0   2015-12-03 CRAN (R 3.2.3)
    ##  magrittr     1.5     2014-11-22 CRAN (R 3.2.2)
    ##  memoise      1.0.0   2016-01-29 CRAN (R 3.2.4)
    ##  munsell      0.4.2   2013-07-11 CRAN (R 3.2.2)
    ##  plyr         1.8.3   2015-06-12 CRAN (R 3.2.2)
    ##  R6           2.1.1   2015-08-19 CRAN (R 3.2.2)
    ##  Rcpp         0.12.1  2015-09-10 CRAN (R 3.2.2)
    ##  reshape2     1.4.1   2014-12-06 CRAN (R 3.2.2)
    ##  rmarkdown    0.9.5   2016-02-22 CRAN (R 3.2.3)
    ##  scales     * 0.3.0   2015-08-25 CRAN (R 3.2.2)
    ##  stringi      0.5-5   2015-06-29 CRAN (R 3.2.2)
    ##  stringr      1.0.0   2015-04-30 CRAN (R 3.2.2)
    ##  tidyr      * 0.3.1   2015-09-10 CRAN (R 3.2.2)
    ##  withr        1.0.1   2016-02-04 CRAN (R 3.2.4)
    ##  yaml         2.1.13  2014-06-12 CRAN (R 3.2.3)
