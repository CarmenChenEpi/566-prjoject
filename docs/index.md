Midterm project
================
Carmen Chen
10/20/2021

Introduction (provide background on your dataset and formulated
question)

Vaccination rate and COVID-19 incidence cases in California.

``` r
cases <- read.csv("Cases.csv")
vaccinations <- read.csv("Vaccinations.csv")
```

``` r
table(vaccinations$Location)
```

    ## 
    ##  AK  AL  AR  AS  AZ BP2  CA  CO  CT  DC DD2  DE  FL  FM  GA  GU  HI  IA  ID IH2 
    ## 311 311 311 312 311 309 311 311 311 311 311 311 311 301 311 312 311 311 311 311 
    ##  IL  IN  KS  KY  LA  MA  MD  ME  MH  MI  MN  MO  MP  MS  MT  NC  ND  NE  NH  NJ 
    ## 311 311 311 311 311 311 311 311 301 311 311 311 312 311 311 311 311 311 311 311 
    ##  NM  NV  NY  OH  OK  OR  PA  PR  RI  RP  SC  SD  TN  TX  US  UT  VA VA2  VI  VT 
    ## 311 311 311 311 311 311 311 311 311 295 311 311 311 311 312 311 311 311 312 311 
    ##  WA  WI  WV  WY 
    ## 311 311 311 311

``` r
vaccinations <- vaccinations[vaccinations$Location == "CA", ]
```

Methods (include how and where the data were acquired, how you cleaned
and wrangled the data, what tools you used for data exploration)

Data of COVID-19 cases and vaccinations was acquired from CovidTracker
and US Center for Disease Control and Prevention, respectively.

Preliminary Results (provide summary statistics in tabular form and
publication-quality figures, take a look at the kable function from
knitr to write nice tables in Rmarkdown)

Conclusion
