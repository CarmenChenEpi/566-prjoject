Midterm project
================
Carmen Chen
10/20/2021

\#Introduction (provide background on your dataset and formulated
question)

Vaccination rate and COVID-19 incidence cases in California.

\#Methods (include how and where the data were acquired, how you cleaned
and wrangled the data, what tools you used for data exploration)

Data of COVID-19 cases and vaccinations was acquired from California
Health & Human Services Agency and US Center for Disease Control and
Prevention, respectively. Data

``` r
cases <- read.csv("Cases.csv")
vaccinations <- read.csv("Vaccinations.csv")
```

``` r
#subset the data
vaccinations <- vaccinations[vaccinations$Location == "CA",]
vaccinations <- vaccinations[, c("Date", "Administered_Dose1_Pop_Pct", "Series_Complete_Pop_Pct")] 
cases <- cases[cases$area == "California",]
cases <- cases[, c("date", "cases", "cumulative_cases", "deaths", "cumulative_deaths")]
#rename the data
vaccinations <- rename(vaccinations, date = Date, dose1 = Administered_Dose1_Pop_Pct, dose2 = Series_Complete_Pop_Pct)
#format date
vaccinations$date <- as.Date(vaccinations$date, format = "%m/%d/%Y")
cases$date <- as.Date(cases$date, format = "%Y-%m-%d")
```

``` r
#check the date
summary(vaccinations$date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2020-12-14" "2021-03-01" "2021-05-18" "2021-05-18" "2021-08-03" "2021-10-20"

``` r
summary(cases$date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2020-02-01" "2020-07-06" "2020-12-10" "2020-12-10" "2021-05-16" "2021-10-20" 
    ##         NA's 
    ##          "1"

``` r
#check for missing value
cases[!complete.cases(cases),]
```

    ##      date cases cumulative_cases deaths cumulative_deaths
    ## 3774 <NA>   481          4600506    269             70741

``` r
vaccinations[!complete.cases(vaccinations),]
```

    ## [1] date  dose1 dose2
    ## <0 rows> (or 0-length row.names)

``` r
#remove rows with missing value
cases <- cases[complete.cases(cases),]
#check the dimensions, headers, footers
dim(vaccinations)
```

    ## [1] 311   3

``` r
dim(cases)
```

    ## [1] 628   5

``` r
head(vaccinations)
```

    ##           date dose1 dose2
    ## 49  2021-10-20  73.5  60.5
    ## 115 2021-10-19  73.4  60.4
    ## 160 2021-10-18  73.4  60.4
    ## 253 2021-10-17  73.3  60.3
    ## 260 2021-10-16  73.2  60.2
    ## 384 2021-10-15  73.1  60.2

``` r
cases <- cases[order(cases$date, decreasing = TRUE),]
head(cases)
```

    ##            date cases cumulative_cases deaths cumulative_deaths
    ## 3773 2021-10-20     0          4600025      0             70472
    ## 3772 2021-10-19   604          4600025      1             70472
    ## 3771 2021-10-18  3269          4599421     10             70471
    ## 3770 2021-10-17  1944          4596152     16             70461
    ## 3769 2021-10-16  2408          4594208     21             70445
    ## 3768 2021-10-15  4418          4591800     13             70424

``` r
tail(vaccinations)
```

    ##             date dose1 dose2
    ## 19527 2020-12-19     0     0
    ## 19608 2020-12-18     0     0
    ## 19627 2020-12-17     0     0
    ## 19737 2020-12-16     0     0
    ## 19762 2020-12-15     0     0
    ## 19821 2020-12-14     0     0

``` r
tail(cases)
```

    ##            date cases cumulative_cases deaths cumulative_deaths
    ## 3151 2020-02-06     6               50      1                 1
    ## 3150 2020-02-05     4               44      0                 0
    ## 3149 2020-02-04     1               40      0                 0
    ## 3148 2020-02-03     5               39      0                 0
    ## 3147 2020-02-02     7               34      0                 0
    ## 3146 2020-02-01    27               27      0                 0

``` r
#remove the data from 2021-10-06 to 2021-10-20
cases <- cases[!(cases$date >= "2021-10-06"),]
#take a look at the variables
str(vaccinations)
```

    ## 'data.frame':    311 obs. of  3 variables:
    ##  $ date : Date, format: "2021-10-20" "2021-10-19" ...
    ##  $ dose1: num  73.5 73.4 73.4 73.3 73.2 73.1 73 72.9 72.9 72.8 ...
    ##  $ dose2: num  60.5 60.4 60.4 60.3 60.2 60.2 60.1 60 59.9 59.9 ...

``` r
summary(vaccinations$dose1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   16.20   53.60   42.30   65.25   73.50

``` r
summary(vaccinations$dose2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00    0.00   39.00   31.47   53.15   60.50

``` r
str(cases)
```

    ## 'data.frame':    613 obs. of  5 variables:
    ##  $ date             : Date, format: "2021-10-05" "2021-10-04" ...
    ##  $ cases            : num  5969 6552 3207 3518 5503 ...
    ##  $ cumulative_cases : num  4546821 4540852 4534300 4531093 4527575 ...
    ##  $ deaths           : num  51 48 50 69 61 53 65 66 75 69 ...
    ##  $ cumulative_deaths: num  70118 70067 70019 69969 69900 ...

``` r
summary(cases$cases)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1    1650    3706    7417    8725   60094

``` r
summary(cases$cumulative_cases)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      27  340258 1457974 2001598 3661841 4546821

``` r
summary(cases$deaths)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0    29.0    71.0   114.4   114.0   707.0

``` r
summary(cases$cumulative_deaths)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    7199   21429   33168   62592   70118

Dimensions, headers, and footers of the two datasets were checked. There
are 311 observations and 3 rows in the “vaccinations” dataset, as well
as 628 observations and 5 rows in the “cases” dataset. Implausible data
(e.g., 0 cases increase) was found in the date variable on “2021-10-20”
in the “cases” dataset. Considering the 14-day incubation period of the
COVID-19 disease, the data from 2021-10-06 to 2021-10-20 were not the
final accurate number of cases and deaths since there are still many
cases and deaths were not reported timely. Thus, these data were removed
from the “cases” dataset.

``` r
#combine the dataset
covid <- merge(vaccinations, cases, by = "date")
```

\#Preliminary Results (provide summary statistics in tabular form and
publication-quality figures, take a look at the kable function from
knitr to write nice tables in Rmarkdown)

\#Conclusion
