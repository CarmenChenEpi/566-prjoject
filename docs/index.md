PM566 Midterm Project
================
Carmen Jiawen Chen
10/20/2021

**Vaccination rates and COVID-19 cases and deaths in California**

**Introduction** (provide background on your dataset and formulated
question)

Vaccination rate and COVID-19 incidence cases and deaths increase in
California.

**Methods** (include how and where the data were acquired, how you
cleaned and wrangled the data, what tools you used for data exploration)

Data of COVID-19 cases and vaccination rates were acquired from
California Health & Human Services Agency and US Center for Disease
Control and Prevention, respectively. Data

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
#check the date
summary(vaccinations$date)
summary(cases$date)
#check for missing value
cases[!complete.cases(cases),]
vaccinations[!complete.cases(vaccinations),]
#remove rows with missing value
cases <- cases[complete.cases(cases),]
#check the dimensions, headers, footers
dim(vaccinations)
dim(cases)
head(vaccinations)
cases <- cases[order(cases$date, decreasing = TRUE),]
head(cases)
tail(vaccinations)
tail(cases)
#remove the data from 2021-10-06 to 2021-10-20
cases <- cases[!(cases$date >= "2021-10-06"),]
#take a look at the variables
str(vaccinations)
summary(vaccinations$dose1)
summary(vaccinations$dose2)
str(cases)
summary(cases$cases)
summary(cases$cumulative_cases)
summary(cases$deaths)
summary(cases$cumulative_deaths)
```

The two datasets were merged into one dataset by date. Final dataset has
296 observations (i.e. rows) and 7 variables (i.e., columns).
Exploratory data analysis was conducted in the merged dataset. No
missing value, implaussible vaule or data error was found. The data
includes COVID-19 partial and fully vaccination rates, daily new cases,
cumulative cases, daily new deaths, as well cumulative deaths from
2020/12/14 to 2021/10/05. Both univariate and bivariate summary
statistics was analyzed. Exploratory graphs were generated between
vaccination rates and cases and deaths.

``` r
#combine the dataset
covid <- merge(vaccinations, cases, by = "date")
#exploratory analysis
dim(covid)
head(covid)
tail(covid)
str(covid)
summary(covid$date)
summary(covid$dose1)
summary(covid$dose2)
summary(covid$cases)
summary(covid$cumulative_cases)
summary(covid$deaths)
summary(covid$cumulative_cases)
covid[!complete.cases(covid),]
```

**Preliminary Results** (provide summary statistics in tabular form and
publication-quality figures, take a look at the kable function from
knitr to write nice tables in Rmarkdown)

``` r
#Table 1. Characteristics of the COVID-19 data
covid %>%
  select("Date" = date,
         "First dose, %" = dose1,
         "Second dose, %" = dose2,
         "Daily new cases" = cases,
         "Cumulative cases" = cumulative_cases,
         "Daily new deaths" = deaths,
         "Cumulative deaths" = cumulative_deaths) %>%
  tbl_summary() %>%
  modify_caption ("**Table 1. Characteristics of the COVID-19 data**")
```

<div id="qkfkrvaifi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qkfkrvaifi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qkfkrvaifi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qkfkrvaifi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qkfkrvaifi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qkfkrvaifi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qkfkrvaifi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qkfkrvaifi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qkfkrvaifi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qkfkrvaifi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qkfkrvaifi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qkfkrvaifi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qkfkrvaifi .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qkfkrvaifi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qkfkrvaifi .gt_from_md > :first-child {
  margin-top: 0;
}

#qkfkrvaifi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qkfkrvaifi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qkfkrvaifi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qkfkrvaifi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qkfkrvaifi .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qkfkrvaifi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qkfkrvaifi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qkfkrvaifi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qkfkrvaifi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qkfkrvaifi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qkfkrvaifi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qkfkrvaifi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qkfkrvaifi .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qkfkrvaifi .gt_left {
  text-align: left;
}

#qkfkrvaifi .gt_center {
  text-align: center;
}

#qkfkrvaifi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qkfkrvaifi .gt_font_normal {
  font-weight: normal;
}

#qkfkrvaifi .gt_font_bold {
  font-weight: bold;
}

#qkfkrvaifi .gt_font_italic {
  font-style: italic;
}

#qkfkrvaifi .gt_super {
  font-size: 65%;
}

#qkfkrvaifi .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <caption><strong>Table 1. Characteristics of the COVID-19 data</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N = 296</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Date</td>
<td class="gt_row gt_center">2020-12-14 to 2021-10-05</td></tr>
    <tr><td class="gt_row gt_left">First dose, %</td>
<td class="gt_row gt_center">52 (15, 64)</td></tr>
    <tr><td class="gt_row gt_left">Second dose, %</td>
<td class="gt_row gt_center">36 (0, 52)</td></tr>
    <tr><td class="gt_row gt_left">Daily new cases</td>
<td class="gt_row gt_center">4,275 (1,810, 11,520)</td></tr>
    <tr><td class="gt_row gt_left">Cumulative cases</td>
<td class="gt_row gt_center">3,669,089 (3,512,726, 3,833,183)</td></tr>
    <tr><td class="gt_row gt_left">Daily new deaths</td>
<td class="gt_row gt_center">64 (23, 159)</td></tr>
    <tr><td class="gt_row gt_left">Cumulative deaths</td>
<td class="gt_row gt_center">62,710 (58,891, 63,905)</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="2">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Range; Median (IQR)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

``` r
#First dose correlation
Cases = cor(covid$dose1, covid$cases, use = "complete")
Deaths = cor(covid$dose1, covid$deaths, use = "complete")
`First dose` = rbind(Cases, Deaths) 
colnames(`First dose`) <- "First dose"
#Second dose correlation
Cases = cor(covid$dose2, covid$cases, use = "complete")
Deaths = cor(covid$dose2, covid$deaths, use = "complete")
`Second dose` = rbind(Cases, Deaths) 
colnames(`Second dose`) <- "Second dose"
#Combine the table
cbind(`First dose`, `Second dose`) %>%
kable(caption = "<b>Table 2. Correlation coefficients of vacciantion rates and cases/deaths<b>")
```

<table>
<caption>
<b>Table 2. Correlation coefficients of vacciantion rates and
cases/deaths<b>
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
First dose
</th>
<th style="text-align:right;">
Second dose
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Cases
</td>
<td style="text-align:right;">
-0.4990338
</td>
<td style="text-align:right;">
-0.3931054
</td>
</tr>
<tr>
<td style="text-align:left;">
Deaths
</td>
<td style="text-align:right;">
-0.7814445
</td>
<td style="text-align:right;">
-0.6816794
</td>
</tr>
</tbody>
</table>

``` r
#Vaccination rates
ggplot(data = covid) +
  geom_point(mapping = aes(x = date, y = dose1, color = "First dose")) +
  geom_point(mapping = aes(x = date, y = dose2, color = "Second dose")) +
  labs(title = "Figure 1. Vaccination rates from 2020-12-14 to 2021-10-05") +
  labs(x = "Date", y = "Vaccination rates")
```

![](index_files/figure-gfm/exploratory%20graphs-1.png)<!-- -->

``` r
#Cases
ggplot(data = covid) +
  geom_point(mapping = aes(x = date, y = cases)) +
  labs(title = "Figure 2a. Daily new cases from 2020-12-14 to 2021-10-05") +
  labs(x = "Date", y = "Daily new cases")
```

![](index_files/figure-gfm/exploratory%20graphs-2.png)<!-- -->

``` r
#Deaths
ggplot(data = covid) +
  geom_point(mapping = aes(x = date, y = deaths)) +
  labs(title = "Figure 2b. Daily new deaths from 2020-12-14 to 2021-10-05") +
  labs(x = "Date", y = "Daily new deaths")
```

![](index_files/figure-gfm/exploratory%20graphs-3.png)<!-- -->

``` r
#Cumulative cases
ggplot(data = covid) +
  geom_point(mapping = aes(x = date, y = cumulative_cases)) +
  labs(title = "Figure 2c. Cumulative cases from 2020-12-14 to 2021-10-05") +
  labs(x = "Date", y = "Cumulative cases")
```

![](index_files/figure-gfm/exploratory%20graphs-4.png)<!-- -->

``` r
#Cumulative deaths
ggplot(data = covid) +
  geom_point(mapping = aes(x = date, y = cumulative_deaths)) +
  labs(title = "Figure 2e. Cumulative deaths from 2020-12-14 to 2021-10-05") +
  labs(x = "Date", y = "Cumulative deaths")
```

![](index_files/figure-gfm/exploratory%20graphs-5.png)<!-- -->

``` r
#Vaccination rates and cases
covid[covid$dose1 > 0,] %>%
ggplot() +
  geom_point(mapping = aes(x = dose1, y = cases, color = "First dose")) +
  geom_smooth(mapping = aes(x = dose1, y = cases)) +
  geom_point(mapping = aes(x = dose2, y = cases, color = "Second dose")) +
  geom_smooth(mapping = aes(x = dose2, y = cases)) +
  labs(title = "Figure 3a. Vaccination rates and cases") +
  labs(x = "Vaccination rates", y = "Daily new cases")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](index_files/figure-gfm/data%20visualization-1.png)<!-- -->

``` r
#Vaccination rates and deaths
covid[covid$dose1 > 0,] %>%
ggplot() +
  geom_point(mapping = aes(x = dose1, y = deaths, color = "First dose")) +
  geom_smooth(mapping = aes(x = dose1, y = deaths)) +
  geom_point(mapping = aes(x = dose2, y = deaths, color = "Second dose")) +
  geom_smooth(mapping = aes(x = dose2, y = deaths)) +
  labs(title = "Figure 3b. Vaccination rates and deaths") +
  labs(x = "Vaccination rates", y = "Daily new deaths")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](index_files/figure-gfm/data%20visualization-2.png)<!-- -->

\#Conclusion
