PM566 Midterm Project
================
Carmen Jiawen Chen
10/20/2021

**Vaccination rates and COVID-19 cases and deaths in California**

**Introduction**

The vaccinations have begun almost a year since its distribution. The
fully vaccinated percentage in California was increasing and stagnated
around 60%. However, the herd immunity didn’t seem to be achieved and
the COVID-19 daily new cases is still very high in California.
Therefore, this project aims to investigate the relationship of
vaccination rates on COVID-19 daily new cases and deaths in California
by using the data from California Health & Human Services Agency and US
Center for Disease Control and Prevention.

**Methods**

Data of COVID-19 cases and vaccination rates were acquired from
California Health & Human Services Agency and US Center for Disease
Control and Prevention, respectively. Data in California and with
variables of interest (i.e., date, percentage of first dose, percentage
of second dose, daily new cases, cumulative cases, daily new deaths, and
cumulative deaths) were extracted. The date variables were formatted in
both datasets.

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
are 311 observations and 3 variables in the “vaccinations” dataset, as
well as 628 observations and 5 variables in the “cases” dataset.
Implausible data (e.g., 0 cases increase) was found in the date variable
on “2021-10-20” in the “cases” dataset. Considering the 14-day
incubation period of the COVID-19 disease, the data from 2021-10-06 to
2021-10-20 were not the final accurate number of cases and deaths since
there are still many cases and deaths were not reported timely. Thus,
these data were removed from the “cases” dataset.

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

The two datasets were merged into one dataset by date variable. Final
dataset has 296 observations and 7 variables. Exploratory data analysis
was conducted in the merged dataset. No missing value, implaussible
vaule or data error was found. The data includes COVID-19 partial and
fully vaccination rates, daily new cases, cumulative cases, daily new
deaths, as well cumulative deaths from 2020/12/14 to 2021/10/05. Both
univariate and bivariate summary statistics was analyzed. Exploratory
graphs were generated between vaccination rates and cases and deaths.

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

**Preliminary Results**

Table 1 presented the summary statistics of the data, including range of
the date and median (IQR) of first dose, second dose, daily new cases,
cumulative cases, daily new deaths, and cumulative deaths. There are a
total of 296 observations collected from 2020-12-14 to 2021-10-05. The
maximum partial and fully vaccination rates in California are 52% and
64%, respectively. The medians (IQRs) of the daily new cases and deaths
are 4275 (1810, 11520) and 64 (23, 159), respectively.

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

<div id="nprkkpzdix" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#nprkkpzdix .gt_table {
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

#nprkkpzdix .gt_heading {
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

#nprkkpzdix .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#nprkkpzdix .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#nprkkpzdix .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nprkkpzdix .gt_col_headings {
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

#nprkkpzdix .gt_col_heading {
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

#nprkkpzdix .gt_column_spanner_outer {
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

#nprkkpzdix .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nprkkpzdix .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nprkkpzdix .gt_column_spanner {
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

#nprkkpzdix .gt_group_heading {
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

#nprkkpzdix .gt_empty_group_heading {
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

#nprkkpzdix .gt_from_md > :first-child {
  margin-top: 0;
}

#nprkkpzdix .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nprkkpzdix .gt_row {
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

#nprkkpzdix .gt_stub {
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

#nprkkpzdix .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nprkkpzdix .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#nprkkpzdix .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nprkkpzdix .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nprkkpzdix .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nprkkpzdix .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nprkkpzdix .gt_footnotes {
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

#nprkkpzdix .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#nprkkpzdix .gt_sourcenotes {
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

#nprkkpzdix .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#nprkkpzdix .gt_left {
  text-align: left;
}

#nprkkpzdix .gt_center {
  text-align: center;
}

#nprkkpzdix .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nprkkpzdix .gt_font_normal {
  font-weight: normal;
}

#nprkkpzdix .gt_font_bold {
  font-weight: bold;
}

#nprkkpzdix .gt_font_italic {
  font-style: italic;
}

#nprkkpzdix .gt_super {
  font-size: 65%;
}

#nprkkpzdix .gt_footnote_marks {
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

Table 2 presented the correlation coefficients of vaccination rates with
daily new cases and deaths. Vaccination rates were negatively associated
with daily new cases and deaths. Such negative association was stronger
in the correlation between vaccination rates and daily new deaths (R:
-0.78 for first dose, -0.68 for second dose).

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
  knitr::kable(caption = "<strong>Table 2. Correlation coefficients of vacciantion rates and cases/deaths</strong>")
```

|        | First dose | Second dose |
|:-------|-----------:|------------:|
| Cases  | -0.4990338 |  -0.3931054 |
| Deaths | -0.7814445 |  -0.6816794 |

<strong>Table 2. Correlation coefficients of vacciantion rates and
cases/deaths</strong>

Exploratory graphs were presented in figure 1 and figure 2a-2e.
Vaccinations in California started in February and the rates continued
to increase. The increase of vaccination rates became more slowly when
it achieved around 60% of first dose vaccination rate. The daily new
cases started to decrease drastically around February and the cases
remained in a stable small number until July. A small break out in daily
new cases occurred in July and it achieved its peak in September. The
trend pattern of the daily new deaths is similar to daily new cases.

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

Data visualization graphs were generated in Figure 3a & 3b to visualize
the relationship between vaccination rates and daily new cases and
deaths. There was an non-symmetrical inverse pattern in the relationship
of vaccination rates and daily new cases in the beginning. Daily new
cases was negatively associated with vaccination rates until the first
dose vaccination rates achieved around 50%. After that, daily new cases
increase drastically with the increase of vaccination rates. This same
pattern was also found in the relationship of vaccination rates and
daily new deaths. However, a stronger negative association was observed
in the daily new deaths figure in the beginning of the data.

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

![](index_files/figure-gfm/data%20visualization-2.png)<!-- -->

**Conclusion**

There is a positive association of vaccination rates and daily new cases
and deaths when the first dose of vaccination rate achieved around 50%.
This may be due to the re-opening of the economic and lift of mask
mandate during that time. Overall, we could see the protective effect of
vaccine towards infection and death according to the data in the
beginning. A stronger negative association in the beginning of
vaccination rates and daily new deaths compared to daily new cases may
be due to a stronger efficacy of the vaccine towards preventing
mortality.
