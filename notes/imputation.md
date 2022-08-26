# Missing Values, Data Imputation, Time-Scale Cross-Checks, and Time-Series Resolution

These three topics are inextricably linked.

## Terms

* Missing values -- a value that we do not have data on (i.e. not a zero or any other numeric value)
* Time-series resolution -- time interval that each time-step in a time series represents (e.g. weekly, 4-weekly, monthly)
* Imputation -- filling in missing values in a time series so that there is one observation at every step
* Time-scale cross-checks -- checking that recorded totals for one time scale (e.g. monthly) match computed totals

## Missing Values

If we believe that a certain symbol or data entry marking/technique indicates a genuine zero for a particular period, then we should change that symbol/marking to a zero in the _harmonization step_

Task: function to replace missing values with zeros when appropriate -- such that the strategy for deciding appropriateness is easily readable in the code, with a view towards allowing the user to adjust the strategy.

There may be different kinds of missing values -- not reportable versus not available -- what do these mean??

Another issue is missing at random versus missing for some reason. The former is typically easier to impute, and the latter people often want to keep as missing because they could contain information.

Sometimes missing values show up as a missing record (i.e. there is no record in the dataset for a particular week) and sometimes it is NA, sometimes empty string, sometimes special string (e.g. "not available").

## Time-Series Resolution

Goal -- one CSV file for each of the following resolutions for all diseases and years from 1924-2000:

  * weekly (1924-1978)
  * 4-weekly (1924-1989)
  * 13-weekly (1924-2000)
  * 52-weekly (1924-2000)
  * monthly (1924-1992)
  * quarterly (1924-2000)
  * yearly (1924-2000)

The first four of these can be addressed with the following harmonization procedure:

  * Define a single week as the starting week for the entire project
  * For each disease and location create a dataset with the following fields:
      * `period_start_date`
      * `period_end_date`
      * `cases`
      * `information_source_type` -- `recorded`, `imputed_from_monthly`, ...
  * Aggregate each of these datasets to the desired resolution by doing the the following:
      * `period_start_date = min(period_start_date)`
      * `period_end_date = max(period_end_date)`
      * `cases = sum(cases)`
      * `information_quality = mean(information_source_type %in% c('recorded', 'imputed_from_monthly')` -- or some sensible definition of quality

The last three (monthly, quarterly, and yearly) are not so easy, but could be handled with a more refined procedure that deals with weeks that are in two months/quarters/years and divides up the weekly totals in proportion to the number of days in the focal month/quarter/year.

When do we leave values missing and when do we impute?  At least do not impute values at the beginning and end of the series.


## Imputation

Types of imputation:

* Summing values at finer resolutions if all finer-scale values are observed (e.g. for a 4-weekly period we have no data, but we do have data on all four of those weeks so we can sum them up)



## Data Harmonization Issues

There are many different ways that cases are added up in the original scans. For the 1924-55 scans, there are weekly and monthly case counts.
These weekly and monthly counts are then totaled in three different ways. Monthly and weekly counts are summed over the year for each province, over each week for all of Canada, and also over the year for all of Canada.

When there are missing weekly reports, there may still be monthly reports, causing discrepancies in the different totals. There are also many instances in this data set where the only one of the yearly Canada totals is present (sum of weekly counts and no sum of monthly counts, or vice-versa). This raises the question of which total should be plotted. 

It seems that we need to figure out imputation methods in order to get accurate Canada - and provincial - totals to plot. 
There don't seem to be such obvious discrepancies for data from 1956 and onward.

## Getting Monthly Data from 'Perfect' Weekly Data

This is easy.  Simply sum up the weekly numbers.

## Getting Monthly Data from a Subset of the Weeks

This is a little harder, but not that bad. There are a few possible approaches.

First let's ignore the fact that most months have a little more than four weeks, and just say that all months have four weeks exactly. Now let the number of weeks with data in the focal month be $m$ and the total count in the available weeks be $x$.  Then the imputed monthly count is $y = 4*x/m$.

Can we do better by accounting for the few extra days that makes 4-weekly data different from monthly data?

## Getting Monthly Data from (possibly subsetted) Weekly and Monthly Data

Why is interesting? Because the weekly and monthly data could logically disagree (e.g. the sum of the weeks is higher than the months).  This not only allows us to identify errors, but it also allows us to average the estimates from the two sources -- monthly and weekly. These averages could be 'smart' by returning NA if the difference between them is larger than a threshold say. The average should also be rounded, and maybe we should use the median -- I don't know.

## Getting Weekly Data from Monthly Data when all Weeks are Missing

Hard but not impossible.  Naively we could pretend like all weeks have the same counts.  So let $y$ be the monthly count, then the weekly count is just $x = y / 4$.

This has big problems including the unrealistic elimination of variation, and the problem that monthly data are not 4-weekly.

The variation problem could be solved by using the variation in months with full weekly and monthly data, to simulate realistic variation but this is certainly harder. Ultimately there is no free lunch here, but there are techniques and trade-offs.

## Getting Weekly Data from Monthly Data when some Weeks are Missing

## Getting Weekly Data from Monthly Data when no Weeks are Missing

This is related to the data-checking-while-imputing situation above.

## Monthly vs 4-Weekly Considerations

Firstly, sometimes we are in the better 4-weekly case so that is good.

But even with monthly data things are not that bad for the 1924-55 data, because the original data define the monthly totals as the sum over a set of full 7-day weeks for most years. This makes imputation much easier, at least conceptually. But it creates a problem for the monthly data, because the months that have been based on 3 weeks say are less comparable with those based on 4 or 5 weeks. Gabrielle's great idea is that we use the monthly data entered in the original sheets for imputation and cross checking purposes, but conduct data analysis on weekly and/or 4-weekly scales. This will still require us to impute data for particular 4-weekly periods, which could get a little more interesting because the 4-weekly periods will necessarily get out of sync with the monthly periods.

## Imputation using Temporal Patterns

It is possible to fit a model of temporal variation in the counts and then use this model to interpolate between non-missing time-points.

## Averaging Imputation Estimates from Different Approaches

If it is possible to use both within-month-based and temporal-pattern-based approaches, it is straightforward to average those estimates and averaging estimates can often have good properties.

## Placing Uncertainty Estimates on the Imputed Values

Ultimately there is no free lunch, and imputed values are just not as reliable as observed values. More sophisticated statical approaches will be able to utilize estimates of imputation uncertainty, and so we should think about how to generate these uncertainty estimates. But doing so requires more principled statistical frameworks than just averaging and multiplying by factors that seem to make intuitive sense.
