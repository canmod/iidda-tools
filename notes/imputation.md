# Data Imputation, Time-Scale Cross-Checks, and Time-Series Resolution

These three topics are inextricably linked.

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
