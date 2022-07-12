# Check if weekly data is 'perfect' or partially missing.
wk_type = function(data){
  (data
   %>% split_data
   %>% .$wk_province
   %>% mutate(cases_this_period=ifelse(is.na(cases_this_period)==TRUE, "0", cases_this_period))

   # for each period_start_date, if any cases_this_period are unclear, Not available, or not reportable,
   # then type = subsetted. otherwise type = perfect.

   #for (date in x$period_start_date) {

     # go through

   #}

   %>% group_by(period_start_date)
   %>% mutate(type = ifelse(cases_this_period=="unclear" | cases_this_period=="Not available" | cases_this_period == "Not reportable", "subsetted", "perfect"))
   %>% ungroup()
   )
}

## Getting Monthly Data from 'Perfect' Weekly Data. This will ideally be a function that takes tidy_data as its argument.
mt_from_wks = function(wk_type){
  (wk_type
   %>% filter(type == "perfect")
   %>% mutate(mt_yr = format(period_end_date, "%b%Y"))
   %>% group_by(mt_yr, iso_3166_2)
   %>% mutate(case_sum = sum(as.numeric(cases_this_period), na.rm=TRUE))
   %>% ungroup()
   %>% distinct(iso_3166_2, case_sum, mt_yr, .keep_all = TRUE)
  )
}

## Getting Monthly Data from a Subset of the Weeks

# If not all weekly data exists (unclear, not available, or not reportable)
# calculate number of weeks in said month, w, and the number of weeks with data, m, and the total count in the available weeks, x.
# the imputed monthly count is then count = w*x/m
# what if this doesn't give integers for case numbers?

mt_from_subsetwk = function(wk_type){
  (wk_type
   %>% filter(type == "subsetted")
   %>% group_by()
   )

}

if(FALSE) {
test = (diphtheria
        %>% split_data()
        %>% .$wk_province
        %>% mutate(cases_this_period=ifelse(is.na(cases_this_period)==TRUE, "0", cases_this_period))

        #Shouldn't do these steps because these cases will go in the next function for subsetted wkly data
        %>% filter(cases_this_period != "unclear" & cases_this_period != "Not available" & cases_this_period != "Not reportable")

        %>% mutate(mt_yr = format(period_end_date, "%b%Y"))
        %>% group_by(mt_yr, iso_3166_2)
        %>% mutate(case_sum = sum(as.numeric(cases_this_period), na.rm=TRUE))
        %>% ungroup()

        %>% distinct(iso_3166_2, case_sum, mt_yr, .keep_all = TRUE)

        #Change period_end_date to month end date? or just keep mt_yr abbreviation
)
}
