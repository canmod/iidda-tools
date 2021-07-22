#' https://stackoverflow.com/questions/43252489/read-excel-with-two-line-headers-in-r#43253853
#' https://cran.r-project.org/web/packages/TableToLongForm/index.html
#' https://readxl.tidyverse.org/articles/articles/multiple-header-rows.html
#' https://alison.rbind.io/blog/2018-07-multiple-headers/
#' https://paul.rbind.io/2019/02/01/tidying-multi-header-excel-data-with-r/
#' https://webbedfeet.netlify.app/post/tidying-messy-excel-data-tidyxl/

#xlsx_cells('../../../data_work/data/ontario/cdi_ca_on__1939_wk__moh.xlsx') %>%
#  filter(col > 4) %>%
#  filter(sheet == "Ontario 1939") %>%
#  behead('right', 'Month')

# manually (by 'save as' in Excel) convert xls to xlsx so that xlsx_cells can be used
#d = xlsx_cells('schoolterm/ca/schoolcal_ca___1950-2002_yr_pr.xlsx')
# retain the comments, which is a great thing for this dataset
#d$comment %>% unique
#d %>%
#  filter(!is.na(character) & (row == 1)) %>%



