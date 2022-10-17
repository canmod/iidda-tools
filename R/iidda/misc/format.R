setwd('~/Development/iidda-pre-github/idda/data/')
library(iidda)
library(readxl)
library(zoo)
library(lubridate)
library(dplyr)
library(tidyxl)
library(unpivotr)
library(curl)
library(tabulizer)
library(pdftools)
library(tesseract)
library(magick)
library(tidyr)

# Demography -----------------

f_demo = function(dat) {
  read_xls(
    dat,
    skip = 2,
    col_types = c("numeric", "text", "numeric", "skip", "numeric", "skip",
                  "numeric", "skip", "numeric", "skip", "numeric", "skip",
                  "numeric", "skip", "numeric", "skip", "numeric", "skip",
                  "numeric", "skip", "numeric", "skip", "numeric", "skip",
                  "numeric", "skip")) %>%
  mutate(
    Year = na.locf(Year),
    Date = ym(paste(Year, match(Month, month.abb), sep = '-'))) %>%
  relocate(Date, .before = Year)
}
f_demo('./demography/ca/bth_ca___1921-65_mn_pr.xls')

f_ca_pop = function(dat) {
  read.csv(
    dat,
    skip = 6,
    col.names = c("Year", "Population_Estimate_1", "Population_Estimate_2"))[1:134,]

}
f_ca_pop('./demography/ca/pop_ca___1867-2000_yr.csv')


# School Terms -----------------


# Format old xls sheet to data frame
xlsx_cells('../../../data_work/data/ontario/cdi_ca_on__1939_wk__moh.xlsx') %>%
  filter(col > 4) %>%
  filter(sheet == "Ontario 1939") %>%
  behead('right', 'Month')

# manually (by 'save as' in Excel) convert xls to xlsx so that xlsx_cells can be used
 d = xlsx_cells('schoolterm/ca/schoolcal_ca___1950-2002_yr_pr.xlsx')
# retain the comments, which is a great thing for this dataset
d$comment %>% unique
d %>%
  filter(!is.na(character) & (row == 1)) %>%







#system(paste0('open ', pdf_files[1]))
#tabulizer::extract_tables(pdf_files[1])
#xx1 = pdftools::pdf_convert(pdf_files[1])
#im = image_read(xx1[4])

tesseract(options = list(user_words_file = 'tssfile'))

# is it smart to read them all in to the same R Session? no ... should read
# them one at a time

# Extract a single file and a single table

i = 1; j = 3 # file #1 and page #4
pdf_images = vector('list', length(pdf_files))
pdf_images[[1]] = image_read(pdf_files[i], density = 300)

output = school_term_ocr(pdf_files[1], 6)
View(output)

school_term_ocr = function(pdf_file, page) {

  # Read in the table and clean it up a bit
  pdf_image = image_read(pdf_file, density = 300)
  focal_dims = dim(image_data(pdf_image[page]))
  focal_page =
    pdf_image[page] %>%
    image_crop(geometry_area(focal_dims[2], focal_dims[3]-900, 0, 570)) %>%
    image_trim() %>%
    image_quantize(colorspace = 'gray')
  focal_page


  # Prep the image for fitting the rows and columns model
  focal_page_for_lines =
    focal_page %>%
    image_threshold(threshold = "70%", type = 'white') %>%
    image_transparent('black') %>%
    image_transparent('white') %>%
    image_negate() %>%
    image_threshold(threshold = "90%", type = 'black') %>%
    image_flatten()
  focal_page_for_lines

  # Fit rows and columns model
  focal_grid = image_canny(focal_page_for_lines)
  focal_lines =
    focal_grid %>%
    image_hough_txt() %>%
    strsplit('\n') %>%
    getElement(1L)
  focal_lines_data =
    data.frame(
      str = {focal_lines[startsWith(focal_lines, 'line')] %>%
          gsub(pattern = "line ", replacement = "")}) %>%
    separate(col = str, sep = '  # ', into = c('xy', 'meta')) %>%
    separate(col = xy, sep = ' ', into = c('xy1', 'xy2')) %>%
    separate(col = xy1, sep = ',', into = c('x1', 'y1')) %>%
    separate(col = xy2, sep = ',', into = c('x2', 'y2')) %>%
    separate(col = meta, sep = ' ', into = c('count', 'angle', 'distance')) %>%
    mutate_all(as.numeric)
  column_locations =
    focal_lines_data %>%
    filter(angle == 0) %>%
    select(x1) %>%
    getElement('x1') %>%
    sort()
  column_locations = column_locations[diff(column_locations) > 50]
  column_locations[1] = 0
  row_locations =
    focal_lines_data %>%
    filter(angle == 90) %>%
    select(y1) %>%
    getElement('y1') %>%
    sort()
  row_locations[1] = 0

  # Prep the image for fitting the ocr models
  focal_page_for_text =
    focal_page %>%
    image_threshold(threshold = "60%", type = 'white') %>%
    image_flatten()
  focal_page_for_text


  output = matrix('', length(row_locations) - 1, length(column_locations) - 1)
  # Loop over the rows and columns, fitting one ocr model to each cell
  for(rr in 1:(length(row_locations)-1)){
    print(rr)
    for(cc in 1:(length(column_locations)-1)){
      #print(cc)
      pp =
        focal_page_for_text %>%
        image_crop(geometry = geometry_area(
          diff(column_locations)[cc],
          diff(row_locations)[rr],
          column_locations[cc],
          row_locations[rr]))
      #print(pp)
      output[rr, cc] =
        pp %>%
        image_ocr_data(language = 'eng') %>%
        getElement('word') %>%
        paste(collapse = ' ') %>%
        gsub(pattern = "[^[:alnum:] ,/]", replacement = "")

      #print(output[rr, cc])
      #readline(prompt="Press [enter] to continue")
    }
  }
  as.data.frame(output) %>%
    setNames(c("board", "fall_opening", "winter_closing", "post_winter_opening", "spring_closing", "post_spring_opening", "summer_closing", "enrollment")) %>%
    filter(board != '') %>%
    mutate(province = ifelse(board == toupper(board), board, NA)) %>%
    relocate(province, .before = board) %>%
    mutate(board = ifelse(board != toupper(board), board, NA))
}
