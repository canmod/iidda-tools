library(curl)
library(iidda)
library(dplyr)
library(magick)
library(tidyr)
library(image.LineSegmentDetector)

setwd('~/Development/iidda/data/school-terms-canada/source-data/')

# OCR from original data sources from edcan (formerly candian education association)
# Obtained from https://www.edcan.ca/2019-2020-school-calendar/
pdf_urls = c(
  "https://www.edcan.ca/wp-content/uploads/EdCanNet_2018-2019-School-Calendar_v1.pdf",
  "https://www.edcan.ca/wp-content/uploads/2017-2018-School-Calendar_v8.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2016-2017-school-calendar_v7.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2015-2016-school-calendar_rev6.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2014-2015-school-calendar_rev4.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2013-2014-school-calendar-rev3.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2012-2013-school-calendar.pdf",
  "http://www.edcan.ca/wp-content/uploads/cea-2011-2012-school-calendar.pdf")

# fixme -- put these somewhere better
pdf_files =
  strsplit(pdf_urls, '/') %>%
  lapply(tail, 1) %>%
  simplify2array()
pdf_files = file.path(getwd(), pdf_files)

# write to iidda
#mapply(curl_download, pdf_urls, pdf_files)



output = yearly_output = list()

file = 3
pdf_image = image_read(pdf_files[file], density = 300)
output[[file]] = list()

for(page in 3:8) {
  print(page)
  output[[file]][[page]] = try(school_term_ocr(pdf_image, page))
}

yearly_output[[file]] =
  Negate(function(x){class(x) == 'try-error'}) %>%
  Filter(output[[file]]) %>%
  dplyr::bind_rows() %>%
  mutate(province = zoo::na.locf(province))

View(yearly_output[[file]])
system(paste0("open ", pdf_files[[file]]))
