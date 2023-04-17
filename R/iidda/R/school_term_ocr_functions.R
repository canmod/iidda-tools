# OCR Extraction of Canadian School Term Data from PDF Files
#
# @param pdf_image result of image_read on character string pointing
# to an EdCan PDF file containing school term data
# @param page integer giving the page number of the PDF file containing
# a school term data table
# @param trim_top number of pixels at the top of the page to trim off
# @param trim_bottom number of pixels at the bottom of the page to trim off
# @param edge_geometry parameters to pass to the imagemagick edge detector
# @noRd
# @importFrom magick image_threshold image_transparent image_negate image_flatten image_canny image_hough_txt
# @export

school_term_ocr = function(pdf_image, page,
                           trim_top = 570, trim_bottom = 330,
                           edge_geometry = "10x10+20%+50%") {

  print("finding the table ... ")
  focal_page = find_table(pdf_image, page, trim_top, trim_bottom)

  print("detecting the tabular grid containing data ... ")
  locations = edge_detect_school_term_grid(focal_page, geometry = edge_geometry)

  print("ocr school term data and filling it into the grid ... ")
  term_matrix = ocr_school_term_text(focal_page, locations)

  print("preping for output ... ")
  as.data.frame(term_matrix) %>%
    setNames(c("board", "fall_opening", "winter_closing",
               "post_winter_opening", "spring_closing",
               "post_spring_opening", "summer_closing", "enrollment")) %>%
    filter(board != '') %>%
    mutate(province = ifelse(board == toupper(board), board, NA)) %>%
    relocate(province, .before = board) %>%
    mutate(board = ifelse(board != toupper(board), board, NA))
}

find_table = function(pdf_image, page,
                      trim_top = 570,
                      trim_bottom = 330) {
  # Read in the table and clean it up a bit
  focal_dims = dim(image_data(pdf_image[page]))
  pdf_image[page] %>%
    image_crop(
      geometry_area(
        focal_dims[2],
        focal_dims[3]-trim_top-trim_bottom,
        0,
        trim_top)) %>%
    image_trim() %>%
    image_quantize(colorspace = 'gray')
}

# @importFrom dplyr mutate_all
# @importFrom tidyr separate
# @noRd
edge_detect_school_term_grid = function(focal_page, geometry = "10x10+20%+50%") {
  # Prep the image for fitting the rows and columns model
  focal_page_for_lines =
    focal_page %>%
    image_threshold(threshold = "70%", type = 'white') %>%
    image_transparent('black') %>%
    image_transparent('white') %>%
    image_negate() %>%
    image_threshold(threshold = "90%", type = 'black') %>%
    image_flatten()

  # Fit rows and columns model
  focal_grid = image_canny(focal_page_for_lines, geometry = geometry)
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
  return(list(row = row_locations, col = column_locations))
}

# @importFrom magick image_data image_crop geometry_area image_trim image_quantize

# @noRd
ocr_school_term_text = function(focal_page, locations) {
  # Prep the image for fitting the ocr models
  focal_page_for_text =
    focal_page %>%
    image_threshold(threshold = "60%", type = 'white') %>%
    image_flatten()

  # Loop over the rows and columns, fitting one ocr model to each cell
  output = matrix('', length(locations$row) - 1, length(locations$col) - 1)
  for(rr in 1:(length(locations$row)-1)){
    for(cc in 1:(length(locations$col)-1)){
      pp =
        focal_page_for_text %>%
        image_crop(geometry = geometry_area(
          diff(locations$col)[cc],
          diff(locations$row)[rr],
          locations$col[cc],
          locations$row[rr]))

      output[rr, cc] =
        pp %>%
        image_ocr_data(language = 'eng') %>%
        getElement('word') %>%
        paste(collapse = ' ') %>%
        gsub(pattern = "[^[:alnum:] ,/]", replacement = "")
    }
  }
  return(output)
}
