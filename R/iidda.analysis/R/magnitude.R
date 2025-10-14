
mag_decomp = function(x, digits = 0) {
  y = as.numeric(x)
  cut_points = c(-Inf, ` ` = 1e+03, K = 1e+06, M = 1e+09, B = 1e+12)
  mag_points = c(K = 1e+03, M = 1e+06, B = 1e+09)
  mags = as.character(cut(y, cut_points, names(cut_points)[-1], right = FALSE))
  if (any(is.na(mags))) {
    msg(
        "At least some of these numbers are too high for this kind of"
      , "plot that requires printing numbers using magnitudes in plots"
      , "(e.g., 5K instead of 5000)."
    ) |> stop()
  }
  big = mags %in% names(mag_points)
  y[big] = y[big] / mag_points[mags[big]]
  y = round(y, digits)
  return(list(y = y, mags = mags))
}

#' @export
magnitude = function(x, digits = 0) {
  decomp = mag_decomp(x, digits)
  y = decomp$y
  mags = decomp$mags
  rnds = formatC(y
    , width = 3 + digits
    , digits = digits
    , format = "f"
    , drop0trailing = FALSE
  )
  z = sprintf("%s%s", rnds, mags)
  z
}
