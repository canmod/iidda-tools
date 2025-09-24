msg = function(...) {
  (c(...)
    |> paste(collapse = " ")
    |> strwrap(prefix = "\n", initial = "")
    |> append("\n\n", after = 0)
  )
}
