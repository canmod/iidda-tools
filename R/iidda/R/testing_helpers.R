compare_columns = function(table1, table2) {
  in_1_not_2 = names(table1)[!(names(table1) %in% names(table2))]
  if (length(in_1_not_2) != 0L) {
    stop("\nthe following column names are in table 1 but not table 2:\n",
         in_1_not_2)
  }
  in_2_not_1 = names(table2)[!(names(table2) %in% names(table1))]
  if (length(in_2_not_1) != 0L) {
    stop("\nthe following column names are in table 2 but not table 1:\n",
         in_2_not_1)
  }
  .trash = lapply(names(table1), function(nm) {
    test_result = all.equal(table1[[nm]], table2[[nm]])
    if (!isTRUE(test_result)) {
      stop("\ncolumn ", nm, " is not the same in both tables:\n",
           test_result)
    }
  })
  if (!isTRUE(all.equal(names(table1), names(table2)))) {
    stop("columns are aparently not in the same order in both tables")
  }
  TRUE
}


compare_with_schema = function(table, metadata) {
  in_table_not_schema = names(table)[!(names(table) %in% metadata$Columns)]
  if(length(in_table_not_schema) != 0L) {
    stop("\nthe following column names are in the table but not the schema:\n",
         in_table_not_schema)
  }
  in_schema_not_table = metadata$Columns[!(metadata$Columns %in% names(table))]
  if(length(in_schema_not_table) != 0L) {
    stop("\nthe following columns in the schema are in the table:\n",
         in_schema_not_table)
  }

  # NOT DONE

  # type_checking_functions = list(
  #   string_default = is.character,
  #   date_ISO8601 = function(x) inherits(x, 'Date'),
  #   integer_default = is.integer
  # )[paste(metadata$Type, metadata$Format, sep = "_")]
  #
  # sapply(seq_along(type_checking_functions), function(i) {
  #   type_checking_functions[[i]](table[[i]])
  # })
  #
  # .trash = lapply(names(table1), function(nm) {
  #   test_result = all.equal(table1[[nm]], table2[[nm]])
  #   if(!isTRUE(test_result)) {
  #     stop("\ncolumn ", nm, " is not the same in both tables:\n",
  #          test_result)
  #   }
  # })
  # if(!isTRUE(all.equal(names(table1), names(table2)))) {
  #   stop("columns are aparently not in the same order in both tables")
  # }
  # TRUE
}
