comment_lines = c(
    "# Automatically generated -- do not edit by hand."
  , "# To update the makefiles in an IIDDA repository,"
  , "# run `iidda::update_makefiles()` from the repository"
  , "# root. See ?iidda::update_makefiles for information"
  , "# on how these makefiles are produced."
)

#' @export
dependency_file = function(source, dataset) {
  deps_abs_path = get_all_dependencies(source, dataset)
  deps_rel_path = relative_paths(deps_abs_path)
  deps = paste(deps_rel_path, collapse = " ")
  targ = sprintf("derived-data/%s/%s/%s.csv", source, dataset, dataset)
  dot_d = sprintf("derived-data/%s/%s/%s.d", source, dataset, dataset)
  body = sprintf("%s : %s", targ, deps)
  writeLines(body, dot_d)
  return(dot_d)
}

#' Update all Makefiles
#'
#' Run this after updating CSV files in `pipelines/{DATA-SOURCE}/tracking`
#' folders and/or in `global-metadata/tracking`. To update the makefile for
#' a single dataset please use \code{\link{make_makefile}}.
#'
#' @param dataset_makefiles Should makefiles for each dataset be constructed
#' using information in the `tracking` folder for that dataset?
#' @param source_makefiles Should generic makefiles for each source be
#' constructed?
#' @param derived_data_makefile Should an overall generic makefile for all
#' sources be constructed?
#' @export
update_makefiles = function(
      dataset_makefiles = TRUE
    , source_makefiles = FALSE
    , derived_data_makefile = FALSE
  ) {
  sources = list_dataset_ids_by_source()
  source_names = names(sources)
  harmonized_sources = grep("_harmonized$", source_names, value = TRUE)
  sorted_source_names = c(setdiff(source_names, harmonized_sources), harmonized_sources)
  make_generic_makefile("derived-data")
  for (source in sorted_source_names) {
    make_generic_makefile(file.path("derived-data", source))
    for(dataset in sources[[source]]) {
      print(sprintf("%s %s", source, dataset))
      make_makefile(source, dataset)
    }
  }
  invisible(NULL)
}

#' Generate Dataset Makefile
#'
#' Run this function from the root of an IIDDA repository.
#'
#' @param source IIDDA source ID.
#' @param dataset IIDDA dataset ID.
#'
#' @export
make_makefile = function(source, dataset) {
  all_ids = list_dataset_ids_by_source()
  pipeline = sprintf("pipelines/%s", source)

  up_source = sub("_harmonized$", "", source)
  up_datasets = all_ids[[up_source]]
  up_pipeline = sprintf("pipelines/%s", up_source)
  up_derived = sprintf("derived-data/%s", up_source)

  any_up = source != up_source
  if (any_up) any_up = dir.exists(file.path("pipelines", up_source))

  pipe_vars = sprintf("PIPELINE := ../../../%s", pipeline)
  if (any_up) {
    pipe_vars = c(pipe_vars
      , sprintf("UPSTREAM := ../../../%s", up_pipeline)
      , sprintf("DERIVED := ../../../%s", up_derived)
    )
  }

  script = get_main_script(source, dataset) |> try()
  if (inherits(script, "try-error")) return(character())
  main = sub(sprintf("^%s/", pipeline), "", script)
  if (length(main) == 1L) {
    main = sprintf("MAIN := $(PIPELINE)/%s", main)
  } else {
    stop("Cannot find main prep script for dataset ", dataset)
  }

  focal_deps = get_all_dependencies(source, dataset)
  focal_deps = setdiff(focal_deps, script)
  if (any_up) {
    up_deps = get_all_dependencies(up_source, up_datasets)
  }

  deps = c()
  if (length(focal_deps) > 0L) {
    rel_focal_deps = sub(sprintf("^%s/", pipeline), "", focal_deps)
    deps = append(deps, unique(sprintf("$(PIPELINE)/%s", rel_focal_deps)))
  }
  if (any_up) {
    if (length(up_deps) > 0L) {
      rel_up_deps = sub(sprintf("^%s/", up_pipeline), "", up_deps)
      deps = append(deps, unique(sprintf("$(UPSTREAM)/%s", rel_up_deps)))
    }
  }

  if (isTRUE(length(deps) == 0L)) {
    deps = ""
    head = sprintf("%s.csv : $(MAIN)", dataset)
  } else {
    head = sprintf("%s.csv : $(MAIN) $(DEPS)", dataset)
    deps_all = deps
    deps1 = sprintf("DEPS := %s", deps[1L])
    if (isTRUE(length(deps) == 1L)) deps_all = deps1
    if (isTRUE(length(deps) > 1L)) {
      deps1 = sprintf("%s \\", deps1)
      depsn = sprintf("\t%s", deps[length(deps)])
      deps_all = c(deps1, depsn)
    }
    if (isTRUE(length(deps) > 2L)) {
      depsi = sprintf("\t%s \\", deps[2:(length(deps) - 1L)])
      deps_all = c(deps1, depsi, depsn)
    }
    deps = c(deps_all, "")
  }

  all = c(sprintf("all : %s.csv", dataset), "")
  body = "\tRscript $<"
  dataset_deps = NULL
  phony = NULL
  if (any_up) {
    body = c("\t$(MAKE) dataset-dependencies", body)
    for (d in all_ids[[up_source]]) make_makefile(up_source, d)
    dataset_dep_dirs = (all_ids[[up_source]]
      |> vapply(get_dataset_path, character(1L), source = up_source)
      |> dirname()
    )
    has_makefile = file.exists(file.path(dataset_dep_dirs, "Makefile"))
    if (!all(has_makefile)) {
      warning(
          "The following upstream derived dataset do not have a makefile:"
        , paste(all_ids[[up_source]][!has_makefile], collapse = "\n")
      )
    }
    dataset_dep_dirs = dataset_dep_dirs[has_makefile]
    dataset_deps = (dataset_dep_dirs
      |> sub(pattern = sprintf("^%s/", up_derived), replacement = "")
      |> sprintf(fmt = "\tcd $(DERIVED)/%s; $(MAKE)")
      |> append("dataset-dependencies :", after = 0)
    )
    phony = c(".PHONY : dataset-dependencies", "")
    body = c(body, "")
  }

  makefile_lines = c(comment_lines
    , pipe_vars
    , main
    , deps
    , all
    , phony
    , head
    , body
    , dataset_deps
  )
  makefile_dir = get_dataset_path(source, dataset) |> dirname()
  makefile_path = file.path(makefile_dir, "Makefile")


  if (!dir.exists(makefile_dir)) dir.create(makefile_dir, recursive = TRUE)
  writeLines(makefile_lines, makefile_path)
  makefile_path
}

#' Make a Generic Makefile
#'
#' This function is for IIDDA maintainers only. It id used to generate
#' a makefile that recursively looks for makefiles within a folder hierarchy.
#'
#' @export
make_generic_makefile = function(makefile_dir) {
  makefile_lines = c(comment_lines
      , "IGNORE_FILE := .iidda-ignore"
      , "IGNORED_DIRS := $(if $(wildcard $(IGNORE_FILE)),$(shell sed 's:/*$$::' $(IGNORE_FILE)),)"
      , "SUBDIRS := $(filter-out $(addsuffix /,$(IGNORED_DIRS)), $(wildcard */))"
      , "COLLECTED_LOGFILE := collected_output.log"
      , ""
      , "all: $(SUBDIRS) collect-logs"
      , ""
      , "$(SUBDIRS):"
      , "\t@echo 'Processing $@'"
      , "\t@if [ -f $@/Makefile ]; then \\"
      , "\t\t$(MAKE) -C $@ > $@output.log 2>&1 || echo 'Error in $@'; \\"
      , "\t\tif [ -f $@/output.log ]; then \\"
      , "\t\t\techo \"Output from $@:\"; \\"
      , "\t\t\tcat $@/output.log; \\"
      , "\t\tfi; \\"
      , "\telse \\"
      , "\t\techo 'No Makefile found in $@'; \\"
      , "\tfi"
      , "\t@echo 'Completed $@'"
      , ""
      , "collect-logs:"
      , "\t@echo 'Collecting logs from leaf directories...' > $(COLLECTED_LOGFILE)"
      , "\t@find . -type d ! -path . -exec sh -c ' \\"
      , "\t\tif [ -f \"{}/Makefile\" ] && [ -z \"$$(find {} -mindepth 1 -maxdepth 1 -type d)\" ]; then \\"
      , "\t\t\tif [ -f \"{}/output.log\" ]; then \\"
      , "\t\t\t\techo \"Output from {}:\" >> $(COLLECTED_LOGFILE); \\"
      , "\t\t\t\tcat \"{}/output.log\" >> $(COLLECTED_LOGFILE); \\"
      , "\t\t\t\techo \"\\n\\n\" >> $(COLLECTED_LOGFILE); \\"
      , "\t\t\tfi; \\"
      , "\t\tfi' \\;"
      , ""
      , "clean:"
      , "\t@find . -type f -name 'master_output.log' -delete"
      , "\t@find . -type f -name 'collected_output.log' -delete"
      , "\t@find . -type f -name 'output.log' -delete"
      , ""
      , ".PHONY: all $(SUBDIRS) collect-logs clean"
  )
  if (!dir.exists(makefile_dir)) dir.create(makefile_dir, recursive = TRUE)
  makefile_path = file.path(makefile_dir, "Makefile")
  writeLines(makefile_lines, makefile_path)
  makefile_path
}
