# Tools for Interacting with the IIDDA API


# schema = get_api_schema()
#
# getters = (schema$paths$`/datasets`$get$parameters
#   %>% list_xpath(c('get', 'summary'))
#   %>% lapply(snakecase_lite)
#   %>% unlist
# )
#
# which_getters = !(getters
#   %>% lapply(is.null)
#   %>% unlist
# )
#
# getters[which_getters]
#
# getter_parameter_schemas = (schema$paths
#   %>% list_xpath('get', 'parameters')
#   %>% lapply(list_xpath, 'schema')
# )
#
# is_parameter_required = (schema$paths
#   %>% list_xpath('get', 'parameters')
#   %>% lapply(list_xpath, 'required')
#   %>% lapply(unlist)
# )
#
# parameter_titles = (schema$paths
#   %>% list_xpath('get', 'parameters')
#   %>% lapply(list_xpath, 'schema', 'title')
#   %>% lapply(unlist)
# )
#
# (schema$paths
#   %>% list_xpath('get', 'parameters')
#   %>% lapply(list_xpath, 'schema', 'default')
#   ## TODO: fix unlisting so that required parameters are not blank
# )[[2]]
#
# (getter_parameter_schemas
#   %>% lapply(list_xpath, 'default')
#   #%>% lapply(unlist)
# )
#
#
# getter_parameters[[1]][[1]]$name
#
# lapply(schema$paths, list_xpath, c('get', 'summary'))
# schema$paths[[2]]$get$summary
# list_xpath(schema)
# list_xpath(schema$paths, c('/datasets', 'get', 'parameters'))
# schema$paths$`/datasets`$get$parameters[[1]]$name
#
# # get_datasets = function(all_metadata = FALSE) {
# #   template = "%{api_url}s/datasets?all_metadata=%{all_metadata}s"
# # }
