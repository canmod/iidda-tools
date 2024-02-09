api_type = iidda.api:::staging  ## production, local
op_type = paste("ops", api_type$type, sep = "_")
api_hook = getFromNamespace(op_type, "iidda.api")
api_url = file.path(api_type$api_url, api_type$base_path)
