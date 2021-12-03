## this function sets up the list-binning parameter functions

setup_list_parameters <- function(parameters, savedata_opts2=savedata_opts,
                                  table_opts2=table_opts, title_opts2=title_opts,
                                  strata_opts2=strata_opts, data_tables2=data_tables,
                                  data_uniqueids2=data_uniqueids
                                  ) {
  # if ("savedata_opts" %in% parameters) {
  #   ## Set savedata defaults
  #   savedata_defaults_list <- formals(FIESTA::savedata_options)[-length(formals(FIESTA::savedata_options))]
  # 
  #   for (i in 1:length(savedata_defaults_list)) {
  #     assign(names(savedata_defaults_list)[[i]], savedata_defaults_list[[i]])
  #   }
  # 
  #   ## Set user-supplied savedata values
  #   if (length(savedata_opts2) > 0) {
  #     for (i in 1:length(savedata_opts2)) {
  #       assign(names(savedata_opts2)[[i]], savedata_opts2[[i]])
  #     }
  #   }
  # }
  # 
  # if ("table_opts" %in% parameters) {
  #   ## Set table defaults
  #   table_defaults_list <- formals(FIESTA::table_options)[-length(formals(FIESTA::table_options))]
  #   
  #   for (i in 1:length(table_defaults_list)) {
  #     assign(names(table_defaults_list)[[i]], table_defaults_list[[i]])
  #   }
  #   
  #   ## Set user-supplied table values
  #   if (length(table_opts2) > 0) {
  #     for (i in 1:length(table_opts2)) {
  #       assign(names(table_opts2)[[i]], table_opts2[[i]])
  #     }
  #   }
  # }
  # 
  # if ("title_opts" %in% parameters) {
  #   ## Set title defaults
  #   title_defaults_list <- formals(FIESTA::title_options)[-length(formals(FIESTA::title_options))]
  #   
  #   for (i in 1:length(title_defaults_list)) {
  #     assign(names(title_defaults_list)[[i]], title_defaults_list[[i]])
  #   }
  #   
  #   ## Set user-supplied title values
  #   if (length(title_opts2) > 0) {
  #     for (i in 1:length(title_opts2)) {
  #       assign(names(title_opts2)[[i]], title_opts2[[i]])
  #     }
  #   }
  # }
  # 
  # if ("strata_opts" %in% parameters) {
  #   ## Set strata defaults
  #   strata_defaults_list <- formals(FIESTA::strata_options)[-length(formals(FIESTA::strata_options))]
  #   
  #   for (i in 1:length(strata_defaults_list)) {
  #     assign(names(strata_defaults_list)[[i]], strata_defaults_list[[i]])
  #   }
  #   
  #   ## Set user-supplied strata values
  #   if (length(strata_opts2) > 0) {
  #     for (i in 1:length(strata_opts2)) {
  #       assign(names(strata_opts2)[[i]], strata_opts2[[i]])
  #     }
  #   }
  # }
  
  # if ("data_tables" %in% parameters) {
  #   ## Set data_tables defaults
  #   data_tables_defaults_list <- formals(FIESTA::data_tables_list)[-length(formals(FIESTA::data_tables_list))]
  #   
  #   for (i in 1:length(data_tables_defaults_list)) {
  #     assign(names(data_tables_defaults_list)[[i]], data_tables_defaults_list[[i]])
  #   }
  #   
  #   ## Set user-supplied strata values
  #   if (length(data_tables2) > 0) {
  #     for (i in 1:length(data_tables2)) {
  #       assign(names(data_tables2)[[i]], data_tables2[[i]])
  #     }
  #   }
  # }
  # 
  # if ("data_uniqueids" %in% parameters) {
  #   ## Set data_uniqueids defaults
  #   data_uniqueids_defaults_list <- formals(FIESTA::data_uniqueids_list)[-length(formals(FIESTA::data_uniqueids_list))]
  #   
  #   for (i in 1:length(data_uniqueids_defaults_list)) {
  #     assign(names(data_uniqueids_defaults_list)[[i]], data_uniqueids_defaults_list[[i]])
  #   }
  #   
  #   ## Set user-supplied strata values
  #   if (length(data_uniqueids2) > 0) {
  #     for (i in 1:length(data_uniqueids2)) {
  #       assign(names(data_uniqueids2)[[i]], data_uniqueids2[[i]])
  #     }
  #   }
  # }
  NULL
}

