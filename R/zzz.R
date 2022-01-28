utils::globalVariables(names(formals(savedata_options)[1:(length(formals(savedata_options))-1)]))
utils::globalVariables(names(formals(strata_options)[1:(length(formals(strata_options))-1)]))
utils::globalVariables(names(formals(table_options)[1:(length(formals(table_options))-1)]))
utils::globalVariables(names(formals(title_options)[1:(length(formals(title_options))-1)]))
utils::globalVariables(names(formals(unit_options)[1:(length(formals(unit_options))-1)]))
utils::globalVariables(names(formals(multest_options)[1:(length(formals(multest_options))-1)]))
utils::globalVariables(names(formals(popFilters)[1:(length(formals(popFilters))-1)]))
utils::globalVariables(names(formals(popTableIDs)[1:(length(formals(popTableIDs))-1)]))
utils::globalVariables(names(formals(popTables)[1:(length(formals(popTables))-1)]))
utils::globalVariables(names(formals(spMakeSpatial_options)[1:(length(formals(spMakeSpatial_options))-1)]))
utils::globalVariables(c("treex", "seedx", "condx", "condf",
                         "count", "vcondsppid", "vcondstrid",
                         "zone_ds", ".cl_ds"))