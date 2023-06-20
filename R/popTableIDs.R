#' List of population table unique IDs.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' table unique IDs to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cond String. Unique identifier of plot in cond.
#' @param plt String. Unique identifier of plot in plt.
#' @param tree String. Unique identifier of plot in tree and seed.
#' @param seed String.
#' @param vsubpspp String.
#' @param vsubpstr String.
#' @param invsubp String.
#' @param subplot String.
#' @param subp_cond String. 
#' @param cond_dwm_calc String. 
#' @param sccm String. 
#' @param grm String. 
#' @param pltu String. 
#' @param condu String. 
#' @param begin String. 
#' @param midpt String. 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords list
#' @examples
#' popTableIDs(cond = "my_unique_id", tree = "my_tree_id")
#' @export popTableIDs

popTableIDs <- function(cond = "PLT_CN", 
                        plt = "CN", 
                        tree = "PLT_CN", 
                        seed = "PLT_CN",
                        vsubpspp = "PLT_CN", 
                        vsubpstr = "PLT_CN",
                        invsubp = "PLT_CN", 
                        subplot = "PLT_CN",
                        subp_cond = "PLT_CN", 
                        cond_dwm_calc = "PLT_CN",
                        sccm = "PLT_CN", 
                        grm = "PLT_CN",
                        pltu = "PLT_CN", 
                        condu = "PLT_CN",
                        begin = "PLT_CN",
                        midpt = "PLT_CN",
                        ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(popTableIDs)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # removes input parameters to create l correctly
  rm(input.params, formallst)
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

