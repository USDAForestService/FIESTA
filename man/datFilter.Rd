\name{datFilter}
\alias{datFilter}
\title{
	Data - Filters data table.
}
\description{
	Subsets a data table by specified filter(s).
}
\usage{
datFilter(x, xfilter = NULL, xfiltervar = NULL, othertabnms = NULL, 
	uniqueid = "PLT_CN", vardelete = NULL, title.filter = NULL, savedata = FALSE, 
	outfolder = NULL, outfn = "datf", outfn.pre = NULL, outfn.date = FALSE, 
	overwrite = FALSE, filternm = NULL, stopifnull = FALSE, returnDT = TRUE, 
	xnm = NULL, gui = FALSE)
}
\arguments{
  \item{x}{ Data frame Spatial*Dataframe or comma-delimited file (*.csv). A data 
	frame to filter. }
  \item{xfilter}{ String. A filter expression. Must be R syntax. 
	(e.g., "STATUSCD == 1", "INVYR %in% 2002:2005"). Use single quotes for strings 
	within double quotes (e.g., "SPP == 'Lodgepole'"). If NULL, a window pops up to 
	select filter variable(s) and filter value(s). }
  \item{xfiltervar}{ String. The filtervar if you know what it is. If NULL, a window 
	will pop up to select filter value(s). }
  \item{othertabnms}{ String vector. Name(s) of the objects or comma-delimited files to subset.
	 Names must be in quotes (e.g., othertables=c("tree", "cond")). }
  \item{uniqueid}{ String. Unique identifier of x. Only needed if othertables != NULL. 
	The uniqueid must be the same for all tables except if PLT_CN and CN. }
  \item{vardelete}{ String vector. Vector of variables you would like deleted from filter
	list. Mostly used for internal queries. }  
  \item{title.filter}{ String. Title of the filter query window. Mostuly used for internal 
	queries. }
  \item{savedata}{ Logical. If TRUE, writes output data to outfolder. }
  \item{outfolder}{ String. Name of output folder if savedata=TRUE. }
  \item{outfn}{ String. Name of output file if savedata=TRUE (*.csv). Do not 
	include extension. If NULL, the file will be named xfiltered_'date'.csv }
  \item{outfn.pre}{ String. Prefix for output table names. }
  \item{outfn.date}{ Logical. If TRUE, adds current date to outfile name. }
  \item{overwrite}{ Logical. If TRUE, overwrites file if exists. }
  \item{filternm}{ String. Optional. Name of filter, for feedback purposes. }
  \item{stopifnull}{ Logical. If TRUE, stop if output is NULL. }
  \item{returnDT}{ Logical. If TRUE, returns a data table. If FALSE, returns a data frame. }
  \item{xnm}{ String. Name for filter attribute. Used for warning messages. }
  \item{gui}{ Logical. If TRUE, pop-up windows will appear for user-interface. }
}
\details{
	If no parameters, then user is prompted for input. If partial parameters, default 
	parameter values are used.
}
\value{
 	A list of the following items:
    
	\item{xf}{ A data frame of filtered x. }
	\item{xfilter}{ The xfilter. }
	If othertables != NULL, the other tables, named with 'in' prefix
}
\author{ Tracey S. Frescino }

\note{ 
	If message returned is 'filter removed all records', either the filter 
	removed all records in x or the filter is incorrect. }


\examples{
	tab <- data.frame(cbind(	CONDCLASS=c(1,1,2,1,3,3,3,1,1,1,2,1), 
			FORTYPCD = c(182,184,201,221,221,184,221,182,182,201,182,221)))
	datFilter(x = tab, xfilter = "FORTYPCD != 182")
	datFilter(x = tab, xfilter = "FORTYPCD == c(184) & CONDCLASS != 3")
}
\keyword{ data }
