% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_operations.R
\name{replace_na_by_type}
\alias{replace_na_by_type}
\title{Replace NA values as default values based no the type of the variable}
\usage{
replace_na_by_type(x, na_replace_list)
}
\arguments{
\item{replace_na_by_type}{the list of default values for each type to be used}
}
\description{
This function allows user to replace NA values based on type of the variable.
For example NA in character columns can be replaced by "", logical NA values as FALSE, integer NA values as 0L and so on.
User need to give the list of the default values what he or she want to be used in replacing.
}
\examples{
\dontrun{

## Give list of NA values
na_replace_list <- list(
  "logical"=FALSE,
  "character"="",
  "integer"=0L,
  "double"=0.0,
  "numeric"=0,
 # "Date"=lubridate::dmy("01-01-1900")
)
d <- left_join(sample_cohort,sample_regdata)
d \%>\%
  mutate_all(replace_na_by_type,na_replace_list)
}

}
\seealso{
Other help functions: 
\code{\link{get_classification_name}()},
\code{\link{get_na_vars}()},
\code{\link{get_var_types}()},
\code{\link{left_join_replace_na}()},
\code{\link{make_regex}()}
}
\concept{help functions}
