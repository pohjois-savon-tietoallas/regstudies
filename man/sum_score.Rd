% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_score.R
\name{sum_score}
\alias{sum_score}
\title{Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.}
\usage{
sum_score(.data, ...)
}
\arguments{
\item{.data}{the data which contains the variables to be summed over.}

\item{...}{the names of the variables to be summed over.}
}
\value{
returns scores summed over the given variables.
}
\description{
Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.
}
\examples{
\dontrun{
# Lets calculate two different Elixhauser scores for a data set 'filtered_d'
elixh_score <- filtered_d \%>\%
  classify_elixhauser(icdcodes=CODE1) \%>\%
  sum_score(score_AHRQ,score_van_Walraven)
  head(elixh_score)
}
}
\concept{classification functions}
