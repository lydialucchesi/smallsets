% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sets_sizing.R
\name{sets_sizing}
\alias{sets_sizing}
\title{Sets sizing}
\usage{
sets_sizing(
  captions = NULL,
  columns = NULL,
  data = NULL,
  icons = NULL,
  legend = NULL,
  resume = NULL,
  tiles = NULL
)
}
\arguments{
\item{captions}{Positive numeric value for caption text size. Default is 3.}

\item{columns}{Positive numeric value for column name text size. Default is 3.}

\item{data}{Positive numeric value for printed data text size. Default is 2.}

\item{icons}{Positive numeric value for legend icon size. Default is 1.}

\item{legend}{Positive numeric value for legend text size. Default is 10.}

\item{resume}{Positive numeric value for resume marker size. Default is 1.}

\item{tiles}{Positive numeric value for Smallset tile size. Default is .2.}
}
\value{
Returns a list with seven elements (the sizing parameters).
}
\description{
Sets sizing parameters for the Smallset Timeline.
}
\details{
Passed to \code{sizing} in \link{Smallset_Timeline}.
}
\examples{
# increase size of caption text
# and add more caption space, so larger caption text fits
Smallset_Timeline(
   data = s_data,
   code = system.file("s_data_preprocess.R", package = "smallsets"),
   sizing = sets_sizing(captions = 4),
   spacing = sets_spacing(captions = 4)
)

}
