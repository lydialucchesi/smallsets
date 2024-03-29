% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sets_spacing.R
\name{sets_spacing}
\alias{sets_spacing}
\title{Sets spacing}
\usage{
sets_spacing(
  captions = NULL,
  degree = NULL,
  header = NULL,
  right = NULL,
  rows = NULL,
  columns = NULL
)
}
\arguments{
\item{captions}{Positive numeric value for amount of caption space. 
When alignment is horizontal, refers to the space below snapshots. 
When alignment is vertical, refers to the space to the right of 
the snapshots. Default is 2.5.}

\item{degree}{Integer between 0-90 (degrees) to rotate column names. Default is 0.}

\item{header}{Positive numeric value for amount of column name space. Default is 1.}

\item{right}{Positive numeric value (>=.5) for amount of space to the right
of each snapshot.}

\item{rows}{Integer for number of Smallset Timeline rows 
(applicable when the alignment is horizontal). Default is 1.}

\item{columns}{Integer for number of Smallset Timeline columns 
(applicable when the alignment is vertical). Default is 1.}
}
\value{
Returns a list with six elements (the spacing parameters).
}
\description{
Sets spacing parameters for the Smallset Timeline.
}
\details{
Passed to \code{spacing} in \link{Smallset_Timeline}.
}
\examples{
# increase space for captions and rotate column names
Smallset_Timeline(
   data = s_data,
   code = system.file("s_data_preprocess.R", package = "smallsets"),
   spacing = sets_spacing(captions = 5, degree = 45)
)

}
