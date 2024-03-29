% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/firestore_objects.R
\name{Cursor}
\alias{Cursor}
\title{Cursor Object}
\usage{
Cursor(values = NULL, before = NULL)
}
\arguments{
\item{values}{The values that represent a position, in the order they appear in the order by clause of a query}

\item{before}{If the position is just before or just after the given values, relative to the sort order defined by the query}
}
\value{
Cursor object
}
\description{
Cursor Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
A position in a query result set.
}
\concept{Cursor functions}
