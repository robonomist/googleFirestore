% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/firestore_objects.R
\name{GoogleFirestoreAdminV1FieldOperationMetadata}
\alias{GoogleFirestoreAdminV1FieldOperationMetadata}
\title{GoogleFirestoreAdminV1FieldOperationMetadata Object}
\usage{
GoogleFirestoreAdminV1FieldOperationMetadata(
  startTime = NULL,
  indexConfigDeltas = NULL,
  progressBytes = NULL,
  endTime = NULL,
  field = NULL,
  progressDocuments = NULL,
  state = NULL
)
}
\arguments{
\item{startTime}{The time this operation started}

\item{indexConfigDeltas}{A list of IndexConfigDelta, which describe the intent of this operation}

\item{progressBytes}{The progress, in bytes, of this operation}

\item{endTime}{The time this operation completed}

\item{field}{The field resource that this operation is acting on}

\item{progressDocuments}{The progress, in documents, of this operation}

\item{state}{The state of the operation}
}
\value{
GoogleFirestoreAdminV1FieldOperationMetadata object
}
\description{
GoogleFirestoreAdminV1FieldOperationMetadata Object
}
\details{
Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
Metadata for google.longrunning.Operation results from FirestoreAdmin.UpdateField.
}
\concept{GoogleFirestoreAdminV1FieldOperationMetadata functions}
