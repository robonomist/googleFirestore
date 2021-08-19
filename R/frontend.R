toFields  <- function(x) {
  lapply(x, function(x) {
    y <-
      if (is.integer(x)) {
        list(integerValue = x)
      } else if (is.double(x)){
        if (inherits(x, "POSIXt")) {
          list(timestampValue = toZulu(x))
        } else {
          list(doubleValue = x)
        }
      } else if (is.logical(x)){
        list(booleanValue = x)
      } else if (is.double(x)){
        list(doubleValue = x)
      } else if (is.character(x)){
        list(stringValue = x)
      } else if (is.list(x)){
        list(mapValue = list(fields = toFields(x)))
      } else {
        list(nullValue = NA)
      }
    if (length(x) > 1 && !is.list(x)) y <- list(arrayValue = list(values = data.frame(values = y)))
    y
  })
}


fromFields  <- function(x) {
  lapply(x, function(x) {
    n <- names(x)
    y <-
      if (n %in% c("doubleValue", "stringValue", "booleanValue", "nullValue")) {
        x[[1]]
      } else if (n == "integerValue"){
        as.integer(x[[1]])
      } else if (n == "arrayValue"){
        x[[1]]$values[[1]]
      } else if (n == "mapValue"){
        fromFields(x[[1]]$fields)
      } else if (n == "timestampValue"){
        fromZulu(x[[1]])
      } else {
        x
      }
    y
  })
}

toZulu <- function(x) {
  format(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

fromZulu <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

parseDocument <- function(x) {
  Document(name = x$name,
           createTime = fromZulu(x$createTime),
           updateTime = fromZulu(x$updateTime),
           fields = fromFields(x$fields))
}


#' @export
documents.createDocument <- function(x, collectionId, documentId = NULL, project = Sys.getenv("FIRESTORE_PROJECT"), database = "(default)") {
  parent <- sprintf("projects/%s/databases/%s/documents", project, database)
  parseDocument(
    projects.databases.documents.createDocument(
      Document = Document(fields = toFields(x)),
      parent = parent,
      collectionId = collectionId,
      documentId = documentId
    ))
}

#' @export
documents.get <- function(collectionId, documentId, mask = NULL, project = Sys.getenv("FIRESTORE_PROJECT"), database = "(default)") {
  name <- sprintf("projects/%s/databases/%s/documents/%s/%s",
                  project, database, collectionId, documentId)
  parseDocument(
    projects.databases.documents.get(
      name = name,
      mask.fieldPaths = DocumentMask(mask)
    ))
}

#' @export
documents.patch <- function(x, collectionId, documentId, updateMask, project = Sys.getenv("FIRESTORE_PROJECT"), database = "(default)") {
  name <- sprintf("projects/%s/databases/%s/documents/%s/%s", project, database, collectionId, documentId)
  parseDocument(
    projects.databases.documents.patch(
      Document = Document(fields = toFields(x)),
      name = name,
      updateMask.fieldPaths = DocumentMask(updateMask)
    )
  )
}
