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
    n <- names(x)[1]
    y <-
      if (length(x[[1]]) == 0L) {
        NULL
      } else if (n %in% c("referenceValue", "doubleValue", "stringValue", "booleanValue", "nullValue")) {
        x[[n]]
      } else if (n == "integerValue"){
        as.integer(x[[n]])
      } else if (n == "arrayValue"){
        fromFields(x[[n]]$values)
      } else if (n == "mapValue"){
        fromFields(x[[n]]$fields)
      } else if (n == "timestampValue"){
        fromZulu(x[[n]])
      } else {
        warning(n, "not implemented")
        x[[1]]
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
  d <- fromFields(x$fields)
  attr(d, "name") <- x$name
  attr(d, "createTime") <- fromZulu(x$createTime)
  attr(d, "updateTime") <- fromZulu(x$updateTime)
  attr(d, "class") <- "firestore_document"
  d
}


#' @export
create_document <- function(x,
                            collectionId,
                            documentId = NULL,
                            project = Sys.getenv("FIRESTORE_PROJECT"),
                            database = "(default)") {

  parent <- sprintf("projects/%s/databases/%s/documents", project, database)

  parseDocument(
    projects.databases.documents.createDocument(
      Document = Document(fields = toFields(x)),
      parent = parent,
      collectionId = collectionId,
      documentId = documentId
    )
  )
}

#' @export
get_document <- function(collectionId,
                         documentId,
                         mask = NULL,
                         project = Sys.getenv("FIRESTORE_PROJECT"),
                         database = "(default)") {

  name <- sprintf("projects/%s/databases/%s/documents/%s/%s",
                  project, database, collectionId, documentId)

  parseDocument(
    projects.databases.documents.get(
      name = name,
      mask.fieldPaths = DocumentMask(mask)
    )
  )
}

#' @export
list_documents <- function(collectionId,
                          pageToken = NULL,
                          orderBy = NULL,
                          transaction = NULL,
                          mask = NULL,
                          pageSize = NULL,
                          readTime = NULL,
                          showMissing = NULL,
                          project = Sys.getenv("FIRESTORE_PROJECT"),
                          database = "(default)") {

  parent <- sprintf("projects/%s/databases/%s/documents",
                  project, database)

  r <- projects.databases.documents.list(
    parent = parent,
    collectionId = collectionId,
    pageToken = pageToken,
    orderBy = orderBy,
    transaction = transaction,
    mask.fieldPaths = DocumentMask(mask),
    pageSize = pageSize,
    readTime = readTime,
    showMissing = showMissing
  )

  lapply(r$documents, parseDocument)
}

#' @export
patch_documents <- function(x,
                            collectionId,
                            documentId,
                            updateMask,
                            project = Sys.getenv("FIRESTORE_PROJECT"),
                            database = "(default)") {

  name <- sprintf("projects/%s/databases/%s/documents/%s/%s", project, database, collectionId, documentId)

  parseDocument(
    projects.databases.documents.patch(
      Document = Document(fields = toFields(x)),
      name = name,
      updateMask.fieldPaths = DocumentMask(updateMask)
    )
  )
}
