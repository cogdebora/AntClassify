#' Validate species names using GBIF API
#'
#' Checks species names against the GBIF taxonomic backbone. If an exact match is found,
#' the accepted name (or canonical name) is used. If no exact match is found, the
#' original name is kept and a warning is issued, indicating that the name may be
#' outdated or misspelled.
#'
#' @param comm A community matrix (rows = sites, columns = species).
#' @param verbose Logical; if TRUE, displays progress messages.
#' @param delay Numeric; seconds to wait between API calls (default 0.5).
#' @importFrom utils URLencode
#' @return A community matrix with updated column names (accepted names when available).
#' @export
#'
#' @examples
#' \dontrun{
#' data(ant_community)
#' comm_validated <- validate_species_names(ant_community, verbose = TRUE)
#' }
validate_species_names <- function(comm, verbose = TRUE, delay = 0.5) {
  if (verbose) message("Starting validation of species names via GBIF...")

  old_names <- colnames(comm)
  new_names <- character(length(old_names))
  unresolved <- character(0)

  for (i in seq_along(old_names)) {
    if (verbose) message("Processing [", i, "/", length(old_names), "]: ", old_names[i])

    name_encoded <- URLencode(old_names[i])
    url <- paste0("https://api.gbif.org/v1/species/match?verbose=false&name=", name_encoded)

    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 30, useragent = "AntClassify/1.0")

    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)

    if (is.null(res)) {
      unresolved <- c(unresolved, old_names[i])
      new_names[i] <- old_names[i]
      next
    }

    texto <- rawToChar(res$content)
    dados <- tryCatch(jsonlite::fromJSON(texto), error = function(e) NULL)

    if (is.null(dados) || is.null(dados$matchType) || dados$matchType != "EXACT") {
      unresolved <- c(unresolved, old_names[i])
      new_names[i] <- old_names[i]
    } else {
      # Exact match: use the accepted name
      if (!is.null(dados$acceptedName)) {
        new_names[i] <- dados$acceptedName
      } else if (!is.null(dados$canonicalName)) {
        new_names[i] <- dados$canonicalName
      } else {
        new_names[i] <- old_names[i]
      }
    }

    Sys.sleep(delay)
  }

  colnames(comm) <- new_names

  if (length(unresolved) > 0) {
    warning(sprintf("%d species name(s) could not be validated by GBIF: %s",
                    length(unresolved), paste(unresolved, collapse = ", ")))
    if (verbose) {
      message("\n*** ATTENTION: The following names were not found or are no longer current:")
      for (p in unresolved) message("   - ", p)
      message("   Please verify these names manually before proceeding.")
    }
  } else {
    if (verbose) message("All species names were successfully validated (exact match in GBIF).")
  }

  return(comm)
}
