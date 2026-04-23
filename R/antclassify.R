#' AntClassify Full Ecological Pipeline
#'
#' Runs all ecological classification functions of the package.
#'
#' @param comm Community matrix (samples x species). Species names as columns, samples as rows.
#' @param verbose Logical; if TRUE, displays progress messages.
#' @param plot Logical; if TRUE, displays guild proportion plots (passed to assign_guild_ants).
#' @param validate Logical; if TRUE, validates species names using GBIF.
#' @param delay Numeric; seconds between API calls (if validate = TRUE).
#' @return A list containing results from all analyses (guilds, exotic, endemic, rarity).
#' @export
antclassify <- function(comm, verbose = TRUE, plot = TRUE, validate = TRUE, delay = 0.5) {

  if (verbose) message("*************************************************")
  if (verbose) message("Starting AntClassify")
  if (verbose) message("*************************************************")

  if (!is.data.frame(comm) && !is.matrix(comm)) {
    stop("Error: input must be a data.frame or matrix.")
  }

  # Convert to data.frame if matrix
  comm <- as.data.frame(comm)

  if (!all(sapply(comm, is.numeric))) {
    stop("Error: all columns should be numerical (species abundance).")
  }

  results <- list()

  # Validate species names if requested (using the correct object name 'comm')
  if (isTRUE(validate)) {
    if (verbose) message("\n>>> Validating species names via GBIF")
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  # Run guild classification
  if (verbose) message("\n>>> Running guild classification")
  results$guilds <- assign_guild_ants(comm, verbose = verbose, plot = plot, validate = FALSE)  # validation already done

  # Run exotic check
  if (verbose) message("\n>>> Checking exotic species")
  results$exotic <- check_exotic_ants(comm, verbose = verbose, plot = plot)

  # Run endemic check
  if (verbose) message("\n>>> Checking Atlantic Forest endemic species")
  results$endemic <- check_endemic_atlantic_ants(comm, verbose = verbose, plot = plot)

  # Run rarity check
  if (verbose) message("\n>>> Checking Atlantic Forest rarity patterns")
  results$rarity <- check_rarity_atlantic_ants(comm, verbose = verbose, plot = plot)

  if (verbose) message("\n***********************************************")
  if (verbose) message("AntClassify finished successfully")
  if (verbose) message("*************************************************")

  return(invisible(results))
}
