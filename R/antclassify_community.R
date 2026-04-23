#' Classify ant communities across multiple sites
#'
#' Applies the AntClassify pipeline to each site (row) of a community matrix
#' and returns aggregated guild abundance and richness matrices.
#'
#' @param comm A community matrix with sites as rows and species as columns.
#'   Species names must match the nomenclature used in the internal databases.
#' @param guild_col Character string giving the name of the guild column to use
#'   for aggregation. Must be one of \code{"antclassify_guild"}, \code{"silva_guild"},
#'   \code{"delabie_guild"}, or \code{"silvestre_guild"}. Default is
#'   \code{"antclassify_guild"}.
#' @param ... Additional arguments passed to \code{\link{assign_guild_ants}} (for
#'   example, \code{validate}, \code{delay}). Note that \code{plot} is always set
#'   to \code{FALSE} inside this function to avoid displaying one plot per site.
#'
#' @return A list with three components:
#'   \item{by_site}{A list of length equal to \code{nrow(comm)}. Each element
#'     contains the full output of \code{\link{assign_guild_ants}} for the
#'     corresponding site.}
#'   \item{guild_abundance}{A matrix of guild abundances (sites × guilds).}
#'   \item{guild_richness}{A numeric vector of length \code{nrow(comm)} giving
#'     the number of distinct guilds found at each site.}
#'
#' @export
#'
#' @examples
#' # Small community matrix with two sites and six species
#' comm <- matrix(c(5, 0, 2, 0, 1, 3,
#'                  2, 4, 0, 2, 0, 0),
#'                nrow = 2, byrow = TRUE,
#'                dimnames = list(c("Site1", "Site2"),
#'                                c("Atta sexdens", "Camponotus atriceps",
#'                                  "Pheidole megacephala", "Wasmannia auropunctata",
#'                                  "Solenopsis saevissima", "Nylanderia fulva")))
#'
#' # Run the classification (validate = FALSE to avoid GBIF calls in examples)
#' res <- antclassify_community(comm, guild_col = "antclassify_guild",
#'                              validate = FALSE)
#' res$guild_abundance
#' res$guild_richness
antclassify_community <- function(comm, guild_col = "antclassify_guild", ...) {

  # Input validation
  if (!is.data.frame(comm) && !is.matrix(comm)) {
    stop("'comm' must be a data frame or matrix with sites as rows and species as columns.")
  }

  valid_guilds <- c("antclassify_guild", "silva_guild", "delabie_guild", "silvestre_guild")
  if (!guild_col %in% valid_guilds) {
    stop("'guild_col' must be one of: ", paste(valid_guilds, collapse = ", "))
  }

  # Coerce to matrix to ensure consistent behavior
  comm <- as.matrix(comm)

  # Row names
  if (is.null(rownames(comm))) {
    rownames(comm) <- paste0("Site_", seq_len(nrow(comm)))
  }

  n_sites <- nrow(comm)

  # Process each site
  process_site <- function(i) {
    site_comm <- comm[i, , drop = FALSE]
    # Remove species absent from this site
    present <- colSums(site_comm, na.rm = TRUE) > 0
    site_comm <- site_comm[, present, drop = FALSE]

    if (ncol(site_comm) == 0) {
      # Empty site -> return empty structures
      empty_table <- data.frame(species = character(), abundance = numeric(),
                                antclassify_guild = character(),
                                silva_guild = character(),
                                delabie_guild = character(),
                                silvestre_guild = character(),
                                stringsAsFactors = FALSE)
      return(list(res = list(table = empty_table, plots = list()),
                  guild_agg = data.frame(Guild = character(), abundance = numeric()),
                  richness = 0L))
    }

    # Suppress plot printing inside assign_guild_ants
    res <- assign_guild_ants(site_comm, plot = FALSE, ...)
    tab <- res$table

    # Aggregate by chosen guild column
    guild_agg <- stats::aggregate(tab$abundance,
                                  by = list(Guild = tab[[guild_col]]),
                                  FUN = sum)
    colnames(guild_agg)[2] <- "abundance"

    list(res = res,
         guild_agg = guild_agg,
         richness = nrow(guild_agg))
  }

  site_results <- lapply(seq_len(n_sites), process_site)

  # Build output components
  by_site <- lapply(site_results, `[[`, "res")
  guild_richness <- vapply(site_results, `[[`, integer(1), "richness")

  # Gather all guild names
  all_guilds <- unique(unlist(
    lapply(site_results, function(x) x$guild_agg$Guild)
  ))

  # Build abundance matrix (sites x guilds)
  guild_abundance <- matrix(0, nrow = n_sites, ncol = length(all_guilds),
                            dimnames = list(rownames(comm), all_guilds))
  for (i in seq_len(n_sites)) {
    g <- site_results[[i]]$guild_agg
    if (nrow(g) > 0) {
      guild_abundance[i, g$Guild] <- g$abundance
    }
  }

  list(by_site = by_site,
       guild_abundance = guild_abundance,
       guild_richness = guild_richness)
}
