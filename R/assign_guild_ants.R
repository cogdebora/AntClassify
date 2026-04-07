#' @title Assign Functional Guilds for Ants
#' @description Classifies ants into functional guilds using four different ecological databases.
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays guild proportion plots.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal theme scale_y_continuous scale_x_discrete element_text
#' @importFrom stringr str_split_fixed str_wrap
#' @importFrom stats reorder
#' @importFrom scales percent
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom utils head
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing species, abundance, percentage, and guild assignments from each system (AntClassify, Silva, Delabie, Silvestre).}
#'   \item{plots}{A list of four ggplot2 objects, one for each guild classification system.}
#' @examples
#' \donttest{
#' # Create a small example community matrix
#' species_list <- c(
#'   "Pachycondyla striata", "Pheidole gertrudae", "Solenopsis saevissima",
#'   "Strumigenys denticulata", "Wasmannia auropunctata", "Nylanderia fulva",
#'   "Odontomachus affinis", "Hypoponera foreli", "Hypoponera sp",
#'   "Ectatomma edentatum", "Acanthognathus rudis", "Acromyrmex subterraneus"
#' )
#' set.seed(123)
#' comm_data <- matrix(
#'   rpois(length(species_list) * 3, lambda = 2),
#'   nrow = 3,
#'   ncol = length(species_list),
#'   dimnames = list(paste0("sample", 1:3), species_list)
#' )
#' result <- assign_guild_ants(comm_data, verbose = FALSE, plot = FALSE)
#' head(result$table)
#' }
#' @export
assign_guild_ants <- function(comm, verbose = TRUE, plot = TRUE, validate = TRUE, delay = 0.5) {
  # Optional validation of species names using GBIF
  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) message("Step 1: Preparing community data...")
  dados_numericos <- as.data.frame(comm)
  colnames(dados_numericos) <- trimws(gsub("[._]", " ", colnames(dados_numericos)))
  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = colSums(dados_numericos, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  df$percentage <- (df$abundance / total_abs) * 100
  genus_list <- stringr::str_split_fixed(df$species, pattern = " ", n = 2)[, 1]

  if (verbose) message("Step 2: Matching species to functional guilds...")
  df$antclassify_guild <- generic_db$guild[match(genus_list, generic_db$target)]
  df$silva_guild <- silva_db$guild[match(df$species, silva_db$target)]
  df$silva_guild[is.na(df$silva_guild)] <- silva_db$guild[match(genus_list[is.na(df$silva_guild)], silva_db$target)]
  df$delabie_guild <- delabie_db$guild[match(genus_list, delabie_db$target)]
  df$silvestre_guild <- silvestre_db$guild[match(df$species, silvestre_db$target)]
  df$silvestre_guild[is.na(df$silvestre_guild)] <- silvestre_db$guild[match(genus_list[is.na(df$silvestre_guild)], silvestre_db$target)]

  guild_cols <- c("antclassify_guild", "silva_guild", "delabie_guild", "silvestre_guild")
  df[guild_cols] <- lapply(df[guild_cols], function(x) ifelse(is.na(x), "Unidentified Guild", x))

  if (verbose) {
    message("\nGuild classification results (first 6 rows):")
    print(head(df))
  }

  if (verbose) message("Step 3: Generating plots...")
  create_plot <- function(data, guild_col, title_text) {
    res <- data %>%
      dplyr::group_by(!!rlang::sym(guild_col)) %>%
      dplyr::summarise(total = sum(abundance), .groups = "drop") %>%
      dplyr::mutate(prop = total / sum(total))

    ggplot2::ggplot(res, ggplot2::aes(x = stats::reorder(!!rlang::sym(guild_col), prop), y = prop, fill = !!rlang::sym(guild_col))) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
      ggplot2::labs(title = title_text, x = "Functional Guild", y = "Proportion") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_text(size = 9))
  }

  p1 <- create_plot(df, "antclassify_guild", "AntClassify Guilds")
  p2 <- create_plot(df, "silva_guild", "Poneromorph Functional Guilds (Silva et al., 2015)")
  p3 <- create_plot(df, "delabie_guild", "Atlantic Forest Functional Guilds (Delabie et al., 2000)")
  p4 <- create_plot(df, "silvestre_guild", "Cerrado Functional Guilds (Silvestre et al., 2003)")

  if (plot) {
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }

  if (verbose) {
    message("\n********************************************************************************")
    message("IMPORTANT NOTICE:")
    message("Please verify all assigned guilds. 'Unidentified Guild' indicates that the")
    message("taxon was not found in the available reference databases.\n")

    message("Guild classification in this analysis follows:")
    message("- Literature-based criteria from:\n")

    message("  Silvestre, R., Brand\u00e3o, C. R. F., & Silva, R. R. (2003). ")
    message("  Grupos funcionales de hormigas: el caso de los gremios del Cerrado. ")
    message("  In F. Fern\u00e1ndez (Ed.), *Introducci\u00f3n a las Hormigas de las Regi\u00f3n Neotropical* ")
    message("  (pp. 113-148). Instituto Alexander Von Humboldt.\n")

    message("  Silva, R. R., Silvestre, R., Brand\u00e3o, C. R. F., Morini, M. S. C., & Delabie, J. H. C. (2015). ")
    message("  Grupos tr\u00f3ficos e guildas em formigas poneromorfas. In: Delabie, J. H. C. et al. ")
    message("  *As formigas poneromorfas do Brasil*. Ilh\u00e9us: Editus, 2015. p. 163-179.\n")

    message("  Delabie, J. H. C., Agosti, D., & Nascimento, I. C. (2000). ")
    message("  Litter ant communities of the Brazilian Atlantic rain forest region. ")
    message("  *Sampling Ground-dwelling Ants: case studies from the world's rain forests. ")
    message("  Curtin University of Technology School of Environmental Biology Bulletin*, v. 18.\n")

    message("- The 'AntClassify Guilds' classification corresponds to the internal")
    message("  classification system implemented in the AntClassify package.\n")

    message("********************************************************************************")
  }

  invisible(list(table = df, plots = list(p1, p2, p3, p4)))
}
