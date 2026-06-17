#' @title Assign Functional Guilds for Ants
#' @description Classifies ants into functional guilds using four different ecological databases.
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays guild proportion plots.
#' @param validate Logical; if \code{TRUE}, validates species names using GBIF before analysis.
#' @param delay Numeric; seconds to wait between GBIF API calls when \code{validate = TRUE}.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_classic theme element_text element_blank element_line scale_y_continuous scale_x_discrete
#' @importFrom stringr str_split_fixed str_wrap
#' @importFrom stats reorder
#' @importFrom scales percent_format
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom utils head
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing species, abundance, percentage, and guild assignments from each system.}
#'   \item{plots}{A list of four ggplot2 objects, one for each guild classification system.}
#' @export
assign_guild_ants <- function(comm, verbose = TRUE, plot = TRUE, validate = TRUE, delay = 0.5) {

  # Optional validation of species names using GBIF
  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) message("Step 1: Preparing community data...")

  # Convert community matrix to data frame and clean column names
  numeric_data <- as.data.frame(comm)
  colnames(numeric_data) <- trimws(gsub("[._]", " ", colnames(numeric_data)))

  # Calculate total abundance
  total_abundance <- sum(colSums(numeric_data, na.rm = TRUE))

  # Create species data frame
  species_df <- data.frame(
    species = colnames(numeric_data),
    abundance = colSums(numeric_data, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  species_df$percentage <- (species_df$abundance / total_abundance) * 100

  # Extract genus names from species names (first word only)
  genus_list <- stringr::str_split_fixed(species_df$species, pattern = " ", n = 2)[, 1]

  if (verbose) message("Step 2: Matching species to functional guilds...")

  # Assign guilds from each classification system
  species_df$antclassify_guild <- generic_db$guild[match(genus_list, generic_db$target)]
  species_df$silva_guild <- silva_db$guild[match(species_df$species, silva_db$target)]
  species_df$silva_guild[is.na(species_df$silva_guild)] <- silva_db$guild[match(genus_list[is.na(species_df$silva_guild)], silva_db$target)]
  species_df$delabie_guild <- delabie_db$guild[match(genus_list, delabie_db$target)]
  species_df$silvestre_guild <- silvestre_db$guild[match(species_df$species, silvestre_db$target)]
  species_df$silvestre_guild[is.na(species_df$silvestre_guild)] <- silvestre_db$guild[match(genus_list[is.na(species_df$silvestre_guild)], silvestre_db$target)]

  # Replace NA with "Unidentified Guild"
  guild_cols <- c("antclassify_guild", "silva_guild", "delabie_guild", "silvestre_guild")
  species_df[guild_cols] <- lapply(species_df[guild_cols], function(x) ifelse(is.na(x), "Unidentified Guild", x))

  # Show first 6 rows of results
  if (verbose) {
    message("\nGuild classification results (first 6 rows):")
    print(head(species_df))
  }

  if (verbose) message("Step 3: Generating plots...")

  # Internal function to create plots
  create_plot <- function(data, guild_col, title_text) {
    plot_data <- data %>%
      dplyr::group_by(!!rlang::sym(guild_col)) %>%
      dplyr::summarise(total = sum(abundance), .groups = "drop") %>%
      dplyr::mutate(prop = total / sum(total))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = stats::reorder(!!rlang::sym(guild_col), prop), y = prop, fill = !!rlang::sym(guild_col))) +
      ggplot2::geom_col(color = "black", width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 45)) +
      ggplot2::labs(title = title_text, x = "Functional Guild", y = "Proportion") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black")
      )
  }

  # Generate plots for each classification system
  p1 <- create_plot(species_df, "antclassify_guild", "Functional Guilds - AntClassify")
  p2 <- create_plot(species_df, "silva_guild", "Functional Guilds - Silva et al. (2015)")
  p3 <- create_plot(species_df, "delabie_guild", "Functional Guilds - Delabie et al. (2000)")
  p4 <- create_plot(species_df, "silvestre_guild", "Functional Guilds - Silvestre et al. (2003)")

  # Print plots if requested
  if (plot) {
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }

  # Short message with references (without full citations)
  if (verbose) {
    message("\nGuild classification sources: Delabie et al. (2000), Silvestre et al. (2003),\n",
            "Silva et al. (2015), and AntClassify internal database.\n",
            "Full citations are available in the package documentation: ?assign_guild_ants\n")
  }

  # Return results invisibly
  invisible(list(table = species_df, plots = list(p1, p2, p3, p4)))
}
