#' @title Identify Exotic Ant Species (Brazil)
#' @description Checks a community matrix for known exotic ant species in Brazil sourced from Vieira (2025).
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays a plot (type controlled by \code{plot_type}).
#' @param plot_type Character; type of plot to display. \code{"status"} (default) shows a bar plot comparing
#'   exotic vs. native abundance proportions. \code{"species"} shows a bar plot of individual exotic species
#'   abundances as a proportion of the total community.
#' @param validate Logical; if \code{TRUE}, validates species names using GBIF before analysis.
#' @param delay Numeric; seconds to wait between GBIF API calls when \code{validate = TRUE}.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_fill_manual labs theme_classic theme element_text element_blank element_line
#' @importFrom scales percent_format
#' @importFrom stats reorder
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing exotic species detected, with columns species, abundance, and percentage.}
#'   \item{plot}{A ggplot2 object.}
#' @export
check_exotic_ants <- function(comm, verbose = TRUE, plot = TRUE, plot_type = c("status", "species"),
                              validate = TRUE, delay = 0.5) {

  plot_type <- match.arg(plot_type)

  if (!is.data.frame(comm) && !is.matrix(comm)) {
    stop("Error: input must be a data.frame or matrix.")
  }

  comm <- as.data.frame(comm)

  if (!all(sapply(comm, is.numeric))) {
    stop("Error: all columns should be numerical (species abundance).")
  }

  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) message("Step 1: Preparing community data...")

  # Internal exotic species list (Brazil)
  exotic_list <- c(
    "Tapinoma melanocephalum", "Technomyrmex vitiensis", "Paratrechina longicornis",
    "Cardiocondyla emeryi", "Cardiocondyla minutior", "Cardiocondyla obscurior",
    "Cardiocondyla wroughtonii", "Monomorium floricola", "Monomorium pharaonis",
    "Pheidole megacephala", "Strumigenys emmae", "Strumigenys rogeri",
    "Tetramorium bicarinatum", "Tetramorium caldarium", "Tetramorium lanuginosum",
    "Tetramorium lucayanum", "Tetramorium simillimum", "Trichomyrmex destructor",
    "Leptogenys maxillosa"
  )

  # Prepare community data
  numeric_data <- as.data.frame(comm)
  clean_names <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(numeric_data))))
  colnames(numeric_data) <- clean_names

  total_abundance <- sum(colSums(numeric_data, na.rm = TRUE))

  # Create species data frame
  species_df <- data.frame(
    species = colnames(numeric_data),
    abundance = as.numeric(colSums(numeric_data, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  species_df$percentage <- (species_df$abundance / total_abundance) * 100
  species_df$origin <- ifelse(species_df$species %in% exotic_list, "Exotic", "Native/Not Listed")

  if (verbose) message("Step 2: Generating results...")

  # Prepare data for status plot
  plot_data <- dplyr::group_by(species_df, origin)
  plot_data <- dplyr::summarise(plot_data, total = sum(abundance), .groups = "drop")
  plot_data <- dplyr::mutate(plot_data, prop = total / sum(total))

  # Identify exotic species
  exotic_species <- species_df[species_df$origin == "Exotic", ]
  rownames(exotic_species) <- NULL

  # --- Generate plot based on plot_type ---
  p <- NULL

  if (plot_type == "status") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = stats::reorder(origin, prop), y = prop, fill = origin)) +
      ggplot2::geom_col(color = "black", width = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c("Exotic" = "#d95f02", "Native/Not Listed" = "#1b9e77")) +
      ggplot2::labs(
        title = "Proportion of Exotic vs. Native Species",
        x = "Status",
        y = "Proportion"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black")
      )
  } else { # plot_type == "species"
    if (nrow(exotic_species) > 0) {
      exotic_plot_data <- exotic_species
      exotic_plot_data$prop_community <- exotic_plot_data$abundance / total_abundance

      p <- ggplot2::ggplot(
        exotic_plot_data,
        ggplot2::aes(x = stats::reorder(species, -prop_community), y = prop_community)
      ) +
        ggplot2::geom_col(fill = "#d95f02", color = "black", width = 0.7) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        ggplot2::labs(
          title = "Exotic Species Detected",
          x = NULL,
          y = "Proportion of Total Community"
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "italic", size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          axis.title = ggplot2::element_text(size = 11),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"),
          legend.position = "none"
        )
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No exotic species detected") +
        ggplot2::theme_void()
    }
  }

  if (plot) print(p)

  # Short message with references (without full citations)
  if (verbose) {
    message("\n********************************************************************************")
    if (nrow(exotic_species) > 0) {
      message("EXOTIC SPECIES DETECTED:")
      print(exotic_species[, c("species", "abundance", "percentage")])
    } else {
      message("No exotic species from the target list were detected in this community.")
    }
    message("\nDATA SOURCE:")
    message("Vieira, V. B. (2025). 'Quem s\u00e3o e onde est\u00e3o as formigas ex\u00f3ticas do Brasil?'")
    message("Master's thesis, Universidade Federal do Paran\u00e1, Curitiba, Brazil.")
    message("Full reference available in the package documentation: ?check_exotic_ants")
    message("********************************************************************************")
  }

  invisible(list(table = exotic_species, plot = p))
}
