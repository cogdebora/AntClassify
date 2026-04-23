#' @title Identify Endemic Ant Species (Atlantic Forest)
#' @description Checks a community matrix for ant species endemic to the Brazilian Atlantic Forest based on Silva et al. (2025).
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays a plot (type controlled by \code{plot_type}).
#' @param plot_type Character; type of plot to display. \code{"status"} (default) shows a bar plot comparing
#'   endemic vs. other species abundance proportions. \code{"species"} shows a bar plot of individual endemic
#'   species abundances as a proportion of the total community.
#' @param validate Logical; if \code{TRUE}, validates species names using GBIF before analysis.
#' @param delay Numeric; seconds to wait between GBIF API calls when \code{validate = TRUE}.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_fill_manual labs theme_classic theme element_text element_blank element_line
#' @importFrom scales percent_format
#' @importFrom stats reorder
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing endemic species detected, with columns species, abundance, and percentage.}
#'   \item{plot}{A ggplot2 object.}
#' @export
check_endemic_atlantic_ants <- function(comm, verbose = TRUE, plot = TRUE,
                                        plot_type = c("status", "species"),
                                        validate = TRUE, delay = 0.5) {

  plot_type <- match.arg(plot_type)

  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) {
    message("\n**********************************************************************************")
    message("ATTENTION: This function identifies endemic species of the BRAZILIAN ATLANTIC FOREST.")
    message("If your data collection did not occur within this biome, this analysis may not")
    message("be appropriate for your study.")
    message("**********************************************************************************\n")
  }

  if (verbose) message("Step 1: Preparing community data...")

  endemic_list <- c(
    "Acanthostichus flexuosus", "Apterostigma serratum", "Brachymyrmex delabiei",
    "Brachymyrmex feitosai", "Camponotus fallatus", "Camponotus hermanni",
    "Camponotus xantogaster", "Carebara nuda", "Carebara pilosa",
    "Crematogaster montana", "Crematogaster stigmatica", "Discothyrea bobi",
    "Eurhopalothrix depressa", "Fulakora cleae", "Heteroponera angulata",
    "Hylomyrma villemantae", "Hypoponera leninei", "Leptanilloides atlanticus",
    "Leptogenys academica", "Leptogenys carioca", "Monomorium delabiei",
    "Mycetagroicus urbanus", "Mycocepurus castrator", "Myrmelachista reticulata",
    "Neoponera concava", "Neoponera latinoda", "Oxyepoecus vivax",
    "Pheidole abakytan", "Pheidole borgmeieri", "Pheidole brunnescens",
    "Pheidole cangussu", "Pheidole cardinalis", "Pheidole curupira",
    "Pheidole eidmanni", "Pheidole foederalis", "Pheidole moseni",
    "Pheidole praeses", "Pheidole protaxi", "Poneracantha wilsoni",
    "Procryptocerus curvistriatus", "Procryptocerus lenkoi", "Pseudomyrmex goeldii",
    "Rhopalothrix plaumanni", "Rogeria lacertosa", "Sphinctomyrmex schoerederi",
    "Strumigenys dentinasis", "Strumigenys substricta", "Tapinoma gibbosum",
    "Typhlomyrmex lavra"
  )

  dados_numericos <- as.data.frame(comm)
  nomes_limpos <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(dados_numericos))))
  colnames(dados_numericos) <- nomes_limpos

  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = as.numeric(colSums(dados_numericos, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  df$percentage <- (df$abundance / total_abs) * 100
  df$status <- ifelse(df$species %in% endemic_list, "Endemic (AF)", "Other/Not Listed")

  if (verbose) message("Step 2: Generating results...")

  res_plot <- dplyr::group_by(df, status)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  endemicas_detectadas <- df[df$status == "Endemic (AF)", ]
  rownames(endemicas_detectadas) <- NULL

  # --- Generate plot based on plot_type (ALWAYS create) ---
  p <- NULL

  if (plot_type == "status") {
    p <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(status, prop), y = prop, fill = status)) +
      ggplot2::geom_col(color = "black", width = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_fill_manual(values = c("Endemic (AF)" = "#2c7bb6", "Other/Not Listed" = "#abd9e9")) +
      ggplot2::labs(
        title = "Proportion of Atlantic Forest Endemic Species",
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
    if (nrow(endemicas_detectadas) > 0) {
      endemic_plot_data <- endemicas_detectadas
      endemic_plot_data$prop_community <- endemic_plot_data$abundance / total_abs

      p <- ggplot2::ggplot(
        endemic_plot_data,
        ggplot2::aes(x = stats::reorder(species, -prop_community), y = prop_community)
      ) +
        ggplot2::geom_col(fill = "#2c7bb6", color = "black", width = 0.7) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        ggplot2::labs(
          title = "Endemic Species of the Atlantic Forest",
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
        ggplot2::annotate("text", x = 1, y = 1, label = "No endemic species detected") +
        ggplot2::theme_void()
    }
  }

  if (plot) print(p)

  if (verbose) {
    message("\n********************************************************************************")
    if (nrow(endemicas_detectadas) > 0) {
      message("ENDEMIC SPECIES DETECTED (ATLANTIC FOREST):")
      print(endemicas_detectadas[, c("species", "abundance", "percentage")])
    } else {
      message("No species from the Atlantic Forest endemic list were detected in this community.")
    }
    message("\nDATA SOURCE AND REFERENCE:")
    message("The endemic species list is sourced from:")
    message("Silva, N. S., Goncalves, D. C. de O., Wazema, C. T., Barbosa, D. A., Prado, L. P. do,")
    message("Andrade-Silva, J., Fernandes, T. T., Silva, R. R., & Morini, M. S. de C. (2025).")
    message("'Endemism and vulnerability of ants in the phytophysiognomies of the Brazilian")
    message("Atlantic Forest'. In: Brazilian Myrmecology: Exploring the World\u2019s Richest Ant Fauna")
    message("(Chapter 16). Editora Cientifica Digital. DOI: 10.37885/250920259.")
    message("********************************************************************************")
  }

  invisible(list(table = endemicas_detectadas, plot = p))
}
