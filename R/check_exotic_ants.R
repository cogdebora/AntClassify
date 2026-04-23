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

  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) message("Step 1: Preparing community data...")

  exotic_list <- c(
    "Tapinoma melanocephalum", "Technomyrmex vitiensis", "Paratrechina longicornis",
    "Cardiocondyla emeryi", "Cardiocondyla minutior", "Cardiocondyla obscurior",
    "Cardiocondyla wroughtonii", "Monomorium floricola", "Monomorium pharaonis",
    "Pheidole megacephala", "Strumigenys emmae", "Strumigenys rogeri",
    "Tetramorium bicarinatum", "Tetramorium caldarium", "Tetramorium lanuginosum",
    "Tetramorium lucayanum", "Tetramorium simillimum", "Trichomyrmex destructor",
    "Leptogenys maxillosa"
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
  df$origin <- ifelse(df$species %in% exotic_list, "Exotic", "Native/Not Listed")

  if (verbose) message("Step 2: Generating results...")

  res_plot <- dplyr::group_by(df, origin)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  exoticas_detectadas <- df[df$origin == "Exotic", ]
  rownames(exoticas_detectadas) <- NULL

  # --- Generate plot based on plot_type (ALWAYS create) ---
  p <- NULL

  if (plot_type == "status") {
    p <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(origin, prop), y = prop, fill = origin)) +
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
    if (nrow(exoticas_detectadas) > 0) {
      exotic_plot_data <- exoticas_detectadas
      exotic_plot_data$prop_community <- exotic_plot_data$abundance / total_abs

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

  if (verbose) {
    message("\n********************************************************************************")
    if (nrow(exoticas_detectadas) > 0) {
      message("EXOTIC SPECIES DETECTED:")
      print(exoticas_detectadas[, c("species", "abundance", "percentage")])
    } else {
      message("No exotic species from the target list were detected in this community.")
    }
    message("\nDATA SOURCE AND REFERENCE:")
    message("The exotic species list used in this function is sourced from:")
    message("VIEIRA, Vitoria Brunetta. 'Quem s\u00e3o e onde est\u00e3o as formigas ex\u00f3ticas do Brasil?'")
    message("Dissertacao (Mestrado em Entomologia) \u2013 Universidade Federal do Parana (UFPR),")
    message("Curitiba, Brasil, 2025.")
    message("********************************************************************************")
  }

  invisible(list(table = exoticas_detectadas, plot = p))
}
