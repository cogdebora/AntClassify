#' @title Identify Ant Rarity Forms (Atlantic Forest)
#' @description Checks a community matrix for ant rarity forms in the Brazilian Atlantic Forest based on Silva et al. (2024).
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays a plot (type controlled by \code{plot_type}).
#' @param plot_type Character; type of plot to display. \code{"status"} (default) shows a bar plot of rarity form
#'   abundance proportions. \code{"species"} shows a bar plot of individual rare species abundances as a proportion
#'   of the total community, colored by rarity form.
#' @param validate Logical; if \code{TRUE}, validates species names using GBIF before analysis.
#' @param delay Numeric; seconds to wait between GBIF API calls when \code{validate = TRUE}.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col labs theme_classic theme element_text element_blank element_line scale_y_continuous
#' @importFrom scales percent_format
#' @importFrom stats reorder
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing rare species detected, with columns species, rarity_form, abundance, and percentage.}
#'   \item{plot}{A ggplot2 object.}
#' @export
check_rarity_atlantic_ants <- function(comm, verbose = TRUE, plot = TRUE,
                                       plot_type = c("status", "species"),
                                       validate = TRUE, delay = 0.5) {

  plot_type <- match.arg(plot_type)

  if (isTRUE(validate)) {
    comm <- validate_species_names(comm, verbose = verbose, delay = delay)
  }

  if (verbose) {
    message("\n********************************************************************************")
    message("ATTENTION:")
    message("This function identifies rare ant species and their rarity forms.")
    message("This classification is specific to the Brazilian Atlantic Forest and")
    message("may not be appropriate for other biomes.")
    message("********************************************************************************\n")
  }

  if (verbose) message("Step 1: Preparing community data and rarity database...")

  rarity_db <- data.frame(
    species = c(
      "Acanthoponera mucronata", "Acropyga guianensis", "Alfaria minuta", "Brachymyrmex coactus",
      "Camponotus trapezoideus", "Ectatomma tuberculatum", "Eurhopalothrix bruchi", "Leptogenys unistimulosa",
      "Mayaponera arhuaca", "Megalomyrmex incisus", "Neivamyrmex punctaticeps", "Oxyepoecus longicephalus",
      "Oxyepoecus reticulatus", "Procryptocerus convergens", "Pseudomyrmex tenuis", "Pseudoponera gilberti",
      "Pseudoponera stigma", "Strumigenys cultrigera", "Strumigenys gytha", "Strumigenys precava",
      "Strumigenys schulzi", "Strumigenys smithii", "Thaumatomyrmex atrox", "Wasmannia scrobifera",
      "Apterostigma acre", "Camponotus atriceps", "Camponotus blandus", "Camponotus crassus",
      "Camponotus lespesii", "Camponotus senex", "Crematogaster arata", "Crematogaster rochai",
      "Cyphomyrmex major", "Dorymyrmex brunneus", "Hypoponera distinguenda", "Linepithema leucomelas",
      "Linepithema micans", "Mycocepurus goeldii", "Mycocepurus smithii", "Myrmelachista arthuri",
      "Myrmelachista catharinae", "Myrmelachista ruzskyi", "Neivamyrmex pseudops", "Ochetomyrmex subpolitus",
      "Octostruma balzani", "Oxyepoecus plaumanni", "Pheidole aberrans", "Pheidole fimbriata",
      "Pheidole obscurithorax", "Pheidole subarmata", "Pogonomyrmex naegelii", "Pseudomyrmex gracilis",
      "Pseudomyrmex phyllophilus", "Pseudomyrmex schuppi", "Solenopsis geminata", "Thaumatomyrmex mutilatus",
      "Typhlomyrmex rogenhoferi", "Wasmannia rochai", "Acropyga xexsanguis", "Acropyga panamensis",
      "Basiceros convexiceps", "Brachymyrmex patagonicus", "Camponotus canescens", "Cephalotes angustus",
      "Crematogaster corticicola", "Cylindromyrmex brasiliensis", "Cyphomyrmex transversus", "Eciton quadriglume",
      "Ectatomma brunneum", "Ectatomma permagnum", "Eurhopalothrix gravis", "Gnamptogenys haenschi",
      "Gnamptogenys interrupta", "Gnamptogenys sulcata", "Holcoponera pleurodon", "Labidus mars",
      "Linepithema pulex", "Mycetarotes carinatus", "Mycetarotes parallelus", "Mycetomoellerius urichii",
      "Mycetophylax olitor", "Neoponera unidentata", "Nesomyrmex echinatinodis", "Oxyepoecus bruchi",
      "Oxyepoecus vezenyii", "Procryptocerus regularis", "Pseudomyrmex oculatus", "Rasopone lunaris",
      "Rogeria subarmata", "Strumigenys carinithorax", "Strumigenys epelys", "Strumigenys fridericimuelleri",
      "Brachymyrmex feitosai", "Heteroponera inermis", "Lachnomyrmex victori", "Megalomyrmex iheringi",
      "Megalomyrmex myops", "Mycetophylax plaumanni", "Oxyepoecus crassinodus", "Strumigenys splendens",
      "Typhlomyrmex reichenspergeri", "Phalacromyrmex fugax", "Poneracantha lucaris", "Strumigenys dapsilis",
      "Strumigenys reticeps", "Lachnomyrmex nordestinus", "Megalomyrmex pusillus", "Oxyepoecus browni",
      "Sphinctomyrmex stali", "Strumigenys sanctipauli", "Brachymyrmex micromegas", "Camponotus tripartitus",
      "Diaphoromyrma sofiae", "Eurhopalothrix speciosa", "Heteroponera robusta", "Mycetarotes senticosus",
      "Mycetophylax auritus", "Neoponera metanotalis", "Strumigenys dentinasis", "Strumigenys substricta"
    ),
    rarity_form = c(
      rep("Form 1", 24), rep("Form 2", 34), rep("Form 3", 34), rep("Form 4", 9),
      rep("Form 5", 4), rep("Form 6", 5), rep("Form 7", 10)
    ),
    stringsAsFactors = FALSE
  )

  dados_numericos <- as.data.frame(comm)
  nomes_limpos <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(dados_numericos))))
  colnames(dados_numericos) <- nomes_limpos

  rarity_db$species <- trimws(rarity_db$species)

  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = as.numeric(colSums(dados_numericos, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  df <- merge(df, rarity_db, by = "species", all.x = TRUE)
  df$rarity_form[is.na(df$rarity_form)] <- "Common"

  df$percentage <- (df$abundance / total_abs) * 100

  if (verbose) message("Step 2: Generating results...")

  res_plot <- dplyr::group_by(df, rarity_form)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  raras_detectadas <- df[df$rarity_form != "Common", ]
  rownames(raras_detectadas) <- NULL

  # --- Generate plot based on plot_type (ALWAYS create) ---
  p <- NULL

  if (plot_type == "status") {
    p <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(rarity_form, prop), y = prop, fill = rarity_form)) +
      ggplot2::geom_col(color = "black", width = 0.7) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Ant Rarity Forms Distribution",
        x = "Rarity Classification",
        y = "Proportion"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 9),
        axis.title = ggplot2::element_text(size = 11),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
        panel.grid = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "black")
      )
  } else { # plot_type == "species"
    if (nrow(raras_detectadas) > 0) {
      rare_plot_data <- raras_detectadas
      rare_plot_data$prop_community <- rare_plot_data$abundance / total_abs

      p <- ggplot2::ggplot(
        rare_plot_data,
        ggplot2::aes(x = stats::reorder(species, -prop_community), y = prop_community, fill = rarity_form)
      ) +
        ggplot2::geom_col(color = "black", width = 0.7) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        ggplot2::labs(
          title = "Rare Species Abundance by Rarity Form",
          x = NULL,
          y = "Proportion of Total Community",
          fill = "Rarity Form"
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "italic", size = 9),
          axis.text.y = ggplot2::element_text(size = 9),
          axis.title = ggplot2::element_text(size = 11),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black"),
          legend.position = "right"
        )
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No rare species detected") +
        ggplot2::theme_void()
    }
  }

  if (plot) print(p)

  if (verbose) {
    message("\n********************************************************************************")
    if (nrow(raras_detectadas) > 0) {
      message("RARE SPECIES DETECTED:")
      print(raras_detectadas[, c("species", "rarity_form", "abundance", "percentage")])
    } else {
      message("No rare species from the target list were detected.")
    }
    message("\nDATA SOURCE AND REFERENCE:")
    message("Silva, N. S., Maciel, E. A., Prado, L. P., Silva, O. G., Barbosa, D. A.,")
    message("Andrade-Silva, J., ... & Morini, M. S. (2024).")
    message("'Ant rarity and vulnerability in Brazilian Atlantic Forest fragments.'")
    message("Biological Conservation, 296, 110640.")
    message("********************************************************************************")
  }

  invisible(list(table = raras_detectadas, plot = p))
}
