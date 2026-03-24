#' @title Identify Endemic Ant Species (Atlantic Forest)
#' @description Checks a community matrix for ant species endemic to the Brazilian Atlantic Forest based on Silva et al. (2025).
#' @param comm A community matrix where species are columns and samples are rows.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme element_text scale_y_continuous scale_fill_manual
#' @importFrom stringr str_wrap
#' @importFrom stats reorder
#' @importFrom scales percent
#' @export
check_endemic_atlantic_ants <- function(comm) {

  # --- WARNING ---
  cat("\n**********************************************************************************\n")
  cat("ATTENTION: This function identifies endemic species of the BRAZILIAN ATLANTIC FOREST.\n")
  cat("If your data collection did not occur within this biome, this analysis may not\n")
  cat("be appropriate for your study.\n")
  cat("**********************************************************************************\n\n")

  message("Step 1: Preparing community data...")

  # Official list of endemic species (Silva et al., 2025)
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

  # Data processing: cleanup names and handle matrix dimensions
  dados_numericos <- as.data.frame(comm)
  nomes_limpos <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(dados_numericos))))
  colnames(dados_numericos) <- nomes_limpos

  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  # Create work dataframe
  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = as.numeric(colSums(dados_numericos, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  # Calculations
  df$percentage <- (df$abundance / total_abs) * 100
  df$status <- ifelse(df$species %in% endemic_list, "Endemic (AF)", "Other/Not Listed")

  message("Step 2: Generating results...")

  # Summary for plot (Avoiding pipe syntax for stability)
  res_plot <- dplyr::group_by(df, status)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  # Plotting with cool blue tones
  p1 <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(status, prop), y = prop, fill = status)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = c("Endemic (AF)" = "#2c7bb6", "Other/Not Listed" = "#abd9e9")) +
    ggplot2::labs(
      title = "Proportion of Atlantic Forest Endemic Species",
      subtitle = "Based on total abundance in the community",
      x = "Status",
      y = "Proportion (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", axis.text = ggplot2::element_text(size = 10))

  # Filtering for return
  endemicas_detectadas <- df[df$status == "Endemic (AF)", ]
  rownames(endemicas_detectadas) <- NULL

  cat("\n********************************************************************************\n")
  if(nrow(endemicas_detectadas) > 0) {
    cat("ENDEMIC SPECIES DETECTED (ATLANTIC FOREST):\n")
    print(endemicas_detectadas[, c("species", "abundance", "percentage")])
  } else {
    cat("No species from the Atlantic Forest endemic list were detected in this community.\n")
  }

  # --- REFERENCE  ---
  cat("\nDATA SOURCE AND REFERENCE:\n")
  cat("The endemic species list is sourced from:\n")
  cat("Silva, N. S., Goncalves, D. C. de O., Wazema, C. T., Barbosa, D. A., Prado, L. P. do,\n")
  cat("Andrade-Silva, J., Fernandes, T. T., Silva, R. R., & Morini, M. S. de C. (2025).\n")
  cat("'Endemism and vulnerability of ants in the phytophysiognomies of the Brazilian\n")
  cat("Atlantic Forest'. In: Brazilian Myrmecology: Exploring the World\u2019s Richest Ant Fauna\n")
  cat("(Chapter 16). Editora Cientifica Digital. DOI: 10.37885/250920259.\n")
  cat("********************************************************************************\n")


  print(p1)


  return(invisible(list(table = endemicas_detectadas, plot = p1)))
}



