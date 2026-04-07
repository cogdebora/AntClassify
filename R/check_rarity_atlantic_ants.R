#' @title Identify Ant Rarity Forms (Atlantic Forest)
#' @description Checks a community matrix for ant rarity forms in the Brazilian Atlantic Forest based on Silva et al. (2024).
#' @param comm A community matrix where species are columns and samples are rows.
#' @param verbose Logical; if \code{TRUE}, displays progress messages.
#' @param plot Logical; if \code{TRUE}, displays the plot of rarity forms distribution.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme scale_y_continuous
#' @importFrom stats reorder
#' @importFrom scales percent
#' @return Invisibly returns a list with two elements:
#'   \item{table}{A data frame containing rare species detected, with columns species, rarity_form, abundance, and percentage.}
#'   \item{plot}{A ggplot2 object showing the distribution of rarity forms.}
#' @examples
#' # Create a small example community matrix
#' species_list <- c(
#'   "Ectatomma brunneum", "Pheidole aberrans", "Camponotus crassus",
#'   "Solenopsis saevissima", "Pachycondyla striata"
#' )
#' set.seed(123)
#' comm_data <- matrix(
#'   rpois(length(species_list) * 3, lambda = 2),
#'   nrow = 3,
#'   ncol = length(species_list),
#'   dimnames = list(paste0("sample", 1:3), species_list)
#' )
#' result <- check_rarity_atlantic_ants(comm_data, verbose = FALSE, plot = FALSE)
#' head(result$table)
#' @export
check_rarity_atlantic_ants <- function(comm, verbose = TRUE, plot = TRUE, validate = TRUE, delay = 0.5) {

  # --- Optional validation of species names using GBIF ---
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

  # (Silva et al. 2024)
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

  # Data processing
  dados_numericos <- as.data.frame(comm)
  nomes_limpos <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(dados_numericos))))
  colnames(dados_numericos) <- nomes_limpos

  rarity_db$species <- trimws(rarity_db$species)

  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  # Summary table
  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = as.numeric(colSums(dados_numericos, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  # Merge with rarity database
  df <- merge(df, rarity_db, by = "species", all.x = TRUE)
  df$rarity_form[is.na(df$rarity_form)] <- "Common"

  df$percentage <- (df$abundance / total_abs) * 100

  if (verbose) message("Step 2: Generating results...")

  # Plotting
  res_plot <- dplyr::group_by(df, rarity_form)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  p1 <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(rarity_form, prop), y = prop, fill = rarity_form)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Ant Rarity Forms Distribution",
      subtitle = "Based on total abundance in the community",
      x = "Rarity Classification",
      y = "Proportion (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  # Filter for return
  raras_detectadas <- df[df$rarity_form != "Common", ]
  rownames(raras_detectadas) <- NULL

  if (verbose) {
    message("\n********************************************************************************")
    if (nrow(raras_detectadas) > 0) {
      message("RARE SPECIES DETECTED:")
      print(raras_detectadas[, c("species", "rarity_form", "abundance", "percentage")])
    } else {
      message("No rare species from the target list were detected.")
    }

    # --- REFERENCE ---
    message("\nDATA SOURCE AND REFERENCE:")
    message("Silva, N. S., Maciel, E. A., Prado, L. P., Silva, O. G., Barbosa, D. A.,")
    message("Andrade-Silva, J., ... & Morini, M. S. (2024).")
    message("'Ant rarity and vulnerability in Brazilian Atlantic Forest fragments.'")
    message("Biological Conservation, 296, 110640.")
    message("********************************************************************************")
  }

  if (plot) print(p1)

  invisible(list(table = raras_detectadas, plot = p1))
}
