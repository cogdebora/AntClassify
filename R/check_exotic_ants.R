#' @title Identify Exotic Ant Species (Brazil)
#' @description Checks a community matrix for known exotic ant species in Brazil sourced from Vieira (2025).
#' @param comm A community matrix where species are columns and samples are rows.
#' @export
check_exotic_ants <- function(comm) {

  message("Step 1: Preparing community data...")

  # Official list of exotic species from Vieira (2025)
  exotic_list <- c(
    "Tapinoma melanocephalum", "Technomyrmex vitiensis", "Paratrechina longicornis",
    "Cardiocondyla emeryi", "Cardiocondyla minutior", "Cardiocondyla obscurior",
    "Cardiocondyla wroughtonii", "Monomorium floricola", "Monomorium pharaonis",
    "Pheidole megacephala", "Strumigenys emmae", "Strumigenys rogeri",
    "Tetramorium bicarinatum", "Tetramorium caldarium", "Tetramorium lanuginosum",
    "Tetramorium lucayanum", "Tetramorium simillimum", "Trichomyrmex destructor",
    "Leptogenys maxillosa"
  )

  # Data processing
  dados_numericos <- as.data.frame(comm)
  nomes_limpos <- trimws(gsub("_", " ", gsub("\\.", " ", colnames(dados_numericos))))
  colnames(dados_numericos) <- nomes_limpos

  total_abs <- sum(colSums(dados_numericos, na.rm = TRUE))

  # Creating the dataframe
  df <- data.frame(
    species = colnames(dados_numericos),
    abundance = as.numeric(colSums(dados_numericos, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  df$percentage <- (df$abundance / total_abs) * 100
  df$origin <- ifelse(df$species %in% exotic_list, "Exotic", "Native/Not Listed")

  message("Step 2: Generating results...")

  # Summary for plot
  res_plot <- dplyr::group_by(df, origin)
  res_plot <- dplyr::summarise(res_plot, total = sum(abundance), .groups = "drop")
  res_plot <- dplyr::mutate(res_plot, prop = total / sum(total))

  p1 <- ggplot2::ggplot(res_plot, ggplot2::aes(x = stats::reorder(origin, prop), y = prop, fill = origin)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_manual(values = c("Exotic" = "#d95f02", "Native/Not Listed" = "#1b9e77")) +
    ggplot2::labs(
      title = "Proportion of Exotic vs. Native Species",
      subtitle = "Based on total abundance in the community",
      x = "Status",
      y = "Proportion"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")


  exoticas_detectadas <- df[df$origin == "Exotic", ]
  rownames(exoticas_detectadas) <- NULL

  cat("\n********************************************************************************\n")
  if(nrow(exoticas_detectadas) > 0) {
    cat("EXOTIC SPECIES DETECTED:\n")
    print(exoticas_detectadas[, c("species", "abundance", "percentage")])
  } else {
    cat("No exotic species from the target list were detected in this community.\n")
  }

  # --- REFERENCE ---
  cat("\nDATA SOURCE AND REFERENCE:\n")
  cat("The exotic species list used in this function is sourced from:\n")
  cat("VIEIRA, Vitoria Brunetta. 'Quem s\u00e3o e onde est\u00e3o as formigas ex\u00f3ticas do Brasil?'\n")
  cat("Dissertacao (Mestrado em Entomologia) \u2013 Universidade Federal do Parana (UFPR),\n")
  cat("Curitiba, Brasil, 2025.\n")
  cat("********************************************************************************\n")


  print(p1)


  return(invisible(list(table = exoticas_detectadas, plot = p1)))
}
