#' @title Assign Functional Guilds for Ants
#' @description Classifies ants into functional guilds using four different ecological databases.
#' @param comm A community matrix where species are columns and samples are rows.
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal theme scale_y_continuous scale_x_discrete element_text
#' @importFrom stringr str_split_fixed str_wrap
#' @importFrom stats reorder
#' @importFrom scales percent
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export
assign_guild_ants <- function(comm) {



  message("Step 1: Preparing community data...")
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

  message("Step 2: Matching species to functional guilds...")
  df$antclassify_guild <- generic_db$guild[match(genus_list, generic_db$target)]
  df$silva_guild <- silva_db$guild[match(df$species, silva_db$target)]
  df$silva_guild[is.na(df$silva_guild)] <- silva_db$guild[match(genus_list[is.na(df$silva_guild)], silva_db$target)]
  df$delabie_guild <- delabie_db$guild[match(genus_list, delabie_db$target)]
  df$silvestre_guild <- silvestre_db$guild[match(df$species, silvestre_db$target)]
  df$silvestre_guild[is.na(df$silvestre_guild)] <- silvestre_db$guild[match(genus_list[is.na(df$silvestre_guild)], silvestre_db$target)]

  guild_cols <- c("antclassify_guild", "silva_guild", "delabie_guild", "silvestre_guild")
  df[guild_cols] <- lapply(df[guild_cols], function(x) ifelse(is.na(x), "Unidentified Guild", x))

  message("\nGuild classification results:")
  print(df)

  message("Step 3: Generating plots...")
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

  print(p1); if(interactive()) readline(prompt="\nPress [Enter] for next graphic...")
  print(p2); if(interactive()) readline(prompt="\nPress [Enter] for next graphic...")
  print(p3); if(interactive()) readline(prompt="\nPress [Enter] for next graphic...")
  print(p4)

  cat("\n********************************************************************************\n")
  cat("IMPORTANT NOTICE:\n")
  cat("Please verify all assigned guilds. 'Unidentified Guild' indicates that the\n")
  cat("taxon was not found in the available reference databases.\n\n")

  cat("Guild classification in this analysis follows:\n")
  cat("- Literature-based criteria from:\n")
  cat("  Silvestre, R., Brandão, C. R. F., & Silva, R. R. (2003).\n")
  cat("  Functional groups of ants: the case of Cerrado guilds.\n")
  cat("  In F. Fernández (Ed.), Introduction to the ants of the tropical region (pp. 113–148).\n\n")

  cat("  Silva, R. R., Silvestre, R., Brandão, C. R. F., Morini, M. S. C., & Delabie, J. H. C. (2015).\n")
  cat("  Trophic groups and guilds in poneromorph ants. Sociobiology, 62(3), 321–329.\n\n")

  cat("  Delabie, J. H. C., Agosti, D., & Nascimento, I. C. (2000).\n")
  cat("  Litter ant communities of the Brazilian Atlantic rain forest:\n")
  cat("  a comparison of two methodologies. Revista Brasileira de Entomologia, 44(1/2), 1–10.\n\n")

  cat("- The 'AntClassify Guilds' classification corresponds to the internal\n")
  cat("  classification system implemented in the AntClassify package.\n")

  cat("********************************************************************************\n")

  return(invisible(list(table = df, plots = list(p1, p2, p3, p4))))
}

