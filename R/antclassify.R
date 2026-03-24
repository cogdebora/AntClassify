#' @title AntClassify Full Ecological Pipeline
#' @description Runs all ecological classification functions of the package.
#' @param comm Community matrix (samples x species).
#' @return A list containing results from all analyses.
#' @export

antclassify <- function(comm) {

  message("*************************************************")
  message("Starting AntClassify")
  message("*************************************************")



    if (!is.data.frame(comm)) {
      stop("Erro: input deve ser um data.frame.")
    }

    if (!all(sapply(comm, is.numeric))) {
      stop("Error: all columns should be numerical (species abundance).")
    }



  results <- list()

  message("\n>>> Running guild classification")
  results$guilds <- assign_guild_ants(comm)

  message("\n>>> Checking exotic species")
  results$exotic <- check_exotic_ants(comm)

  message("\n>>> Checking Atlantic Forest endemic species")
  results$endemic <- check_endemic_atlantic_ants(comm)

  message("\n>>> Checking Atlantic Forest rarity patterns")
  results$rarity <- check_rarity_atlantic_ants(comm)

  message("\n***********************************************")
  message("AntClassify finished successfully")
  message("*************************************************")

  return(invisible(results))
}
