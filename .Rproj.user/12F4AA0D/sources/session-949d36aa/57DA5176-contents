# AntClassify

<!-- badges: start -->
[![R-CMD-check](https://github.com/cogdebora/AntClassify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cogdebora/AntClassify/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

AntClassify is an R package designed to **standardize ant community analyses**, particularly for Neotropical and Brazilian Atlantic Forest assemblages. It automates:

- Classification of species into **functional guilds** based on trophic strategies and foraging behavior. The package offers two approaches: (1) classification using established criteria from the literature (Delabie et al., 2000; Silvestre et al., 2003; Silva et al., 2015), and (2) a **built-in classification** derived from urban ant communities.
- Identification of **exotic species** recorded in Brazil (Vieira, 2025).
- Identification of **endemic species** of the Atlantic Forest (Silva et al., 2025).
- Classification of **rarity** based on geographic distribution and local abundance (Silva et al., 2024).

By automating these tasks, AntClassify reduces manual effort and increases reproducibility, making it a practical tool for researchers working with ant assemblages.

## Installation

You can install the **development version** of AntClassify from GitHub:

```r
# install.packages("remotes")
remotes::install_github("cogdebora/AntClassify")
```

Once the package is accepted on CRAN, you will also be able to install it with:

```r
install.packages("AntClassify")
```

## Example

Below is a reproducible example using a standardized test dataset:

```r
library(AntClassify)

# Create test dataset (same structure used in package tests)
ant_test_data <- data.frame(
  "Pheidole megacephala" = 10,
  "Strumigenys emmae" = 5,
  "Paratrechina longicornis" = 8,
  "Hypoponera leninei" = 3,
  "Camponotus fallatus" = 2,
  "Ectatomma brunneum" = 1,
  "Ectatomma permagnum" = 1,
  "Pheidole aberrans" = 1,
  "Pheidole fimbriata" = 1,
  "Pheidole obscurithorax" = 1,
  check.names = FALSE
)

# Run full pipeline
results <- antclassify(ant_test_data)

# View outputs
results$exotic$table
results$endemic$table
results$rarity$table

# Plot outputs
print(results$exotic$plot)
print(results$endemic$plot)
print(results$rarity$plot)
```

For more detailed examples and function documentation, see the package vignettes:

```r
vignette("AntClassify")
```

## Citation

If you use AntClassify in your research, please cite the following references:

- Silva, N. S., Maciel, E. A., Prado, L. P., Silva, O. G., Barbosa, D. A., Andrade-Silva, J., ... & Morini, M. S. (2024). Ant rarity and vulnerability in Brazilian Atlantic Forest fragments. *Biological Conservation*, 296, 110640. DOI: https://doi.org/10.1016/j.biocon.2024.110640

- Silva, N. S., Gonçalves, D. C. de O., Wazema, C. T., Barbosa, D. A., Prado, L. P. do, Andrade-Silva, J., Fernandes, T. T., Silva, R. R., & Morini, M. S. de C. (2025). Endemism and vulnerability of ants in the phytophysiognomies of the Brazilian Atlantic Forest. In *Brazilian Myrmecology: Exploring the World’s Richest Ant Fauna* (Cap. 16, pp. 371–394). Editora Científica Digital. DOI: https://doi.org/10.37885/250920259

- Vieira, V. B. (2024). *Quem são e onde estão as formigas exóticas do Brasil?* [Dissertação de Mestrado, Universidade Federal do Paraná]. Curitiba, PR, Brasil.

- Silvestre, R., Brandão, C. R. F., & Silva, R. R. (2003). Grupos funcionales de hormigas: el caso de los gremios del Cerrado. In F. Fernández (Ed.), *Introducción a las Hormigas de las Región Neotropical* (pp. 113–148). Instituto Alexander Von Humboldt.

- Silva, R. R., Silvestre, R., Brandão, C. R. F., Morini, M. S. C., & Delabie, J. H. C. (2015).  Grupos trófi cos e guildas em formigas poneromorfas. In: Delabie, Jacques H. C. et al.  *As formigas poneromorfas do Brasil*. Ilhéus: Editus, 2015. p. 163-179.

- Delabie, J. H. C., Agosti, D., & Nascimento, I. C. (2000). Litter ant communities of the Brazilian Atlantic rain
forest region. *Sampling Ground-dwelling Ants: case studies from the world’s rain forests. Curtin University of Technology School of Environmental Biology Bulletin*,v. 18.

Additionally, if you use the package itself, please cite:

- Gonçalves, D. C. O., et al. (2026). *AntClassify: An R package for ant community analysis* (Version 0.1.0) [Computer software].  
  https://github.com/cogdebora/AntClassify


