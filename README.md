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

Below is a reproducible example using a standardized test dataset. To keep the demonstration fast, we set validate = FALSE; you can enable GBIF validation by using validate = TRUE.

```r
library(AntClassify)

# Create example dataset (35 species)
data <- data.frame(
  Atta_sexdens = 50,
  Camponotus_atriceps = 40,
  Crematogaster_sp = 35,
  Cyphomyrmex_minutus = 30,
  Cyphomyrmex_rimosus = 28,
  Ectatomma_edentatum = 25,
  Heteroponera_mayri = 22,
  Holcoponera_striatula = 20,
  Monomorium_floricola = 18,
  Monomorium_pharaonis = 17,
  Pheidole_megacephala = 16,
  Strumigenys_emmae = 15,
  Strumigenys_rogeri = 14,
  Nylanderia_fulva = 13,
  Odontomachus_chelifer = 12,
  Oxyepoecus_reticulatus = 11,
  Pachycondyla_striata = 10,
  Apterostigma_serratum = 9,
  Brachymyrmex_delabiei = 8,
  Brachymyrmex_feitosai = 7,
  Camponotus_fallatus = 6,
  Camponotus_hermanni = 5,
  Camponotus_xanthogaster = 4,
  Pheidole_aberrans = 3,
  Pheidole_fimbriata = 3,
  Pheidole_obscurithorax = 2,
  Pheidole_subarmata = 2,
  Strumigenys_fridericimuelleri = 2,
  Heteroponera_inermis = 2,
  Oxyepoecus_browni = 2,
  Sphinctomyrmex_stali = 1,
  Strumigenys_sanctipauli = 1,
  Brachymyrmex_micromegas = 1,
  Camponotus_tripartitus = 1,
  Diaphoromyrma_sofiae = 1
)

# Convert underscores to spaces in species names
colnames(data) <- gsub("_", " ", colnames(data))

# Run full pipeline (validation disabled for speed)
results <- antclassify(data, validate = FALSE, plot = FALSE)

# View outputs
names(results)
head(results$guilds$table)
results$exotic$table
results$endemic$table
results$rarity$table
```

## Generating plots

To generate plots, set plot = TRUE in the individual functions or in antclassify().

```r
# Distribution of rarity forms
check_rarity_atlantic_ants(data, validate = FALSE, plot = TRUE, plot_type = "status")
```

```r
# Rare species abundance by rarity form
check_rarity_atlantic_ants(data, validate = FALSE, plot = TRUE, plot_type = "species")
```

For more detailed examples and function documentation, see the package vignettes:

```r
vignette("antclassify_workflow", package = "AntClassify")
```
## Citation

If you use AntClassify in your research, please cite the following references:

Silva, N. S., Maciel, E. A., Prado, L. P., Silva, O. G., Barbosa, D. A., Andrade-Silva, J., ... & Morini, M. S. (2024). Ant rarity and vulnerability in Brazilian Atlantic Forest fragments. Biological Conservation, 296, 110640. DOI: https://doi.org/10.1016/j.biocon.2024.110640

Silva, N. S., Gonçalves, D. C. de O., Wazema, C. T., Barbosa, D. A., Prado, L. P. do, Andrade-Silva, J., Fernandes, T. T., Silva, R. R., & Morini, M. S. de C. (2025). Endemism and vulnerability of ants in the phytophysiognomies of the Brazilian Atlantic Forest. In Brazilian Myrmecology: Exploring the World’s Richest Ant Fauna (Cap. 16, pp. 371–394). Editora Científica Digital. DOI: https://doi.org/10.37885/250920259

Vieira, V. B. (2025). Quem são e onde estão as formigas exóticas do Brasil? [Dissertação de Mestrado, Universidade Federal do Paraná]. Curitiba, PR, Brasil.

Silvestre, R., Brandão, C. R. F., & Silva, R. R. (2003). Grupos funcionales de hormigas: el caso de los gremios del Cerrado. In F. Fernández (Ed.), Introducción a las Hormigas de la Región Neotropical (pp. 113–148). Instituto Alexander Von Humboldt.

Silva, R. R., Silvestre, R., Brandão, C. R. F., Morini, M. S. C., & Delabie, J. H. C. (2015). Grupos tróficos e guildas em formigas poneromorfas. In: Delabie, Jacques H. C. et al. As formigas poneromorfas do Brasil. Ilhéus: Editus, 2015. p. 163-179.

Delabie, J. H. C., Agosti, D., & Nascimento, I. C. (2000). Litter ant communities of the Brazilian Atlantic rain forest region. Sampling Ground-dwelling Ants: case studies from the world’s rain forests. Curtin University of Technology School of Environmental Biology Bulletin, v. 18.

Additionally, if you use the package itself, please cite:

Gonçalves, D. C. O., et al. (2026). AntClassify: An R package for ant community analysis (Version 0.1.0) [Computer software].
https://github.com/cogdebora/AntClassify
