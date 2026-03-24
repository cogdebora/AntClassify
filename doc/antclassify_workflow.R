## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(AntClassify)

## -----------------------------------------------------------------------------
dados <- data.frame(
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

colnames(dados) <- gsub("_", " ", colnames(dados))

dados

## -----------------------------------------------------------------------------
resultado <- antclassify(dados)

## -----------------------------------------------------------------------------
names(resultado)
head(resultado$guilds$table)

