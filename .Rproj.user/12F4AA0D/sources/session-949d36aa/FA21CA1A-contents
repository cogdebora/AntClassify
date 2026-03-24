library(testthat)
library(AntClassify)

# 1. Prepare standardized test data based on validated species lists
# These frequencies are designed to trigger specific results in each function
ant_test_data <- data.frame(
  # Exotic species (Expected: 3)
  "Pheidole megacephala" = 10,
  "Strumigenys emmae" = 5,
  "Paratrechina longicornis" = 8,
  # Endemic species (Expected: 2)
  "Hypoponera leninei" = 3,
  "Camponotus fallatus" = 2,
  # Rare species (Expected: 5)
  "Ectatomma brunneum" = 1,
  "Ectatomma permagnum" = 1,
  "Pheidole aberrans" = 1,
  "Pheidole fimbriata" = 1,
  "Pheidole obscurithorax" = 1,
  check.names = FALSE
)

test_that("AntClassify identifies and filters species correctly", {

  # Execute the full ecological pipeline
  results <- antclassify(ant_test_data)

  # VALIDATION: Exotic Ants (Vieira 2025)
  # Expecting exactly 3 species detected and filtered
  expect_equal(nrow(results$exotic$table), 3)
  expect_true("Pheidole megacephala" %in% results$exotic$table$species)

  # VALIDATION: Endemic Atlantic Forest Ants (Silva et al. 2025)
  # Expecting exactly 2 species detected and filtered
  expect_equal(nrow(results$endemic$table), 2)
  expect_true("Hypoponera leninei" %in% results$endemic$table$species)

  # VALIDATION: Rarity Forms (Silva et al. 2024)
  # Expecting exactly 5 rare species detected and filtered
  expect_equal(nrow(results$rarity$table), 5)
  expect_true("Ectatomma permagnum" %in% results$rarity$table$species)
})

test_that("AntClassify generates and displays visual outputs", {

  # Execute the pipeline to retrieve plot objects
  results <- antclassify(ant_test_data)

  # Verify if plot objects were correctly created as ggplot objects
  expect_s3_class(results$exotic$plot, "ggplot")
  expect_s3_class(results$endemic$plot, "ggplot")
  expect_s3_class(results$rarity$plot, "ggplot")


  print(results$exotic$plot)
  print(results$endemic$plot)
  print(results$rarity$plot)
})
