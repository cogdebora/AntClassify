library(testthat)
library(AntClassify)

# Force non-interactive mode to prevent "Press Enter" prompts during CRAN checks
options(interactive = FALSE)

# Mock readline globally to ensure automated tests do not hang
assignInNamespace("readline", function(...) "", ns = "base")

# Execute all tests in the package
test_check("AntClassify")

# Restore readline to its original state (safety measure)
assignInNamespace("readline", base::readline, ns = "base")
