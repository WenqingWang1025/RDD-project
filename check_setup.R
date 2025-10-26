# ============================================
# Environment Check Script - Simple Version
# ============================================
# Run this script first to verify your setup
# ============================================

cat("\n")
cat("========================================\n")
cat("  RDD Replication Package - Setup Check\n")
cat("  (Simple Version - All in One Folder)\n")
cat("========================================\n\n")

# Check 1: Working directory
cat("1. Checking working directory...\n")
wd <- getwd()
cat("   Current directory:", wd, "\n")

# Check 2: Required data file
cat("\n2. Checking required data file...\n")
if (file.exists("enricoall2.csv")) {
  cat("   ✓ enricoall2.csv\n")
  data_present <- TRUE
} else {
  cat("   ✗ MISSING: enricoall2.csv\n")
  data_present <- FALSE
}

# Check 3: Optional data files
cat("\n3. Checking optional data files...\n")
optional_files <- c(
  "state_population_even_years_1946_1992.csv",
  "seats_by_state_wide_1946_1992.csv",
  "historical_state_population_by_year.csv"
)

for (file in optional_files) {
  if (file.exists(file)) {
    cat("   ✓", file, "\n")
  } else {
    cat("   ⚠ Optional:", file, "(will be generated if missing)\n")
  }
}

# Check 4: Required R scripts
cat("\n4. Checking R scripts...\n")
required_scripts <- c(
  "baseline.R",
  "adabyyear.R",
  "adabystate.R",
  "dembyyear.R",
  "dembystate.R"
)

all_scripts_present <- TRUE
for (script in required_scripts) {
  if (file.exists(script)) {
    cat("   ✓", script, "\n")
  } else {
    cat("   ✗ MISSING:", script, "\n")
    all_scripts_present <- FALSE
  }
}

# Check 5: Required packages
cat("\n5. Checking required R packages...\n")
required_packages <- c("rdrobust", "dplyr", "tidyr", "readr", "ggplot2")

missing_packages <- c()
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("   ✓", pkg, "\n")
  } else {
    cat("   ✗ NOT INSTALLED:", pkg, "\n")
    missing_packages <- c(missing_packages, pkg)
  }
}

# Check 6: R version
cat("\n6. Checking R version...\n")
r_version <- getRversion()
cat("   R version:", as.character(r_version), "\n")
if (r_version >= "4.0.0") {
  cat("   ✓ Version is adequate\n")
} else {
  cat("   ⚠ Recommended R >= 4.0.0\n")
}

# Check 7: Write permissions
cat("\n7. Checking write permissions...\n")
test_file <- "test_write_permission.tmp"
can_write <- tryCatch({
  file.create(test_file)
  file.remove(test_file)
  TRUE
}, error = function(e) {
  FALSE
})

if (can_write) {
  cat("   ✓ Can write to directory\n")
} else {
  cat("   ✗ Cannot write to directory\n")
}

# Summary
cat("\n")
cat("========================================\n")
cat("  Summary\n")
cat("========================================\n\n")

if (data_present && all_scripts_present && length(missing_packages) == 0 && can_write) {
  cat("✓ All checks passed! You're ready to run analyses.\n\n")
  cat("Next steps:\n")
  cat("  1. Run: source('run_all.R')  # Automatic\n")
  cat("  OR\n")
  cat("  2. Run scripts manually in order (see README.md)\n")
  cat("\nAll outputs will be saved in THIS folder.\n")
} else {
  cat("⚠ Some issues need to be resolved:\n\n")
  
  if (!data_present) {
    cat("- Missing enricoall2.csv. Add this file to the folder.\n")
  }
  
  if (!all_scripts_present) {
    cat("- Missing R scripts. Make sure all .R files are in this folder.\n")
  }
  
  if (length(missing_packages) > 0) {
    cat("- Missing packages. Install them with:\n")
    cat("  install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n", sep="")
  }
  
  if (!can_write) {
    cat("- Cannot write to directory. Check folder permissions.\n")
  }
}

cat("\n")
cat("========================================\n")
cat("Note: All output files (.csv, .png) will be\n")
cat("saved in the SAME folder as your R scripts!\n")
cat("========================================\n\n")
