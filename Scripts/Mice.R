setwd("/Users/amaga/Desktop/MM2/")

library(SlicerMorphR)
library(geomorph)

# Create simple 3D array for medianEstimates
cat("Processing median estimates as 3D array...\n")

# Find all JSON files recursively in medianEstimates directories
all_json_files <- list.files(path = ".", pattern = "median.*\\.json$", 
                            recursive = TRUE, full.names = TRUE)

# Filter to only medianEstimates directories
median_json_files <- all_json_files[grepl("medianEstimates", all_json_files)]

# TEMPORARY: Exclude ref_4 files (incomplete analysis)
# Remove this section when ref_4 analysis is completed
median_json_files <- median_json_files[!grepl("ref_4", median_json_files)]

cat("Found", length(median_json_files), "median estimate JSON files (excluding ref_4)\n")

if (length(median_json_files) == 0) {
  stop("No median estimate JSON files found!")
}

# Read first file to get dimensions
first_landmarks <- read.markups.json(median_json_files[1])
n_landmarks <- nrow(first_landmarks)
n_coords <- ncol(first_landmarks)

cat("Landmarks:", n_landmarks, "Coordinates:", n_coords, "\n")

# Initialize 3D array
# Dimensions: [landmarks, coordinates, files]
n_files <- length(median_json_files)
medianEst <- array(NA, dim = c(n_landmarks, n_coords, n_files))

# Create dimension names
landmark_names <- paste0("LM", 1:n_landmarks)
coord_names <- c("X", "Y", "Z")

# Create file names from full paths (remove leading ./ and base path)
file_paths <- sub("^\\./", "", median_json_files)
file_names <- file_paths

# Set dimension names
dimnames(medianEst) <- list(
  landmarks = landmark_names,
  coordinates = coord_names,
  files = file_names
)

# Fill the array
cat("Reading files...\n")
for (i in 1:n_files) {
  cat("Reading file", i, "of", n_files, ":", file_names[i], "\n")
  tryCatch({
    landmarks <- read.markups.json(median_json_files[i])
    medianEst[, , i] <- as.matrix(landmarks)
  }, error = function(e) {
    cat("Error reading", median_json_files[i], ":", e$message, "\n")
  })
}

cat("\nmedianEst 3D array created with dimensions:", paste(dim(medianEst), collapse = " x "), "\n")
cat("Structure: [landmarks, coordinates, files]\n")
cat("Sample file names:\n")
for (i in 1:min(5, length(file_names))) {
  cat("  ", i, ":", file_names[i], "\n")
}

# Create grouping factors
cat("\nCreating grouping factors...\n")

# Extract components from file paths
# Example path: "Estimates/2025-10-21_19_47_39/medianEstimates/129S1_SVIMJ__median.mrk.json"

# ID factor: extract the actual filename (remove .json extension)
ID <- character(n_files)
for (i in 1:n_files) {
  # Get just the filename part
  filename <- basename(file_names[i])
  # Remove .json extension
  ID[i] <- sub("\\.json$", "", filename)
}
ID <- as.factor(ID)

# Replicate factor: extract ref_X from the path if it exists, otherwise use "ref_1"
Replicate <- character(n_files)
for (i in 1:n_files) {
  # Look for ref_X pattern in the path
  ref_match <- regmatches(file_names[i], regexpr("ref_\\d+", file_names[i]))
  if (length(ref_match) > 0) {
    Replicate[i] <- ref_match
  } else {
    # If no ref_X found, assume it's ref_1 (for the current structure)
    Replicate[i] <- "ref_1"
  }
}
Replicate <- as.factor(Replicate)

# Iteration factor: extract timestamp and convert to 1 or 2 within each replicate
Iteration <- character(n_files)
# Get all unique timestamps first
timestamps <- character(n_files)
for (i in 1:n_files) {
  # Extract timestamp pattern (YYYY-MM-DD_HH_MM_SS)
  timestamp_match <- regmatches(file_names[i], regexpr("\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}", file_names[i]))
  if (length(timestamp_match) > 0) {
    timestamps[i] <- timestamp_match
  } else {
    timestamps[i] <- "unknown"
  }
}

# For each replicate, assign iteration numbers 1, 2 based on timestamp order
unique_replicates <- unique(Replicate)
for (rep in unique_replicates) {
  # Get files for this replicate
  rep_indices <- which(Replicate == rep)
  rep_timestamps <- timestamps[rep_indices]
  
  # Get unique timestamps for this replicate and sort them
  unique_rep_timestamps <- unique(rep_timestamps)
  unique_rep_timestamps <- sort(unique_rep_timestamps)
  
  # Assign iteration numbers within this replicate
  for (i in rep_indices) {
    if (timestamps[i] != "unknown") {
      iteration_num <- which(unique_rep_timestamps == timestamps[i])
      Iteration[i] <- as.character(iteration_num)
    } else {
      Iteration[i] <- "1"
    }
  }
}

Iteration <- as.factor(Iteration)

# Create summary
cat("Grouping factors created:\n")
cat("  ID levels:", length(levels(ID)), "(unique filenames)\n")
cat("  Replicate levels:", length(levels(Replicate)), "-", paste(levels(Replicate), collapse = ", "), "\n")
cat("  Iteration levels:", length(levels(Iteration)), "-", paste(levels(Iteration), collapse = ", "), "\n")

# Show first few examples
cat("\nFirst 10 file groupings:\n")
for (i in 1:min(10, n_files)) {
  cat(sprintf("  %2d: ID=%s, Replicate=%s, Iteration=%s\n", 
              i, ID[i], Replicate[i], Iteration[i]))
}

# Check factor lengths match file count
cat("\nFactor lengths check:\n")
cat("  Total files:", n_files, "\n")
cat("  ID length:", length(ID), "\n")
cat("  Replicate length:", length(Replicate), "\n")
cat("  Iteration length:", length(Iteration), "\n")

if (length(ID) == n_files && length(Replicate) == n_files && length(Iteration) == n_files) {
  cat("✓ All factor lengths match file count\n")
} else {
  cat("✗ Factor lengths do not match file count\n")
}

# Logic check for missing files
cat("\n=== FILE STRUCTURE ANALYSIS ===\n")

# Expected structure: each timestamp should have exactly 61 files
expected_files_per_timestamp <- 61

# Get unique combinations
unique_timestamps <- unique(timestamps)
unique_iterations <- unique(Iteration)

cat("Expected files per timestamp:", expected_files_per_timestamp, "\n")
cat("Found timestamps:", length(unique_timestamps), "\n")
cat("Found iterations:", length(unique_iterations), "\n")

# Check files per timestamp
for (ts in unique_timestamps) {
  files_in_timestamp <- sum(timestamps == ts)
  cat("Timestamp", ts, "has", files_in_timestamp, "files")
  if (files_in_timestamp == expected_files_per_timestamp) {
    cat(" ✓\n")
  } else {
    cat(" ✗ (expected", expected_files_per_timestamp, ")\n")
  }
}

# Check for unique IDs
unique_IDs <- unique(ID)
cat("\nUnique IDs found:", length(unique_IDs), "\n")
if (length(unique_IDs) == expected_files_per_timestamp) {
  cat("✓ Correct number of unique IDs\n")
} else {
  cat("✗ Expected", expected_files_per_timestamp, "unique IDs\n")
}

# Check if each ID appears the expected number of times
expected_appearances_per_ID <- length(unique_timestamps)
id_counts <- table(ID)

cat("\nID appearance check (each ID should appear", expected_appearances_per_ID, "times):\n")
problematic_ids <- character(0)
for (id_name in names(id_counts)) {
  count <- id_counts[id_name]
  if (count != expected_appearances_per_ID) {
    cat("  ✗", id_name, "appears", count, "times (expected", expected_appearances_per_ID, ")\n")
    problematic_ids <- c(problematic_ids, id_name)
  }
}

if (length(problematic_ids) == 0) {
  cat("✓ All IDs appear the correct number of times\n")
} else {
  cat("Found", length(problematic_ids), "problematic IDs\n")
}

# Detailed missing file analysis
if (length(problematic_ids) > 0) {
  cat("\n=== MISSING FILE ANALYSIS ===\n")
  
  for (ts in unique_timestamps) {
    cat("\nTimestamp:", ts, "\n")
    files_in_ts <- file_names[timestamps == ts]
    ids_in_ts <- ID[timestamps == ts]
    
    missing_ids <- setdiff(levels(ID), ids_in_ts)
    if (length(missing_ids) > 0) {
      cat("  Missing IDs:", paste(head(missing_ids, 10), collapse = ", "))
      if (length(missing_ids) > 10) cat(" ... and", length(missing_ids) - 10, "more")
      cat("\n")
    } else {
      cat("  ✓ All IDs present\n")
    }
  }
}

# Summary recommendation
cat("\n=== SUMMARY ===\n")
total_expected_files <- expected_files_per_timestamp * length(unique_timestamps)
cat("Expected total files:", total_expected_files, "\n")
cat("Actual total files:", n_files, "\n")

if (n_files == total_expected_files) {
  cat("✓ File count matches expectation\n")
} else {
  cat("✗ Missing", total_expected_files - n_files, "files\n")
  cat("Recommendation: Check if some analyses failed to complete\n")
  
  # Detailed analysis of exactly which files are missing
  cat("\n=== DETAILED MISSING FILE ANALYSIS ===\n")
  
  # Get the complete list of unique IDs that should exist
  all_unique_ids <- levels(ID)
  cat("Total unique IDs found:", length(all_unique_ids), "\n")
  
  # Check each timestamp for missing files
  for (i in 1:length(unique_timestamps)) {
    ts <- unique_timestamps[i]
    iteration_num <- i
    
    # Try to determine which ref folder this timestamp belongs to
    ref_info <- "unknown"
    sample_files_for_ts <- file_names[timestamps == ts]
    if (length(sample_files_for_ts) > 0) {
      # Look for ref pattern in the file paths
      ref_matches <- regmatches(sample_files_for_ts, regexpr("ref_\\d+", sample_files_for_ts))
      if (length(ref_matches) > 0) {
        ref_info <- unique(ref_matches)[1]
      }
    }
    
    cat("\nIteration", iteration_num, "- Timestamp:", ts)
    if (ref_info != "unknown") {
      cat(" (", ref_info, ")")
    }
    cat("\n")
    
    # Get IDs present in this timestamp
    present_ids <- ID[timestamps == ts]
    present_unique_ids <- unique(present_ids)
    
    cat("  Files present:", length(present_ids), "\n")
    cat("  Unique IDs present:", length(present_unique_ids), "\n")
    
    # Find missing IDs
    missing_ids <- setdiff(all_unique_ids, present_unique_ids)
    
    if (length(missing_ids) > 0) {
      cat("  Missing IDs (", length(missing_ids), "):\n")
      
      # Print missing IDs in groups of 5 for readability
      for (j in seq(1, length(missing_ids), by = 5)) {
        end_idx <- min(j + 4, length(missing_ids))
        ids_subset <- missing_ids[j:end_idx]
        cat("    ", paste(ids_subset, collapse = ", "), "\n")
      }
    } else {
      cat("  ✓ No missing IDs\n")
    }
  }
  
  # Overall missing file summary
  cat("\n=== OVERALL MISSING FILES SUMMARY ===\n")
  
  # First, let's group by ref folder to understand the structure better
  cat("Structure by ref folder:\n")
  ref_folder_summary <- data.frame(
    ref = character(),
    timestamps = character(),
    total_files = integer(),
    stringsAsFactors = FALSE
  )
  
  # Get unique ref folders
  all_refs <- unique(Replicate[Replicate != "unknown"])
  if (length(all_refs) == 0) {
    # If no ref folders found, try to extract from paths
    all_refs <- character(0)
    for (ts in unique_timestamps) {
      sample_files_for_ts <- file_names[timestamps == ts]
      if (length(sample_files_for_ts) > 0) {
        ref_matches <- regmatches(sample_files_for_ts, regexpr("ref_\\d+", sample_files_for_ts))
        if (length(ref_matches) > 0) {
          all_refs <- c(all_refs, unique(ref_matches)[1])
        }
      }
    }
    all_refs <- unique(all_refs)
  }
  
  for (ref in all_refs) {
    # Find timestamps for this ref
    ref_timestamps <- character(0)
    ref_file_count <- 0
    
    for (ts in unique_timestamps) {
      sample_files_for_ts <- file_names[timestamps == ts]
      if (length(sample_files_for_ts) > 0) {
        ref_matches <- regmatches(sample_files_for_ts, regexpr("ref_\\d+", sample_files_for_ts))
        if (length(ref_matches) > 0 && ref %in% ref_matches) {
          ref_timestamps <- c(ref_timestamps, ts)
          ref_file_count <- ref_file_count + sum(timestamps == ts)
        }
      }
    }
    
    if (length(ref_timestamps) > 0) {
      cat("  ", ref, ": ", length(ref_timestamps), " iterations, ", ref_file_count, " total files\n", sep="")
      for (ts in ref_timestamps) {
        files_count <- sum(timestamps == ts)
        cat("    ", ts, " (", files_count, " files)\n", sep="")
      }
    }
  }
  
  cat("\n")
  
  # Count how many times each ID appears
  id_counts <- table(ID)
  expected_count <- length(unique_timestamps)
  
  missing_completely <- character(0)
  missing_partially <- character(0)
  
  for (id_name in all_unique_ids) {
    if (id_name %in% names(id_counts)) {
      count <- id_counts[id_name]
      if (count < expected_count) {
        missing_partially <- c(missing_partially, paste0(id_name, " (", count, "/", expected_count, ")"))
      }
    } else {
      missing_completely <- c(missing_completely, id_name)
    }
  }
  
  if (length(missing_completely) > 0) {
    cat("IDs missing from ALL iterations (", length(missing_completely), "):\n")
    for (j in seq(1, length(missing_completely), by = 5)) {
      end_idx <- min(j + 4, length(missing_completely))
      ids_subset <- missing_completely[j:end_idx]
      cat("  ", paste(ids_subset, collapse = ", "), "\n")
    }
  }
  
  if (length(missing_partially) > 0) {
    cat("\nIDs missing from SOME iterations (", length(missing_partially), "):\n")
    for (j in seq(1, length(missing_partially), by = 3)) {
      end_idx <- min(j + 2, length(missing_partially))
      ids_subset <- missing_partially[j:end_idx]
      cat("  ", paste(ids_subset, collapse = ", "), "\n")
    }
  }
  
  if (length(missing_completely) == 0 && length(missing_partially) == 0) {
    cat("✓ All IDs are present in all iterations\n")
  }
}


#GPA part
gpa = gpagen(medianEst)
pca = gm.prcomp(gpa$coords)

# One-liner plot
plot(pca$x[,1:2], col=as.numeric(Replicate), pch=ifelse(Iteration=="1", 1, 3))
# Label specific points
selected_ids <- levels(ID)[c(3,51,54)]
for(id in selected_ids) {
  indices <- which(ID == id)
  text(pca$x[indices, 1], pca$x[indices, 2], substr(id, 1, 3), pos=3, cex=0.7)
}