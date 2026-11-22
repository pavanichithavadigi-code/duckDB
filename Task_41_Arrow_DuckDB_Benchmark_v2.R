###############################################################################
# TASK 41: ASSESS ARROW / DUCKDB FOR LARGE-DATA WORKLOADS  (version 2, starter)
#
# Question: can Arrow / DuckDB help when our clinical data gets big?
#
# We compare 4 setups on the same synthetic data:
#   1. data.table + CSV      (what we use today, the baseline)
#   2. Arrow + Parquet       (Arrow reads Parquet, then data.table works on it)
#   3. DuckDB + CSV          (DuckDB runs SQL directly on the CSV file)
#   4. DuckDB + Parquet      (DuckDB runs SQL directly on the Parquet file)
#
# Same 4 jobs for every setup: Read, Filter, Join + Summarise, Join.
#
# Simple words:
#   CSV     = plain text file. Easy, but big and slow.
#   Parquet = compressed file that stores each column separately.
#   Arrow   = R package that reads/writes Parquet quickly.
#   DuckDB  = a small database inside R. You write SQL, it reads the file.
#
# All data are synthetic. This is a learning benchmark, not a validated process.
#
# HOW TO RUN: open in RStudio and run the whole script (about 1-3 minutes).
# To test BIGGER data, change N_LAB_RECORDS in SECTION 3 and run again.
###############################################################################

# SECTION 1: Packages ---------------------------------------------------------

required_packages <- c("data.table", "arrow", "duckdb", "DBI", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(duckdb)
  library(DBI)
  library(ggplot2)
})

# Stops data.table from adding hidden "Index" notes when printing tables.
options(datatable.auto.index = FALSE)

cat("\nTASK 42: ARROW AND DUCKDB BENCHMARK (v2)\n")
cat("========================================\n")
cat("R version:", R.version.string, "\n")
for (pkg in required_packages) cat(pkg, ":", as.character(packageVersion(pkg)), "\n")

# SECTION 2: Project folder ---------------------------------------------------

 
project_dir <-getwd()
for (folder in c("data", "results", "reports")) {
  dir.create(file.path(project_dir, folder), recursive = TRUE, showWarnings = FALSE)
}
cat("\nProject folder:", normalizePath(getwd()), "\n")

# SECTION 3: Settings ---------------------------------------------------------

N_SUBJECTS        <- 10000L     # number of subjects
N_LAB_RECORDS     <- 1000000L   # lab rows. Try 5000000L for bigger data.
N_REPEATS         <- 3L         # each test is run this many times
SPEEDUP_THRESHOLD <- 2          # "clearly faster" means 2 times faster

set.seed(42042)
tag <- format(N_LAB_RECORDS, scientific = FALSE)   # used in output file names

cat("\nSubjects    :", format(N_SUBJECTS, big.mark = ","), "\n")
cat("Lab records :", format(N_LAB_RECORDS, big.mark = ","), "\n")
cat("Repeats     :", N_REPEATS, "\n")

# Computer details matter, because all three tools use several CPU threads.
duck_con_info <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
duck_threads <- DBI::dbGetQuery(duck_con_info, "SELECT current_setting('threads') AS t")$t
DBI::dbDisconnect(duck_con_info, shutdown = TRUE)

cat("CPU cores   :", parallel::detectCores(), "\n")
cat("data.table threads:", getDTthreads(), "| DuckDB threads:", duck_threads, "\n")

# SECTION 4: Synthetic data ---------------------------------------------------

# DM = one row per subject.
dm <- data.table(
  STUDYID = "STUDY-42-POC",
  USUBJID = sprintf("SUBJ-%06d", seq_len(N_SUBJECTS)),
  AGE     = sample(18:85, N_SUBJECTS, replace = TRUE),
  SEX     = sample(c("M", "F"), N_SUBJECTS, replace = TRUE),
  TRT01A  = sample(c("Placebo", "Drug A", "Drug B"), N_SUBJECTS, replace = TRUE),
  COUNTRY = sample(c("IND", "USA", "GBR", "DEU"), N_SUBJECTS, replace = TRUE)
)

# LAB = many rows per subject.
lab <- data.table(
  STUDYID = "STUDY-42-POC",
  USUBJID = sample(dm$USUBJID, N_LAB_RECORDS, replace = TRUE),
  PARAMCD = sample(c("ALT", "AST", "HGB", "CREAT", "GLUC"), N_LAB_RECORDS, replace = TRUE),
  AVAL    = round(pmax(rnorm(N_LAB_RECORDS, mean = 50, sd = 15), 0), 2),
  ADY     = sample(1:180, N_LAB_RECORDS, replace = TRUE),
  AVISIT  = sample(c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12"),
                   N_LAB_RECORDS, replace = TRUE)
)

cat("\nDM rows :", format(nrow(dm), big.mark = ","), "\n")
cat("LAB rows:", format(nrow(lab), big.mark = ","), "\n")
print(head(lab, 3))

# SECTION 5: Save the data as CSV and Parquet ---------------------------------

dm_csv      <- file.path("data", "synthetic_dm.csv")
lab_csv     <- file.path("data", "synthetic_lab.csv")
dm_parquet  <- file.path("data", "synthetic_dm.parquet")
lab_parquet <- file.path("data", "synthetic_lab.parquet")

fwrite(dm, dm_csv)
fwrite(lab, lab_csv)
write_parquet(dm, dm_parquet)
write_parquet(lab, lab_parquet)

size_mb <- function(path) round(file.info(path)$size / 1024^2, 2)
file_sizes <- data.table(
  DATA = c("DM", "LAB"),
  CSV_MB = c(size_mb(dm_csv), size_mb(lab_csv)),
  PARQUET_MB = c(size_mb(dm_parquet), size_mb(lab_parquet))
)
file_sizes[, TIMES_SMALLER := round(CSV_MB / PARQUET_MB, 1)]

cat("\nFile sizes (MB):\n")
print(file_sizes)
fwrite(file_sizes, file.path("results", paste0("file_sizes_", tag, ".csv")))

# SECTION 6: The benchmark helper ---------------------------------------------

benchmark_results <- data.table()

# bench() runs one job several times, saves the timing, and returns the result.
# The "<<-" below saves into the table that lives outside the function.
bench <- function(setup, task, fun) {
  seconds <- numeric(N_REPEATS)
  for (i in seq_len(N_REPEATS)) {
    gc()                                            # clean memory first
    seconds[i] <- system.time(result <- fun())[["elapsed"]]
  }
  benchmark_results <<- rbind(benchmark_results, data.table(
    SETUP = setup,
    TASK = task,
    MEDIAN_SECONDS = round(median(seconds), 4),
    MIN_SECONDS = round(min(seconds), 4),
    MAX_SECONDS = round(max(seconds), 4),
    RESULT_ROWS = nrow(result),
    RESULT_SIZE_MB = round(as.numeric(object.size(result)) / 1024^2, 2)
  ))
  result
}

# Same summary calculation for the data.table and Arrow setups.
summarise_lab <- function(joined) {
  joined[, .(
    N_RECORDS = .N,
    MEAN_AVAL = round(mean(AVAL), 2),
    SD_AVAL   = round(sd(AVAL), 2)
  ), by = .(TRT01A, PARAMCD)][order(TRT01A, PARAMCD)]
}

# Columns taken from DM in the full join (same for every setup).
dm_cols <- c("USUBJID", "AGE", "SEX", "TRT01A", "COUNTRY")

# SECTION 7: Setup 1 - data.table + CSV (baseline) ----------------------------

cat("\nSetup 1: data.table + CSV\n")
setup1 <- "data.table + CSV"

dt_read <- bench(setup1, "Read", function() fread(lab_csv))

dt_filter <- bench(setup1, "Filter", function() {
  x <- fread(lab_csv, select = c("USUBJID", "PARAMCD", "AVAL"))   # only needed columns
  x[PARAMCD == "ALT" & AVAL > 60]
})

dt_summary <- bench(setup1, "Join + Summarise", function() {
  joined <- merge(fread(lab_csv), fread(dm_csv)[, .(USUBJID, TRT01A)], by = "USUBJID")
  summarise_lab(joined)
})

dt_join <- bench(setup1, "Join", function() {
  merge(fread(lab_csv), fread(dm_csv)[, ..dm_cols], by = "USUBJID")
})

# SECTION 8: Setup 2 - Arrow + Parquet ----------------------------------------

cat("Setup 2: Arrow + Parquet\n")
setup2 <- "Arrow + Parquet"

# read_parquet() gives a table; as.data.table() makes it a data.table.
arrow_read <- bench(setup2, "Read", function() as.data.table(read_parquet(lab_parquet)))

arrow_filter <- bench(setup2, "Filter", function() {
  x <- as.data.table(read_parquet(lab_parquet,
                                  col_select = c("USUBJID", "PARAMCD", "AVAL")))
  x[PARAMCD == "ALT" & AVAL > 60]
})

arrow_summary <- bench(setup2, "Join + Summarise", function() {
  lab_a <- as.data.table(read_parquet(lab_parquet))
  dm_a  <- as.data.table(read_parquet(dm_parquet))
  summarise_lab(merge(lab_a, dm_a[, .(USUBJID, TRT01A)], by = "USUBJID"))
})

arrow_join <- bench(setup2, "Join", function() {
  lab_a <- as.data.table(read_parquet(lab_parquet))
  dm_a  <- as.data.table(read_parquet(dm_parquet))
  merge(lab_a, dm_a[, ..dm_cols], by = "USUBJID")
})

# SECTION 9: Setups 3 and 4 - DuckDB on CSV and on Parquet --------------------

cat("Setup 3 and 4: DuckDB\n")

# One DuckDB connection (in memory). A "view" is a named shortcut to a file.
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

full_path <- function(p) normalizePath(p, winslash = "/", mustWork = TRUE)

DBI::dbExecute(con, sprintf("CREATE VIEW dm_csv AS SELECT * FROM read_csv_auto('%s')",
                            full_path(dm_csv)))
DBI::dbExecute(con, sprintf("CREATE VIEW lab_csv AS SELECT * FROM read_csv_auto('%s')",
                            full_path(lab_csv)))
DBI::dbExecute(con, sprintf("CREATE VIEW dm_parquet AS SELECT * FROM read_parquet('%s')",
                            full_path(dm_parquet)))
DBI::dbExecute(con, sprintf("CREATE VIEW lab_parquet AS SELECT * FROM read_parquet('%s')",
                            full_path(lab_parquet)))

# Runs the 4 jobs with SQL. fmt is "CSV" or "Parquet".
run_duckdb <- function(fmt) {
  setup <- paste("DuckDB +", fmt)
  lab <- if (fmt == "CSV") "lab_csv" else "lab_parquet"
  dm  <- if (fmt == "CSV") "dm_csv"  else "dm_parquet"
  run_sql <- function(sql) as.data.table(DBI::dbGetQuery(con, sql))

  list(
    read = bench(setup, "Read", function() {
      run_sql(sprintf("SELECT * FROM %s", lab))
    }),
    filter = bench(setup, "Filter", function() {
      run_sql(sprintf("SELECT USUBJID, PARAMCD, AVAL FROM %s
                       WHERE PARAMCD = 'ALT' AND AVAL > 60", lab))
    }),
    summary = bench(setup, "Join + Summarise", function() {
      run_sql(sprintf("SELECT d.TRT01A, l.PARAMCD,
                              COUNT(*) AS N_RECORDS,
                              ROUND(AVG(l.AVAL), 2) AS MEAN_AVAL,
                              ROUND(STDDEV_SAMP(l.AVAL), 2) AS SD_AVAL
                       FROM %s l INNER JOIN %s d ON l.USUBJID = d.USUBJID
                       GROUP BY d.TRT01A, l.PARAMCD
                       ORDER BY d.TRT01A, l.PARAMCD", lab, dm))
    }),
    join = bench(setup, "Join", function() {
      run_sql(sprintf("SELECT l.*, d.AGE, d.SEX, d.TRT01A, d.COUNTRY
                       FROM %s l INNER JOIN %s d ON l.USUBJID = d.USUBJID", lab, dm))
    })
  )
}

duck_csv     <- run_duckdb("CSV")
duck_parquet <- run_duckdb("Parquet")

# SECTION 10: Do all setups give the same answers? ----------------------------

cat("\nChecking that all setups agree...\n")

# Compare two summary tables (ignores row names and number type).
same_summary <- function(a, b) {
  isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE))
}

same_count <- function(...) length(unique(c(...))) == 1L

checks <- data.table(
  CHECK = c(
    "Read: every setup returns all lab rows",
    "Filter: row counts agree",
    "Join: row counts agree",
    "Summary: Arrow + Parquet equals data.table",
    "Summary: DuckDB + CSV equals data.table",
    "Summary: DuckDB + Parquet equals data.table"
  ),
  PASSED = c(
    all(c(nrow(dt_read), nrow(arrow_read), nrow(duck_csv$read),
          nrow(duck_parquet$read)) == N_LAB_RECORDS),
    same_count(nrow(dt_filter), nrow(arrow_filter), nrow(duck_csv$filter),
               nrow(duck_parquet$filter)),
    same_count(nrow(dt_join), nrow(arrow_join), nrow(duck_csv$join),
               nrow(duck_parquet$join)),
    same_summary(dt_summary, arrow_summary),
    same_summary(dt_summary, duck_csv$summary),
    same_summary(dt_summary, duck_parquet$summary)
  )
)
checks[, RESULT := ifelse(PASSED, "PASS", "FAIL")]
checks[, PASSED := NULL]
print(checks)
fwrite(checks, file.path("results", paste0("consistency_checks_", tag, ".csv")))

# SECTION 11: Timing results and speed-up -------------------------------------

setorder(benchmark_results, TASK, SETUP)
cat("\nBenchmark results (seconds, lower is faster):\n")
print(benchmark_results)
fwrite(benchmark_results, file.path("results", paste0("benchmark_results_", tag, ".csv")))

# Speed-up = baseline time / this time. Above 1 means faster than data.table + CSV.
baseline <- benchmark_results[SETUP == "data.table + CSV", .(TASK, BASE_SECONDS = MEDIAN_SECONDS)]
speedup <- merge(benchmark_results, baseline, by = "TASK")
speedup[, SPEEDUP := round(BASE_SECONDS / MEDIAN_SECONDS, 2)]

seconds_wide <- dcast(benchmark_results, TASK ~ SETUP, value.var = "MEDIAN_SECONDS")
speedup_wide <- dcast(speedup, TASK ~ SETUP, value.var = "SPEEDUP")

cat("\nMedian seconds by job:\n")
print(seconds_wide)
cat("\nSpeed-up compared with data.table + CSV (above 1 = faster):\n")
print(speedup_wide)
fwrite(speedup_wide, file.path("results", paste0("speedup_", tag, ".csv")))

# SECTION 12: Plot ------------------------------------------------------------

# One bar per setup in every job. Shorter bar = faster.
time_plot <- ggplot(benchmark_results,
                    aes(x = SETUP, y = MEDIAN_SECONDS, fill = SETUP)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ TASK, scales = "free_x") +
  labs(
    title = "Task 42: median time by job (shorter is faster)",
    subtitle = paste(format(N_LAB_RECORDS, big.mark = ","), "lab rows,",
                     N_REPEATS, "repeats"),
    x = NULL, y = "Seconds"
  ) +
  theme_minimal()

print(time_plot)
ggsave(file.path("results", paste0("benchmark_plot_", tag, ".png")),
       time_plot, width = 11, height = 6, dpi = 150)

# SECTION 13: Compatibility test - does the data survive CSV and Parquet? -----

# A tiny table with the things that matter in clinical data.
test_df <- data.table(
  ID = 1:4,
  VISIT_DATE = as.Date(c("2025-01-10", "2025-02-15", NA, "2025-04-01")),
  SEX = factor(c("M", "F", "F", NA)),
  AVAL = c(1.5, NA, 3.25, 4),
  COMMENT = c("ok", NA, "", "high")
)
attr(test_df$AVAL, "label") <- "Analysis Value"     # like a SAS label

test_csv <- file.path("data", "roundtrip_test.csv")
test_parquet <- file.path("data", "roundtrip_test.parquet")
fwrite(test_df, test_csv)
write_parquet(test_df, test_parquet)

back_csv <- fread(test_csv)
back_parquet <- as.data.table(read_parquet(test_parquet))

# Write, read back, then ask: is it still the same?
roundtrip_checks <- function(back, format_name) {
  ok <- c(
    inherits(back$VISIT_DATE, "Date"),
    is.factor(back$SEX),
    identical(is.na(back$AVAL), is.na(test_df$AVAL)),
    identical(is.na(back$COMMENT), is.na(test_df$COMMENT)),
    identical(attr(back$AVAL, "label"), "Analysis Value")
  )
  data.table(
    FORMAT = format_name,
    CHECK = c("Date stays a Date",
              "Factor stays a factor",
              "Missing numbers stay missing",
              "Empty text and missing text stay different",
              "Column label is kept"),
    RESULT = ifelse(ok, "PASS", "FAIL")
  )
}

compatibility <- rbind(roundtrip_checks(back_csv, "CSV"),
                       roundtrip_checks(back_parquet, "Parquet"))
cat("\nCompatibility test (write a file, read it back):\n")
print(compatibility)
fwrite(compatibility, file.path("results", "compatibility_test.csv"))

