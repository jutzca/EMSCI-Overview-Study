###############################################################################
# EMSCI – Longitudinal retention, follow-up completeness & missing-data analysis
#
# Answers the reviewer request for:
#   * retention statistics per recovery stage (overall and by year of injury)
#   * assessment-specific denominators (ISNCSCI, SCIM, WISCI, NCS, SSEP, MEP)
#   * distinction between "not assessed" (missing) and "not testable" (NT)
#   * number lost to follow-up vs. not-yet-due (administrative censoring)
#   * missingness patterns (complete / monotone dropout / intermittent / late entry)
#   * SCIM II -> SCIM III transition over calendar time
#   * deaths (only if a death variable exists – otherwise reported as "not captured")
#
# Input : long-format export, one row per patient x ExamStage (CSV, comma or semicolon)
# Output: CSV tables + PNG/PDF figures + a draft text for the response letter
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(patchwork)
})

## ---- 0. USER SETTINGS ------------------------------------------------------
# Pick the CSV export (dialog opens in ~/Downloads when run in RStudio).
# To skip the dialog, set e.g. input_file <- "~/Downloads/emsci_export.csv"
if (!exists("input_file")) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    input_file <- rstudioapi::selectFile(caption = "Select EMSCI CSV export",
                                         path = "~/Downloads",
                                         filter = "CSV files (*.csv)")
  } else {
    input_file <- file.choose()   # plain R: browse to Downloads yourself
  }
}
input_file <- path.expand(input_file)
out_dir     <- file.path(dirname(input_file), "retention_output")  # next to the data
data_cutoff <- as.Date("2025-12-31")       # <- date of the data export / freeze

# Optional: name of a death variable if one exists in your export
# (e.g. "Deceased" coded TRUE/FALSE or 1/0). Leave NULL if not captured.
death_var   <- NULL

# Exam stages in chronological order and the UPPER bound (days post-injury)
# of each EMSCI assessment window. A patient is only counted in the denominator
# of a stage once that window has closed before data_cutoff.
# EMSCI stages: very acute (<16 days post-injury), acute I (16-40 days),
# acute II (70-98 days), acute III (150-186 days), chronic (300-446 days).
stage_levels <- c("very acute", "acute I", "acute II", "acute III", "chronic")
stage_labels <- c("Very acute\n(<16 d)", "Acute I\n(16-40 d)", "Acute II\n(70-98 d)",
                  "Acute III\n(150-186 d)", "Chronic\n(300-446 d)")
stage_window_end_days <- c(`very acute` = 15, `acute I` = 40, `acute II` = 98,
                           `acute III` = 186, chronic = 446)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- 1. READ DATA ----------------------------------------------------------
# Read everything as character so "NT"/"ND" codes are preserved;
# blank cells become NA (= not assessed).
# CSV: the separator (comma or semicolon, as Excel writes in DE/CH locales)
# is detected automatically from the header line.
first_line <- readLines(input_file, n = 1, warn = FALSE)
csv_delim  <- if (lengths(regmatches(first_line, gregexpr(";", first_line))) >
                  lengths(regmatches(first_line, gregexpr(",", first_line)))) ";" else ","
raw <- read_delim(input_file, delim = csv_delim, na = c("", "NA"),
                  col_types = cols(.default = col_character()),
                  trim_ws = TRUE, show_col_types = FALSE,
                  locale = locale(encoding = "UTF-8"))
names(raw) <- sub("^﻿", "", trimws(names(raw)))   # drop Excel BOM if present

# ID column: accept both "random_ID" (new export) and "RandomID" (old export)
if ("random_ID" %in% names(raw)) raw <- rename(raw, RandomID = random_ID)

required <- c("RandomID", "ExamStage", "Year_DOI")
miss_cols <- setdiff(required, names(raw))
if (length(miss_cols)) stop("Missing columns: ", paste(miss_cols, collapse = ", "))

nt_codes <- c("NT", "ND", "UNK", "N/A")   # codes meaning "attempted, not testable"

# ---- Instruments analysed ----------------------------------------------------
# dai   = "days after injury" column: present => the assessment was documented
# value = result column(s); any valid (non-NT) value => "Valid score"
# Instruments whose DAI column is not in the file are skipped automatically.
instruments <- list(
  ISNCSCI = list(dai = "ISNCSCI_DAI", value = c("AIS", "TMS")),
  SCIM    = list(dai = "SCIM23_DAI",  value = c("SCIM3_TotalScore", "SCIM2_TotalScore")),
  WISCI   = list(dai = "WISCI_DAI",   value = c("WISCI_score")),
  NCS     = list(dai = "NCS_DAI",     value = c("CMAP_R_tib", "CMAP_L_tib", "CMAP_R_uln", "CMAP_L_uln",
                                                "DML_R_tib", "DML_L_tib", "DML_R_uln", "DML_L_uln")),
  SSEP    = list(dai = "SSEP_DAI",    value = c("SSEP_Lat_P40_R_tib", "SSEP_Lat_P40_L_tib",
                                                "SSEP_Lat_N20_R_uln", "SSEP_Lat_N20_L_uln")),
  MEP     = list(dai = "MEP_DAI",     value = c("MEP_Lat_R_tib", "MEP_Lat_L_tib",
                                                "MEP_Lat_R_digmin", "MEP_Lat_L_digmin",
                                                "MEP_Lat_R_hal", "MEP_Lat_L_hal"))
)
keep <- vapply(instruments, function(x) x$dai %in% names(raw), logical(1))
if (any(!keep)) message("Skipping instruments not in file: ",
                        paste(names(instruments)[!keep], collapse = ", "))
instruments <- instruments[keep]
instrument_levels <- names(instruments)
st_cols <- paste0("st_", instrument_levels)

# Status of one instrument at one visit:
#   "Valid score"  – at least one result recorded
#   "Not testable" – documented (DAI present) but only NT/ND or no result
#                    (e.g. non-ambulatory, no response) = non-applicable
#   "Not assessed" – nothing recorded (true missing)
instrument_status <- function(d, dai, value_cols) {
  value_cols <- intersect(value_cols, names(d))
  vals  <- as.matrix(d[, value_cols, drop = FALSE])
  valid <- rowSums(!is.na(vals) & !(toupper(vals) %in% nt_codes)) > 0
  if (!length(value_cols)) valid <- rep(FALSE, nrow(d))
  nt    <- rowSums(!is.na(vals) & (toupper(vals) %in% nt_codes)) > 0
  if (!length(value_cols)) nt <- rep(FALSE, nrow(d))
  case_when(valid ~ "Valid score",
            nt | !is.na(d[[dai]]) ~ "Not testable",
            TRUE ~ "Not assessed")
}

dat <- raw %>%
  mutate(
    ExamStage = factor(trimws(ExamStage), levels = stage_levels),
    Year_DOI  = as.integer(Year_DOI),
    # date of injury ("2004-09-16 00:00:00 UTC" -> 2004-09-16);
    # falls back to 1 July of Year_DOI if DOI is missing
    doi_approx = if ("DOI" %in% names(raw)) as.Date(substr(DOI, 1, 10)) else as.Date(NA),
    doi_approx = coalesce(doi_approx, as.Date(sprintf("%d-07-01", Year_DOI))))

for (i in seq_along(instruments))
  dat[[st_cols[i]]] <- instrument_status(dat, instruments[[i]]$dai, instruments[[i]]$value)

has <- function(col) if (col %in% names(dat)) !is.na(dat[[col]]) else rep(FALSE, nrow(dat))
dat <- dat %>%
  mutate(
    # SCIM version used at that visit (based on which total score is filled)
    scim_version = case_when(
      has("SCIM3_TotalScore") ~ "SCIM III",
      has("SCIM2_TotalScore") ~ "SCIM II",
      has("SCIM23_DAI")       ~ "Version not recorded",
      TRUE ~ NA_character_),
    any_assessment = if_any(all_of(st_cols), ~ .x != "Not assessed"))

if (any(is.na(dat$ExamStage)))
  warning("Some ExamStage values did not match stage_levels and were dropped: ",
          paste(unique(raw$ExamStage[is.na(dat$ExamStage)]), collapse = ", "))
dat <- filter(dat, !is.na(ExamStage))

# Exclude patients injured after the data cut-off (e.g. 2026 injuries)
after_cutoff <- unique(dat$RandomID[!is.na(dat$doi_approx) & dat$doi_approx > data_cutoff])
if (length(after_cutoff))
  message("Excluded ", length(after_cutoff), " patients injured after ",
          format(data_cutoff, "%d %b %Y"))
dat <- filter(dat, !(RandomID %in% after_cutoff))

## ---- 2. PATIENT x STAGE GRID ----------------------------------------------
# Ensure every patient has all 5 stages (absent rows = not assessed),
# and flag whether the stage window had closed by the data cut-off.
patients <- dat %>%
  group_by(RandomID) %>%
  summarise(Year_DOI   = first(na.omit(Year_DOI)),
            doi_approx = first(na.omit(doi_approx)),
            Center     = if ("Center" %in% names(dat)) first(Center) else NA_character_,
            .groups = "drop")

if (!is.null(death_var) && death_var %in% names(raw)) {
  deaths <- raw %>%
    group_by(RandomID) %>%
    summarise(deceased = any(toupper(.data[[death_var]]) %in% c("TRUE", "1", "YES")),
              .groups = "drop")
  patients <- left_join(patients, deaths, by = "RandomID")
} else {
  patients$deceased <- NA   # mortality not captured
}

grid <- dat %>%
  select(RandomID, ExamStage, any_assessment, starts_with("st_"), scim_version) %>%
  complete(RandomID = patients$RandomID, ExamStage,
           fill = c(list(any_assessment = FALSE),
                    setNames(as.list(rep("Not assessed", length(st_cols))), st_cols))) %>%
  # if a patient has duplicated rows for one stage keep the most complete one
  group_by(RandomID, ExamStage) %>%
  arrange(desc(any_assessment), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(patients, by = "RandomID") %>%
  mutate(window_end = doi_approx + stage_window_end_days[as.character(ExamStage)],
         eligible   = !is.na(window_end) & window_end <= data_cutoff)

## ---- 3. PATIENT-LEVEL FOLLOW-UP STATUS ------------------------------------
pt <- grid %>%
  arrange(RandomID, ExamStage) %>%
  group_by(RandomID, Year_DOI, deceased) %>%
  summarise(
    pattern       = paste(as.integer(any_assessment), collapse = ""),
    n_assessed    = sum(any_assessment),
    first_stage   = if (any(any_assessment)) min(as.integer(ExamStage)[any_assessment]) else NA_integer_,
    last_stage    = if (any(any_assessment)) max(as.integer(ExamStage)[any_assessment]) else NA_integer_,
    early_baseline = any(any_assessment & ExamStage %in% c("very acute", "acute I")),
    chronic_done  = any(any_assessment & ExamStage == "chronic"),
    chronic_due   = any(eligible & ExamStage == "chronic"),
    .groups = "drop") %>%
  mutate(
    gaps = mapply(function(p, f, l) {
      if (is.na(f)) return(FALSE)
      grepl("0", substr(p, f, l))
    }, pattern, first_stage, last_stage),
    missing_pattern = case_when(
      n_assessed == 0                                   ~ "No assessment recorded",
      !chronic_due & !chronic_done                      ~ "Follow-up ongoing (not yet due)",
      gaps                                              ~ "Intermittent missing",
      last_stage == 5 & first_stage == 1                ~ "Complete",
      last_stage == 5 & first_stage > 1                 ~ "Late entry, complete thereafter",
      last_stage < 5                                    ~ "Monotone dropout"),
    followup_status = case_when(
      isTRUE(deceased) | deceased %in% TRUE             ~ "Deceased",
      chronic_done                                      ~ "Reached chronic",
      !chronic_due                                      ~ "Follow-up ongoing",
      n_assessed == 0                                   ~ "No assessment recorded",
      TRUE ~ paste0("Lost after ", stage_levels[last_stage])),
    followup_status = factor(followup_status, levels = c(
      "Reached chronic", "Lost after acute III", "Lost after acute II",
      "Lost after acute I", "Lost after very acute", "Deceased",
      "No assessment recorded", "Follow-up ongoing"))
  )

## ---- 4. TABLES -------------------------------------------------------------
pct <- function(x, n) 100 * x / ifelse(n > 0, n, NA_real_)

# T1 – retention / cohort flow by year of injury (+ overall row)
cohort_flow <- function(d) {
  d %>% summarise(
    n_registered            = n(),
    n_any_assessment        = sum(n_assessed > 0),
    n_early_baseline        = sum(early_baseline),
    n_chronic_due           = sum(chronic_due),
    n_chronic_done          = sum(chronic_done & chronic_due),
    n_lost_to_followup      = sum(chronic_due & !chronic_done &
                                  !(deceased %in% TRUE) & n_assessed > 0),
    n_deceased              = if (all(is.na(deceased))) NA_integer_ else sum(deceased %in% TRUE),
    n_followup_ongoing      = sum(!chronic_due & !chronic_done),
    .groups = "drop") %>%
    mutate(pct_chronic_retention = round(pct(n_chronic_done, n_chronic_due), 1),
           pct_lost              = round(pct(n_lost_to_followup, n_chronic_due), 1))
}
t1 <- bind_rows(
  pt %>% group_by(Year_DOI) %>% cohort_flow() %>% mutate(Year_DOI = as.character(Year_DOI)),
  pt %>% cohort_flow() %>% mutate(Year_DOI = "Overall"))
write_csv(t1, file.path(out_dir, "T1_cohort_flow_by_year.csv"))

# T2 – attendance (any assessment) per stage and year, eligible denominator
stage_year <- grid %>%
  filter(eligible) %>%
  group_by(Year_DOI, ExamStage) %>%
  summarise(n_eligible = n(), n_assessed = sum(any_assessment), .groups = "drop") %>%
  mutate(pct_assessed = pct(n_assessed, n_eligible))
stage_overall <- grid %>%
  filter(eligible) %>%
  group_by(ExamStage) %>%
  summarise(n_eligible = n(), n_assessed = sum(any_assessment), .groups = "drop") %>%
  mutate(pct_assessed = pct(n_assessed, n_eligible))
write_csv(bind_rows(mutate(stage_year, Year_DOI = as.character(Year_DOI)),
                    mutate(stage_overall, Year_DOI = "Overall")),
          file.path(out_dir, "T2_attendance_by_stage_and_year.csv"))

# T3 – assessment-specific completeness (valid / not testable / not assessed)
instr_long <- grid %>%
  filter(eligible) %>%
  pivot_longer(starts_with("st_"), names_to = "instrument", values_to = "status") %>%
  mutate(instrument = sub("^st_", "", instrument),
         instrument = factor(instrument, levels = instrument_levels),
         status = factor(status, levels = c("Valid score", "Not testable", "Not assessed")))

t3 <- instr_long %>%
  count(instrument, ExamStage, status, .drop = FALSE) %>%
  group_by(instrument, ExamStage) %>%
  mutate(denominator = sum(n), pct = round(pct(n, denominator), 1)) %>%
  ungroup()
write_csv(t3, file.path(out_dir, "T3_instrument_completeness_by_stage.csv"))

t3_year <- instr_long %>%
  group_by(Year_DOI, instrument, ExamStage) %>%
  summarise(denominator   = n(),
            n_valid       = sum(status == "Valid score"),
            n_not_testable = sum(status == "Not testable"),
            n_not_assessed = sum(status == "Not assessed"),
            .groups = "drop") %>%
  mutate(pct_documented = pct(n_valid + n_not_testable, denominator))
write_csv(t3_year, file.path(out_dir, "T3b_instrument_completeness_by_year.csv"))

# T4 – missing-data patterns
t4 <- pt %>% count(missing_pattern, sort = TRUE) %>% mutate(pct = round(pct(n, sum(n)), 1))
write_csv(t4, file.path(out_dir, "T4_missingness_patterns.csv"))
t4b <- pt %>% filter(n_assessed > 0) %>% count(pattern, sort = TRUE) %>%
  mutate(pct = round(pct(n, sum(n)), 1))
write_csv(t4b, file.path(out_dir, "T4b_stage_patterns_detailed.csv"))

# T5 – SCIM version by year
t5 <- dat %>% filter(!is.na(scim_version)) %>%
  count(Year_DOI, scim_version) %>%
  group_by(Year_DOI) %>% mutate(pct = round(pct(n, sum(n)), 1)) %>% ungroup()
write_csv(t5, file.path(out_dir, "T5_scim_version_by_year.csv"))

## ---- 5. FIGURES ------------------------------------------------------------
theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  legend.position = "bottom"))
names(stage_labels) <- stage_levels

# ---- Colour scheme: simple and muted ------------------------------------------
# Two muted hues + greys, used the same way in every panel:
#   slate blue = data present / retained / main estimate
#   sand       = loss to follow-up, and SCIM II (the older version)
#   light grey = not assessed / not yet due / not recorded
# Blue vs sand stays distinguishable for colour-blind readers and in greyscale.
pal <- list(
  heat     = c("#f2f4f7", "#c5d0dd", "#8199b5", "#3f5f86", "#26405f"),  # A: low -> high
  year_lo  = "#d9d9d9", year_hi = "#6e6e6e",            # B: per-year lines
  pooled   = "#12877a",                                  # B: pooled estimate (teal, stands out from grey lines)
  retained = "#3f5f86",                                  # C: reached chronic
  loss     = c("#ece2cf", "#d8c4a0", "#bfa173", "#8f7448"),  # C: later -> earlier dropout
  valid = "#3f5f86", not_testable = "#aebdd0",            # D
  scim2 = "#d8c4a0", scim3 = "#3f5f86",                   # E
  pattern = "#3f5f86",                                    # F
  grey_light = "#e4e4e1", grey_mid = "#a8a8a4", ink = "#1f1f1f")

scale_fill_pct <- function(name) {
  scale_fill_gradientn(colours = pal$heat,
                       limits = c(0, 100), na.value = "grey97", name = name,
                       labels = function(x) paste0(x, "%"))
}
# year axes: first year, then every 5th year, and the last year
# (e.g. 2001, 2005, 2010, 2015, 2020, 2025)
every_5_years <- scale_x_discrete(breaks = function(x) {
  y <- as.integer(x)
  x[y == min(y) | y %% 5 == 0 | y == max(y)]
})

# F1 – heatmap: % patients with any assessment, year x stage (eligible only)
f1_dat <- grid %>%
  group_by(Year_DOI, ExamStage) %>%
  summarise(n_eligible = sum(eligible),
            n_assessed = sum(any_assessment & eligible), .groups = "drop") %>%
  mutate(pct_assessed = ifelse(n_eligible > 0, 100 * n_assessed / n_eligible, NA))

f1 <- ggplot(f1_dat, aes(ExamStage, factor(Year_DOI), fill = pct_assessed)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(is.na(pct_assessed), "not due",
                               sprintf("%.0f%%", pct_assessed)),
                colour = !is.na(pct_assessed) & pct_assessed > 50),
            size = 2.6, lineheight = 0.9) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = pal$ink), guide = "none") +
  scale_fill_pct("% assessed") +
  scale_x_discrete(labels = stage_labels) +
  labs(title = "A  Follow-up completeness by year of injury",
       x = NULL, y = "Year of injury")

# F2 – retention curves among patients with an early (very acute / acute I) baseline
f2_dat <- grid %>%
  semi_join(filter(pt, early_baseline), by = "RandomID") %>%
  filter(eligible)
f2_year <- f2_dat %>% group_by(Year_DOI, ExamStage) %>%
  summarise(n = n(), ret = mean(any_assessment) * 100, .groups = "drop")
f2_all <- f2_dat %>% group_by(ExamStage) %>%
  summarise(n = n(), ret = mean(any_assessment) * 100,
            lo = 100 * binom.test(sum(any_assessment), n())$conf.int[1],
            hi = 100 * binom.test(sum(any_assessment), n())$conf.int[2], .groups = "drop")

f2 <- ggplot() +
  geom_line(data = f2_year, aes(ExamStage, ret, group = Year_DOI, colour = Year_DOI),
            alpha = 0.7) +
  geom_ribbon(data = f2_all, aes(ExamStage, ymin = lo, ymax = hi, group = 1),
              fill = pal$pooled, alpha = 0.12) +
  geom_line(data = f2_all, aes(ExamStage, ret, group = 1), linewidth = 1.2, colour = pal$pooled) +
  geom_point(data = f2_all, aes(ExamStage, ret), size = 2.2, colour = pal$pooled) +
  geom_label(data = f2_all, aes(ExamStage, ret, label = sprintf("%.0f%%", ret)),
             vjust = -0.6, size = 3, fontface = "bold", colour = pal$pooled,
             fill = "white", linewidth = 0, label.padding = unit(0.12, "lines")) +
  scale_colour_gradient(low = pal$year_lo, high = pal$year_hi, name = "Year of injury",
                        # first year, every 5th year, last year (2001, 2005, ..., 2025)
                        breaks = function(l) {
                          y <- seq(ceiling(l[1]), floor(l[2]))
                          y[y == min(y) | y %% 5 == 0 | y == max(y)]
                        }) +
  scale_x_discrete(labels = stage_labels) +
  scale_y_continuous(limits = c(0, 105), labels = function(x) paste0(x, "%")) +
  labs(title = "B  Retention after an early baseline assessment",
       x = NULL, y = "% assessed at stage") +
  guides(colour = guide_colourbar(barwidth = unit(9, "cm")))

# F3 – follow-up status at data cut-off, by year
status_cols <- c("Reached chronic"         = pal$retained,
                 "Lost after acute III"    = pal$loss[1],
                 "Lost after acute II"     = pal$loss[2],
                 "Lost after acute I"      = pal$loss[3],
                 "Lost after very acute"   = pal$loss[4],
                 "Deceased"                = pal$ink,
                 "No assessment recorded"  = pal$grey_mid,
                 "Follow-up ongoing"       = pal$grey_light)
f3 <- pt %>% count(Year_DOI, followup_status) %>%
  group_by(Year_DOI) %>% mutate(N = sum(n)) %>% ungroup() %>%
  ggplot(aes(factor(Year_DOI), n, fill = fct_rev(followup_status))) +
  geom_col(position = "fill", width = 0.8) +
  scale_fill_manual(values = status_cols, name = NULL, drop = TRUE) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "C  Follow-up status at data cut-off",
       subtitle = paste("Last stage attended; cut-off", format(data_cutoff, "%d %b %Y")),
       x = "Year of injury", y = "% of patients") +
  guides(fill = guide_legend(nrow = 3, reverse = TRUE))

# F4 – instrument-specific completeness (valid / not testable / not assessed)
f4 <- t3 %>%
  ggplot(aes(ExamStage, pct, fill = fct_rev(status))) +
  geom_col(width = 0.8) +
  facet_wrap(~ instrument, nrow = 1) +
  scale_fill_manual(
    values = c("Valid score" = pal$valid, "Not testable" = pal$not_testable,
               "Not assessed" = pal$grey_light),
    breaks = c("Valid score", "Not testable", "Not assessed"),
    labels = c("Valid score" = "Valid score",
               "Not testable" = "Not testable (documented but scored NT / no result,\ne.g. non-ambulatory; non-applicable rather than missing)",
               "Not assessed" = "Not assessed (missing)"),
    name = paste("")) +
  scale_x_discrete(labels = c("VA", "A-I", "A-II", "A-III", "C")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "D  Assessment-specific completeness",
       x = NULL, y = "% of eligible patients") +
  guides(fill = guide_legend(title.position = "right", ncol = 1)) +
  theme(legend.title = element_text(size = 9, colour = "grey30"),
        legend.text  = element_text(size = 9))
f5
# F6 – SCIM version transition
f6 <- ggplot(t5, aes(factor(Year_DOI), n, fill = scim_version)) +
  geom_col(position = "fill", width = 0.8) +
  scale_fill_manual(values = c("SCIM II" = pal$scim2, "SCIM III" = pal$scim3,
                               "Version not recorded" = pal$grey_light), name = NULL) +
  every_5_years +
  scale_y_continuous(labels = percent) +
  labs(title = "E  SCIM version used over time",
       x = "Year of injury", y = "% of SCIM assessments")
f6
# F7 – missingness patterns
f7 <- t4 %>%
  mutate(missing_pattern = fct_reorder(missing_pattern, n)) %>%
  ggplot(aes(pct, missing_pattern)) +
  geom_col(fill = pal$pattern, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "F  Patterns of missing follow-up", x = "% of patients", y = NULL)

# Save individual figures
save_fig <- function(p, name, w, h) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(out_dir, paste0(name, ".pdf")), p, width = w, height = h, device = pdf)
}
n_years <- length(unique(na.omit(pt$Year_DOI)))
save_fig(f1, "F1_heatmap_completeness_year_stage", 8, 2 + 0.3 * n_years)
save_fig(f2, "F2_retention_curves", 8, 5)
save_fig(f3, "F3_followup_status_by_year", 9, 5.5)
save_fig(f4, "F4_instrument_completeness", 11, 4.5)
save_fig(f6, "F5_scim_version_by_year", 8, 4)
save_fig(f7, "F6_missingness_patterns", 7, 3.5)

# Combined multi-panel figure (e.g. for supplement)
combined <- (f1 | f2) / f3 / f4 / (f6 | f7) +
  plot_layout(heights = c(1.4, 1, 1, 0.8))
save_fig(combined, "Figure_retention_combined", 15, 20 + 0.15 * n_years)

## ---- 6. DRAFT TEXT FOR THE RESPONSE LETTER ---------------------------------
ov  <- filter(t1, Year_DOI == "Overall")
so  <- stage_overall %>% mutate(txt = sprintf("%s %.1f%%", ExamStage, pct_assessed))
yrs <- range(pt$Year_DOI, na.rm = TRUE)
inst_chronic <- t3 %>% filter(ExamStage == "chronic") %>%
  select(instrument, status, pct) %>%
  pivot_wider(names_from = status, values_from = pct, values_fill = 0)

death_sentence <- if (is.na(ov$n_deceased)) {
  paste("Mortality and reasons for non-attendance are not systematically captured in the",
        "EMSCI database; missing follow-up therefore cannot be differentiated into death,",
        "withdrawal, transfer to non-participating centres or true loss to follow-up, and",
        "the figures below represent non-attendance rather than confirmed loss to follow-up.")
} else sprintf("%.1f%% of patients were recorded as deceased.",
               pct(ov$n_deceased, ov$n_registered))

txt <- c(
  sprintf("Among patients injured between %d and %d, %.1f%% had at least one assessment and %.1f%% had an early baseline assessment (very acute, <16 days, or acute I, 16-40 days post-injury).",
          yrs[1], yrs[2], pct(ov$n_any_assessment, ov$n_registered),
          pct(ov$n_early_baseline, ov$n_registered)),
  sprintf("Using stage-specific denominators restricted to patients whose assessment window had closed before the data cut-off (%s), the proportion assessed was: %s.",
          format(data_cutoff, "%d %B %Y"), paste(so$txt, collapse = "; ")),
  sprintf("Of the patients due for the chronic assessment (300-446 days post-injury), %.1f%% attended and %.1f%% did not attend.",
          ov$pct_chronic_retention, ov$pct_lost),
  death_sentence,
  sprintf("Missing-data patterns: %s.",
          paste(sprintf("%s %.1f%%", t4$missing_pattern, t4$pct), collapse = "; ")),
  "Assessment-specific completeness at the chronic stage (valid score / not testable / not assessed):",
  sprintf("  %s: %.1f%% / %.1f%% / %.1f%%", inst_chronic$instrument,
          inst_chronic$`Valid score`, inst_chronic$`Not testable`,
          inst_chronic$`Not assessed`)
)
writeLines(txt, file.path(out_dir, "draft_response_text.txt"))
cat(txt, sep = "\n")
cat("\nAll outputs written to:", normalizePath(out_dir), "\n")
