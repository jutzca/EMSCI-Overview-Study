## EMSCI retention and missing-data analysis

This is the R code for the retention and data-completeness analyses in:

# The European Multi-Center Study about Spinal Cord Injury (EMSCI): An International Patient Registry and Collaborative Network Jutzeler et al, 2026. Spinal Cord

## EMSCI is an international prospective registry for spinal cord injury, established in 2001 (31 centres, 11 countries, 7,502 participants up to 2025; NCT01571531). Assessments take place at five time windows: very acute (<16 days), acute I (16–40 days), acute II (70–98 days), acute III (150–186 days) and chronic (300–446 days).

What the script does
Calculates retention per stage, overall and by year of injury. A stage counts only for patients whose assessment window closed before the data cut-off.
Reports completeness separately for each instrument (ISNCSCI, SCIM, WISCI, NCS, SSEP, MEP): valid score, not testable, or not assessed.
Classifies each patient's missing-data pattern: complete, monotone dropout, intermittent missing, late entry, or follow-up still ongoing.
Shows the switch from SCIM II to SCIM III over calendar time.
Writes CSV tables (T1–T5), figures (F1–F6 plus one combined figure), and a draft paragraph for the reviewer response.
Usage
r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "forcats", "scales", "patchwork"))
input_file <- "path/to/emsci_export.csv"   # leave unset to pick the file in a dialog
source("emsci_retention_analysis.R")

Input: a long-format CSV with one row per patient and exam stage. Required columns are RandomID, ExamStage and Year_DOI. DOI and the instrument columns are optional.

Settings: set data_cutoff to your export date. Set death_var if the export contains a death column.

Output: results go to retention_output/, next to the input file.

Data availability

Patient data are not included. Access to EMSCI data is managed by the EMSCI Steering Board (emsci.org).

Contact

Catherine R. Jutzeler, catherine.jutzeler@hest.ethz.ch
