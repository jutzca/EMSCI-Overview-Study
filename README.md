# EMSCI Retention and Missing-Data Analysis

R code for the retention, assessment completeness, and missing-data analyses reported in:

**The European Multi-Center Study about Spinal Cord Injury (EMSCI): An International Patient Registry and Collaborative Network**  
Jutzeler et al., 2026. *Spinal Cord*.

## About EMSCI

The European Multi-Center Study about Spinal Cord Injury (EMSCI) is an international prospective patient registry and collaborative research network established in 2001. By 2025, EMSCI included 31 participating centres across 11 countries and had enrolled 7,502 participants (ClinicalTrials.gov identifier: [NCT01571531](https://clinicaltrials.gov/study/NCT01571531)).

Clinical assessments are scheduled across five post-injury time windows:

| Assessment stage | Time after injury |
|---|---|
| Very acute | <16 days |
| Acute I | 16–40 days |
| Acute II | 70–98 days |
| Acute III | 150–186 days |
| Chronic | 300–446 days |

## What the analysis does

The script (`emsci_retention_analysis.R`) performs the following analyses:

### 1. Assessment retention

- Calculates patient retention at each assessment stage.
- Reports overall retention and retention stratified by year of injury.
- Accounts for the data cut-off date: an assessment stage is included in the retention denominator only if its assessment window has closed by the specified cut-off date.

### 2. Instrument-level data completeness

Reports assessment completeness separately for each of the following instruments:

- **ISNCSCI** – International Standards for Neurological Classification of Spinal Cord Injury
- **SCIM** – Spinal Cord Independence Measure
- **WISCI** – Walking Index for Spinal Cord Injury
- **NCS** – Nerve Conduction Studies
- **SSEP** – Somatosensory Evoked Potentials
- **MEP** – Motor Evoked Potentials

For each instrument, assessments are classified as:

- Valid score available
- Not testable
- Not assessed

### 3. Patient-level missing-data patterns

Classifies each patient's longitudinal assessment pattern into five categories:

- **Complete:** All eligible assessment stages have available assessments.
- **Monotone dropout:** Assessments are available up to a certain stage, followed by consecutive missing assessments.
- **Intermittent missing:** Missing assessments occur between observed assessments.
- **Late entry:** The first available assessment occurs after the initial assessment stage.
- **Follow-up ongoing:** One or more scheduled assessment windows have not yet closed at the data cut-off.

### 4. SCIM version transition

Visualises the transition from SCIM II to SCIM III over calendar time.

## Requirements

The analysis was developed in R and uses the following packages:

```r
install.packages(c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "forcats",
  "scales",
  "patchwork"
))
```

## Input data

The script requires a long-format CSV export with one row per patient and assessment stage.

### Required columns

| Column | Description |
|---|---|
| `RandomID` | Unique patient identifier |
| `ExamStage` | Assessment stage |
| `Year_DOI` | Year of date of injury |

### Optional columns

- `DOI`: Date of injury.
- Instrument-specific columns: Required for the corresponding instrument-level completeness analyses.
- Death variable: Can be specified if a death indicator is available in the export.

Patient-level identifiers and assessment stages must be consistently represented across records.

**Note:** Patient-level data are not included in this repository.

## Usage

1. Clone or download this repository.
2. Install the required R packages.
3. Specify the input CSV file and analysis settings.
4. Run the script.

```r
# Specify the input file.
# If left unset, the script opens a file-selection dialog.
input_file <- "path/to/emsci_export.csv"

# Source the analysis script.
source("emsci_retention_analysis.R")
```

### Analysis settings

Before running the analysis, configure the following settings in the script:

| Setting | Description |
|---|---|
| `input_file` | Path to the EMSCI CSV export. |
| `data_cutoff` | Date of the data export / analysis cut-off. |
| `death_var` | Name of the death indicator column, if available. |

The data cut-off date is particularly important because it determines which assessment windows are considered eligible for retention and missing-data analyses.

## Outputs

All results are written to a directory named `retention_output/`, located next to the input data file.

The script generates:

- **Tables:** CSV files T1–T5.
- **Figures:** Individual figures F1–F6 and one combined figure.
- **Reviewer response:** A draft paragraph summarising the retention and missing-data findings for use in the manuscript's reviewer response.

The output files can be used for further inspection, reporting, and manuscript preparation.

## Data availability

Patient-level EMSCI data are not publicly available and are not included in this repository.

Access to EMSCI data is governed by the EMSCI Steering Board. Further information is available at [emsci.org](https://www.emsci.org/).

## Contact

**Prof. Dr. sc. nat. Catherine R. Jutzeler**  
Professor of Biomedical Data Science  
ETH Zürich  
[catherine.jutzeler@hest.ethz.ch](mailto:catherine.jutzeler@hest.ethz.ch)

## Citation

If you use this code, please cite the associated publication:

Jutzeler CR et al. (2026). *The European Multi-Center Study about Spinal Cord Injury (EMSCI): An International Patient Registry and Collaborative Network.* **Spinal Cord**.
