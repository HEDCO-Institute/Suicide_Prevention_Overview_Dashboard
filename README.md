# Suicide Prevention Overview Dashboard

**Date of Release:** 9/16/2026
**Title:** Findings From School-Based Suicide Prevention Overview of Reviews <br>
**Repo Authors:** Shaina Trevino

## **🔹 Overview**

This repository contains the code and data to produce our interactive [Suicide Prevention Overview Dashboard]([https://hedco-institute.shinyapps.io/suicide_prevention_overview_dashboard/]).

This repository follows **[AEA Data and Code Availability Standards](https://datacodestandard.org/)** and includes:
- Datasets used to generate reported results.
- R code necessary to reproduce the dashboard.
- Computational environment details to ensure reproducibility.

## **🔹 Data and Code Availability Statement**

### **Data Sources**
The data used in this dashboard are extracted estimates and GRADE certainty ratings from a set of included systematic reviews on suicide prevention interventions.
- Metadata (variable names and descriptions) are provided in the `data/SPO_overview_dashboard_codebook.xlsx` file.

The following datasets used for the dashboard are available in the `data` subfolder:

| Data File | Description | Data Structure | 
|-----------|-------------|-----------|-----------|
| `SPO_GRADE_Certainty.xlsx` | Extracted estimates from reviews, GRADE certainty ratings, and narrative interpretations | One row per extracted estimate | 
| `spo_review_pdf_links.xlsx` | Links from each included review to its source PDF | One row per included review |
| `SPO_overview_dashboard_codebook.xlsx` | Descriptions for variables in data files | One tab per data file, One row per variable |
<br>

### **Handling of Missing Data**
- Some subgroup/moderator estimates do not report enough information to reliably determine which subgroup was favored. These are coded in `SPO_GRADE_Certainty.xlsx`'s `Favored group` column as "Cannot detect due to XX certainty rating".
- Missing values in optional fields (e.g., `Data Collection Notes`, `Applicability concerns`) were intentionally left blank in the source file.

## **🔹 Code**
The script used to generate this dashboard is in the repository root:
- `app.R` contains all the code to load the raw data, transform and compute necessary fields, and generate the dashboard.

Within `app.R`, a `DEV_MODE` flag near the top of the Configuration section controls whether row labels are prefixed with their internal reference ID and whether the detail sidebar shows Reference ID/Estimate ID fields for testing.


## **🔹 Instructions for Replication**

### **Data Preparation and Analysis**
To replicate our results:

**If you have RStudio and Git installed and connected to your GitHub account:**

1. Clone the [repository]([https://github.com/HEDCO-Institute/Suicide_Prevention_Overview_Dashboard]) to your local machine ([click for help](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2))
1. Open the `.Rproj` R project in RStudio (this should automatically activate the `renv` environment)
1. Open and run the `app.R` script

**If you need to install or connect R, RStudio, Git, and/or GitHub:**

1. [Create a GitHub account](https://happygitwithr.com/github-acct.html#github-acct)
1. [Install R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
1. [Install Git](https://happygitwithr.com/install-git.html)
1. [Link Git to your GitHub account](https://happygitwithr.com/hello-git.html)
1. [Sign into GitHub in RStudio](https://happygitwithr.com/https-pat.html)

**To reproduce our results without using Git and GitHub, you may use the following steps:**

1. Download the ZIP file from the [repository]([https://github.com/HEDCO-Institute/Suicide_Prevention_Overview_Dashboard])
1. Extract all files to your local machine
1. Open the `.Rproj` R project in RStudio (this will automatically set the working directory and activate the `renv` environment)
1. Open and run the `app.R` script

## **🔹 Computational Requirements**

### **Software Environment**
- **R Version:** 4.5.2
- **Operating System:** Windows 11
- **R Packages:** `shiny`, `reactable`, `rio`, `here`, `tidyverse`, `htmltools`, `glue`

### **Reproducing the Environment**
Opening the R project will automatically install the correct package versions and set up the environment using the `renv` package. To manually load the environment:
 
1. Install `renv` (if not already installed):
```r
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
```
 
2. Restore the packages recorded in `renv.lock`:
```r
renv::restore()
```
 
3. If needed, load the environment:
```r
renv::load()
```

### **Licensing**
The data and code in this repository are licensed under a Creative Commons Attribution 4.0 International License (CC BY 4.0); see the LICENSE file in the main root directory for full terms.

## **🔹 Contact Information**
For questions about this replication package, contact:
✉️ **Shaina Trevino** (strevino@uoregon.edu)
