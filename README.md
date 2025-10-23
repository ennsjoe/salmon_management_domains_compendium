# LAPSE: Legislation Applicable to Pacific Salmon and Ecosystems

[![Project Status](https://img.shields.io/badge/status-active-success.svg)]()
[![R Version](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)]()

## Overview

**LAPSE** (Legislation Applicable to Pacific Salmon and Ecosystems) is a comprehensive framework and tool for categorizing Canadian federal and provincial legislation by their relevance to Pacific salmon conservation and management. The project bridges the gap between legal language and natural resource management by mapping legislation to internationally recognized biodiversity threat categories (IUCN threats) through a novel system of "management domains."

### Key Features

- **Automated HTML parsing** of Canadian federal and British Columbia provincial legislation
- **Keyword-based classification** system linking legal clauses to salmon management concerns
- **SQLite database** containing structured legislative data with semantic labels
- **Interactive Shiny dashboard** for exploring legislation by management domain, IUCN threat, jurisdiction, and more
- **Technical documentation** via R Markdown report with comprehensive visualizations
- **Management domain framework** that aligns regulatory concerns with conservation threats

## Project Structure

```
.
├── Database_Legislation_Table.R      # Parses HTML files and creates legislation metadata
├── Database_Paragraph_Table.R        # Extracts sections, headings, and paragraphs
├── Database_Paragraph_Label_Table.R  # Assigns management domains, IUCN threats, and scope
├── Database_CU_Tables.R              # Loads Conservation Unit and agency data
├── app.R                             # Shiny web application dashboard
├── LAPSE_Technical_Brief.Rmd         # Comprehensive technical report
├── Compendium_Checker.R              # Excel export utility for validation
├── Extract_Implements_Enhanced.R     # NLP-based extraction of legislative instruments
├── *.csv                             # Keyword and classification lookup tables
├── *_style.css                       # Styling for Shiny app and R Markdown
└── output/
    └── legislation.db                # SQLite database (generated)
```

## Installation

### Prerequisites

- R (>= 4.0)
- RStudio (recommended)
- Required R packages (see Dependencies section)

### Setup

1. **Clone the repository:**
   ```bash
   git clone https://github.com/yourusername/salmon_management_domains_compendium.git
   cd salmon_management_domains_compendium
   ```

2. **Install R dependencies:**
   ```r
   install.packages(c(
     "here", "data.table", "xml2", "rvest", "stringi", "stringr",
     "RSQLite", "DBI", "quanteda", "shiny", "shinyWidgets",
     "ggplot2", "dplyr", "tidyr", "networkD3", "reactable",
     "htmltools", "openxlsx", "beepr"
   ))
   ```

3. **Prepare HTML files:**
   - Create a `legislation_html/` directory in the project root
   - Download HTML versions of legislation from [CanLII](https://www.canlii.org/) or official government websites
   - Place HTML files in `legislation_html/`

## Usage

### 1. Build the Database

Run the scripts in the following order to construct the legislation database:

```r
source("Database_Legislation_Table.R")      # Step 1: Extract legislation metadata
source("Database_Paragraph_Table.R")        # Step 2: Parse paragraphs and sections
source("Database_Paragraph_Label_Table.R")  # Step 3: Apply keyword matching and labels
source("Database_CU_Tables.R")              # Step 4: Load Conservation Unit data
```

This will create `output/legislation.db` containing all structured data.

### 2. Launch the Dashboard

```r
shiny::runApp("app.R")
```

The interactive dashboard allows you to:
- Filter legislation by **Management Domain** (e.g., Fisheries, Pollution, Water Use)
- Toggle between **Federal** and **Provincial** jurisdiction
- Select specific **Acts** and **Regulations**
- Search for keywords across all legislation
- View **keyword frequency**, **IUCN threat co-occurrence**, and **clause type distribution** visualizations

### 3. Generate the Technical Report

```r
rmarkdown::render("LAPSE_Technical_Brief.Rmd")
```

This produces an HTML report with:
- Background on Canadian salmon legislation
- Methodology for keyword development and matching
- Results including section counts by management domain, IUCN threat, and scope
- Case studies on specific threats (e.g., Geological Events, Dams & Water Management)
- Interactive tables and visualizations

### 4. Export for Validation

```r
source("Compendium_Checker.R")
```

Exports an Excel file (`Compendium Checker.xlsx`) with paragraph-level metadata, labels, and matched keywords for manual review.

## Management Domains

The LAPSE framework categorizes legislation into the following management domains:

- **Agriculture**
- **Aquaculture and Hatcheries**
- **Climate Change and Natural Disasters**
- **Fisheries**
- **Forest and Range**
- **Human Disturbance**
- **Invasive or Problematic Species and Disease**
- **Mining and Energy**
- **Pollution**
- **Restoration**
- **Spatial Designation**
- **Species Status and Assessment**
- **Transportation Infrastructure**
- **Water Use and Watercourse Modifications**

Each domain aligns with one or more IUCN Level 2 threat categories.

## Scope Classification

Legislation is also classified by its **scope** of relevance to Pacific salmon:

1. **Salmon** – Explicitly mentions Pacific salmon species
2. **Fish** – Refers to fish that include salmon
3. **Habitat** – Refers to habitat that includes salmon habitat
4. **Governance** – Administrative processes and structure

## Data Sources

- **Legislation HTML**: [CanLII](https://www.canlii.org/), [Federal Legislation](https://laws-lois.justice.gc.ca/), [BC Legislation](https://www.bclaws.gov.bc.ca/)
- **IUCN Threat Classification**: [IUCN-CMP Direct Threats Classification 4.0](https://docs.google.com/spreadsheets/d/1yfm7ua9hQJpjycx6FYQJ6Jy5LA4bP0XP61EW3-sX8ZY/edit)
- **Conservation Unit Data**: DFO Recovery Potential Assessments

## Key Files

### CSV Lookup Tables

- `clause_type_keywords.csv` – Keywords for clause type classification
- `governance_keywords.csv` – Management domain keywords for governance
- `salmon_scope_keywords.csv` – Keywords indicating salmon-specific scope
- `management_domain_threat_table.csv` – Mapping of management domains to IUCN threats
- `cu_ranking.csv` – Conservation Unit threat rankings from COSEWIC RPAs
- `legislation_url.csv` – URLs to full legislation on government websites

### Database Schema

The SQLite database (`output/legislation.db`) contains:

1. **LegislationMetadata** – Jurisdiction, act name, legislation name and type
2. **LegislationParagraphs** – Section, heading, and paragraph text
3. **paragraph_label_table** – Labels (Management Domain, IUCN, Clause Type, Scope) with matched keywords
4. **agencies** – Responsible ministers and agencies
5. **cu_ranking** – Conservation Unit and threat assessment data
6. **Keyword tables** – Reference tables for all keyword lists

## Dependencies

### Core R Packages

- `here` – Project-relative paths
- `data.table` – High-performance data manipulation
- `xml2`, `rvest` – HTML parsing
- `stringi`, `stringr` – Text processing
- `RSQLite`, `DBI` – Database connectivity
- `quanteda` – Text analysis and keyword matching

### Dashboard

- `shiny`, `shinyWidgets` – Interactive web application
- `ggplot2` – Visualizations
- `dplyr`, `tidyr` – Data wrangling
- `reactable`, `htmltools` – Interactive tables

### Reporting

- `rmarkdown`, `knitr` – Dynamic documents
- `networkD3` – Sankey diagrams
- `openxlsx` – Excel export

### Optional (for enhanced extraction)

- `udpipe` – NLP and dependency parsing
- `caret`, `randomForest` – Machine learning

## Citation

If you use LAPSE in your research, please cite:

> Enns, J., and Lagasse, C. (2025). *Legislation Applicable to Pacific Salmon and Ecosystems (LAPSE): A Framework for Categorizing Canadian Legislation by Salmon Management Domains.* Technical Brief. [URL]

## Authors

- **Joe Enns** – Project lead, methodology, database development
- **Cory Lagasse** – Co-author, keyword development, dashboard design
- **Max Elinson** – Contributing developer

## License

This project is licensed under the [MIT License](LICENSE).

## Acknowledgments

- IUCN and Conservation Measures Partnership for the Direct Threats Classification framework
- Fisheries and Oceans Canada (DFO) for Recovery Potential Assessment data
- CanLII for providing accessible legal databases

## Disclaimer

⚠️ **None of the information presented in this project qualifies as legal advice.** The authors are aquatic biologists with limited legal training. This tool is intended for research and informational purposes only.

## Contact

For questions, issues, or collaboration inquiries, please open an issue on GitHub or contact:

- Joe Enns: [email or contact info]
- Cory Lagasse: [email or contact info]

---

**Last Updated:** January 2025
