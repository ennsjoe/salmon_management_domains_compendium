################################################################################
# Title: Journal Article Figures - LAPSE
# Authors: Joe Enns, Cory Lagasse
# Date Created: 2025-01-XX
# Purpose / Description:
#   Creates publication-quality black and white figures for journal submission
#   from the LAPSE legislative database.
# Dependencies: DBI, RSQLite, data.table, ggplot2, here
# Outputs:
#   High-resolution PNG/PDF figures saved to "figures/" directory
################################################################################

## Load Libraries ----
library(here)
library(DBI)
library(RSQLite)
library(data.table)
library(ggplot2)
library(stringr)
library(scales)
library(networkD3)
library(htmlwidgets)
library(webshot2)

cat("=====================================\n")
cat("LAPSE Journal Figures Generator\n")
cat("=====================================\n\n")

## Setup ----

# Create figures directory if it doesn't exist
figures_dir <- here("figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
  cat("Created figures directory:", figures_dir, "\n")
}

# Connect to Database
db_path <- file.path(here("output"), "legislation.db")
if (!file.exists(db_path)) {
  stop("Database file not found at: ", db_path)
}

conn <- dbConnect(SQLite(), dbname = db_path)
cat("Connected to database\n\n")

# Load Tables
cat("Loading data...\n")
legislation_table <- as.data.table(dbReadTable(conn, "LegislationMetadata"))
paragraph_table <- as.data.table(dbReadTable(conn, "LegislationParagraphs"))
label_table <- as.data.table(dbReadTable(conn, "paragraph_label_table"))

# Load agencies table for Sankey charts
agencies_table <- tryCatch({
  as.data.table(dbReadTable(conn, "agencies"))
}, error = function(e) {
  cat("Warning: agencies table not found. Sankey charts will be skipped.\n")
  NULL
})

dbDisconnect(conn)
cat("Data loaded successfully\n\n")

## Define Journal Theme ----
# Black and white theme optimized for publication

theme_journal <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text
      text = element_text(color = "black", family = "sans"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0, size = base_size + 2),
      
      # Grid
      panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # Background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Legend
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
      legend.key = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      
      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Grayscale palette for categories
scale_fill_journal <- function(...) {
  scale_fill_manual(
    values = c(
      "Federal" = "gray30",
      "Provincial" = "gray60",
      "Unknown" = "gray90"
    ),
    ...
  )
}

## Helper Functions ----

save_figure <- function(plot, filename, width = 7, height = 5, dpi = 300) {
  # Save as PNG
  png_path <- file.path(figures_dir, paste0(filename, ".png"))
  ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  cat(sprintf("✓ Saved: %s\n", png_path))
  
  # Also save as PDF for vector graphics
  pdf_path <- file.path(figures_dir, paste0(filename, ".pdf"))
  ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf,
    bg = "white"
  )
  cat(sprintf("✓ Saved: %s\n", pdf_path))
}

# Special function for saving Sankey charts (networkD3 widgets)
save_sankey <- function(sankey, filename, width = 800, height = 600) {
  # Save as interactive HTML
  html_path <- file.path(figures_dir, paste0(filename, ".html"))
  saveWidget(sankey, html_path, selfcontained = TRUE)
  cat(sprintf("✓ Saved: %s\n", html_path))
  
  # Try to save as PNG using webshot2
  tryCatch({
    png_path <- file.path(figures_dir, paste0(filename, ".png"))
    webshot2::webshot(
      url = html_path,
      file = png_path,
      vwidth = width,
      vheight = height,
      delay = 1
    )
    cat(sprintf("✓ Saved: %s\n", png_path))
  }, error = function(e) {
    cat(sprintf("Note: Could not create PNG version of Sankey chart.\n"))
    cat(sprintf("      HTML version saved. Use browser to export as image if needed.\n"))
  })
}

## ============================================================================
## FIGURE 1: Management Domain Section Counts by Jurisdiction
## ============================================================================

cat("\nGenerating Figure 1: Management Domain Counts...\n")

# Extract Management Domain labels
domain_labels <- label_table[
  label_type == "Management Domain" & !is.na(label_value),
  .(paragraph_id, management_domain = label_value)
]

# Merge with paragraph and legislation data
merged_data <- merge(
  domain_labels,
  paragraph_table[, .(paragraph_id, Section, legislation_id)],
  by = "paragraph_id"
)

merged_data <- merge(
  merged_data,
  legislation_table[, .(legislation_id, jurisdiction)],
  by = "legislation_id"
)

# Count unique sections by domain and jurisdiction
domain_counts <- merged_data[
  !is.na(Section),
  .(section_count = uniqueN(paste(Section, legislation_id))),
  by = .(management_domain, jurisdiction)
]

# Order by total count
domain_order <- domain_counts[
  , .(total = sum(section_count)), 
  by = management_domain
][order(-total)]

domain_counts[, management_domain := factor(
  management_domain, 
  levels = rev(domain_order$management_domain)
)]

# Create plot
fig1 <- ggplot(domain_counts, aes(x = management_domain, y = section_count, fill = jurisdiction)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
  coord_flip() +
  scale_fill_journal(name = "Jurisdiction") +
  labs(
    title = "Legislative Sections by Management Domain and Jurisdiction",
    x = "Management Domain",
    y = "Number of Sections"
  ) +
  theme_journal()

save_figure(fig1, "figure_01_domain_jurisdiction", width = 8, height = 6)

## ============================================================================
## FIGURE 2: IUCN Threat Category Distribution
## ============================================================================

cat("\nGenerating Figure 2: IUCN Threat Distribution...\n")

# Extract IUCN labels
iucn_labels <- label_table[
  label_type == "IUCN" & !is.na(label_value),
  .(paragraph_id, iucn_threat = label_value)
]

# Merge and count
iucn_merged <- merge(
  iucn_labels,
  paragraph_table[, .(paragraph_id, Section, legislation_id)],
  by = "paragraph_id"
)

iucn_merged <- merge(
  iucn_merged,
  legislation_table[, .(legislation_id, jurisdiction)],
  by = "legislation_id"
)

iucn_counts <- iucn_merged[
  !is.na(Section),
  .(section_count = uniqueN(paste(Section, legislation_id))),
  by = .(iucn_threat, jurisdiction)
]

# Order by total count
iucn_order <- iucn_counts[
  , .(total = sum(section_count)), 
  by = iucn_threat
][order(-total)]

iucn_counts[, iucn_threat := factor(
  iucn_threat, 
  levels = rev(iucn_order$iucn_threat)
)]

# Create plot
fig2 <- ggplot(iucn_counts, aes(x = iucn_threat, y = section_count, fill = jurisdiction)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
  coord_flip() +
  scale_fill_journal(name = "Jurisdiction") +
  labs(
    title = "Legislative Sections by IUCN Threat Category",
    x = "IUCN Threat Category (Level 2)",
    y = "Number of Sections"
  ) +
  theme_journal(base_size = 9)

save_figure(fig2, "figure_02_iucn_threats", width = 8, height = 8)

## ============================================================================
## FIGURE 3: Scope Distribution
## ============================================================================

cat("\nGenerating Figure 3: Scope Distribution...\n")

# Extract scope labels
scope_labels <- label_table[
  label_type %in% c("Management Domain", "Salmon Scope") & !is.na(scope),
  .(paragraph_id, scope)
]

# Remove duplicates and merge
scope_labels <- unique(scope_labels)
scope_merged <- merge(
  scope_labels,
  paragraph_table[, .(paragraph_id, Section, legislation_id)],
  by = "paragraph_id"
)

scope_counts <- scope_merged[
  !is.na(Section),
  .(section_count = uniqueN(paste(Section, legislation_id))),
  by = scope
]

# Order by scope level
scope_counts[, scope := factor(scope, levels = c("1 - Salmon", "2 - Fish", "3 - Habitat", "4 - Governance"))]

# Create plot
fig3 <- ggplot(scope_counts, aes(x = scope, y = section_count)) +
  geom_bar(stat = "identity", fill = "gray40", color = "black", linewidth = 0.3) +
  labs(
    title = "Legislative Sections by Scope Category",
    x = "Scope",
    y = "Number of Sections"
  ) +
  theme_journal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figure(fig3, "figure_03_scope_distribution", width = 6, height = 5)

## ============================================================================
## FIGURE 4: Clause Type Distribution
## ============================================================================

cat("\nGenerating Figure 4: Clause Type Distribution...\n")

# Extract clause type labels
clause_labels <- label_table[
  label_type == "Clause Type" & !is.na(label_value),
  .(paragraph_id, clause_type = label_value)
]

clause_merged <- merge(
  clause_labels,
  paragraph_table[, .(paragraph_id, Section, legislation_id)],
  by = "paragraph_id"
)

clause_counts <- clause_merged[
  !is.na(Section),
  .(section_count = uniqueN(paste(Section, legislation_id))),
  by = clause_type
]

# Order by count
clause_counts <- clause_counts[order(-section_count)]
clause_counts[, clause_type := factor(clause_type, levels = clause_type)]

# Create plot
fig4 <- ggplot(clause_counts, aes(x = clause_type, y = section_count)) +
  geom_bar(stat = "identity", fill = "gray40", color = "black", linewidth = 0.3) +
  coord_flip() +
  labs(
    title = "Legislative Sections by Clause Type",
    x = "Clause Type",
    y = "Number of Sections"
  ) +
  theme_journal()

save_figure(fig4, "figure_04_clause_types", width = 7, height = 5)

## ============================================================================
## FIGURE 5: Domain Co-occurrence Network
## ============================================================================

cat("\nGenerating Figure 5: Domain Co-occurrence...\n")

# Get paragraphs with management domains
domain_para <- label_table[
  label_type == "Management Domain" & !is.na(label_value),
  .(paragraph_id, management_domain = label_value)
]

# Merge with section info
domain_para <- merge(
  domain_para,
  paragraph_table[, .(paragraph_id, Section, legislation_id)],
  by = "paragraph_id"
)

# Group by section to find co-occurrences
section_domains <- domain_para[
  , .(domains = list(unique(management_domain))),
  by = .(Section, legislation_id)
]

# Generate pairs
pairs_list <- lapply(section_domains$domains, function(doms) {
  if (length(doms) < 2) return(NULL)
  pairs <- combn(sort(doms), 2, simplify = FALSE)
  data.table(
    domain1 = sapply(pairs, `[`, 1),
    domain2 = sapply(pairs, `[`, 2)
  )
})

pairs_dt <- rbindlist(pairs_list[!sapply(pairs_list, is.null)])

# Count co-occurrences
cooccur <- pairs_dt[, .N, by = .(domain1, domain2)][order(-N)]

# Take top 15 for readability
cooccur_top <- head(cooccur, 15)

# Create a combined label for plotting
cooccur_top[, pair := paste(domain1, "↔", domain2)]
cooccur_top[, pair := factor(pair, levels = pair)]

# Create plot
fig5 <- ggplot(cooccur_top, aes(x = pair, y = N)) +
  geom_bar(stat = "identity", fill = "gray40", color = "black", linewidth = 0.3) +
  coord_flip() +
  labs(
    title = "Top 15 Management Domain Co-occurrences",
    x = "Domain Pair",
    y = "Number of Co-occurring Sections"
  ) +
  theme_journal(base_size = 9)

save_figure(fig5, "figure_05_domain_cooccurrence", width = 8, height = 6)

## ============================================================================
## FIGURE 6: Sankey Diagrams - Legislation to Agency (Federal & Provincial)
## ============================================================================

if (!is.null(agencies_table)) {
  cat("\nGenerating Figure 6: Sankey Charts (Legislation → Agency)...\n")
  
  # Compute unique section count per legislation_id
  section_counts <- paragraph_table[
    !is.na(Section),
    .(section_count = uniqueN(Section)),
    by = legislation_id
  ]
  
  # Merge with legislation metadata - FILTER TO ACTS ONLY
  section_counts <- merge(
    section_counts,
    legislation_table[legislation_type == "Act", .(legislation_id, act_name, jurisdiction)],
    by = "legislation_id",
    all.x = FALSE
  )
  
  # Merge with agencies
  sankey_data <- merge(
    agencies_table,
    section_counts,
    by = "act_name",
    all.x = TRUE
  )
  
  # Remove rows with NA section_count
  sankey_data <- sankey_data[!is.na(section_count)]
  
  # Function to build Sankey for a given jurisdiction
  build_sankey_journal <- function(data, jurisdiction_label, color_gray) {
    data <- data[jurisdiction == jurisdiction_label]
    if (nrow(data) == 0) {
      cat(sprintf("  No data for %s jurisdiction.\n", jurisdiction_label))
      return(NULL)
    }
    
    # Create nodes
    nodes <- unique(c(data$act_name, data$agency))
    nodes_dt <- data.table(name = nodes)
    nodes_dt[, group := jurisdiction_label]
    
    # Create links (legislation → agency)
    data[, source := match(act_name, nodes) - 1]
    data[, target := match(agency, nodes) - 1]
    
    links <- data[, .(source, target, value = section_count), by = .(source, target)]
    
    # Create grayscale color scheme for journal
    color_js <- sprintf(
      "d3.scaleOrdinal().domain(['Federal', 'Provincial']).range(['%s', '%s'])",
      color_gray[1],
      color_gray[2]
    )
    
    sankey <- sankeyNetwork(
      Links = links,
      Nodes = nodes_dt,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 12,
      nodeWidth = 30,
      NodeGroup = "group",
      colourScale = JS(color_js),
      fontFamily = "sans-serif",
      iterations = 100
    )
    
    return(sankey)
  }
  
  # Define grayscale colors for journal
  gray_colors <- c("gray30", "gray60")
  
  # Generate Federal Sankey
  cat("  Creating Federal jurisdiction Sankey...\n")
  sankey_federal <- build_sankey_journal(
    sankey_data, 
    "Federal", 
    gray_colors
  )
  
  if (!is.null(sankey_federal)) {
    save_sankey(
      sankey_federal, 
      "figure_06a_sankey_federal",
      width = 900,
      height = 700
    )
  }
  
  # Generate Provincial Sankey
  cat("  Creating Provincial jurisdiction Sankey...\n")
  sankey_provincial <- build_sankey_journal(
    sankey_data, 
    "Provincial", 
    gray_colors
  )
  
  if (!is.null(sankey_provincial)) {
    save_sankey(
      sankey_provincial, 
      "figure_06b_sankey_provincial",
      width = 900,
      height = 700
    )
  }
  
} else {
  cat("\nSkipping Figure 6: agencies table not available\n")
}

## ============================================================================
## Summary Report
## ============================================================================

cat("\n=====================================\n")
cat("FIGURE GENERATION COMPLETE\n")
cat("=====================================\n\n")

cat("Generated figures:\n")
cat("  1. Management Domain × Jurisdiction\n")
cat("  2. IUCN Threat Distribution\n")
cat("  3. Scope Distribution\n")
cat("  4. Clause Type Distribution\n")
cat("  5. Domain Co-occurrence\n")
if (!is.null(agencies_table)) {
  cat("  6a. Sankey: Federal Legislation → Agency\n")
  cat("  6b. Sankey: Provincial Legislation → Agency\n")
}
cat("\n")

cat(sprintf("All figures saved to: %s\n", figures_dir))
cat("\nStatic figures include:\n")
cat("  - High-resolution PNG (300 dpi)\n")
cat("  - Vector PDF for publication\n")
if (!is.null(agencies_table)) {
  cat("\nSankey diagrams include:\n")
  cat("  - Interactive HTML version\n")
  cat("  - Static PNG (if webshot2 available)\n")
  cat("  - Open HTML in browser to manually export as image if needed\n")
}
cat("\n")

cat("=====================================\n")
cat("Ready for journal submission!\n")
cat("=====================================\n")