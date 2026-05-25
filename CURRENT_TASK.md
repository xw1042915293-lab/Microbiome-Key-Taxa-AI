# CURRENT_TASK.md

## Current Phase

Phase 8: Final reproducible report integration

## Previous Status

Phase 1: PASS  
Phase 2: PASS  
Phase 3: PASS  
Phase 4A: PASS  
Phase 4B: PASS  
Phase 5: PASS  
Phase 6: PASS  
Phase 7: PASS  

## Goal

Integrate all completed analysis outputs into a final reproducible HTML report.

Do not implement new analysis methods in Phase 8.

## Allowed Files

Read or modify only:

- templates/report_template.qmd
- R/report_prepare.R
- R/report_render.R
- R/workflow_run.R if needed
- modules/mod_report.R if needed
- scripts/phase8_smoke.R

## Inputs

Use existing job_dir files:

- tables/alpha_diversity.csv
- tables/alpha_stats.csv
- tables/beta_pcoa_coordinates.csv
- tables/beta_permanova.csv
- tables/differential_taxa.csv
- tables/differential_taxa_significant.csv
- tables/ml_feature_importance.csv
- tables/ml_model_metrics.csv
- tables/network_nodes.csv
- tables/network_edges.csv
- tables/key_taxa_score.csv
- tables/key_taxa_top20.csv

- json/diff_summary.json
- json/ml_summary.json
- json/network_summary.json
- json/key_taxa_summary.json
- json/llm_request_diff.json
- json/llm_response_diff.json

- ai/diff_interpretation.md
- ai/methods.md
- ai/figure_legends.md
- ai/llm_diff_interpretation.md
- ai/llm_methods.md
- ai/llm_figure_legends.md

- figures/alpha_shannon_boxplot.png
- figures/beta_pcoa_bray.png
- figures/diff_taxa_barplot.png
- figures/ml_importance.png
- figures/ml_confusion_matrix.png
- figures/network_plot.png
- figures/key_taxa_score_barplot.png

## Outputs

Generate:

- report/report.html

PDF is optional in Phase 8. Do not fail the phase if PDF generation is unavailable.

## Report Sections

The final report must include:

1. Project Overview
2. Data Quality Summary
3. Methods
4. Alpha Diversity
5. Beta Diversity
6. Differential Abundance Analysis
7. AI-Constrained Interpretation
8. Machine Learning Biomarker Screening
9. Co-occurrence Network Analysis
10. Key Taxa Score
11. Candidate Key Taxa Summary
12. Figure Legends
13. Reproducibility Record
14. Supplementary Tables

## Reproducibility Record

The report must include:

- job_id
- analysis time
- group variable
- taxonomic level
- R version
- microeco version if available
- input file names if available
- generated output files
- Key Taxa Score formula
- AI interpretation rule summary

## Smoke Test

Create:

- scripts/phase8_smoke.R

The smoke test should:

1. use a completed Phase 7 job_dir
2. render the final HTML report
3. check that report/report.html exists
4. check that the report includes the key sections:
   - Alpha Diversity
   - Beta Diversity
   - Differential Abundance
   - Machine Learning
   - Network Analysis
   - Key Taxa Score
   - Reproducibility Record
5. print PASS or FAIL

Run the smoke test at most once.

If it fails, stop and report the error.

## Do Not Do

- Do not implement new statistical methods
- Do not implement new ML models
- Do not implement new network methods
- Do not modify Phase 1–7 logic
- Do not call LLM API
- Do not read PROJECT_SPEC.md
- Do not scan the whole repository