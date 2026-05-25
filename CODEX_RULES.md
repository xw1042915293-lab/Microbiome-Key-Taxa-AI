# CODEX_RULES.md

## Project

Project name: Microbiome Key Taxa AI

Chinese title:

基于 microeco 2.0 与大语言模型的微生物组关键菌筛选、可信解释与可复现报告生成系统

## Core Positioning

This project is not a general AI research platform.

This project is not a replacement for microeco.

This project is a vertical microbiome analysis system built on top of microeco 2.0. Its core goals are:

1. microbiome data validation
2. microeco-based downstream analysis
3. key taxa discovery
4. statistically constrained AI interpretation
5. reproducible HTML/PDF report generation

## Tech Stack

Use:

- R Shiny
- microeco
- vegan
- ggplot2
- Quarto
- SQLite
- LLM API
- renv

Do not use:

- Spring Boot
- Vue
- Qt
- UE
- large frontend/backend architecture

## Directory Rules

- `app.R` only loads UI and server.
- `global.R` only loads packages, source files, and global configuration.
- `R/` contains pure business logic functions.
- `modules/` contains Shiny modules.
- `templates/` contains Quarto and prompt templates.
- `results/job_xxx/` stores all job outputs.

## Shiny Reactive Rules

Very important:

1. Functions in `R/` must not access Shiny `reactiveValues`.
2. Functions in `R/` must not use `analysis_state$xxx`.
3. Business functions must accept normal R arguments.
4. Business functions must be runnable in normal `Rscript` mode.
5. Shiny modules may read `analysis_state$xxx`, but must pass ordinary values to business functions.

Wrong:

```r
run_basic_analysis <- function(analysis_state) {
  job_dir <- analysis_state$job_dir
}
```

Correct:

```r
run_basic_analysis <- function(input_data, job_dir, group_var, beta_distance = "bray") {
  # run analysis
}
```

## Output Rules

Every analysis job must save outputs under the current `job_dir`.

Required output types:

- tables: CSV
- figures: PNG and PDF
- objects: RDS
- JSON summaries: JSON
- AI outputs: Markdown
- reports: HTML/PDF

Do not only show results in Shiny. Results must be saved to disk.

## Analysis Function Rules

Every analysis function must:

1. check input arguments
2. return a plain R list
3. save outputs to `job_dir`
4. use `tryCatch` where appropriate
5. return readable error messages
6. not depend on Shiny runtime

## AI Rules

AI must not directly interpret raw abundance tables.

AI workflow must be:

```text
statistical results
↓
structured JSON
↓
rule-based significance and reliability checks
↓
LLM interpretation
```

AI must not:

1. describe non-significant results as significant
2. describe correlation as causation
3. invent biological mechanisms without evidence
4. ignore small sample size warnings
5. make disease-related claims without context

## Debugging Rules

1. Do not repeatedly run the same failing script.
2. The same smoke test may run at most 2 times.
3. If it fails twice, stop and summarize the error.
4. Do not automatically rewrite and rerun in a loop.
5. Before changing code, identify the exact file and function causing the error.
6. Fix the smallest necessary part first.
7. Do not expand features while fixing bugs.

## Current Development Principle

Build the project phase by phase.

Do not jump ahead.

Current order:

1. Phase 1: Upload and data validation
2. Phase 2: microeco object + Alpha/Beta diversity
3. Phase 3: differential analysis + basic Quarto report
4. Phase 4: statistically constrained AI interpretation
5. Phase 5: machine learning
6. Phase 6: network analysis
7. Phase 7: Key Taxa Score
8. Phase 8: full reproducible report
