# Phase 3 Acceptance Record

## Conclusion

PASS

## Smoke Test

Command:

```text
Rscript scripts/phase3_smoke.R
```

Result:

```text
phase3 ok
```

## Latest Job Directory

```text
D:/Microbiome Key Taxa AI/results/job_20260522_125641_fflx2v
```

## Confirmed Output Files

- `tables/differential_taxa.csv`
- `tables/differential_taxa_significant.csv`
- `json/diff_summary.json`
- `figures/diff_taxa_barplot.png`
- `figures/diff_taxa_barplot.pdf`
- `report/report.html`

## Notes

- When no taxa pass `FDR < 0.05`, the pipeline still generates the significant table, summary JSON, and an exploratory barplot.
- The barplot subtitle is labeled: `Exploratory: no FDR-significant taxa`.
- Phase 3 does not include AI, machine learning, network analysis, Key Taxa Score, or PDF report generation.

