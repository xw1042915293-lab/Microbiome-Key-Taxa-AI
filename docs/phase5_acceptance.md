# Phase 5 验收记录

## 验收结论

PASS

## Smoke Test

`Rscript scripts/phase5_smoke.R`

## 已生成的核心文件

- `tables/ml_feature_importance.csv`
- `tables/ml_model_metrics.csv`
- `json/ml_summary.json`
- `figures/ml_importance.png`
- `figures/ml_importance.pdf`
- `figures/ml_confusion_matrix.png`
- `figures/ml_confusion_matrix.pdf`
- 二分类时：`figures/ml_roc.png` / `figures/ml_roc.pdf`

## 验收确认

- Random Forest 已完成
- `ml_summary.json` 包含 `sample_size` 和 `reliability`
- 没有因果化表述
- `R/` 业务函数没有访问 Shiny `reactiveValues`

## Phase 5 不包含

- network analysis
- Key Taxa Score
- PDF report
