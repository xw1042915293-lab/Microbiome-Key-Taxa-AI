# Phase 7 Acceptance Record (Key Taxa Score)

## 1. Phase 7 验收结论

PASS

## 2. Smoke test

```bash
Rscript scripts/phase7_smoke.R
```

## 3. Smoke test 结果

PASS

## 4. job_dir

`D:/Microbiome Key Taxa AI/results/job_20260522_125641_fflx2v`

## 5. 已生成文件

- `tables/key_taxa_score.csv`
- `tables/key_taxa_top20.csv`
- `json/key_taxa_summary.json`
- `figures/key_taxa_score_barplot.png`
- `figures/key_taxa_score_barplot.pdf`

## 6. 验收确认

- `key_taxa_score.csv` 包含规定列
- `key_taxa_score` 均在 0–1 范围内
- `rank` 按 `key_taxa_score` 降序排列
- `recommendation_level` 已生成
- `key_taxa_summary.json` 包含 `used_sources`、`weights`、`reliability`
- 未调用 LLM API

## 7. Phase 7 不包含

- 新机器学习模型
- 新网络方法
- PDF 报告

