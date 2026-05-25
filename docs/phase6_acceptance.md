# Phase 6 Acceptance

## 1. Phase 6 验收结论

PASS

## 2. Smoke Test

```bash
Rscript scripts/phase6_smoke.R
```

## 3. Smoke Test 结果

PASS

## 4. 已实现内容

- Genus-level abundance matrix
- Spearman correlation network
- abs(rho) >= 0.6
- FDR < 0.05
- no-edge placeholder plot

## 5. 已生成核心文件

- tables/network_nodes.csv
- tables/network_edges.csv
- json/network_summary.json
- figures/network_plot.png
- figures/network_plot.pdf

## 6. 验收确认

- network_edges.csv 即使无边也列完整
- network_nodes.csv 包含中心性指标
- network_summary.json 已生成
- R/ 业务函数没有访问 Shiny reactiveValues

## 7. Phase 6 不包含

- Key Taxa Score
- PDF report
- 新的 LLM API 调用

