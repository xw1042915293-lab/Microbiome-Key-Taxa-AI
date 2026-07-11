# 微生物组关键菌筛选与可信解释系统 V1.0

## 现场演示提纲

本文档用于答辩或软件演示时快速跑通一套完整流程，并核对“应该生成哪些文件、报告在哪里、重点展示哪些模块”。示例以仓库自带的 `data/example_*.tsv` 为准。正式演示前应先在冻结后的 V1.0 上完整预演。

## 示例数据位置
示例数据在项目根目录的 `data/`：
- `data/example_abundance.tsv`
- `data/example_metadata.tsv`
- `data/example_taxonomy.tsv`

建议直接用这一套数据进行现场演示，文件小、出图快、结构标准。

## 推荐上传顺序
在“快速开始”或“上传数据”页面按界面顺序导入：
1. abundance: `data/example_abundance.tsv`
2. metadata: `data/example_metadata.tsv`
3. taxonomy: `data/example_taxonomy.tsv`

然后创建任务并保存输入。

## 推荐选择的 group_var
在参数设置步骤，推荐选择：
- `Group`

说明：示例 metadata 同时包含 `Group` 与 `Treatment`，为了演示分组最清晰、结果最直观，优先用 `Group`。

## 完整运行后应生成哪些文件（用于现场核对）
运行完整分析流程后，进入对应任务目录：
`results/<job_id>/`

以下文件建议作为“跑通标志”进行核对（存在即可，不要求逐个打开）：

1. 输入固化
- `input/abundance.tsv`
- `input/metadata.tsv`
- `input/taxonomy.tsv`

2. 可复现记录
- `reproducibility.json`

3. 表格产物（tables）
- `tables/data_check_summary.csv`
- `alpha/tables/alpha_diversity.csv`
- `alpha/tables/alpha_stats.csv`
- `beta/tables/beta_pcoa_coordinates.csv`
- `beta/tables/beta_permanova.csv`
- `beta/tables/beta_dispersion.csv`
- `beta/tables/beta_dispersion_distances.csv`
- `tables/differential_taxa.csv`
- `tables/differential_taxa_significant.csv`
- `tables/ml_feature_importance.csv`
- `tables/ml_model_metrics.csv`
- `tables/network_nodes.csv`
- `tables/network_edges.csv`
- `tables/key_taxa_score.csv`
- `tables/key_taxa_top20.csv`

4. 图形产物（figures）
- `alpha/figures/overview/`：七类四指标概览图
- `alpha/figures/observed/`：Observed 七类图形
- `alpha/figures/chao1/`：Chao1 七类图形
- `alpha/figures/shannon/`：Shannon 七类图形
- `alpha/figures/simpson/`：Simpson 七类图形
- `beta/figures/pcoa/`：五类论文级 PCoA 图
- `beta/figures/dispersion/`：PERMDISP 诊断图
- `figures/diff_volcano.png`
- `figures/ml_importance.png`
- `figures/network_plot.png`
- `figures/key_taxa_score_barplot.png`

5. AI/LLM 相关文本与结构化记录（如该流程已启用并成功生成）
- `ai/diff_interpretation.md`
- `ai/methods.md`
- `ai/figure_legends.md`
- `ai/llm_diff_interpretation.md`
- `ai/llm_methods.md`
- `ai/llm_figure_legends.md`
- `json/llm_request_diff.json`
- `json/llm_response_diff.json`

6. 汇总 JSON（便于报告与复盘）
- `json/diff_summary.json`
- `json/ml_summary.json`
- `json/network_summary.json`
- `json/key_taxa_summary.json`

7. 最终报告
- `report/report.html`

提示：运行页面和结果总览会展示阶段状态与产物信息，演示前应同时在任务目录核对关键文件。

## 报告路径示例
报告固定写入 job 目录的：
- `results/<job_id>/report/report.html`

例如（示意）：
- `results/job_20260522_153012_ab12cd/report/report.html`

## 演示时重点展示哪些模块（建议顺序）
推荐一套“从结果到解释、从统计到可复现”的叙事顺序：

1. Alpha Diversity
- 展示 Alpha 页面的四指标概览、指标切换和总体检验表，说明丰富度与多样性指标的区别。

2. Beta Diversity
- 展示 Beta 页面的“95%置信椭圆 + 组中心”视图、PERMANOVA 和 PERMDISP 表，并切换到离散度诊断图说明统计边界。

3. Differential Abundance
- 展示 `Diff Abundance` 页签的 volcano 图与显著差异列表（强调可追溯到 `differential_taxa_significant.csv`）。

4. AI-Constrained Interpretation
- 在 `Report`（或报告内容）中展示 AI/LLM 生成的差异解释、方法与图例文本（强调“受约束、可复查、落盘到 ai/ 与 json/”）。

5. Machine Learning Biomarker Screening
- 展示 `tables/ml_feature_importance.csv` 与 `figures/ml_importance.png`（强调“可用于候选 biomarker 排序与模型指标”）。

6. Co-occurrence Network Analysis
- 展示 `tables/network_nodes.csv`/`tables/network_edges.csv` 与 `figures/network_plot.png`（强调“网络层面的共现关系”）。

7. Key Taxa Score
- 展示 `tables/key_taxa_score.csv`、`tables/key_taxa_top20.csv` 与 `figures/key_taxa_score_barplot.png`（强调“综合评分/排序用于关键菌汇总”）。

8. Reproducibility Record
- 打开 `reproducibility.json`（强调“输入文件 MD5 + 参数记录 + 可复现实验链条”，并指出每个 job 独立目录便于留档与对比）。
