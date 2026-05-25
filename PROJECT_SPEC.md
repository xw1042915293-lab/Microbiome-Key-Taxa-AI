# Microbiome Key Taxa AI 项目规范

## 1. 项目名称

**中文名称：**

基于 microeco 2.0 与大语言模型的微生物组关键菌筛选、可信解释与可复现报告生成系统

**英文名称：**

An AI-assisted microbiome key taxa discovery, trustworthy interpretation and reproducible report generation system based on microeco 2.0

**项目简称：**

Microbiome Key Taxa AI

---

## 2. 项目定位

本项目不是一个泛 AI 科研平台，也不是重新开发一个微生物组分析 R 包。

本项目定位为：

> 面向微生物组测序结果的专用再分析、候选关键菌筛选、可信 AI 解释与可复现报告生成系统。

底层分析依赖成熟 R 生态，尤其是 `microeco 2.0`。

`microeco 2.0` 已经在微生物组组学下游分析方面提供了较完整的能力，包括数据预处理、多样性分析、差异检验、机器学习、功能预测、网络分析、代谢物溯源与多组学整合等。因此，本项目不重复造分析方法库，而是在其上层构建：

```text
交互式流程
数据自动校验
候选关键菌综合评分
统计约束 AI 解释
可复现报告生成
论文级结果输出
```

---

## 3. 项目不做什么

为了避免方向发散，本项目明确不做以下内容：

```text
1. 不做 FASTQ 原始测序数据上游分析
2. 不做 QIIME2 / DADA2 / Kraken2 的完整替代
3. 不做 BioIntelOS 类泛 AI 科研平台
4. 不重新实现 microeco 已经实现的统计方法
5. 不做大型多组学全流程平台
6. 不让 AI 直接读取原始表格自由解释
7. 不把 AI 输出作为无依据的生物学结论
8. 不把相关性描述成因果关系
```

---

## 4. 项目核心创新点

系统必须始终围绕 4 个创新点开发。

### 4.1 微生物组专用数据校验

系统自动检查：

```text
SampleID 是否匹配
FeatureID 是否匹配
taxonomy 层级是否完整
metadata 分组变量是否合理
是否有重复样本
是否有缺失值
是否适合机器学习
是否满足差异分析基本样本量要求
```

### 4.2 差异分析 + 机器学习 + 网络分析联合筛选关键菌

系统不是只输出差异菌，而是输出候选关键菌：

```text
差异丰度结果
+
机器学习特征重要性
+
网络中心性
=
Key Taxa Score
```

这是本项目的核心方法模块。

### 4.3 统计约束的 AI 可信解释

AI 不直接解释原始数据。

流程必须是：

```text
统计结果表
↓
结构化 JSON
↓
规则判断显著性、方向、可靠性
↓
LLM 生成解释
```

AI 只能基于结构化统计结果生成：

```text
result summary
biological interpretation
methods
figure legend
caution notes
```

### 4.4 可复现报告生成

每次分析必须记录：

```text
输入文件
文件 MD5
样本数量
特征数量
分析时间
分组变量
过滤参数
统计方法
p 值校正方法
R 版本
microeco 版本
AI prompt 版本
Key Taxa Score 公式
```

最终输出：

```text
HTML report
PDF report
figures
tables
JSON summaries
reproducibility record
```

---

## 5. 技术栈

### 5.1 主技术栈

```text
R Shiny
microeco 2.0
Quarto
LLM API
SQLite
renv
```

### 5.2 R 包

```r
shiny
bslib
DT
tidyverse
data.table
microeco
vegan
ggplot2
ggpubr
pheatmap
ComplexHeatmap
randomForest
pROC
glmnet
igraph
ggraph
jsonlite
httr2
yaml
DBI
RSQLite
digest
quarto
renv
```

---

## 6. 总体架构

```text
Microbiome Key Taxa AI
│
├── UI Layer
│   └── R Shiny
│
├── Workflow Layer
│   ├── job creation
│   ├── parameter management
│   ├── analysis orchestration
│   └── progress tracking
│
├── Data Layer
│   ├── file import
│   ├── data validation
│   ├── microeco object construction
│   └── result persistence
│
├── Analysis Layer
│   ├── alpha diversity
│   ├── beta diversity
│   ├── taxonomic composition
│   ├── differential abundance
│   ├── machine learning
│   ├── network analysis
│   └── key taxa score
│
├── AI Layer
│   ├── result-to-JSON conversion
│   ├── rule-based reliability checks
│   ├── LLM prompt generation
│   └── AI text generation
│
├── Report Layer
│   ├── Quarto rendering
│   ├── HTML report
│   ├── PDF report
│   └── reproducibility record
│
└── Storage Layer
    ├── uploads/
    ├── results/
    └── SQLite database
```

---

## 7. 项目目录结构

```text
microbiome-keytaxa-ai/
│
├── PROJECT_SPEC.md
├── README.md
├── app.R
├── global.R
├── config.yml
├── renv.lock
├── .gitignore
│
├── R/
│   ├── 00_packages.R
│   ├── 01_config.R
│   ├── 02_utils_file.R
│   ├── 03_utils_plot.R
│   ├── 04_utils_json.R
│   ├── 05_database.R
│   │
│   ├── data_import.R
│   ├── data_check.R
│   ├── build_microeco.R
│   │
│   ├── analysis_alpha.R
│   ├── analysis_beta.R
│   ├── analysis_abundance.R
│   ├── analysis_diff.R
│   ├── analysis_ml.R
│   ├── analysis_network.R
│   ├── key_taxa_score.R
│   │
│   ├── ai_rules.R
│   ├── ai_prompt.R
│   ├── ai_client.R
│   ├── ai_interpretation.R
│   │
│   ├── report_prepare.R
│   ├── report_render.R
│   └── workflow_run.R
│
├── modules/
│   ├── mod_upload.R
│   ├── mod_data_check.R
│   ├── mod_parameters.R
│   ├── mod_run_analysis.R
│   ├── mod_results_overview.R
│   ├── mod_alpha.R
│   ├── mod_beta.R
│   ├── mod_diff.R
│   ├── mod_ml.R
│   ├── mod_network.R
│   ├── mod_key_taxa.R
│   ├── mod_ai_report.R
│   └── mod_download.R
│
├── templates/
│   ├── report_template.qmd
│   ├── methods_template.md
│   ├── figure_legend_template.md
│   └── prompts/
│       ├── alpha_prompt.txt
│       ├── beta_prompt.txt
│       ├── diff_prompt.txt
│       ├── ml_prompt.txt
│       ├── network_prompt.txt
│       └── key_taxa_prompt.txt
│
├── data/
│   ├── example_abundance.tsv
│   ├── example_metadata.tsv
│   └── example_taxonomy.tsv
│
├── uploads/
│   └── .gitkeep
│
├── results/
│   └── .gitkeep
│
├── database/
│   └── app.sqlite
│
├── www/
│   ├── style.css
│   └── logo.png
│
└── tests/
    ├── test_data_import.R
    ├── test_data_check.R
    ├── test_key_taxa_score.R
    └── test_ai_rules.R
```

---

## 8. 数据输入规范

### 8.1 abundance table

必须是 feature × sample 格式：

```text
FeatureID    Sample1    Sample2    Sample3
ASV1         120        80         45
ASV2         0          34         90
ASV3         13         22         10
```

要求：

```text
第一列必须是 FeatureID
其他列必须是样本 ID
数值必须为非负数
不能有重复 FeatureID
```

### 8.2 metadata

```text
SampleID    Group      Treatment
Sample1     Control    CK
Sample2     Treatment  Drug
Sample3     Treatment  Drug
```

要求：

```text
必须包含 SampleID
SampleID 不能重复
必须至少有一个分组变量
分组变量每组样本数建议 >= 3
```

### 8.3 taxonomy

```text
FeatureID    Kingdom    Phylum           Class              Order    Family    Genus
ASV1         Bacteria   Proteobacteria    Gammaproteobacteria ...
ASV2         Bacteria   Firmicutes        Bacilli             ...
```

要求：

```text
必须包含 FeatureID
建议包含 Kingdom, Phylum, Class, Order, Family, Genus
FeatureID 必须能与 abundance table 匹配
```

---

## 9. 统一任务目录规范

每次用户运行分析，系统必须创建一个 job。

目录格式：

```text
results/
└── job_YYYYMMDD_HHMMSS_xxxxxx/
    ├── input/
    │   ├── abundance.tsv
    │   ├── metadata.tsv
    │   └── taxonomy.tsv
    │
    ├── objects/
    │   ├── microeco_dataset.rds
    │   └── analysis_state.rds
    │
    ├── tables/
    │   ├── data_check_summary.csv
    │   ├── alpha_diversity.csv
    │   ├── alpha_stats.csv
    │   ├── beta_permanova.csv
    │   ├── differential_taxa.csv
    │   ├── ml_feature_importance.csv
    │   ├── network_nodes.csv
    │   ├── network_edges.csv
    │   └── key_taxa_score.csv
    │
    ├── figures/
    │   ├── alpha_shannon_boxplot.pdf
    │   ├── alpha_shannon_boxplot.png
    │   ├── beta_pcoa_bray.pdf
    │   ├── beta_pcoa_bray.png
    │   ├── diff_volcano.pdf
    │   ├── diff_heatmap.pdf
    │   ├── ml_roc.pdf
    │   ├── ml_importance.pdf
    │   └── network_plot.pdf
    │
    ├── json/
    │   ├── alpha_summary.json
    │   ├── beta_summary.json
    │   ├── diff_summary.json
    │   ├── ml_summary.json
    │   ├── network_summary.json
    │   ├── key_taxa_summary.json
    │   └── reproducibility.json
    │
    ├── ai/
    │   ├── alpha_interpretation.md
    │   ├── beta_interpretation.md
    │   ├── diff_interpretation.md
    │   ├── key_taxa_interpretation.md
    │   ├── methods.md
    │   └── figure_legends.md
    │
    ├── report/
    │   ├── report.html
    │   └── report.pdf
    │
    └── logs/
        ├── run.log
        └── error.log
```

---

## 10. 数据流

系统必须遵守这个流程：

```text
Step 1. 上传文件
↓
Step 2. 保存原始输入文件
↓
Step 3. 自动识别 csv / tsv
↓
Step 4. 数据校验
↓
Step 5. 用户选择分组变量、比较组、分类水平、分析参数
↓
Step 6. 构建 microeco::microtable 对象
↓
Step 7. 运行基础分析
    ├── Alpha diversity
    ├── Beta diversity
    ├── Taxonomic composition
    └── Differential abundance
↓
Step 8. 运行增强分析
    ├── Machine learning
    └── Network analysis
↓
Step 9. 计算 Key Taxa Score
↓
Step 10. 结构化结果 JSON
↓
Step 11. AI 可信解释
↓
Step 12. Quarto 生成报告
↓
Step 13. 用户下载结果
```

---

## 11. 分析模块职责

### 11.1 data_import.R

职责：

```text
读取 csv / tsv / txt
自动判断分隔符
标准化列名
返回 list(abundance, metadata, taxonomy)
```

必须暴露函数：

```r
read_microbiome_inputs(abundance_path, metadata_path, taxonomy_path)
detect_delimiter(file_path)
standardize_input_tables(input_list)
```

### 11.2 data_check.R

职责：

```text
检查输入数据合法性
生成 data_check_summary
返回 pass/warning/error
```

必须暴露函数：

```r
check_abundance_table(abundance)
check_metadata_table(metadata)
check_taxonomy_table(taxonomy)
check_sample_matching(abundance, metadata)
check_feature_matching(abundance, taxonomy)
check_group_variable(metadata, group_var)
run_all_data_checks(input_list, group_var = NULL)
```

输出格式：

```r
list(
  status = "pass" | "warning" | "error",
  checks = data.frame(
    check_name = character(),
    status = character(),
    message = character()
  ),
  summary = list(
    n_samples = integer(),
    n_features = integer(),
    groups = list()
  )
)
```

### 11.3 build_microeco.R

职责：

```text
把 abundance / metadata / taxonomy 转为 microeco 对象
保存 RDS
```

必须暴露函数：

```r
build_microeco_dataset(abundance, metadata, taxonomy)
save_microeco_dataset(dataset, job_dir)
```

### 11.4 analysis_alpha.R

职责：

```text
计算 Alpha diversity
绘制箱线图
统计组间差异
输出表格、图、JSON
```

必须暴露函数：

```r
run_alpha_analysis(dataset, group_var, job_dir)
plot_alpha_boxplot(alpha_table, group_var, index = "Shannon", output_path)
summarize_alpha_for_ai(alpha_table, alpha_stats, group_var)
```

### 11.5 analysis_beta.R

职责：

```text
计算 Bray-Curtis / Jaccard
PCoA
PERMANOVA
输出表格、图、JSON
```

必须暴露函数：

```r
run_beta_analysis(dataset, group_var, job_dir, distance = "bray")
plot_beta_pcoa(beta_result, group_var, output_path)
summarize_beta_for_ai(beta_result, permanova_table, group_var)
```

### 11.6 analysis_diff.R

职责：

```text
差异丰度分析
输出差异菌表
生成显著菌摘要
```

第一版方法：

```text
Wilcoxon
Kruskal-Wallis
FDR correction
```

第二版再加入：

```text
LEfSe
DESeq2
ANCOM-BC
```

必须暴露函数：

```r
run_diff_analysis(dataset, group_var, tax_level, job_dir)
plot_diff_taxa_bar(diff_table, output_path)
summarize_diff_for_ai(diff_table, group_var)
```

### 11.7 analysis_ml.R

职责：

```text
基于 genus-level abundance 做分类建模
输出模型表现和特征重要性
```

第一版只做：

```text
Random Forest
```

第二版加入：

```text
LASSO
SVM
```

必须暴露函数：

```r
run_ml_analysis(dataset, group_var, tax_level, job_dir)
check_ml_sample_size(metadata, group_var)
train_random_forest(feature_matrix, label)
plot_rf_importance(importance_table, output_path)
summarize_ml_for_ai(model_metrics, importance_table)
```

样本量规则：

```text
n < 20：仅探索性展示
20 <= n < 50：谨慎解释
n >= 50：可作为较稳定模型分析
```

### 11.8 analysis_network.R

职责：

```text
基于物种丰度构建共现网络
计算节点中心性
输出 node/edge 表
```

第一版：

```text
Spearman correlation network
```

必须暴露函数：

```r
run_network_analysis(dataset, tax_level, job_dir)
build_spearman_network(abund_matrix, rho_cutoff = 0.6, p_cutoff = 0.05)
calculate_network_centrality(graph)
plot_network(graph, output_path)
summarize_network_for_ai(node_table, edge_table)
```

### 11.9 key_taxa_score.R

这是项目核心模块。

职责：

```text
整合差异分析、机器学习、网络中心性
计算候选关键菌综合评分
```

必须暴露函数：

```r
calculate_key_taxa_score(diff_table, ml_importance, network_nodes)
normalize_score(x, higher_is_better = TRUE)
rank_key_taxa(score_table, top_n = 20)
summarize_key_taxa_for_ai(score_table)
```

初始评分公式：

```text
KeyTaxaScore =
0.4 × DifferentialScore
+ 0.4 × MLImportanceScore
+ 0.2 × NetworkCentralityScore
```

其中：

```text
DifferentialScore = normalized(-log10(FDR) × abs(log2FC))
MLImportanceScore = normalized(Random Forest importance)
NetworkCentralityScore = normalized(degree + betweenness)
```

如果某个模块没有结果，只用可用模块重新归一化权重。

例如没有网络分析：

```text
KeyTaxaScore =
0.5 × DifferentialScore
+ 0.5 × MLImportanceScore
```

---

## 12. AI 模块规范

### 12.1 AI 绝对不能做的事

```text
不能直接根据菌名编造机制
不能把相关性写成因果
不能把 p >= 0.05 写成显著
不能无依据地说某菌导致疾病
不能忽略样本量不足
不能生成没有统计依据的结论
```

### 12.2 ai_rules.R

职责：

```text
在调用 LLM 前先判断结果可靠性
```

必须暴露函数：

```r
classify_significance(p_value, fdr = NULL)
classify_direction(log2fc)
classify_ml_reliability(n_samples, n_groups)
classify_network_reliability(n_taxa, n_samples)
generate_caution_notes(summary_json)
```

规则：

```text
FDR < 0.05：显著
0.05 <= FDR < 0.1：趋势
FDR >= 0.1：不显著
样本量 < 20：机器学习仅供探索
PERMANOVA 显著时建议检查 beta dispersion
```

### 12.3 AI 输入 JSON 标准

示例：

```json
{
  "analysis_type": "key_taxa_score",
  "group_variable": "Treatment",
  "comparison": "Treatment_vs_Control",
  "sample_size": {
    "total": 30,
    "Control": 15,
    "Treatment": 15
  },
  "top_taxa": [
    {
      "taxon": "Lactobacillus",
      "level": "Genus",
      "fdr": 0.003,
      "log2fc": 1.52,
      "direction": "increased_in_treatment",
      "rf_importance_rank": 1,
      "network_degree_rank": 3,
      "key_taxa_score": 0.91,
      "evidence": [
        "differential_abundance",
        "machine_learning",
        "network_centrality"
      ]
    }
  ],
  "caution_notes": [
    "The result indicates association, not causation.",
    "Biological interpretation should be validated experimentally."
  ]
}
```

### 12.4 AI 输出格式

AI 必须返回结构化内容：

```json
{
  "result_summary": "...",
  "biological_interpretation": "...",
  "caution_notes": "...",
  "methods_text": "...",
  "figure_legend": "...",
  "conclusion": "..."
}
```

---

## 13. Shiny 页面结构

页面采用左侧导航或顶部导航。

```text
1. Home
2. Upload Data
3. Data Check
4. Parameters
5. Run Analysis
6. Results
   ├── Data Overview
   ├── Alpha Diversity
   ├── Beta Diversity
   ├── Differential Taxa
   ├── Machine Learning
   ├── Network Analysis
   └── Key Taxa Score
7. AI Interpretation
8. Report
9. Download
```

每个页面对应一个 Shiny module。

---

## 14. 状态管理

系统中必须有统一的 `analysis_state`。

```r
analysis_state <- reactiveValues(
  job_id = NULL,
  job_dir = NULL,
  input_paths = NULL,
  input_data = NULL,
  check_result = NULL,
  parameters = NULL,
  dataset = NULL,
  alpha_result = NULL,
  beta_result = NULL,
  diff_result = NULL,
  ml_result = NULL,
  network_result = NULL,
  key_taxa_result = NULL,
  ai_result = NULL,
  report_paths = NULL,
  status = "idle"
)
```

状态只能由 workflow 函数修改，不允许各模块随意改。

---

## 15. 数据库设计

SQLite 表：

### 15.1 jobs

```sql
CREATE TABLE jobs (
  job_id TEXT PRIMARY KEY,
  created_at TEXT,
  updated_at TEXT,
  status TEXT,
  group_var TEXT,
  tax_level TEXT,
  job_dir TEXT,
  n_samples INTEGER,
  n_features INTEGER,
  has_ai INTEGER,
  has_report INTEGER
);
```

### 15.2 job_files

```sql
CREATE TABLE job_files (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  job_id TEXT,
  file_type TEXT,
  original_name TEXT,
  stored_path TEXT,
  md5 TEXT
);
```

### 15.3 job_results

```sql
CREATE TABLE job_results (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  job_id TEXT,
  result_type TEXT,
  file_path TEXT,
  created_at TEXT
);
```

---

## 16. 开发阶段

### Phase 0：项目骨架

目标：

```text
项目能启动
页面能切换
示例数据能加载
```

完成：

```text
app.R
global.R
目录结构
renv 初始化
config.yml
Shiny 基础页面
```

### Phase 1：上传与数据校验

目标：

```text
上传三个表格并完成检查
```

完成：

```text
自动识别 csv/tsv
SampleID 检查
FeatureID 检查
缺失值检查
重复值检查
分组变量检查
数据检查摘要表
```

验收标准：

```text
上传示例数据后，系统显示 pass/warning/error。
```

### Phase 2：microeco 对象与基础分析

目标：

```text
跑通 microeco 分析闭环
```

完成：

```text
microtable 对象构建
Alpha diversity
Beta diversity
Shannon boxplot
PCoA plot
PERMANOVA
```

验收标准：

```text
results/job_xxx/figures/ 下有 Alpha 和 Beta 图。
results/job_xxx/tables/ 下有统计表。
```

### Phase 3：差异分析与基础报告

目标：

```text
生成第一个完整 HTML 报告
```

完成：

```text
差异丰度分析
差异菌表
基础 Quarto 报告
HTML 下载
```

验收标准：

```text
用户可以下载 report.html。
```

### Phase 4：AI 可信解释

目标：

```text
AI 能基于 JSON 生成解释
```

完成：

```text
统计结果转 JSON
AI 规则判断
Prompt 模板
LLM API 调用
AI 解释结果写入 ai/*.md
```

验收标准：

```text
AI 不把不显著结果写成显著。
AI 输出包含 caution notes。
```

### Phase 5：机器学习

目标：

```text
完成 Random Forest 标志物筛选
```

完成：

```text
样本量检查
Random Forest
Accuracy
AUC
Feature importance
重要菌图
```

验收标准：

```text
输出 ml_feature_importance.csv 和 ml_importance.pdf。
```

### Phase 6：网络分析

目标：

```text
完成 Spearman 共现网络和中心性
```

完成：

```text
相关性矩阵
节点表
边表
degree
betweenness
网络图
```

验收标准：

```text
输出 network_nodes.csv、network_edges.csv、network_plot.pdf。
```

### Phase 7：Key Taxa Score

目标：

```text
输出候选关键菌排名
```

完成：

```text
整合 diff / ML / network
计算 KeyTaxaScore
输出 Top 20
AI 解释关键菌
```

验收标准：

```text
报告中出现 Key Taxa Score 表和解释。
```

### Phase 8：可复现报告

目标：

```text
输出完整 HTML/PDF 报告
```

完成：

```text
完整 Quarto 模板
Methods
Results
Figure legends
Reproducibility record
Supplementary tables
```

验收标准：

```text
report.html 和 report.pdf 均可生成。
```

---

## 17. Codex 必须遵守的开发规则

每次写代码必须遵守：

```text
1. 不把所有逻辑写进 app.R
2. 每个 R 文件只负责一个清晰模块
3. 所有函数必须有输入检查和错误提示
4. 所有输出必须保存到 job_dir
5. 所有图必须同时保存 PDF 和 PNG
6. 所有统计结果必须保存 CSV
7. 所有 AI 输入必须保存 JSON
8. 所有 AI 输出必须保存 Markdown
9. 所有参数必须记录到 reproducibility.json
10. 不允许 AI 直接读取原始 abundance table 生成结论
11. 不允许跳过数据校验
12. 不允许把相关性写成因果
13. 不允许没有样本量提醒就运行机器学习解释
14. 不引入 Spring Boot、Vue、Qt 或 UE
15. 不把项目扩展成泛 AI 科研平台
```
调试规则：
1. 同一 smoke test 最多运行 2 次。
2. 连续失败后必须停止。
3. 不允许在不知道原因的情况下反复修改和运行。
4. 不允许把 Shiny reactiveValues 传入 R/ 目录中的业务函数。
5. 所有业务函数必须能在普通 Rscript 环境下独立运行。


## 25. 项目一句话总结

> 本项目不是做一个大而全的 AI 科研平台，而是基于 microeco 2.0 构建微生物组专用的关键菌筛选、可信解释和可复现报告生成系统，重点解决微生物组测序结果从分析结果到论文材料之间的“最后一公里”问题。
