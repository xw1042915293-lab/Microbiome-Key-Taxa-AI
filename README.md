# Microbiome Key Taxa AI

## 项目简介
Microbiome Key Taxa AI 是一个用于“关键菌/标志物筛选 + 可复现实验记录 + 报告生成”的 R Shiny 应用。用户上传 abundance / metadata / taxonomy 三张表后，系统会为每次运行创建独立的 `results/job_*/` 目录，并把分析中间结果、图表、JSON、AI 辅助文本与最终 `report.html` 全部固化到该 job 目录中，便于复现与答辩展示。

## 核心功能
- Upload Inputs: 上传并固化三类输入文件到 job 目录（同时记录 MD5 与可复现信息）。
- Data Check: 必做数据校验，并将汇总保存为 `tables/data_check_summary.csv`。
- Parameters: 从 metadata 中选择 `group_var`（分组变量），写入 `reproducibility.json`。
- Run Full Workflow (Phase 2-8): 一键跑完整流程并写出核心产物（Alpha/Beta/Diff/ML/Network/Key Taxa/AI/Report）。
- Alpha / Beta / Diff: 在 Shiny 内直接预览主要图表与显著差异结果。
- Report: 基于 Quarto 模板渲染最终 HTML 报告，并在浏览器中打开。

## 技术栈
- R 4.6.0
- Shiny + bslib + DT
- microeco + vegan + tidyverse 相关工具包（见 `renv.lock`）
- Quarto（报告渲染：`quarto::quarto_render()`）
- SQLite（作业与文件记录：DBI + RSQLite）
- renv（依赖锁定与恢复：`.Rprofile` 自动 `source("renv/activate.R")`）
- LLM/AI 配置：见 `config.yml`（如需启用对应能力，按配置项设置环境变量）

## 目录结构
```text
.
├─ app.R                    # Shiny 应用入口（页面导航与模块组装）
├─ global.R                 # 全局加载：packages/config/R/ 与 modules/
├─ config.yml               # 路径与 AI/LLM 参数配置
├─ data/                    # 示例输入数据（用于演示/自检）
├─ modules/                 # Shiny 模块：Upload/Data Check/Parameters/Run/Alpha/Beta/Diff/Report
├─ R/                       # 业务逻辑与分析流程（本次交付整理不改动）
├─ templates/               # Quarto 报告模板（report_template.qmd）
├─ results/                 # 每次运行自动生成 job 目录（重要输出都在这里）
├─ database/                # SQLite 数据库默认位置（database/app.sqlite）
├─ uploads/                 # 上传缓存目录（如配置需要）
└─ www/                     # 前端静态资源（CSS 等）
```

## 安装依赖
1. 安装 R（推荐与本项目一致的 R 4.6.0）。
2. 安装 RStudio（Desktop 版即可）。
3. 安装 Quarto（用于渲染 `report.html`）。
4. 在项目根目录启动 R/RStudio，执行：

```r
# 推荐：使用 renv 按锁定版本恢复依赖
renv::restore()
```

如果不使用 renv，也可以手动安装缺失包（见下方 FAQ）。

## 启动方式
在项目根目录执行：

```r
shiny::runApp()
```

## 输入文件格式
支持 `.tsv/.csv/.txt`（建议使用 tsv，首行为表头）。示例文件位于：
- `data/example_abundance.tsv`
- `data/example_metadata.tsv`
- `data/example_taxonomy.tsv`

### 1) Abundance（丰度表）
- 第一列必须是特征 ID（示例为 `FeatureID`）。
- 其余列为样本列名（如 `Sample1...`），必须与 metadata 的 `SampleID` 一致。
- 单元格为非负数值（计数或相对丰度均可，建议不要混用）。

### 2) Metadata（样本信息表）
- 必须包含 `SampleID` 列。
- 其余列为可选分组变量（例如 `Group`、`Treatment`），用于后续差异/机器学习等。

### 3) Taxonomy（分类注释表）
- 第一列必须是 `FeatureID`，与 abundance 第一列一一对应。
- 其余列为分类层级（示例：`Kingdom/Phylum/Class/Order/Family/Genus`）。

## 分析流程（Shiny 页面顺序）
1. Upload Data: 依次上传 abundance/metadata/taxonomy，点击 `Create Job & Save Inputs` 创建 job 并固化输入。
2. Data Check: 点击 `Run Data Check` 做必需校验（汇总会写到 `tables/data_check_summary.csv`）。
3. Parameters: 选择 `Group variable`（group_var），点击 `Save Parameters`。
4. Run Analysis: 点击 `Run Full Workflow (Phase 2-8)` 一键运行并生成全套结果文件。
5. Alpha / Beta / Diff: 在对应 tab 预览关键图与差异结果。
6. Report: 点击 `Render HTML Report` 生成 `report/report.html` 并通过 `Open Report` 打开。

## 输出结果说明（每次运行一个 job 目录）
每个 job 会生成一个目录：`results/job_YYYYMMDD_HHMMSS_xxxxxx/`，核心子目录：
- `input/`: 固化后的输入（统一重命名为 `abundance.tsv/metadata.tsv/taxonomy.tsv`）
- `tables/`: 表格产物（alpha/beta/diff/ml/network/key_taxa 等）
- `figures/`: 关键图（alpha/beta/diff/ml/network/key_taxa 等）
- `json/`: 结构化汇总与（如启用）LLM 请求/响应记录
- `ai/`: AI/LLM 辅助生成的解释、方法与图例文本（Markdown）
- `report/`: `report.html`（最终交付展示）
- `logs/`: 错误日志（如运行失败会写入 `logs/error.log`）
- `reproducibility.json`: 复现记录（输入文件 MD5、参数等）

常见关键文件（完整跑通后应存在）：
- `report/report.html`
- `tables/alpha_diversity.csv`, `tables/alpha_stats.csv`
- `tables/beta_pcoa_coordinates.csv`, `tables/beta_permanova.csv`
- `tables/differential_taxa.csv`, `tables/differential_taxa_significant.csv`
- `tables/ml_feature_importance.csv`, `tables/ml_model_metrics.csv`
- `tables/network_nodes.csv`, `tables/network_edges.csv`
- `tables/key_taxa_score.csv`, `tables/key_taxa_top20.csv`
- `figures/alpha_shannon_boxplot.png`, `figures/beta_pcoa_bray.png`, `figures/diff_volcano.png`
- `figures/ml_importance.png`, `figures/network_plot.png`, `figures/key_taxa_score_barplot.png`
- `json/*_summary.json`, `ai/*.md`, `reproducibility.json`

## 常见问题（FAQ）
### 1) Missing required R packages
启动时报错类似 `Missing required R packages: ...`：
- 优先执行 `renv::restore()`；或对缺失包执行 `install.packages("包名")`。

### 2) randomForest 包缺失
机器学习相关步骤可能需要 `randomForest`：
```r
install.packages("randomForest")
```
安装后重启 R 会话再运行。

### 3) Quarto 不可用 / report.html 无法生成
- 需要安装 Quarto（桌面程序），并确保系统可调用。
- 同时需要 R 包 `quarto`（本项目依赖中包含，但仍可能因环境问题缺失）。
- 若报错，查看 job 目录下的 `logs/error.log`（以及 R 控制台输出）。

### 4) 没有选择 group variable
Run Analysis 报错 `group_var not set`：
- 先到 `Parameters` 选择分组变量（来自 metadata，除 `SampleID` 外的列），点击 `Save Parameters`。

### 5) report.html 没生成
常见原因：
- 未先完成 `Run Full Workflow`（报告依赖 job 目录已有结果文件）。
- Quarto 环境不可用（见上条）。
- 渲染失败可在 Shiny 通知中看到错误信息，并检查 `logs/error.log`。

## 当前限制
- 当前一键流程中部分参数为固定默认值（例如 `beta_distance = "bray"`、`tax_level = "Genus"`）。
- 输入文件需要严格满足列名约定：metadata 必须有 `SampleID`；abundance/taxonomy 必须有 `FeatureID`，且样本/特征需要能正确对齐。
- `report.html` 依赖 Quarto 环境；不同机器的 Quarto 安装情况会直接影响报告渲染。
