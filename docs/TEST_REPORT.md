# TEST REPORT（测试报告）

面向对象：硕士毕业论文支撑材料 / 系统验收材料  
范围声明：本报告仅基于仓库既有验收记录与既有 job 产物进行“核对式测试报告整理”，不运行任何 smoke test，不修改任何 R 代码。

## 1. 测试目的

- 验证系统在“上传 → 校验 → 参数选择 → 一键流程 → 产物落地 → HTML 报告生成”的闭环中，关键功能可用且产物可被验收复核。
- 验证输出文件结构与命名稳定，便于论文归档、答辩展示与复现追溯。
- 验证异常输入/缺失前置步骤时，系统能阻断或给出可定位提示，避免误导性输出。

## 2. 测试环境

### 2.1 软件与依赖（静态记录）

- 操作系统：Windows（本机路径形态为 `D:/...`）
- R 版本：4.6.0（来源：`renv.lock`）
- 依赖管理：`renv`（锁定文件：`renv.lock`）
- Web 框架：Shiny（用于交互界面）
- 统计分析依赖：microeco、vegan、randomForest 等（具体版本以 `renv.lock` 为准）
- 报告渲染：Quarto（系统要求外部 Quarto 可用；R 侧通过 `quarto::quarto_render()` 调用）
- AI/LLM：以配置文件 `config.yml` 为准；验收与报告渲染不要求调用外部 LLM（详见既有 Phase 8 验收记录中的“不调用 LLM API”说明）

### 2.2 路径与目录约定

- 项目根目录：`D:/Microbiome Key Taxa AI`
- 输出目录：`results/`
- 本报告引用的“已通过 job”：
  - `D:/Microbiome Key Taxa AI/results/job_20260522_125641_fflx2v`

## 3. 测试数据

### 3.1 示例数据来源

- 使用仓库内置示例数据：`data/example_abundance.tsv`、`data/example_metadata.tsv`、`data/example_taxonomy.tsv`
- 推荐分组变量：`Group`（来源：`docs/DEMO_CASE.md` 与 `README.md` 的运行建议）

### 3.2 测试数据特点（验收视角）

- 文件体积小、列结构标准、能够覆盖全流程的主要分支（含差异、ML、网络、Key Taxa 与报告渲染）。
- 结果仅用于演示与探索性说明，不用于形成真实科研结论。

## 4. 功能测试表

说明：

- 本次不执行脚本测试；“实际结果/证据”来自既有阶段验收记录与通过 job 的产物核对。
- 阶段验收记录详见：`docs/phase3_acceptance.md`、`docs/phase4b_acceptance.md`、`docs/phase5_acceptance.md`、`docs/phase6_acceptance.md`、`docs/phase7_acceptance.md`、`docs/phase8_acceptance.md` 与 `docs/final_acceptance_summary.md`。

| 编号 | 功能点 | 前置条件 | 操作步骤（验收口径） | 预期结果 | 实际结果/证据 | 结论 |
|---|---|---|---|---|---|---|
| FT-01 | 数据上传 | Shiny 可启动 | 上传 abundance/metadata/taxonomy 并创建 job | 创建 job 目录并固化输入 | 已存在通过 job：`.../input/abundance.tsv`、`metadata.tsv`、`taxonomy.tsv` | 通过 |
| FT-02 | 数据校验 | 已创建 job | 执行 Data Check | 生成校验汇总表，并在界面提示状态 | 通过 job 生成 `tables/data_check_summary.csv` | 通过 |
| FT-03 | 参数选择 | metadata 含分组列 | 选择 `group_var` 并保存 | 参数记录写入可复现记录 | 通过 job 存在 `reproducibility.json` | 通过 |
| FT-04 | Alpha diversity | 已完成 FT-02/03 | 运行流程或进入 Alpha 模块预览 | 输出 alpha 表格与图形 | 通过 job 存在 `tables/alpha_diversity.csv`、`tables/alpha_stats.csv`、`figures/alpha_shannon_boxplot.png` | 通过 |
| FT-05 | Beta diversity | 已完成 FT-02/03 | 运行流程或进入 Beta 模块预览 | 输出 beta 表格与图形 | 通过 job 存在 `tables/beta_pcoa_coordinates.csv`、`tables/beta_permanova.csv`、`figures/beta_pcoa_bray.png` | 通过 |
| FT-06 | 差异丰度分析 | 已完成 FT-02/03 | 运行差异分析 | 输出完整表、显著表、汇总 JSON 与图形 | 通过 job 存在 `tables/differential_taxa*.csv`、`json/diff_summary.json`、`figures/diff_volcano.png` | 通过 |
| FT-07 | AI 可信解释（受约束） | 已生成统计产物 | 生成 AI 解释文本（本地规则/或可选 LLM） | 输出受约束解释文本与留痕（可选） | 通过 job 存在 `ai/diff_interpretation.md`、`ai/methods.md`、`ai/figure_legends.md`，并存在 `json/llm_request_diff.json`/`json/llm_response_diff.json` | 通过 |
| FT-08 | 机器学习标志菌筛选 | 已完成 FT-02/03 | 运行 Random Forest 模块 | 输出重要性表、指标表与图形 | 通过 job 存在 `tables/ml_feature_importance.csv`、`tables/ml_model_metrics.csv`、`figures/ml_importance.png`、`figures/ml_confusion_matrix.png` | 通过 |
| FT-09 | 共现网络分析 | 已完成 FT-02/03 | 运行 Spearman 网络模块 | 输出 nodes/edges 表、汇总 JSON 与网络图 | 通过 job 存在 `tables/network_nodes.csv`、`tables/network_edges.csv`、`json/network_summary.json`、`figures/network_plot.png` | 通过 |
| FT-10 | Key Taxa Score | 已完成 FT-06/08/09（至少其一） | 计算并输出 Key Taxa Score | 输出打分表、Top 表与图形 | 通过 job 存在 `tables/key_taxa_score.csv`、`tables/key_taxa_top20.csv`、`figures/key_taxa_score_barplot.png` | 通过 |
| FT-11 | HTML 报告生成 | 已存在各阶段产物 | 渲染 Quarto 报告 | 生成 `report/report.html` 且可打开 | 通过 job 存在 `report/report.html`（文件大小约 2.3MB） | 通过 |
| FT-12 | 结果文件管理 | 任意一次运行 | 检查 job 目录结构与命名稳定性 | 输入/表格/图形/JSON/AI/报告/日志集中在 job | 通过 job 目录包含 `input/ tables/ figures/ json/ ai/ report/ logs/ reproducibility.json` | 通过 |

## 5. 输出文件完整性检查

检查对象：`D:/Microbiome Key Taxa AI/results/job_20260522_125641_fflx2v`

### 5.1 目录结构完整性

- `input/`：存在（含 3 个固化输入文件）
- `tables/`：存在（含 alpha/beta/diff/ml/network/key_taxa 等表格）
- `figures/`：存在（含 alpha/beta/diff/ml/network/key_taxa 等图形）
- `json/`：存在（含 diff/ml/network/key_taxa 汇总与 LLM 留痕）
- `ai/`：存在（含受约束解释文本与可选 LLM 解释文本）
- `report/`：存在（含 `report.html`）
- `logs/`：存在（用于错误定位与运行记录）
- `reproducibility.json`：存在（用于复现追溯）

### 5.2 关键产物抽样核对（代表性文件）

- Alpha：`tables/alpha_diversity.csv`、`figures/alpha_shannon_boxplot.png`
- Beta：`tables/beta_permanova.csv`、`figures/beta_pcoa_bray.png`
- Diff：`tables/differential_taxa.csv`、`json/diff_summary.json`、`figures/diff_volcano.png`
- ML：`tables/ml_feature_importance.csv`、`figures/ml_importance.png`
- Network：`tables/network_edges.csv`、`figures/network_plot.png`
- Key Taxa：`tables/key_taxa_score.csv`、`figures/key_taxa_score_barplot.png`
- Report：`report/report.html`

## 6. 异常情况测试

说明：以下为“验收场景下应覆盖的异常用例”，用于论文与验收材料的完整性说明；本次不执行脚本测试，仅给出预期系统行为（与用户手册/设计约束一致）。

| 编号 | 异常场景 | 触发方式 | 预期系统行为（验收口径） |
|---|---|---|---|
| ET-01 | 缺失输入文件 | 仅上传其中 1-2 个表 | 阻止创建可运行 job 或提示缺失项，避免进入后续分析 |
| ET-02 | metadata 缺失 `SampleID` | 上传不含 `SampleID` 的 metadata | Data Check 报错或 Parameters 无法选择分组；给出明确提示 |
| ET-03 | abundance 样本列与 metadata 不对齐 | SampleID 不一致/缺失 | Data Check 报错或 warning 并阻断关键分析步骤 |
| ET-04 | taxonomy 与 abundance 特征不对齐 | FeatureID 不一致/缺失 | Data Check 报错或 warning，并阻断聚合/后续分析 |
| ET-05 | 未保存 `group_var` 直接运行 | 跳过 Parameters | Run Analysis 阶段提示 `group_var not set`，不产生误导性结果 |
| ET-06 | Quarto 不可用导致报告失败 | 未安装/不可调用 Quarto | 报告渲染失败时给出错误提示，并可通过 job 下日志定位问题 |
| ET-07 | 无显著差异/网络无边 | 数据导致统计不显著或网络稀疏 | 仍输出完整结果表与占位文件（或明确提示“无显著/无边”），流程不中断 |
| ET-08 | LLM 不可用 | 未设置 API key 或网络不可达 | 不影响验收主链路；受约束的本地解释与报告生成仍可完成（或明确跳过 LLM） |

## 7. 测试结论

在不运行任何 smoke test、仅依据既有阶段验收记录与通过 job 产物核对的前提下：

- 系统的主流程闭环完整，关键模块（上传、校验、参数、Alpha/Beta、差异、AI 解释、ML、网络、Key Taxa、HTML 报告）均有可核对的落地产物作为证据。
- job 目录结构稳定，输出文件集中管理，能够支撑“验收核对 + 论文材料整理 + 复现追溯”的交付目标。
- AI 解释与探索性分析模块在文档口径上应持续强调边界：不进行因果/机制推断，机器学习与网络结果为探索性证据，不夸大结论强度。

