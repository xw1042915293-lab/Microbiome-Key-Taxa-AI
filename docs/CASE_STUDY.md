# CASE STUDY（案例研究：示例数据全流程说明）

面向对象：硕士毕业论文支撑材料 / 系统验收材料  
范围声明：本案例基于仓库自带示例数据与既有系统功能进行说明，不引入新方法、不修改任何 R 代码。  
重要提示：本案例全部结果仅用于演示与探索性说明，不可直接作为真实科研结论或因果/机制证据。

## 1. 示例数据简介

### 1.1 数据来源与文件

示例输入数据位于项目根目录 `data/`：

- `data/example_abundance.tsv`：丰度表（特征 x 样本）
- `data/example_metadata.tsv`：样本信息表
- `data/example_taxonomy.tsv`：分类注释表

### 1.2 数据规模（便于论文描述）

该示例数据规模较小，适合现场演示与验收核对：

- 样本数：6（Sample1～Sample6）
- 特征数：4（丰度表除表头外 4 行）
- 分组变量：`Group`，两组（Control=3，Treatment=3）
- taxonomy 提供层级：Kingdom / Phylum / Class / Order / Family / Genus

### 1.3 数据用途边界

本示例数据用于验证系统“闭环运行 + 产物落地 + 报告生成”，不用于评估方法学优越性，也不用于形成可推广的生物学结论。

## 2. 分析流程

系统推荐的最小闭环流程如下（详尽操作步骤可参考 `docs/USER_GUIDE.md` 与 `docs/DEMO_CASE.md`）：

1. 启动 Shiny 应用（RStudio 中运行 `shiny::runApp()`）。
2. 在 `Upload Data` 页面上传三张表，并点击创建 job（系统会在 `results/` 下生成独立 job 目录并固化输入）。
3. 在 `Data Check` 页面执行校验，确认整体状态为可继续（pass 或可解释的 warning）。
4. 在 `Parameters` 页面选择分组变量 `group_var = Group` 并保存（写入可复现记录）。
5. 在 `Run Analysis` 页面执行 `Run Full Workflow (Phase 2-8)`（生成表格、图形、JSON、AI 文本与报告）。
6. 在 `Report` 页面渲染并打开 `report.html`（作为验收交付与论文附件的主要汇总材料）。

本仓库的既有验收记录中，存在一份已通过的示例 job，可用于核对目录结构与产物齐全性：

- `results/job_20260522_125641_fflx2v/`

注意：用户每次运行都会生成新的 job 目录，上述路径仅作为“通过样例”的参考。

## 3. Alpha diversity 结果说明

### 3.1 主要产物

- Alpha 指标表：`tables/alpha_diversity.csv`
- 统计检验结果：`tables/alpha_stats.csv`
- 关键图形（Shannon）：`figures/alpha_shannon_boxplot.png`（及可选 `.pdf`）

### 3.2 解读口径（论文可用表述）

- Shannon 指数用于描述样本内多样性水平；数值越大通常表示多样性越高，但其生物学意义需结合实验背景讨论。
- 组间差异的统计检验结果以 `alpha_stats.csv` 为准；在论文中建议同时报告检验方法与校正后的显著性指标（如 FDR）。
- 本示例样本量很小（每组 3 个样本），即使出现差异，也应仅作为探索性观察，不应夸大结论强度。

## 4. Beta diversity 结果说明

### 4.1 主要产物

- PCoA 坐标：`tables/beta_pcoa_coordinates.csv`
- PERMANOVA 结果：`tables/beta_permanova.csv`
- 关键图形（Bray-Curtis PCoA）：`figures/beta_pcoa_bray.png`（及可选 `.pdf`）

### 4.2 解读口径（论文可用表述）

- PCoA 图用于展示样本间距离结构在低维空间的近似；点间距离反映 Bray-Curtis 距离意义下的相似性差异。
- PERMANOVA 检验用于评估“距离矩阵层面”的组间差异是否显著；其结果不等同于机制解释。
- 当样本量较小或组内离散度差异较大时，PCoA 分离与检验结论都需谨慎解释，可在讨论部分明确其探索性性质。

## 5. 差异菌结果说明

### 5.1 主要产物

- 全量差异结果：`tables/differential_taxa.csv`
- 显著差异结果：`tables/differential_taxa_significant.csv`
- 汇总 JSON（供解释/报告引用）：`json/diff_summary.json`
- 可视化：`figures/diff_volcano.png`、`figures/diff_taxa_barplot.png`

### 5.2 解读口径（论文可用表述）

- 以校正后的显著性（FDR）作为主要判定依据，避免仅用 raw p-value 进行结论性表述。
- 若同时给出效应方向（如 log2FC），论文中应明确其定义（基于组均值比值的对数）及适用前提，并避免将其解读为因果影响。
- 当显著差异数量很少或为 0 时，系统仍会输出占位结果表，便于验收与报告结构稳定；论文中可将其表述为“未观察到统计显著差异（探索性）”。

## 6. AI 解释结果说明

### 6.1 主要产物

- 受约束解释文本（本地规则生成）：`ai/diff_interpretation.md`、`ai/methods.md`、`ai/figure_legends.md`
- 可选 LLM 留痕与输出（若启用）：`json/llm_request_diff.json`、`json/llm_response_diff.json`、`ai/llm_*.md`

### 6.2 可信与边界说明（验收/论文必须强调）

- AI 文本的定位是“对已生成统计产物的摘要式说明”，其信息来源应可回溯到 `json/diff_summary.json` 与相关结果表。
- AI 文本不得输出因果/机制结论，也不得把“不显著”表述成“显著”；论文中引用时建议保留免责声明或在段首明确“统计关联、探索性解释”。
- 若启用 LLM，建议在论文附录保留请求/响应留痕文件，便于审查解释文本的生成边界与可追溯性。

## 7. 机器学习结果说明

### 7.1 主要产物

- 特征重要性：`tables/ml_feature_importance.csv`
- 模型指标：`tables/ml_model_metrics.csv`
- 汇总 JSON：`json/ml_summary.json`
- 图形：`figures/ml_importance.png`、`figures/ml_confusion_matrix.png`（二分类时可能包含 `figures/ml_roc.png`）

### 7.2 解读口径（论文可用表述）

- Random Forest 在本系统中用于“探索性标志菌候选排序”，并非临床/诊断模型，也不用于因果推断。
- 样本量很小时（本示例每组 3 个），模型指标更适合作为演示与流程验证，论文中应避免将其表述为可泛化性能。
- 论文可将 ML 结果作为“与差异分析/网络证据的补充视角”，强调其辅助性与探索性。

## 8. 共现网络结果说明

### 8.1 主要产物

- 节点表：`tables/network_nodes.csv`
- 边表：`tables/network_edges.csv`
- 汇总 JSON：`json/network_summary.json`
- 网络图：`figures/network_plot.png`

### 8.2 解读口径（论文可用表述）

- 网络基于相关性（Spearman）构建，用于刻画“共现关系的统计关联结构”，不代表直接相互作用或因果链路。
- 节点中心性指标（如 degree、betweenness）可用于识别在网络结构中更“核心”的节点，但其生物学意义需结合实验背景谨慎讨论。
- 在样本量小或特征数少时，网络结构对阈值（相关系数阈值、FDR 阈值）较敏感；论文中建议明确阈值与探索性性质。

## 9. Key Taxa Score 结果说明

### 9.1 主要产物

- 打分表：`tables/key_taxa_score.csv`
- Top 列表：`tables/key_taxa_top20.csv`
- 汇总 JSON：`json/key_taxa_summary.json`
- 图形：`figures/key_taxa_score_barplot.png`

### 9.2 解读口径（论文可用表述）

- Key Taxa Score 是将多类证据（差异、ML、网络）进行归一化融合后的工程化排序指标，用于候选优先级展示。
- 该分数不等同于“生物学关键性证明”；在论文中建议将其表述为“多证据线索下的候选排序”，并在讨论中强调其验证需求。
- 若某些证据源缺失（例如网络无边、ML 不稳定），融合策略应以汇总 JSON 中记录的“可用证据源与权重/提示”为准进行说明。

## 10. 最终报告说明

### 10.1 报告位置与性质

- 报告文件：`report/report.html`
- 报告定位：对 job 目录中已生成的产物进行汇总展示（图表 + 表格 + 方法与图例文本 + 可复现记录），便于答辩展示与验收交付。

### 10.2 论文材料组织建议（不夸大结论）

- 正文：可选取 Alpha/Beta/差异等核心图作为主结果展示，并以“统计关联”口径撰写。
- 附录/补充材料：可将差异表、ML 重要性、网络 nodes/edges、Key Taxa Score 表作为补充表格或链接到报告中的对应段落。
- 可复现性：建议在论文中说明系统按 job 固化输入与参数，并给出 `reproducibility.json` 的作用（输入追溯/参数记录/阈值记录）。

## 11. 案例结论

在示例数据上，系统能够完成从输入固化、数据校验、参数记录到多模块分析与 HTML 报告汇总的完整闭环，并产出可被核对的表格、图形与结构化汇总文件，为毕业论文的“系统实现与验收”部分提供支撑材料。所有分析结果仅用于演示与探索性说明，论文表述应避免将其上升为因果或机制结论。

## 12. 注意：所有结果均为示例和探索性结果

- 示例数据仅用于验证系统流程与产物结构，不代表真实研究结论。
- 机器学习、网络与 Key Taxa Score 等模块均为探索性分析工具，不能替代独立验证与严格实验设计。
- AI 解释为“受约束摘要”，不得用于夸大统计结果或输出因果/机制推断。

