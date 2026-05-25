# 方法描述（METHODS DESCRIPTION）

本文档用于硕士论文/开题报告中的“方法实现”章节撰写，强调本系统是 microeco 2.0 上层应用；机器学习与网络为探索性分析；AI 解释不做因果结论。

## 1. Alpha diversity 方法
数据来源：
- 使用 microeco 2.0 数据对象（microtable）中的样本表与丰度表。

计算：
- 调用 microeco 的 Alpha 多样性计算接口生成多样性指标表（系统以 Shannon 指数作为必须输出与展示的核心指标）。

组间检验（Shannon）：
- 若分组变量 `group_var` 仅包含 2 组：使用 Wilcoxon 秩和检验（`wilcox.test(Shannon ~ group)`）。
- 若包含 3 组及以上：使用 Kruskal-Wallis 检验（`kruskal.test(Shannon ~ group)`）。
- 对 p 值做 FDR 校正：`p.adjust(p_value, method = "fdr")`。

输出：
- `tables/alpha_diversity.csv`：包含 `SampleID`、Alpha 指标列与分组列。
- `tables/alpha_stats.csv`：Shannon 的检验方法、p 值、FDR、样本数与组数。
- `figures/alpha_shannon_boxplot.png/.pdf`：箱线图 + 抖动散点。

解释边界：
- 仅报告统计关联，不推断因果机制。

## 2. Beta diversity 方法
距离度量：
- Bray-Curtis 距离（系统当前仅支持 `distance="bray"`）。
- 计算使用 `vegan::vegdist`，输入矩阵为“样本 x 特征”（将丰度表转置后计算）。

降维展示：
- 使用经典多维尺度/PCoA：`cmdscale(dist_obj, k = 2, eig = TRUE)`，输出 2D 坐标。
- 计算前两轴解释度：`eig / sum(eig[eig > 0])`。

组间差异检验：
- 使用 PERMANOVA：`vegan::adonis2(dist_obj ~ group, permutations = 999)`。

输出：
- `tables/beta_pcoa_coordinates.csv`：PCo1/PCo2 坐标、SampleID 与分组列。
- `tables/beta_permanova.csv`：adonis2 的统计表。
- `figures/beta_pcoa_bray.png/.pdf`：PCoA 散点图（按组着色）。

解释边界：
- PERMANOVA 反映距离矩阵上的组间差异，不等价于具体驱动因素或因果关系。

## 3. 差异丰度分析方法
分析层级：
- 在指定分类层级（系统默认 `tax_level="Genus"`）聚合丰度后逐 taxon 进行检验。

统计检验：
- 两组：Wilcoxon 秩和检验（`wilcox.test(exact = FALSE)`）。
- 三组及以上：Kruskal-Wallis 检验（`kruskal.test`）。

效应量：
- 两组情况下计算 `log2FC`：以两组均值比值 `log2(mean(group2) / mean(group1))`（当两组均值均为正数时计算，否则置为 NA）。
- 多组情况下 `log2FC` 置为 NA（避免错误的二元解释）。

多重校正与显著性：
- p 值做 FDR 校正：`p.adjust(p_value, method="fdr")`。
- 显著阈值：`FDR < 0.05`。
- 同时支持“趋势”识别用于解释文本：`0.05 <= FDR < 0.1`。

可视化：
- Volcano：x 为 `log2FC`，y 为 `-log10(FDR)`；绘制 `FDR=0.05` 参考线。
- Barplot：优先绘制显著 taxon；若无显著结果，则绘制按 raw p-value 排序的 TopN（标记为 exploratory）。

输出：
- `tables/differential_taxa.csv`
- `tables/differential_taxa_significant.csv`
- `json/diff_summary.json`
- `figures/diff_volcano.png/.pdf`、`figures/diff_taxa_barplot.png/.pdf`

解释边界：
- 差异分析反映统计层面的组间差异与效应方向；不直接给出机制与因果结论。

## 4. Random Forest 机器学习方法（探索性）
目的：
- 以随机森林对分组标签进行监督学习，输出可解释的特征重要性，用于探索性候选标志物筛选。

特征矩阵构建：
- 使用 microeco 聚合到指定分类层级（默认 Genus）的丰度矩阵；
- 组织为 `X: samples x features`，标签 `y` 为 `group_var` 的因子；
- 过滤 `y` 缺失的样本，确保至少 2 个类别。

模型训练与指标：
- `randomForest::randomForest(x=X, y=y, importance=TRUE)`；
- 训练集预测类别与（若二分类）预测概率；
- 输出 confusion matrix 与 accuracy；二分类补充 sensitivity/specificity/balanced accuracy 与 ROC AUC（若可计算）。

可靠性分级（用于提示而非断言）：
- `n < 20`：exploratory only
- `20 <= n < 50`：caution
- `n >= 50`：acceptable

输出：
- `tables/ml_feature_importance.csv`
- `tables/ml_model_metrics.csv`
- `json/ml_summary.json`（包含 reliability 与 caution）
- `figures/ml_importance.png/.pdf`、`figures/ml_confusion_matrix.png/.pdf`（二分类可有 `figures/ml_roc.png/.pdf`）

解释边界（必须强调）：
- 机器学习输出描述预测相关模式，**不得作为因果证据**；样本量小或类别不平衡时尤其需要谨慎。

## 5. Spearman 共现网络方法（探索性）
目的：
- 以 taxon 间的相关性构建共现网络，用于探索性刻画网络结构与可能的“中心节点”。

网络构建：
- 对每对 taxon 在样本维度上进行 Spearman 相关检验：`cor.test(method="spearman", exact=FALSE)`；
- 对边的 p 值做 FDR 校正；
- 筛边阈值（默认）：`abs(rho) >= 0.6` 且 `FDR < 0.05`；
- 无边/稀疏网络时仍输出空表与占位图，保证流程稳定。

中心性计算：
- Degree、Betweenness（normalized）、Closeness（normalized）、Eigenvector centrality；
- 记录连通分量（component）。

输出：
- `tables/network_nodes.csv`（含中心性与展示用 display_taxon 等）
- `tables/network_edges.csv`（含 rho/fdr/正负边等）
- `json/network_summary.json`（含阈值、规模与 caution）
- `figures/network_plot.png/.pdf`

解释边界（必须强调）：
- “相关不等于因果”；网络结构不应被解读为直接相互作用证据。

## 6. Key Taxa Score 公式
Key Taxa Score 用于对候选 taxon 进行工程化排序，融合三类证据：差异证据、ML 重要性、网络中心性。所有子分数均被归一化到 [0, 1]。

### 6.1 子分数定义（归一化）
1. 差异证据分数（differential_score）：
```text
raw_diff = (-log10(max(FDR, 1e-300))) * abs(log2FC)
differential_score = minmax_normalize(raw_diff)
```

2. 随机森林重要性分数（ml_importance_score）：
```text
ml_importance_score = minmax_normalize(importance)
```

3. 网络中心性分数（network_centrality_score）：
```text
raw_net = degree + betweenness
network_centrality_score = minmax_normalize(raw_net)
```

其中 `minmax_normalize(x) = (x - min(x)) / (max(x) - min(x))`；若 `max=min`，则归一化结果置为 0（保证稳定输出）。

### 6.2 融合公式（按 taxon 可用证据加权平均）
对每个 taxon，仅使用其“可用且非 NA”的证据项参与计算，分母为对应权重之和：

```text
KeyTaxaScore(t) = Σ_i [ w_i * s_i(t) ] / Σ_i [ w_i ]  ,  i ∈ {diff, ml, network} 且 s_i(t) 有效
```

权重采用预设规则按“本次运行中是否存在对应证据源”自动选择：
- diff + ml + network：`w_diff=0.4, w_ml=0.4, w_network=0.2`
- diff + ml：`0.5, 0.5`
- diff + network：`0.6, 0.4`
- ml + network：`0.6, 0.4`
- 仅单一来源：权重为 1.0

推荐等级：
- `KeyTaxaScore >= 0.75`：High
- `0.50 <= KeyTaxaScore < 0.75`：Medium
- `< 0.50`：Low

输出：
- `tables/key_taxa_score.csv`、`tables/key_taxa_top20.csv`
- `json/key_taxa_summary.json`（包含 used_sources、weights、reliability）

解释边界：
- 分数用于候选优先级排序；不等价于因果“关键性”证明。

## 7. AI 解释约束规则
本系统将 AI 解释设计为“受统计结果约束的摘要生成”，包括本地规则解释与（可选）LLM 摘要两部分。

### 7.1 本地规则解释（Phase 4A）
输入：
- `json/diff_summary.json` 与差异结果表；
- （可用时）`tables/alpha_stats.csv`、`tables/beta_permanova.csv`。

规则：
- `FDR < 0.05`：作为显著结果列表；
- `0.05 <= FDR < 0.1`：作为探索性趋势（trend）；
- 必须包含谨慎声明：解释为统计约束摘要，**不推断因果与机制**。

输出：
- `ai/diff_interpretation.md`、`ai/methods.md`、`ai/figure_legends.md`

### 7.2 LLM 受约束摘要（Phase 4B，可选）
LLM 输入限制：
- 只提供 `diff_summary.json` 与 Phase 4A 三份文本；
- 明确“不读原始 abundance 表”。

LLM 约束要点（提示词规则）：
- 只能使用提供的统计 JSON 与本地解释文本；
- 不得改变统计结论；
- 不得把不显著结果写成显著；
- 不得推断因果或机制；
- 输出必须为单个 JSON：键为 `diff_interpretation/methods/figure_legends`，值为 Markdown。

留痕：
- `json/llm_request_diff.json`、`json/llm_response_diff.json`
- `ai/llm_diff_interpretation.md`、`ai/llm_methods.md`、`ai/llm_figure_legends.md`

## 8. 报告生成方法（Quarto）
流程：
1. 收集 job 目录中既有产物的路径与存在性信息，构造 `report_ctx`（不进行新分析）。
2. 使用 Quarto 渲染模板 `templates/report_template.qmd`：
   - `quarto::quarto_render(input=template, execute_params=list(job_dir=job_dir, report_ctx=ctx))`
3. 将渲染产物复制为：`results/<job_id>/report/report.html`

特点：
- 报告只引用 job 目录内已落盘的结果，减少“运行时不一致”。
- 报告生成阶段不调用 LLM API。

## 9. 可复现性记录方法
核心策略：以 job 目录作为最小可复现实验单元，结合 `reproducibility.json` 记录关键元信息。

记录内容（示例）：
- 输入文件：保存到 `input/`，同时记录原始文件名与 MD5。
- 参数：记录 `group_var` 等关键参数。
- 阶段信息：记录 Phase6/Phase7 等阶段的时间戳、阈值（如网络阈值）、权重（Key Taxa Score 融合权重）、可靠性提示等。
- 错误信息：失败时写入 `logs/error.log`（便于追踪与复现问题）。

