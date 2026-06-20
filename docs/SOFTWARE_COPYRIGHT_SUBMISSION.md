# 软件著作权提交说明

本文档面向软件著作权登记、毕业答辩材料整理和系统验收场景，概述本项目的用途、运行环境、主要功能、操作流程、输出物和当前限制。

## 1. 软件名称

- 中文名称：微生物组关键菌筛选与可复现报告生成系统
- 英文名称：Microbiome Key Taxa AI

## 2. 软件用途

本软件是一个基于 R Shiny 的微生物组数据分析系统，面向 abundance、metadata、taxonomy 三类输入数据，提供数据校验、Alpha/Beta 多样性分析、差异丰度分析、机器学习筛选、共现网络分析、关键菌综合评分以及 HTML 报告生成能力。

系统强调以下交付目标：

- 将一次分析过程固化为一个独立 `job` 目录，便于复核与复现。
- 将统计结果、图表、JSON 汇总、AI 解释文本和最终报告统一归档。
- 通过图形化页面降低非编程用户的使用门槛，适合演示、答辩和教学展示。

## 3. 软件环境

建议运行环境如下：

- 操作系统：Windows 10/11
- R：4.6.0
- Quarto：1.9.37 或可兼容版本
- 主要框架：Shiny、bslib、DT
- 主要分析依赖：microeco、vegan、randomForest、pROC
- 依赖管理：renv
- 数据记录：SQLite

说明：

- 项目根目录包含 `renv.lock`，可通过 `renv::restore()` 恢复依赖环境。
- 若在 Windows 终端查看中文文档或日志出现乱码，建议先执行 `chcp 65001` 切换到 UTF-8。

## 4. 主要功能

1. 输入文件上传与固化保存
2. 数据一致性与合法性校验
3. 分组变量与分析参数记录
4. Alpha diversity 分析
5. Beta diversity 分析与 PERMANOVA
6. 差异丰度分析与结果汇总
7. 受约束的 AI 解释文本生成
8. 基于 Random Forest 的机器学习筛选
9. 基于 Spearman 相关性的共现网络分析
10. 多证据融合的 Key Taxa Score 排序
11. 基于 Quarto 的 HTML 报告生成
12. 结果目录、日志与可复现记录管理

## 5. 典型使用流程

1. 打开项目根目录并执行 `renv::restore()`。
2. 执行 `shiny::runApp()` 启动系统。
3. 上传 `abundance / metadata / taxonomy` 三类输入文件。
4. 创建任务目录并保存输入副本。
5. 运行 Data Check。
6. 选择 `group_var` 等参数。
7. 运行完整工作流。
8. 在结果页或报告页查看图表、表格和 `report.html`。

## 6. 输入与输出

输入文件要求：

- `abundance`：第一列为 `FeatureID`，其余列为样本。
- `metadata`：必须包含 `SampleID`。
- `taxonomy`：必须包含 `FeatureID`，并与 abundance 特征对应。

一次完整运行后，系统会在 `results/job_YYYYMMDD_HHMMSS_xxxxxx/` 下生成：

- `input/`：输入副本
- `tables/`：CSV 结果表
- `figures/`：PNG/PDF 图形
- `json/`：结构化汇总与可选的 LLM 请求响应
- `ai/`：方法、图例与解释文本
- `report/report.html`：最终展示报告
- `logs/`：运行日志
- `reproducibility.json`：输入 MD5、参数和阶段记录

## 7. 软件特点

- 采用任务目录隔离机制，避免不同运行结果互相覆盖。
- 强调结果可追溯，输入、参数、图表和报告统一归档。
- 支持在未配置 API key 时使用本地降级文本，避免完整流程中断。
- 通过 smoke 脚本和最小测试脚本支撑交付前自检。

## 8. 本地验证情况

截至 2026 年 6 月 10 日，本项目已完成以下本地验证：

- `Rscript scripts/run_tests.R`
- `Rscript scripts/phase2_smoke.R`
- `Rscript scripts/phase3_smoke.R`
- `Rscript scripts/phase4b_smoke.R`
- `Rscript scripts/phase5_smoke.R`
- `Rscript scripts/phase6_smoke.R`
- `Rscript scripts/phase7_smoke.R`
- `Rscript scripts/phase8_smoke.R`
- `Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3850, launch.browser=FALSE)"`

验证结果：

- 各阶段 smoke 脚本通过。
- Shiny 服务可正常启动，并可通过 `http://127.0.0.1:3850` 访问。
- 未配置 LLM API key 时，Phase 4B 走本地降级输出，不阻塞完整流程。

## 9. 当前边界与限制

- 当前默认工作流中的部分参数仍为固定默认值，例如 `beta_distance = "bray"`、`tax_level = "Genus"`。
- 本系统输出为统计关联与探索性发现，不应表述为生物学因果结论。
- 报告生成依赖 Quarto 环境，若本机未正确安装 Quarto，`report.html` 可能无法渲染。
- 机器学习、网络分析和 Key Taxa Score 结果适合候选筛选与展示，不等同于最终实验验证结论。

## 10. 提交材料建议

软件著作权登记或答辩提交时，建议同时附上：

- `README.md`
- `docs/SYSTEM_DESIGN.md`
- `docs/USER_GUIDE.md`
- `docs/SYSTEM_REQUIREMENTS.md`
- 示例输入文件：`data/example_*.tsv`
- 一份完整运行后的 `results/job_*/report/report.html`

