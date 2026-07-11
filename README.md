# 微生物组关键菌筛选与可信解释系统 V1.0

**Microbiome Key Taxa AI** 是一套基于 R Shiny 的微生物组数据分析与报告生成应用。系统将数据检查、多样性分析、差异丰度分析、机器学习筛选、共现网络分析、关键菌综合评分和受约束结果解释组织为图形化流程，并为每次运行保存独立、可追溯的任务产物。

> 当前状态：V1.0 申报材料整理与版本冻结前。已知测试和报告页问题见 `docs/TEST_REPORT.md`，修复并重新验收前不应将当前工作区标记为正式发布版。

## 核心能力

- 导入并固化丰度表、样本信息表和分类注释表；
- 检查必要字段、非法丰度、重复标识以及样本、特征对齐情况；
- 完成 Alpha/Beta 多样性和差异丰度分析；
- 使用随机森林进行探索性特征筛选；
- 使用 Spearman 相关构建共现网络；
- 融合差异、机器学习和网络证据，形成 Key Taxa Score；
- 基于结构化统计结果生成受约束解释，避免显著性升级和因果化表述；
- 使用 Quarto 生成 HTML/PDF 报告；
- 浏览历史任务并下载报告、关键表格、图形或完整结果包。

## 标准流程

```text
导入三类数据
→ 创建独立任务
→ 数据质量检查
→ 选择分组变量
→ 运行完整分析
→ 查看结果总览
→ 生成报告
→ 下载并归档任务产物
```

## 技术组成

- R、Shiny、bslib、DT；
- microeco、vegan、randomForest、pROC 等分析组件；
- SQLite 与文件系统任务目录；
- Quarto 报告模板；
- renv 依赖管理；
- 可选的大模型 API。

具体依赖版本以 `renv.lock` 为准。系统调用开源 R 包完成底层统计计算，自研重点是数据校验、工作流编排、任务管理、多证据评分、解释约束、界面展示和报告组织。

## 项目结构

```text
.
├─ app.R                     # Shiny 应用入口和页面组装
├─ global.R                  # 配置、依赖和源文件加载
├─ config.yml                # 路径、上传限制及可选模型配置
├─ R/                        # 数据处理、分析、工作流和报告业务函数
├─ modules/                  # Shiny 页面模块
├─ templates/                # Quarto 与提示词模板
├─ data/                     # 示例数据
├─ tests/                    # 自动化测试
├─ scripts/                  # 环境、测试和阶段验证脚本
├─ docs/                     # 需求、设计、手册和申报材料
├─ results/                  # 任务输出目录
├─ database/                 # SQLite 默认位置
├─ uploads/                  # 上传缓存目录
└─ www/                      # 前端静态资源
```

## 安装与启动

在项目根目录打开 R 或 RStudio，恢复依赖：

```r
renv::restore()
```

启动应用：

```r
shiny::runApp()
```

命令行启动示例：

```powershell
Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3850, launch.browser=TRUE)"
```

如果 `Rscript` 未加入 PATH，请使用 RStudio 或本机 `Rscript.exe` 的完整路径。

## 输入格式

支持 UTF-8 编码的 `.tsv`、`.csv` 和 `.txt`，推荐 TSV。

| 文件 | 必要字段 | 主要要求 |
|---|---|---|
| 丰度表 | `FeatureID` | 其余列为样本，单元格为非负数值 |
| 样本信息表 | `SampleID` | 标识唯一，并至少有一个有效分组变量 |
| 分类注释表 | `FeatureID` | 特征与丰度表对应，建议包含 Genus 等层级 |

示例文件：

- `data/example_abundance.tsv`
- `data/example_metadata.tsv`
- `data/example_taxonomy.tsv`

## 任务输出

每次运行创建 `results/job_YYYYMMDD_HHMMSS_xxxxxx/`：

```text
job_xxx/
├─ input/                    # 输入副本
├─ alpha/                    # Alpha 专属结果
│  ├─ tables/               # Alpha 指标和统计表
│  └─ figures/              # 按指标、图形类型分类的 PNG/PDF
├─ beta/                     # Beta 专属结果
│  ├─ tables/               # PCoA、PERMANOVA、PERMDISP
│  └─ figures/              # 论文级 PCoA 和离散度诊断图
├─ tables/                   # CSV 结果表
├─ figures/                  # PNG/PDF 图形
├─ json/                     # 结构化摘要和可选模型留痕
├─ ai/                       # 方法、图例和解释文本
├─ objects/                  # R 对象
├─ report/                   # HTML/PDF 报告
├─ logs/                     # 运行日志
└─ reproducibility.json      # 文件校验值、参数和阶段状态
```

Alpha 和 Beta 图形均不与其他分析图片混放。Alpha 在 `alpha/figures/` 下按指标和图形类型分类；Beta 在 `beta/figures/pcoa/` 保存论文级排序图，在 `beta/figures/dispersion/` 保存组内离散度诊断图。

报告只引用当前任务中已经落盘的分析产物，不在渲染阶段重新执行统计计算。

## 测试

自动化测试入口：

```powershell
Rscript scripts/run_tests.R
```

项目还包含各阶段 smoke 脚本。正式发布或申报前，应在冻结版本上执行自动化测试、应用启动检查和一次完整示例流程，并更新测试报告。

## 结果解释边界

- 差异和多样性结果表示统计关联，不直接证明因果关系；
- 随机森林结果用于探索性特征筛选；
- 共现网络的边不代表直接相互作用；
- Key Taxa Score 用于候选优先级排序，不等于实验验证；
- 自动解释不能替代专业判断和人工审核。

## 文档导航

- 软件统一信息：`docs/SOFTWARE_PROFILE.md`
- 软著材料说明：`docs/SOFTWARE_COPYRIGHT_SUBMISSION.md`
- 需求规格说明：`docs/SYSTEM_REQUIREMENTS.md`
- 系统设计说明：`docs/SYSTEM_DESIGN.md`
- 用户操作手册：`docs/USER_GUIDE.md`
- 测试与验收记录：`docs/TEST_REPORT.md`
- 创新点与边界：`docs/INNOVATION_POINTS.md`

## 安全提示

- 不要将 API 密钥写入源代码、配置样例、截图或结果包；
- 导入真实研究数据前完成必要的授权和去标识化；
- 任务压缩包包含输入副本，传递前应检查数据敏感性；
- 正式归档时同时保存冻结代码、`renv.lock`、完整任务和最终报告。
