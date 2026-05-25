# 用户使用手册（Microbiome Key Taxa AI）

本文档面向第一次使用本项目的人，按“从打开 RStudio 到生成 report.html”的顺序说明操作。本文档不会引导新增功能或修改分析逻辑。

## 1. 如何打开 RStudio
1. 安装并打开 RStudio（RStudio Desktop）。
2. 用 RStudio 打开本项目根目录（推荐方式）：
   - 菜单 `File -> Open Project...`，选择项目根目录（包含 `app.R`、`global.R` 的目录）。
   - 如果没有 `.Rproj` 文件也没关系，后续手动设置工作目录即可。

## 2. 如何设置工作目录
工作目录必须是项目根目录（能直接看到 `app.R`、`config.yml`、`modules/`、`R/` 的那个目录）。

在 RStudio 中设置方式：
1. 打开 Console（控制台）。
2. 执行（把路径改成你的实际路径）：

```r
setwd("D:/Microbiome Key Taxa AI")
getwd()
```

如果 `getwd()` 输出的目录下能看到 `app.R`，说明设置正确。

## 3. 如何安装缺失 R 包
本项目使用 `renv` 锁定依赖版本；根目录的 `.Rprofile` 会自动激活 `renv`。

推荐安装方式（一次性恢复全部依赖）：
```r
renv::restore()
```

如果你不想用 renv，也可以按报错提示手动安装缺失包，例如：
```r
install.packages(c("shiny", "bslib", "DT"))
```

安装完成后建议重启 R 会话：
- 菜单 `Session -> Restart R`

## 4. 如何运行 shiny::runApp()
在项目根目录的 RStudio Console 执行：
```r
shiny::runApp()
```

运行后会弹出 Shiny 窗口（或在浏览器中打开）。看到顶部导航栏（Home/Upload Data/Data Check/.../Report）说明启动成功。

## 5. 如何上传 abundance / metadata / taxonomy
进入顶部导航 `Upload Data` 页签：
1. 在 `Abundance table (tsv/csv/txt)` 上传丰度表。
2. 在 `Metadata (tsv/csv/txt)` 上传样本信息表。
3. 在 `Taxonomy (tsv/csv/txt)` 上传分类注释表。
4. 点击 `Create Job & Save Inputs`。

成功后页面会显示：
- `job_id: ...`
- `job_dir: ...`

同时输入会被固化到当前 job 目录：
- `results/<job_id>/input/abundance.tsv`
- `results/<job_id>/input/metadata.tsv`
- `results/<job_id>/input/taxonomy.tsv`

## 6. 如何进入 Data Check
点击顶部导航 `Data Check` 页签，然后点击：
- `Run Data Check`

运行完成后：
- 页面会显示 `Overall status: pass / warning / error`
- 表格区会列出各项检查结果
- 汇总会写入：`results/<job_id>/tables/data_check_summary.csv`

说明：
- Data Check 是必做步骤；后续分析依赖这里读入并校验过的数据。

## 7. 如何选择 Parameters
点击顶部导航 `Parameters` 页签：
1. 在 `Group variable` 下拉框选择分组变量（来自 metadata，除 `SampleID` 外的列）。
2. 点击 `Save Parameters` 保存参数。

保存成功后，参数会写入当前 job 的：
- `results/<job_id>/reproducibility.json`

注意：
- metadata 必须包含 `SampleID` 列，否则这里会提示 “missing SampleID”。

## 8. 如何 Run Analysis
点击顶部导航 `Run Analysis` 页签：
1. 确保你已经做过 `Data Check`，并且已在 `Parameters` 保存 `group_var`。
2. 点击 `Run Full Workflow (Phase 2-8)`。

运行过程：
- 页面会显示 `job_id/job_dir/status/report` 信息。
- 下方会出现一个“产物存在性表格”（artifact table），用于快速确认哪些文件已经生成。

如果运行失败：
- Shiny 会弹红色通知（包含错误信息）。
- 同时会写日志到：`results/<job_id>/logs/error.log`

## 9. 如何查看 Alpha / Beta / Diff / Report
运行完成后按页签查看：

1. `Alpha Diversity`
   - 预览 `figures/alpha_shannon_boxplot.png`
2. `Beta Diversity`
   - 预览 `figures/beta_pcoa_bray.png`
3. `Diff Abundance`
   - 预览 `figures/diff_volcano.png`
   - 查看“Significant Taxa”显著差异表
4. `Report`
   - 点击 `Render HTML Report` 渲染最终报告
   - 若渲染成功，会出现 `Open Report` 按钮

## 10. 如何打开 report.html
在 `Report` 页签点击 `Open Report`（会在新标签页打开）。

你也可以在文件系统中直接打开（双击）：
- `results/<job_id>/report/report.html`

## 11. 常见报错处理

### 11.1 Missing required R packages
现象：
- 启动或运行时提示：`Missing required R packages: ...`

处理：
1. 优先执行（推荐）：
```r
renv::restore()
```
2. 或按提示逐个安装缺失包：
```r
install.packages("包名")
```
3. 安装后 `Session -> Restart R`，再重新 `shiny::runApp()`。

### 11.2 randomForest 包缺失
现象：
- 机器学习/标志物筛选相关步骤报错提示缺少 `randomForest`。

处理：
```r
install.packages("randomForest")
```
安装后重启 R 会话并重新运行。

### 11.3 Quarto 不可用
现象：
- 点击 `Render HTML Report` 后报错，或提示无法调用 Quarto。

处理思路：
1. 确认已安装 Quarto（桌面程序，不只是 R 包）。
2. 确认 R 包 `quarto` 已安装（本项目依赖中包含，但环境异常时可能缺失）：
```r
install.packages("quarto")
```
3. 查看错误细节：
   - Shiny 弹窗提示
   - `results/<job_id>/logs/error.log`

### 11.4 没有选择 group variable
现象：
- 在 `Run Analysis` 点击运行后报错类似：`group_var not set. Please save it in Parameters.`

处理：
1. 回到 `Parameters` 页签。
2. 在 `Group variable` 选择一列（例如 `Group`）。
3. 点击 `Save Parameters`，再重新运行分析。

### 11.5 report.html 没生成
现象：
- `Report` 页签一直显示 `Report not yet generated.`，或没有出现 `Open Report`。

处理：
1. 先确认已经在 `Run Analysis` 跑完 `Run Full Workflow (Phase 2-8)`（报告依赖结果文件）。
2. 再点击 `Render HTML Report`。
3. 若仍失败，优先排查 Quarto（见 11.3），并查看：
   - `results/<job_id>/logs/error.log`
   - `results/<job_id>/report/` 下是否有中间文件（如有）

