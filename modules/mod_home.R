# 首页：展示工作流入口与核心模块概览。

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$div(
      class = "dashboard-page",
      shiny::tags$div(
        class = "kkai-home-hero animated-gradient-bg",
        shiny::tags$div(
          class = "hero-card glassmorphism-card",
          shiny::tags$h2("Microbiome Key Taxa AI", class = "kkai-title glow-text"),
          shiny::tags$p("面向微生物组下游分析与关键菌筛选的 AI 平台", class = "kkai-subtitle"),
          shiny::tags$div(
            class = "kkai-hero-actions",
            shiny::actionButton(ns("start_analysis"), "开始分析", icon = shiny::icon("rocket"), class = "btn btn-primary primary-button pulse-hover"),
            shiny::actionButton(ns("open_demo"), "打开示例模式", icon = shiny::icon("laptop-code"), class = "btn btn-outline-primary pulse-hover")
          )
        ),
        shiny::tags$div(class = "kkai-hero-side glassmorphism-card", shiny::uiOutput(ns("active_job_card")))
      ),

      shiny::tags$div(class = "kkai-section-title", shiny::icon("project-diagram"), " 分析工作流"),
      shiny::tags$div(
        class = "workflow-grid",
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "01"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("file-upload"), " 上传数据"), shiny::tags$div(class = "kkai-workflow-desc", "导入丰度表、样本信息与注释表")),
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "02"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("check-double"), " 数据检查"), shiny::tags$div(class = "kkai-workflow-desc", "一致性、缺失值与分组变量校验")),
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "03"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("sliders-h"), " 参数设置"), shiny::tags$div(class = "kkai-workflow-desc", "分类层级、距离度量与分组变量")),
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "04"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("play-circle"), " 运行分析"), shiny::tags$div(class = "kkai-workflow-desc", "一键执行全流程并跟踪进度")),
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "05"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("chart-pie"), " 结果总览"), shiny::tags$div(class = "kkai-workflow-desc", "卡片化查看图表与关键结论")),
        shiny::tags$div(class = "workflow-step-card interactive-hover", style = "cursor: pointer;", onclick = sprintf("Shiny.setInputValue('%s', 'demo', {priority: 'event'});", ns("nav_card")), shiny::tags$div(class = "kkai-workflow-kicker", "06"), shiny::tags$div(class = "kkai-workflow-name", shiny::icon("file-pdf"), " 生成报告"), shiny::tags$div(class = "kkai-workflow-desc", "导出 HTML/PDF 报告与结果压缩包"))
      ),

      shiny::tags$div(class = "kkai-section-title", shiny::icon("layer-group"), " 核心模块"),
      shiny::tags$div(
        class = "card-grid-3",
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-1", style = "cursor: pointer;", onclick = "document.getElementById('desc-alpha_beta').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("chart-bar"), " Alpha/Beta 多样性"), shiny::tags$p("多样性指标、PCoA 与基础统计汇总。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-2", style = "cursor: pointer;", onclick = "document.getElementById('desc-diff').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("chart-line"), " 差异丰度"), shiny::tags$p("火山图与显著菌筛选。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-3", style = "cursor: pointer;", onclick = "document.getElementById('desc-key_taxa').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("star"), " 关键菌评分"), shiny::tags$p("融合差异、机器学习和网络证据的综合排序。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-4", style = "cursor: pointer;", onclick = "document.getElementById('desc-ai').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("robot"), " AI 解释"), shiny::tags$p("基于统计与模型输出的约束式解释。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-5", style = "cursor: pointer;", onclick = "document.getElementById('desc-ml').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("brain"), " 机器学习"), shiny::tags$p("筛选判别特征并输出重要性与性能概览。", class = "kkai-muted")),
        bslib::card(class = "dashboard-card interactive-module-card border-gradient-6", style = "cursor: pointer;", onclick = "document.getElementById('desc-network').scrollIntoView({behavior: 'smooth', block: 'start'})", shiny::tags$h5(shiny::icon("project-diagram"), " 网络分析"), shiny::tags$p("共现网络可视化与结构特征汇总。", class = "kkai-muted"))
      ),
      # --- 核心模块深度解读区域 ---
      shiny::tags$div(class = "kkai-section-title", style = "margin-top: 50px;", shiny::icon("book-open"), " 核心模块深度解读与图例说明"),
      shiny::tags$div(
        class = "dashboard-page",
        bslib::card(
          class = "dashboard-card",
          
          # Alpha/Beta 多样性
          shiny::tags$div(
            id = "desc-alpha_beta", style = "padding: 20px 0; border-bottom: 1px solid #eee;",
            shiny::tags$h4(shiny::icon("chart-bar", class = "text-primary"), " Alpha/Beta 多样性 (Alpha/Beta Diversity)"),
            shiny::tags$p("多样性分析是微生物组研究的基础，用于评估群落的丰富度、均匀度以及样本间的整体差异。"),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("Alpha 多样性 (Shannon/Simpson 指数)："), "反映单个样本内的物种丰富程度。指数越高，说明样本内包含的物种种类越多、分布越均匀。通常使用箱线图展示不同分组间的差异，并伴随 Wilcoxon/Kruskal-Wallis 检验。"),
              shiny::tags$li(shiny::tags$b("Beta 多样性 (PCoA 降维分析)："), "计算样本两两之间的距离（如 Bray-Curtis 距离）。在 PCoA 图中，每个点代表一个样本，空间距离越近的点，说明其微生物群落结构越相似。如果不同颜色的点（代表不同分组）在图中明显分群，说明分组对菌群结构产生了显著影响。")
            )
          ),
          
          # 差异丰度分析
          shiny::tags$div(
            id = "desc-diff", style = "padding: 20px 0; border-bottom: 1px solid #eee;",
            shiny::tags$h4(shiny::icon("chart-line", class = "text-success"), " 差异丰度分析 (Differential Abundance)"),
            shiny::tags$p("寻找在不同疾病状态或处理组之间发生显著改变的微生物（即 Biomarker）。"),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("火山图 (Volcano Plot) 解读："), 
                "横坐标代表 ", shiny::tags$code("Log2 倍数变化 (Fold Change)"), "，横坐标绝对值越大，说明该物种在两组间的丰度差异越悬殊；",
                "纵坐标代表 ", shiny::tags$code("-Log10(P-value)"), "，点越高说明统计学显著性越强。"
              ),
              shiny::tags$li(shiny::tags$b("富集方向："), "火山图中右侧的点代表在实验组/疾病组显著上升（富集）的物种，左侧的点代表显著下降的物种。")
            )
          ),
          
          # 关键菌评分
          shiny::tags$div(
            id = "desc-key_taxa", style = "padding: 20px 0; border-bottom: 1px solid #eee;",
            shiny::tags$h4(shiny::icon("star", class = "text-warning"), " 关键菌评分体系 (Key Taxa Score)"),
            shiny::tags$p("本平台独创的综合评估算法，克服了单一统计方法的局限性。评分由三个维度的证据加权得出："),
            shiny::tags$ol(
              shiny::tags$li(shiny::tags$b("统计学差异 (Differential Weight)："), "来源于 Wilcoxon 检验的 P 值及效应量，衡量直接的丰度变化差异。"),
              shiny::tags$li(shiny::tags$b("机器学习重要性 (ML Importance Weight)："), "来源于随机森林模型中该物种对分组判别的贡献度 (MeanDecreaseGini)，衡量其预测价值。"),
              shiny::tags$li(shiny::tags$b("网络拓扑核心度 (Network Hub Weight)："), "来源于共现网络中的 Degree 中心性，衡量该物种在群落生态网络中的枢纽地位。")
            ),
            shiny::tags$p(shiny::tags$em("图例说明：柱状图中的总长度代表综合得分，内部颜色的分段代表上述三个证据来源的各自贡献比例。"))
          ),
          
          # AI 解释
          shiny::tags$div(
            id = "desc-ai", style = "padding: 20px 0; border-bottom: 1px solid #eee;",
            shiny::tags$h4(shiny::icon("robot", class = "text-info"), " AI 智能解释引擎 (AI Interpretation)"),
            shiny::tags$p("传统分析通常只输出枯燥的 P 值和图表，我们的 AI 引擎能够模拟专业生信工程师为您解读结果："),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("结论提炼："), "自动扫描所有分析模块的统计显著性，提取最核心的发现（例如“Alpha 多样性无显著差异，但关键菌群结构发生偏移”）。"),
              shiny::tags$li(shiny::tags$b("学术语境："), "输出的文字符合标准的科研论文 (SCI) 语境风格，您可以直接将其作为撰写论文 Results 部分的参考草稿。")
            )
          ),
          
          # 机器学习
          shiny::tags$div(
            id = "desc-ml", style = "padding: 20px 0; border-bottom: 1px solid #eee;",
            shiny::tags$h4(shiny::icon("brain", class = "text-danger"), " 机器学习特征筛选 (Machine Learning)"),
            shiny::tags$p("采用随机森林 (Random Forest) 算法构建疾病分类或状态预测模型。"),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("特征重要性图 (Variable Importance)："), "图中条形越长的物种，说明它对模型区分不同样本的作用越大，是潜在的高价值 Biomarker。"),
              shiny::tags$li(shiny::tags$b("性能概览："), "系统会自动划分训练集与测试集，并计算 OOB (Out-of-Bag) 错误率，用于评估模型稳健性。")
            )
          ),
          
          # 网络分析
          shiny::tags$div(
            id = "desc-network", style = "padding: 20px 0;",
            shiny::tags$h4(shiny::icon("project-diagram", style = "color: #6f42c1;"), " 共现网络分析 (Co-occurrence Network)"),
            shiny::tags$p("超越单个物种，从群落生态系统层面探究微生物间的相互作用网络。计算基于 Spearman 强相关性 (默认阈值 R > 0.6, P < 0.05)。"),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("节点 (Nodes)："), "图中的圆点代表不同的物种。", shiny::tags$b("圆点的大小"), "与其在网络中的 Degree (连接数) 成正比。圆点越大，说明它与其他物种的交互越频繁，是维持群落稳定性的 Hub (枢纽) 物种。"),
              shiny::tags$li(shiny::tags$b("连线 (Edges)："), "点与点之间的连线代表显著相关性。", shiny::tags$span(style="color:red; font-weight:bold;", "红线"), " 表示正相关 (协同生长、生态位相似)；", shiny::tags$span(style="color:blue; font-weight:bold;", "蓝线"), " 表示负相关 (生态位竞争、拮抗作用)。"),
              shiny::tags$li(shiny::tags$b("网络密度与模块化："), "密集的子网络往往代表具有相似功能的微生物公会 (Guild)。")
            )
          )
        )
      )
    )
  )
}

mod_home_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Resolve root session for navbar navigation
    root_session <- parent_session %||% (if (!is.null(session$parent)) session$parent else session)

    shiny::observeEvent(input$start_analysis, {
      shiny::updateTabsetPanel(session = root_session, inputId = "main_nav", selected = "quick_start")
      try(bslib::nav_select(id = "main_nav", selected = "quick_start", session = root_session), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_demo, {
      shiny::updateTabsetPanel(session = root_session, inputId = "main_nav", selected = "demo")
      try(bslib::nav_select(id = "main_nav", selected = "demo", session = root_session), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$nav_card, {
      req(input$nav_card)
      parent_session <- if (!is.null(session$parent)) session$parent else session
      shiny::updateTabsetPanel(session = parent_session, inputId = "main_nav", selected = input$nav_card)
      try(bslib::nav_select(id = "main_nav", selected = input$nav_card, session = parent_session), silent = TRUE)
    }, ignoreInit = TRUE)

    output$active_job_card <- shiny::renderUI({
      if (is.null(state$job_id) || is.null(state$job_dir)) {
        return(
          bslib::card(
            class = "dashboard-card kkai-job-card",
            bslib::card_header("当前任务"),
            shiny::tags$div(class = "kkai-kv", shiny::tags$div(shiny::tags$b("任务 ID："), " ", shiny::tags$code("(none)"))),
            shiny::tags$div(class = "kkai-muted", "从“快速开始”开始创建任务与运行分析。")
          )
        )
      }

      st <- state$status %||% "idle"
      badge_kind <- if (grepl("error|fail", st, ignore.case = TRUE)) {
        "error"
      } else if (grepl("running", st, ignore.case = TRUE) || st %in% c("running_full_workflow", "running")) {
        "warning"
      } else {
        "success"
      }

      bslib::card(
        class = "dashboard-card kkai-job-card",
        bslib::card_header("当前任务"),
        shiny::tags$div(
          class = "kkai-job-top",
          shiny::tags$div(class = "kkai-job-id", shiny::tags$b("任务 ID："), " ", shiny::tags$code(state$job_id)),
          ui_status_badge(st, kind = badge_kind)
        ),
        shiny::tags$div(
          class = "kkai-job-actions",
          shiny::actionButton(session$ns("open_results"), "打开结果总览", class = "btn btn-outline-primary"),
          shiny::actionButton(session$ns("open_report"), "打开报告", class = "btn btn-outline-dark")
        ),
        shiny::tags$details(
          shiny::tags$summary("开发者信息"),
          shiny::tags$div(
            class = "kkai-details-grid",
            shiny::tags$div(shiny::tags$b("任务目录：")),
            shiny::tags$div(class = "kkai-codeblock", normalizePath(state$job_dir, winslash = "/", mustWork = FALSE))
          )
        )
      )
    })

    shiny::observeEvent(input$open_results, {
      shiny::updateTabsetPanel(session = root_session, inputId = "main_nav", selected = "results")
      try(bslib::nav_select(id = "main_nav", selected = "results", session = root_session), silent = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$open_report, {
      shiny::updateTabsetPanel(session = root_session, inputId = "main_nav", selected = "report")
      try(bslib::nav_select(id = "main_nav", selected = "report", session = root_session), silent = TRUE)
    }, ignoreInit = TRUE)
  })
}
