// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}


// syntax highlighting functions from skylighting:
/* Function definitions for syntax highlighting generated by skylighting: */
#let EndLine() = raw("\n")
#let Skylighting(fill: none, number: false, start: 1, sourcelines) = {
   let blocks = []
   let lnum = start - 1
   let bgcolor = rgb("#f1f3f5")
   for ln in sourcelines {
     if number {
       lnum = lnum + 1
       blocks = blocks + box(width: if start + sourcelines.len() > 999 { 30pt } else { 24pt }, text(fill: rgb("#aaaaaa"), [ #lnum ]))
     }
     blocks = blocks + ln + EndLine()
   }
   block(fill: bgcolor, width: 100%, inset: 8pt, radius: 2pt, blocks)
}
#let AlertTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let AnnotationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let AttributeTok(s) = text(fill: rgb("#657422"),raw(s))
#let BaseNTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let BuiltInTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let CharTok(s) = text(fill: rgb("#20794d"),raw(s))
#let CommentTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let CommentVarTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ConstantTok(s) = text(fill: rgb("#8f5902"),raw(s))
#let ControlFlowTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let DataTypeTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DecValTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DocumentationTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ErrorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let ExtensionTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let FloatTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let FunctionTok(s) = text(fill: rgb("#4758ab"),raw(s))
#let ImportTok(s) = text(fill: rgb("#00769e"),raw(s))
#let InformationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let KeywordTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let NormalTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let OperatorTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let OtherTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let PreprocessorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let RegionMarkerTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let SpecialCharTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let SpecialStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let StringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let VariableTok(s) = text(fill: rgb("#111111"),raw(s))
#let VerbatimStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let WarningTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))



#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: doc => article(
  title: [Microbiome Key Taxa AI Report],
  toc_title: [Table of contents],
  toc_depth: 3,
  doc,
)

This report is generated by #strong[Microbiome Key Taxa AI].

= Project Overview
<project-overview>
This is an integrated, reproducible HTML report summarizing results from: Alpha/Beta diversity, differential abundance, AI-constrained interpretation, machine learning screening, co-occurrence network analysis, and the final Key Taxa Score.

= Executive Summary
<executive-summary>
#table(
  columns: (41.84%, 58.16%),
  align: (left,left,),
  table.header([Item], [Value],),
  table.hline(),
  [Samples], [6],
  [Features (Genus level for ML)], [4],
  [Group variable], [Group],
  [Significant taxa (differential analysis)], [0],
  [ML reliability], [exploratory only],
  [Network edges], [1],
  [Top candidate key taxa], [Lactobacillus, Escherichia, Bacteroides, Bifidobacterium],
)
#block[
#Skylighting(([],
[],
[#NormalTok("**Group sample counts:**");],));
]
#block[
#table(
  columns: 2,
  align: (left,right,),
  table.header([group], [n],),
  table.hline(),
  [Control], [3],
  [Treatment], [3],
)
]
= Data Quality Summary
<data-quality-summary>
#table(
  columns: 2,
  align: (left,left,),
  table.header([Field], [Value],),
  table.hline(),
  [Samples], [6],
  [Features], [4],
  [Group variable], [Group],
)
#block[
#table(
  columns: 2,
  align: (left,right,),
  table.header([group], [n],),
  table.hline(),
  [Control], [3],
  [Treatment], [3],
)
]
#block[
#Skylighting(([],
[],
[#NormalTok("Unavailable: data_check_summary.csv is empty.");],));
]
= Methods
<methods>
== Alpha/Beta Diversity
<alphabeta-diversity>
Alpha diversity indices are computed per sample and compared between groups when a group variable is provided. Beta diversity is summarized using Bray-Curtis distance with ordination and PERMANOVA.

== Differential Abundance Analysis
<differential-abundance-analysis>
Differential abundance identifies taxa associated with the group variable. Multiple-testing correction is applied (FDR). Results are descriptive and should be interpreted with study design context.

== Machine Learning (Random Forest)
<machine-learning-random-forest>
Random Forest is used as a screening tool to rank taxa by importance for distinguishing groups. When sample size is small, results are treated as exploratory.

== Co-occurrence Network
<co-occurrence-network>
A Spearman correlation network is built from genus-level abundances using correlation and FDR thresholds. Co-occurrence reflects correlation patterns and does not imply direct interaction.

== Key Taxa Score
<key-taxa-score>
KeyTaxaScore integrates available evidence sources (differential, ML importance, and network centrality) using weighted, normalized components. If a taxon lacks one evidence source, the score is computed from the remaining available evidence for that taxon and marked as exploratory via the evidence fields.

== AI Interpretation
<ai-interpretation>
This report includes pre-generated AI interpretation text when available. Phase 8 does not call any LLM API during report rendering.

= Alpha Diversity
<alpha-diversity>
#table(
  columns: (7.32%, 7.32%, 4.88%, 8.13%, 6.5%, 8.13%, 8.13%, 8.13%, 8.94%, 8.13%, 8.13%, 8.13%, 8.13%),
  align: (left,right,right,right,right,right,right,right,right,right,right,right,left,),
  table.header([SampleID], [Observed], [Chao1], [se.chao1], [ACE], [se.ACE], [Shannon], [Simpson], [InvSimpson], [Fisher], [Pielou], [Coverage], [Group],),
  table.hline(),
  [Sample1], [3], [3], [0.0000000], [3.00000], [0.8164966], [0.4642788], [0.2336694], [1.304920], [0.5409540], [0.4226047], [1.0000000], [Control],
  [Sample2], [3], [3], [0.0000000], [NA], [NA], [0.9533805], [0.5653114], [2.300498], [0.5426904], [0.8678043], [1.0000000], [Control],
  [Sample3], [4], [4], [0.0000000], [4.00000], [1.0000000], [0.9255611], [0.5327794], [2.140317], [0.7575847], [0.6676512], [1.0000000], [Control],
  [Sample4], [4], [4], [0.0000000], [4.00000], [0.8660254], [0.8394393], [0.4414926], [1.790487], [0.8553476], [0.6055275], [1.0000000], [Treatment],
  [Sample5], [4], [4], [0.4330127], [5.08642], [1.2345988], [0.9077691], [0.5148148], [2.061069], [0.8578964], [0.6548170], [0.9888889], [Treatment],
  [Sample6], [3], [3], [0.0000000], [3.00000], [0.8164966], [0.7191484], [0.4000462], [1.666795], [0.5926364], [0.6545971], [1.0000000], [Treatment],
)
#table(
  columns: 6,
  align: (left,left,right,right,right,right,),
  table.header([index], [test], [p\_value], [n\_samples], [n\_groups], [fdr],),
  table.hline(),
  [Shannon], [wilcoxon], [0.7], [6], [2], [0.7],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/alpha_shannon_boxplot.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 1. Shannon alpha diversity by group.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= Beta Diversity
<beta-diversity>
#table(
  columns: 6,
  align: (left,right,right,right,right,right,),
  table.header([term], [Df], [SumOfSqs], [R2], [F], [Pr(\>F)],),
  table.hline(),
  [Model], [1], [0.0596067], [0.2220052], [1.141423], [0.4],
  [Residual], [4], [0.2088858], [0.7779948], [NA], [NA],
  [Total], [5], [0.2684926], [1.0000000], [NA], [NA],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/beta_pcoa_bray.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 2. Bray-Curtis PCoA ordination (beta diversity).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= Differential Abundance Analysis
<differential-abundance-analysis-1>
#table(
  columns: (8.16%, 5.1%, 2.04%, 5.61%, 7.65%, 5.61%, 65.82%),
  align: (left,left,right,right,right,right,left,),
  table.header([display\_taxon], [tax\_level], [fdr], [log2fc], [mean\_abundance], [prevalence], [taxon],),
  table.hline(),
  [Escherichia], [Genus], [1], [0.2686067], [0.6473770], [1.0000000], [ASV1|Bacteria|Proteobacteria|Gammaproteobacteria|Enterobacterales|Enterobacteriaceae|Escherichia],
  [Lactobacillus], [Genus], [1], [-0.6746185], [0.2326185], [0.8333333], [ASV2|Bacteria|Firmicutes|Bacilli|Lactobacillales|Lactobacillaceae|Lactobacillus],
  [Bacteroides], [Genus], [1], [-0.0761510], [0.1050726], [1.0000000], [ASV3|Bacteria|Bacteroidota|Bacteroidia|Bacteroidales|Bacteroidaceae|Bacteroides],
  [Bifidobacterium], [Genus], [1], [-0.7719484], [0.0149319], [0.6666667], [ASV4|Bacteria|Actinobacteriota|Actinobacteria|Bifidobacteriales|Bifidobacteriaceae|Bifidobacterium],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/diff_taxa_barplot.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 3. Differential abundance summary (barplot).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= AI-Constrained Interpretation
<ai-constrained-interpretation>
== Differential Taxa Interpretation
<differential-taxa-interpretation>
This local interpretation summarizes the differential abundance results for #NormalTok("Group"); at the #NormalTok("Genus"); level using #NormalTok("wilcoxon");.

= Significant taxa
<significant-taxa>
No FDR-significant taxa were detected in this analysis.

= Caution
<caution>
These statements are statistically constrained summaries. They do not imply causation or mechanism.

= Machine Learning Biomarker Screening
<machine-learning-biomarker-screening>
#table(
  columns: 2,
  align: (left,right,),
  table.header([display\_taxon], [importance],),
  table.hline(),
  [Lactobacillus], [0.0020000],
  [Escherichia], [-0.0070000],
  [Bacteroides], [-0.0170000],
  [Bifidobacterium], [-0.0316667],
)
#table(
  columns: (10.69%, 7.63%, 7.63%, 8.4%, 7.63%, 6.87%, 12.98%, 9.16%, 9.16%, 13.74%, 6.11%),
  align: (left,left,right,right,right,right,left,right,right,right,right,),
  table.header([model], [tax\_level], [n\_samples], [n\_features], [n\_classes], [accuracy], [reliability], [sensitivity], [specificity], [balanced\_accuracy], [roc\_auc],),
  table.hline(),
  [random\_forest], [Genus], [6], [4], [2], [1], [exploratory only], [1], [1], [1], [1],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/ml_importance.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 4. Random Forest feature importance (Top 20; display\_taxon shown).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/ml_confusion_matrix.png", width: 75.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 5. Random Forest confusion matrix.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= Co-occurrence Network Analysis
<co-occurrence-network-analysis>
#table(
  columns: (22.54%, 9.86%, 16.9%, 14.08%, 21.13%, 15.49%),
  align: (left,right,right,right,right,right,),
  table.header([display\_taxon], [degree], [betweenness], [closeness], [mean\_abundance], [prevalence],),
  table.hline(),
  [Escherichia], [1], [0], [1], [0.6473770], [1.0000000],
  [Lactobacillus], [1], [0], [1], [0.2326185], [0.8333333],
  [Bacteroides], [0], [0], [0], [0.1050726], [1.0000000],
  [Bifidobacterium], [0], [0], [0], [0.0149319], [0.6666667],
)
#table(
  columns: 5,
  align: (left,left,right,right,left,),
  table.header([source\_display], [target\_display], [rho], [fdr], [sign],),
  table.hline(),
  [Escherichia], [Lactobacillus], [-0.9428571], [0.028828], [negative],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/network_plot.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 6. Co-occurrence network (Spearman correlation; co-occurrence does not imply direct interaction).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


Co-occurrence does not imply direct interaction.

= Key Taxa Score
<key-taxa-score-1>
KeyTaxaScore formula (Phase 7):

#Skylighting(([#NormalTok("KeyTaxaScore =");],
[#NormalTok("0.4 * DifferentialScore +");],
[#NormalTok("0.4 * MLImportanceScore +");],
[#NormalTok("0.2 * NetworkCentralityScore");],
[],
[#NormalTok("DifferentialScore = normalized(-log10(FDR) * abs(log2FC))");],
[#NormalTok("MLImportanceScore = normalized(Random Forest importance)");],
[#NormalTok("NetworkCentralityScore = normalized(degree + betweenness)");],));
#table(
  columns: (10.13%, 9.49%, 13.29%, 10.76%, 9.49%, 12.03%, 12.66%, 15.82%, 6.33%),
  align: (left,right,left,left,right,right,right,right,left,),
  table.header([display\_taxon], [key\_taxa\_score], [recommendation\_level], [evidence\_sources], [evidence\_count], [differential\_score], [ml\_importance\_score], [network\_centrality\_score], [tax\_level],),
  table.hline(),
  [Lactobacillus], [1.0000000], [High], [ml|network], [2], [NA], [1.0000000], [1], [NA],
  [Escherichia], [0.8217822], [High], [ml|network], [2], [NA], [0.7326733], [1], [NA],
  [Bacteroides], [0.2904290], [Low], [ml|network], [2], [NA], [0.4356436], [0], [NA],
  [Bifidobacterium], [0.0000000], [Low], [ml|network], [2], [NA], [0.0000000], [0], [NA],
  [Escherichia], [0.0000000], [Low], [diff], [1], [0], [NA], [NA], [Genus],
  [Lactobacillus], [0.0000000], [Low], [diff], [1], [0], [NA], [NA], [Genus],
  [Bacteroides], [0.0000000], [Low], [diff], [1], [0], [NA], [NA], [Genus],
  [Bifidobacterium], [0.0000000], [Low], [diff], [1], [0], [NA], [NA], [Genus],
)
#figure([
#box(image("../results/job_20260610_230026_w2boye/figures/key_taxa_score_barplot.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Figure 7. Key Taxa Score barplot (Top ranked candidates).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


= Candidate Key Taxa Summary
<candidate-key-taxa-summary>
#table(
  columns: (21.62%, 20.27%, 28.38%, 22.97%, 6.76%),
  align: (left,right,left,left,right,),
  table.header([display\_taxon], [key\_taxa\_score], [recommendation\_level], [evidence\_sources], [rank],),
  table.hline(),
  [Lactobacillus], [1.0000000], [High], [ml|network], [1],
  [Escherichia], [0.8217822], [High], [ml|network], [2],
  [Bacteroides], [0.2904290], [Low], [ml|network], [3],
  [Bifidobacterium], [0.0000000], [Low], [ml|network], [4],
  [Escherichia], [0.0000000], [Low], [diff], [5],
  [Lactobacillus], [0.0000000], [Low], [diff], [6],
  [Bacteroides], [0.0000000], [Low], [diff], [7],
  [Bifidobacterium], [0.0000000], [Low], [diff], [8],
)
#block[
#block[
#Skylighting(([#NormalTok("**Reliability:** standard ");],
[],
[#NormalTok("**Used sources:** diff, ml, network ");],));
]
]
= Reproducibility Record
<reproducibility-record>
#strong[Job dir:] d:Key Taxa AI\_20260610\_230026\_w2boye

#strong[Job ID:] job\_20260610\_230026\_w2boye

#strong[Created at:] 2026-06-10 23:00:26

#strong[Input files:]

- abundance
- metadata
- taxonomy

#strong[R version:] R version 4.6.0 (2026-04-24 ucrt)

#strong[microeco version:] 2.2.0

#strong[Generated outputs (expected):]

- tables/alpha\_diversity.csv : OK
- tables/beta\_permanova.csv : OK
- tables/differential\_taxa.csv : OK
- tables/ml\_feature\_importance.csv : OK
- tables/network\_nodes.csv : OK
- tables/key\_taxa\_score.csv : OK
- figures/alpha\_shannon\_boxplot.png : OK
- figures/beta\_pcoa\_bray.png : OK
- figures/diff\_taxa\_barplot.png : OK
- figures/ml\_importance.png : OK
- figures/network\_plot.png : OK
- figures/key\_taxa\_score\_barplot.png : OK
- json/key\_taxa\_summary.json : OK
- ai/diff\_interpretation.md : OK

#strong[AI interpretation rule summary:] Phase 8 does not call any LLM API; it only includes pre-generated markdown/JSON outputs when present.

= Supplementary Tables
<supplementary-tables>
This report was generated on 2026-06-10 using Quarto.
