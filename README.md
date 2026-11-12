# Medical Coding Fundamentals — Lesson 1: "What's Wrong With You?"

**Notes from the Abstracts**  
andrewrcrocker.substack.com

## About

This repository contains the article text, R visualization code, and rendered chart assets for Lesson 1 of the Medical Coding Fundamentals series published at *Notes from the Abstracts*.

The lesson introduces ICD-10-CM through the anatomy of a single code (E11.9 / E11.319 / E11.3211), the structure of the classification system (Alphabetic Index and Tabular List), and the coding workflow, using the fictional patient Dale Hammond as a recurring case study.

## Contents

| File | Description |
|------|-------------|
| `lesson-1-whats-wrong-with-you.md` | Article text (Markdown) |
| `lesson1_charts.R` | R script for all four visualizations |
| `chart1_code_anatomy.png` | Code anatomy diagram (E11.3211) |
| `chart2_e11_tree.png` | E11 family tree (fourth-character breakdown) |
| `chart3_specificity.png` | Specificity comparison (E11.9 vs E11.319 vs E11.3211) |
| `chart4_coding_workflow.png` | Coding workflow (Documentation → Index → Tabular List) |
| `chart5_codebook_page.png` | Simulated ICD-10-CM Tabular List page (E11 category) |

## Reproducing the Charts

The R script requires `ggplot2` and `dplyr`. A custom `theme_1950s()` function is defined within the script. Charts render at 1456px wide, 16:9 aspect ratio, at 100 DPI.

```r
Rscript lesson1_charts.R
```

## License

Copyright © 2026 Andrew R. Crocker. See LICENSE for terms.
