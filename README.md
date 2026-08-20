# Shopkeepers in Modern China

This repository contains the data, R scripts, and analytical outputs for a computational study of shopkeepers in the Chinese- and English-language press published in China from the late nineteenth to the mid-twentieth century.

The project uses large-scale text analysis to recover a social group that is often difficult to trace in conventional archival sources. It combines corpus construction, temporal analysis, collocation analysis, named-entity recognition, personâ€“organization networks, structural topic modeling, article segmentation, and LLM-assisted extraction of sociological information.

## Research scope

The principal Chinese corpus consists of articles from the *Shenbao* (ç”³æŠ¥, 1872â€“1949). Sixteen terms associated with shopkeepers and proprietors are used to locate relevant material:

`åŠä¸»`, `åº—ä¸»`, `æ£§ä¸»/æ ˆä¸»`, `èŽŠä¸»/åº„ä¸»`, `è™Ÿä¸»/å·ä¸»`, `è¡Œä¸»`, `é‹ªä¸»/é“ºä¸»`, `é¤¨ä¸»/é¦†ä¸»`, `åº—ä¸»å©¦/åº—ä¸»å¦‡`, `åº—æ±/åº—ä¸œ`, `æ±å®¶/ä¸œå®¶`, `æ±ä¸»/ä¸œä¸»`, `åº—å®¶`, `æŽŒæ«ƒ/æŽŒæŸœ`, `å•†æˆ¶/å•†æˆ·`, and `æ¥­ä¸»/ä¸šä¸»`.

The comparative English-language corpus draws on ProQuest's *Chinese Historical Newspapers* collection for the period approximately 1850â€“1950. Its searches use `shopkeeper`, `shopowner`, `shop owner`, and `grocer`.

The repository supports several related questions:

- How did the vocabulary and public visibility of shopkeepers change over time?
- In what legal, commercial, institutional, and social contexts did shopkeepers appear?
- Which people and organizations formed the principal affiliation networks around shopkeepers?
- What themes structured newspaper discourse about shopkeeping?
- What can computational extraction reveal about shopkeepers' names, origins, businesses, locations, institutions, and other social relations?

## Repository structure

| Path | Contents |
| --- | --- |
| `Corpus/` | Article metadata, shortened texts, and tokenized corpora derived from the Chinese- and English-language newspaper collections |
| `Data/` | Query-term tables, historiographical source tables, term-frequency summaries, and word-embedding results |
| `Scripts/` | Numbered R scripts for corpus construction and the principal analyses |
| `Collocations/` | Tables and graphs produced by the collocation analysis |
| `Networks/` | Network tables, graph outputs, and Cytoscape-ready node and edge lists |
| `Topic_Modeling/` | Inputs, outputs, tables, and graphs for structural topic modeling |
| `Figures/` | Publication figures and centrality profiles |

## Analytical workflow

The scripts are numbered to reflect the broad order of analysis:

| Script | Purpose |
| --- | --- |
| `01-shopkeeper_SB.R` | Constructs and describes the *Shenbao* corpus using the sixteen Chinese query terms |
| `02-shopkeeper-prq.R` | Constructs and analyzes the comparative English-language newspaper corpus |
| `03-shop-coloc.R` | Measures and visualizes collocations around Chinese shopkeeper terms across five historical periods |
| `04-shopkeeper_ner.R` | Applies HistText's Chinese named-entity recognition model, with batch processing for long documents |
| `05-shopkeeper_nwrk.R` | Builds personâ€“organization affiliation networks, calculates centralities and communities, and exports Cytoscape tables |
| `05-weighted_centralities.R` | Recalculates period networks with weighted and normalized centrality measures |
| `06-shopkeeper_tm.R` | Fits and interprets structural topic models; the documented principal model contains eight topics |
| `07-SB_Shop_Segment.R` | Filters, segments, and extracts shopkeeper-related passages from poorly segmented long *Shenbao* documents |
| `08-shopkeeper-soc.R` | Processes LLM-assisted sociological extractions, including names, origins, shops, locations, institutions, and other actors |

The scripts do not constitute a single automated pipeline. They record successive stages of a research workflow and should be read before execution because some contain optional save/load statements, exploratory code, or references to intermediate objects created in an earlier session.

## Requirements

The analysis is written in R. The scripts collectively use the following packages:

- `histtext`
- `tidyverse`, `dplyr`, `tidyr`, `stringr`, `purrr`, `readr`
- `lubridate`
- `tidytext`
- `quanteda`, `quanteda.textstats`
- `stm`, `stminsights`
- `igraph`, `tidygraph`, `ggraph`, `widyr`
- `data.table`
- `ggplot2`, `RColorBrewer`, `patchwork`
- `DT`, `kableExtra`
- `jsonlite`

Some scripts require access to corpora and models served through `histtext`, including the revised *Shenbao* corpus (`shunpao-revised`), the ProQuest corpus (`proquest`), and the Chinese NER model `trftc_nopunct:zh:ner`. Access to licensed newspaper content may depend on the user's institutional permissions.

## Getting started

Clone the repository and open an R session in its root directory:

```bash
git clone https://github.com/ankeqiang/Shopkeepers.git
cd Shopkeepers
```

Install the required R packages that are available from CRAN:

```r
install.packages(c(
  "tidyverse", "lubridate", "tidytext", "quanteda",
  "quanteda.textstats", "stm", "stminsights", "igraph",
  "tidygraph", "ggraph", "widyr", "data.table", "RColorBrewer",
  "patchwork", "DT", "kableExtra", "jsonlite"
))
```

Install and configure `histtext` separately according to the package's access instructions. Then inspect the scripts in numerical order, beginning with `Scripts/01-shopkeeper_SB.R` and `Scripts/02-shopkeeper-prq.R`.

## Reproducibility notes

Several qualifications are important when reusing the repository:

- Later scripts expect in-memory objects or `.RData` workspaces produced during earlier stages; examples include `shop_bind_tok2`, `shops_ftext400`, `shopkeeper_complete_ftext401`, `shops_ner`, `clean_sb5`, and `shops_socdata`.
- The network workflow refers to externally curated correction or normalization files at specific stages. Check the comments in `05-shopkeeper_nwrk.R` before running it.
- Paths used for exported files may need to be changed to match a local directory structure.
- Full newspaper texts may be governed by provider licenses even when derived metadata, scripts, and selected analytical outputs are present in this repository.
- The segmentation code is a rule-based research procedure designed for the formatting and OCR characteristics of the historical *Shenbao* corpus; its output should be validated before reuse with another corpus.
- The sociological extraction workflow includes data produced with an external large-language-model API. Credentials and raw API responses are not included.

For partial reproduction, the supplied CSV files can be used independently to inspect query terms, corpus metadata, derived tables, and selected analytical results without rerunning corpus retrieval.

## Outputs

The repository includes:

- chronological visualizations of shopkeeper mentions;
- collocation tables and graphs by historical period;
- named-entity and personâ€“organization network outputs;
- weighted centrality rankings and centrality profiles;
- structural topic-model inputs, tables, and figures;
- Cytoscape-compatible network files; and
- tables describing query construction and the comparative historiography of shopkeepers.

## Citation

If you use this repository, please cite the associated publication and the archived release of the data and code. Publication details and a persistent archive identifier should be added here when available.

Suggested interim citation:

> Christian Henriot. *Shopkeepers in Modern China: Data and Computational Analysis*. GitHub repository. https://github.com/ankeqiang/Shopkeepers

## License

Unless otherwise indicated, the R source code in this repository is licensed under the GNU General Public License v3.0 or later. Original documentation, figures, and research data created by the project are licensed under the Creative Commons Attribution 4.0 International License. These licenses do not apply to third-party newspaper texts, corpus content, or metadata subject to the terms of their respective providers.


