# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Jessica Minnier's archive of talks and slides (2016 onward). It is not a software project: there is no build system, test suite, or top-level `_quarto.yml`. Each talk lives in its own self-contained folder named `YYYY_MM_<Venue>_<Topic>` (e.g. `2026_08_JSM_AI_BC`). The source file and the rendered HTML are both committed. The rendered decks are served from `https://jminnier-talks.netlify.app/<folder>/<file>.html`, and `README.md` holds the index table that links to them.

## Generations of slide tooling

- **2016–2021: R Markdown + xaringan** (`*.Rmd`, `css/xaringan-themer.css`, `css/my-theme.css`, shared `libs/` at the root). `xaringen_template/` is the starter deck for that era. Render these with `rmarkdown::render()` in R.
- **2023–2025: Quarto revealjs** with a custom SCSS theme (`css/sky_modified_smaller_font.scss`), `freeze: auto`, chalkboard, and sometimes the `coatless/webr` extension. Each talk vendors its own copy of `_extensions/`.
- **2026 on: Quarto `clean-revealjs`** (the `grantmcdermott/clean` extension, vendored per talk under `_extensions/`). Settings: `self-contained: true`, 1280×720, `from: markdown-smart`, a `logo:` and `footer:`, and small CSS helper classes (`.small`, `.vsmall`, `.alert`) declared inline in `include-in-header`. When you start a new 2026-style deck, copy the YAML header from `2026_08_cdssr_jsm_recap/jsm_2026_recap.qmd` or `2026_08_JSM_AI_BC/JSM_VA_2026_08.qmd`.
- Some talks were built elsewhere (Google Slides, PowerPoint, another repo), so only the `.html`, `.pdf`, or `.pptx` is here, e.g. `2026_06_WNAR_*`, `2025_11_AI_Summit_OHSU`, `2026_svo_hs.html`.

## Commands

```bash
# Render a Quarto deck (run from inside the talk folder so _extensions/ resolves)
cd 2026_08_JSM_AI_BC && quarto render JSM_VA_2026_08.qmd

# Render an older xaringan deck
Rscript -e 'rmarkdown::render("2020_01_Knight_DiagnosticTests/2020_01_Knight_Diagnostic.Rmd")'

# HTML -> PDF: decktape for xaringan (see decktape_convert_to_pdf.R); for revealjs use the generic plugin or ?print-pdf in Chrome
decktape remark <deck>.html <deck>.pdf

# Visually check layout (the browser tool rejects file:// URLs)
cd <talk dir> && python3 -m http.server 8777 --bind 127.0.0.1
# open http://127.0.0.1:8777/<deck>.html?v=N#/<slide-slug>; bump ?v= on every reload, because Chrome caches the old render
```

## Conventions

- **No incremental bullets.** Write plain `-` lists and remove any `::: {.incremental}` wrappers. After rendering, `grep -c 'class="fragment"' <deck>.html` should print 0.
- **Keep `from: markdown-smart`** in clean-revealjs decks. Without it, Pandoc inserts U+00A0 after "vs." or "et al.", and some PDF viewers draw that as a missing-glyph box.
- **Jessica edits the `.qmd` and drops new files into `images/` while you work.** Diff against her current file before writing back, and never overwrite her working copy wholesale. Re-list `images/` before you edit.
- Screenshot originals named `* - 1.png` are gitignored and stay local. Slides reference the exported `.jpg`.
- Commit the rendered `.html` together with the source, because Netlify serves the committed HTML. When you add a talk, add a row to the `README.md` table as well, in date order.
- One copy of each rendered deck per talk folder. Don't keep duplicate HTML across folders.

## Decisions log

Record lasting decisions here (newest first), with the date and the reason.

- 2026-11-10: HIP 523 2026 data-equity talk restyled to clean-revealjs. Its Shiny app was rewritten as `2026_11_HIP523_DataEquity/equity_app_2026/` and deploys under a new shinyapps.io name, `equity_regression_2026`, so the 2025 deck's app link keeps working. Speaker notes are written as bullets in the deck and pulled into a handout by `make_speaker_notes.R`.
- 2026-11-10: Brought the README talk table up to date (2019–2026), including the SOM sample-size reruns in `2019_03_SampleSizeAACR/`. Talks without source in the repo are marked "rendered only".
- 2026-11-10: Created this CLAUDE.md. Repo conventions belong here. Collaboration preferences belong in Claude's auto-memory.
- 2026-08-12: Removed the duplicate `WNAR_VA_2026.html` from `2026_08_JSM_AI_BC/`. The canonical copy is in `2026_06_WNAR_AI_BC/`.
- 2026-08-12: Added `.claude/` and `* - [0-9].png` screenshot originals to `.gitignore`.
- 2026-08-12: 2026 decks use `clean-revealjs`, self-contained, 1280×720, with `from: markdown-smart`.
- 2026-04-09: Slide links in README moved to the `jminnier-talks.netlify.app` domain.
