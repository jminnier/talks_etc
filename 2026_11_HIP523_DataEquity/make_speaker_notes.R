# Build a one-page speaker notes handout from the deck's ::: {.notes} blocks.
# Run from this folder after any edit to the deck:
#   Rscript make_speaker_notes.R && quarto render speaker_notes.qmd

deck <- "2026_11_HIP523_dataequity.qmd"
lines <- readLines(deck)

# skip YAML and code chunks
body_start <- which(lines == "---")[2] + 1
lines <- lines[body_start:length(lines)]
in_chunk <- cumsum(grepl("^```", lines)) %% 2 == 1

# rough minutes per slide for a 50-minute slot (default 1 min)
minutes <- c(
  "Learning Objectives" = 1, "Where This Fits in HIP 523" = 1,
  "Example 1: Opioid Analgesia in the ED" = 3, "Example 2: Statin Prescribing" = 3,
  "Example 3: Readmission After Heart Failure" = 3,
  "Discussion: Post-attribute Bias" = 1.5, "Discussion: Omitted Variable Bias" = 1.5,
  "Discussion: Disparities vs. Mechanisms" = 1.5,
  "Confounder or Mediator?" = 1.5, "Modeling Decisions" = 1.5,
  "Discussion: Breakout Rooms (8 min)" = 9, "Thank you!" = 2, "References" = 0
)
section_minutes <- 0.5   # level-1 section title slides

slides <- list()
cur <- NULL
in_notes <- FALSE
for (i in seq_along(lines)) {
  l <- lines[i]
  if (in_chunk[i]) next
  if (grepl("^#{1,2} ", l)) {
    if (!is.null(cur)) slides[[length(slides) + 1]] <- cur
    title <- trimws(sub("\\{.*\\}", "", sub("^#{1,2} ", "", l)))
    cur <- list(title = title, level = nchar(sub(" .*", "", l)), notes = character())
  } else if (grepl("^:::\\s*\\{\\.notes\\}", l)) {
    in_notes <- TRUE
  } else if (in_notes && grepl("^:::\\s*$", l)) {
    in_notes <- FALSE
  } else if (in_notes && nzchar(trimws(l))) {
    cur$notes <- c(cur$notes, l)
  }
}
slides[[length(slides) + 1]] <- cur

mins <- vapply(slides, function(s) {
  if (s$title %in% names(minutes)) minutes[[s$title]]
  else if (s$level == 1) section_minutes else 1
}, numeric(1))
start <- c(0, head(cumsum(mins), -1))
mmss <- function(x) sprintf("%d:%02d", floor(x), round((x %% 1) * 60))

out <- c(
  "---",
  'title: "Speaker notes: Data Equity, HIP 523"',
  'subtitle: "November 11, 2026 · 50 minutes · virtual"',
  "format:",
  "  html:",
  "    embed-resources: true",
  "    toc: false",
  "    css: speaker_notes.css",
  "---",
  "",
  paste0("Generated from `", deck, "`. Times are rough start times; total about ",
         round(sum(mins)), " minutes."),
  ""
)
for (k in seq_along(slides)) {
  s <- slides[[k]]
  out <- c(out, paste0("### ", k + 1, ". ", s$title, " <span class='t'>", mmss(start[k]), "</span>"), "")
  out <- c(out, if (length(s$notes)) s$notes else "- (no notes)", "")
}
writeLines(out, "speaker_notes.qmd")
writeLines(c(
  "body { max-width: 860px; font-size: 15px; }",
  "h3 { font-size: 1.05rem; margin-top: 1.1rem; margin-bottom: .3rem; color: #0b3d5c; }",
  "h3 .t { float: right; color: #b8002e; font-weight: 600; }",
  "ul { margin-bottom: .2rem; }",
  "@media print { h3 { break-after: avoid; } li { break-inside: avoid; } }"
), "speaker_notes.css")
message("Wrote speaker_notes.qmd: ", length(slides), " slides, ~", round(sum(mins)), " min")
