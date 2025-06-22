# scripts/build_reference_pages.R

library(fs)
library(tools)
library(devtools)
library(roxygen2)
library(brio)
library(purrr)
library(refloraR)

pkg <- "/Users/domingoscardoso/Library/Mobile Documents/com~apple~CloudDocs/Publications_Bioinformatics/refloraR_new_R_package/refloraR"
ref_dir <- path("reference")
dir_create(ref_dir)
fname="reflora_download.R"
# Load functions from package
devtools::load_all(pkg)
exports <- getNamespaceExports("refloraR")

# Group by prefix (e.g., get_, filter_, etc.)
prefix <- function(name) sub("(_).*", "\\1", name)
grouped <- split(exports, sapply(exports, prefix))



# Helper to convert Rd to markdown sections
# Helper to convert Rd to markdown sections
extract_rd_markdown <- function(fname) {
  rd_db <- tools::Rd_db("refloraR")
  rd <- rd_db[[paste0(fname, ".Rd")]]
  if (is.null(rd)) return(NULL)

  usage <- capture.output(tools::Rd2txt(rd))

  tags <- list(
    description = tools:::.Rd_get_section(rd, "description"),
    details = tools:::.Rd_get_section(rd, "details"),
    value = tools:::.Rd_get_section(rd, "value"),
    examples = tools:::.Rd_get_section(rd, "examples"),
    arguments = tools:::.Rd_get_section(rd, "arguments")
  )

  args_md <- if (!is.null(tags$arguments)) {
    arg_text <- paste(tags$arguments, collapse = " ")
    pattern <- "\\\\item\\s*\\{([^}]*)\\}\\s*\\{([^}]*)\\}"
    arg_items <- regmatches(arg_text, gregexpr(pattern, arg_text, perl = TRUE))[[1]]

    arg_parsed <- vapply(arg_items, function(x) {
      name <- sub(".*?\\{([^}]*)\\}\\{.*", "\\1", x)
      desc <- sub(".*?\\{[^}]*\\}\\{(.*)\\}", "\\1", x)
      sprintf("| %s | %s |", trimws(name), trimws(desc))
    }, character(1))

    paste("**Arguments**\n\n| Argument | Description |\n|----------|-------------|", paste(arg_parsed, collapse = "\n"), sep = "\n")
  } else ""

  paste(
    if (!is.null(tags$description)) paste("**Description**\n", paste(tags$description, collapse = "\n"), "\n\n") else "",
    if (!is.null(tags$details)) paste("**Details**\n", paste(tags$details, collapse = "\n"), "\n\n") else "",
    if (nzchar(args_md)) paste(args_md, "\n\n") else "",
    if (!is.null(tags$value)) paste("**Value**\n", paste(tags$value, collapse = "\n"), "\n\n") else "",
    if (!is.null(tags$examples)) paste("**Examples**\n```r\n", paste(tags$examples, collapse = "\n"), "\n```\n") else ""
  )
}

# Generate .qmd file per function
make_qmd <- function(fun) {
  qmd_file <- path(ref_dir, paste0(fun, ".qmd"))
  help_md <- extract_rd_markdown(fun)

  if (is.null(help_md)) help_md <- "Documentation not available."

  doc_text <- paste(
    sprintf("---\ntitle: \"%s\"\ndescription: \"Documentation for `%s()` from the refloraR package.\"\ntoc: true\ntoc-depth: 3\n---\n", fun, fun),
    sprintf("\n```{r}\n#| eval: false\nrefloraR::%s()\n```\n\n", fun),
    help_md
  )

  write_file(doc_text, qmd_file)
}

# Write per-function files
walk(exports, make_qmd)

# Write index in styled format
index_lines <- c(
  "---",
  "title: \"Function Reference\"",
  "format:",
  "  html:",
  "    toc: true",
  "    toc-depth: 3",
  "---",
  "",
  "<style>",
  "  body {",
  "    background-image: url('/figures/reflora_bg.png');",
  "    background-repeat: repeat-y;",
  "    background-size: cover;",
  "    background-attachment: fixed;",
  "    background-position: top center;",
  "    position: relative;",
  "    z-index: 0;",
  "  }",
  "",
  "  .background-overlay {",
  "    position: fixed;",
  "    top: 0;",
  "    left: 0;",
  "    width: 100%;",
  "    height: 100%;",
  "    background-color: rgba(255, 255, 255, 0.8);",
  "    z-index: -1;",
  "  }",
  "</style>",
  ""
)

for (fun in exports) {
  index_lines <- c(index_lines,
                   sprintf("## %s", fun),
                   sprintf("- [%s](/reference/%s.qmd)", fun, fun),
                   "")
}

writeLines(index_lines, path(ref_dir, "index.qmd"))
