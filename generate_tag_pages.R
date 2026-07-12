library(sf)
library(dplyr)

detections <- readRDS("data/detections.rds")

tag_ids <-
  detections |>
  st_drop_geometry() |>
  distinct(tag_id) |>
  arrange(tag_id) |>
  pull(tag_id)

template <-
  readLines(
    "detections_by_tag.qmd",
    warn = FALSE
  )

for(tag in tag_ids) {
  
  page_text <-
    gsub(
      'selected_tag <- "013EFFF1"',
      paste0(
        'selected_tag <- "',
        tag,
        '"'
      ),
      template
    )
  
  writeLines(
    page_text,
    paste0(
      "detections_",
      tag,
      ".qmd"
    )
  )
  
}