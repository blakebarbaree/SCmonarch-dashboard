# ============================================================
# SC MONARCH DASHBOARD
# BUILD DASHBOARD DATA
# ============================================================

library(data.table)
library(sf)
library(dplyr)

# ============================================================
# LOAD SOURCE DATA
# ============================================================

load(
  "data/dashboard_source_data.RData"
)

# ============================================================
# OUTPUT DIRECTORY
# ============================================================

dir.create(
  "data",
  showWarnings = FALSE
)

# ============================================================
# SITE BOUNDARIES
# ============================================================

boundary102$SiteStatus <-
  factor(
    boundary102$SiteStatus,
    levels = c(
      "Active",
      "Other"
    )
  )

saveRDS(
  boundary102,
  "data/boundary102.rds"
)

# ============================================================
# DETECTIONS
# ============================================================

detections <-
  copy(
    pts_sites102_final
  )

# ============================================================
# COORDINATE OFFSET CORRECTION
# ============================================================
# Detection and episode coordinates are in a projected coordinate
# system that aligns with UTM Zone 10N after applying a consistent
# offset. Offsets are estimated by comparing detection site means
# to boundary site centroids.

det_site_means <-
  detections[
    !is.na(SiteName),
    .(
      det_x = mean(x1_, na.rm = TRUE),
      det_y = mean(y1_, na.rm = TRUE),
      n_pts = .N
    ),
    by = SiteName
  ]

boundary_centroids_utm <-
  boundary102[
    boundary102$SiteName %in% det_site_means$SiteName,
  ] |>
  st_transform(26910) |>
  st_centroid()

boundary_centroids_dt <-
  as.data.table(
    st_drop_geometry(boundary_centroids_utm)
  )

boundary_coords <-
  as.data.table(
    st_coordinates(boundary_centroids_utm)
  )

boundary_centroids_dt[
  ,
  `:=`(
    boundary_x = boundary_coords$X,
    boundary_y = boundary_coords$Y
  )
]

coord_compare <-
  merge(
    det_site_means,
    boundary_centroids_dt[
      ,
      .(
        SiteName,
        boundary_x,
        boundary_y
      )
    ],
    by = "SiteName",
    all = FALSE
  )

coord_compare[
  ,
  `:=`(
    diff_x = boundary_x - det_x,
    diff_y = boundary_y - det_y
  )
]

x_offset <-
  median(
    coord_compare$diff_x,
    na.rm = TRUE
  )

y_offset <-
  median(
    coord_compare$diff_y,
    na.rm = TRUE
  )

cat(
  "\nCoordinate offsets applied:\n",
  "x_offset = ", round(x_offset, 2), "\n",
  "y_offset = ", round(y_offset, 2), "\n"
)

# ============================================================
# TAG SUMMARY
# ============================================================

# Detection metrics

tag_summary <-
  
  detections[
    ,
    .(
      first_detection =
        min(
          obs_date,
          na.rm = TRUE
        ),
      
      last_detection =
        max(
          obs_date,
          na.rm = TRUE
        ),
      
      n_detections = .N
    ),
    by = tag_id
  ]

# Episode metrics

episode_summary <-
  
  episodes_clean_final[
    ,
    .(
      n_visits =
        sum(
          tolower(episode_type) == "visit",
          na.rm = TRUE
        ),
      
      n_forays =
        sum(
          tolower(episode_type) == "foray",
          na.rm = TRUE
        ),
      
      n_episodes = .N,
      
      mean_episode_duration_days =
        round(
          mean(
            duration_min,
            na.rm = TRUE
          ) / 1440,
          1
        )
    ),
    by = tag_id
  ]

tag_summary <-
  
  merge(
    tag_summary,
    episode_summary,
    by = "tag_id",
    all.x = TRUE
  )

# Tracking period

tag_summary[
  ,
  tracking_period :=
    paste(
      format(
        first_detection,
        "%d-%b-%Y"
      ),
      "to",
      format(
        last_detection,
        "%d-%b-%Y"
      )
    )
]

# Behavior summary

behavior_summary <-
  
  detections[
    ,
    .(
      behaviors =
        paste(
          sort(
            unique(
              behavior_type
            )
          ),
          collapse = ", "
        )
    ),
    by = tag_id
  ]

# Primary site

primary_site <-
  
  detections[
    ,
    .N,
    by = .(
      tag_id,
      SiteName
    )
  ][
    order(
      tag_id,
      -N
    )
  ][
    ,
    .SD[1],
    by = tag_id
  ][
    ,
    .(
      tag_id,
      primary_site =
        SiteName
    )
  ]

# Merge remaining summaries

tag_summary <-
  
  Reduce(
    function(x, y)
      merge(
        x,
        y,
        by = "tag_id",
        all = TRUE
      ),
    
    list(
      tag_summary,
      behavior_summary,
      primary_site
    )
  )

# ============================================================
# DETECTIONS SF
# ============================================================

detections[
  ,
  `:=`(
    x1_map = x1_ + x_offset,
    y1_map = y1_ + y_offset
  )
]

detections_sf <-
  
  st_as_sf(
    detections,
    coords = c(
      "x1_map",
      "y1_map"
    ),
    crs = 26910,
    remove = FALSE
  ) |>
  st_transform(3857)

detections_sf <-
  
  merge(
    detections_sf,
    tag_summary,
    by = "tag_id",
    all.x = TRUE
  )

saveRDS(
  detections_sf,
  "data/detections.rds"
)

# ============================================================
# EPISODE START POINTS
# ============================================================

episodes_clean_final[
  ,
  `:=`(
    start_x_map = start_x + x_offset,
    start_y_map = start_y + y_offset,
    end_x_map = end_x + x_offset,
    end_y_map = end_y + y_offset
  )
]

episodes_sf <-
  
  st_as_sf(
    episodes_clean_final,
    coords = c(
      "start_x_map",
      "start_y_map"
    ),
    crs = 26910,
    remove = FALSE
  ) |>
  st_transform(3857)

episodes_sf <-
  
  merge(
    episodes_sf,
    tag_summary,
    by = "tag_id",
    all.x = TRUE
  )

saveRDS(
  episodes_sf,
  "data/episodes.rds"
)

# ============================================================
# EPISODE MOVEMENT LINES
# ============================================================

episode_lines <-
  
  lapply(
    seq_len(
      nrow(
        episodes_clean_final
      )
    ),
    
    function(i){
      
      row <-
        episodes_clean_final[i]
      
      if(
        any(
          is.na(
            c(
              row$start_x_map,
              row$start_y_map,
              row$end_x_map,
              row$end_y_map            
              )
          )
        )
      ){
        return(NULL)
      }
      
      st_linestring(
        matrix(
          c(
            row$start_x,
            row$start_y,
            row$end_x,
            row$end_y
          ),
          byrow = TRUE,
          ncol = 2
        )
      )
    }
  )

keep <-
  !sapply(
    episode_lines,
    is.null
  )

episode_lines_sf <-
  
  st_sf(
    episodes_clean_final[
      keep
    ],
    
    geometry =
      st_sfc(
        episode_lines[
          keep
        ],
        crs = 26910
      )
  )

episode_lines_sf <-
  
  merge(
    episode_lines_sf,
    tag_summary,
    by = "tag_id",
    all.x = TRUE
  )

episode_lines_sf <-
  st_transform(
    episode_lines_sf,
    3857
  )

saveRDS(
  episode_lines_sf,
  "data/episode_lines.rds"
)

# ============================================================
# DASHBOARD SUMMARY
# ============================================================

dashboard_summary <-
  
  episodes_clean_final[
    ,
    .(
      n_tags =
        uniqueN(tag_id),
      
      n_episodes = .N,
      
      n_visits =
        sum(
          tolower(episode_type) == "visit",
          na.rm = TRUE
        ),
      
      n_forays =
        sum(
          tolower(episode_type) == "foray",
          na.rm = TRUE
        )
    )
  ]

dashboard_summary[
  ,
  total_points :=
    nrow(detections)
]

dashboard_summary[
  ,
  first_detection :=
    min(
      detections$obs_date,
      na.rm = TRUE
    )
]

dashboard_summary[
  ,
  last_detection :=
    max(
      detections$obs_date,
      na.rm = TRUE
    )
]

saveRDS(
  dashboard_summary,
  "data/dashboard_summary.rds"
)

# ============================================================
# SAVE TAG SUMMARY
# ============================================================

saveRDS(
  tag_summary,
  "data/tag_summary.rds"
)

# ============================================================
# REPORT
# ============================================================

cat(
  "\nDashboard data successfully created.\n\n"
)

cat(
  "Files written:\n"
)

print(
  list.files(
    "data"
  )
)

cat(
  "\nNumber of tags:",
  nrow(tag_summary),
  "\n"
)

cat(
  "\nSample tag summary:\n"
)

print(
  tag_summary[
    1:5
  ]
)