# SCmonarch-dashboard
Visualization tool for Blu+ tracking data from radio-tagged monarchs. The data have already been cleaned and processed. One map show the full points that likely includes location errors. Another map has the starting and end points for detection episodes with minimal location error.

Online visualization tool: https://blakebarbaree.github.io/SCmonarch-dashboard/

Dashboard Architecture:
SCmonarch-dashboard/
│
├── data/
│   ├── boundary102.rds
│   ├── detections.rds
│   ├── episodes.rds
│   ├── episode_lines.rds
│   └── tag_summary.rds
│
├── scripts/
│   └── 01_build_dashboard_data.R
│
├── index.qmd
├── detections.qmd
├── episodes.qmd
├── styles.css
├── _quarto.yml
│
└── docs/
