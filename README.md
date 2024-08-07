
![Logo](https://raw.githubusercontent.com/Juns26/MSO-2D/main/images/home_page.png)


# Project Management

This project provides project managers a scheduling tool to help them plan out activities effectively. The tool includes a Shiny application for dynamic UI elements to modify project for crashing and/or resource leveling.
## Demo

Link to demo: https://js26.shinyapps.io/new_app/

**Home tab**

The home tab consist of 2 sections, instructions and sample data for download.

- Instruction on how to navigate the interface and description of parameters used.
![Logo](https://raw.githubusercontent.com/Juns26/MSO-2D/main/images/home_page_instruction.png)

- Sample data download section to tryout the interface.
![Logo](https://raw.githubusercontent.com/Juns26/MSO-2D/main/images/home_page_download.png)


**Data tab** 

- Attach files or manually input data, with preview section.
![Logo](https://raw.githubusercontent.com/Juns26/MSO-2D/main/images/data_page_input.png)

**Dashboard tab**

- After correct input of data, view the project schedule, as well as other charts for costs and resource usage.
![Logo](https://raw.githubusercontent.com/Juns26/MSO-2D/main/images/dashboard_page.png)

## Files
1. construction.R : Code that contains the backend calculations
2. app.R : Code that contains the frontend interface
3. csv files : Available data to download to test out interface
4. www : Code containing css stylings and csv files for interface download

## Requirements to run locally

#### 1. Clone the project
```bash
  git clone https://github.com/Juns26/MSO-2D
```

#### 2. Ensure you have [R](https://cran.rstudio.com/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed.


#### 3. Install the following packages in Rstudio using the following command:
```bash
install.packages(c('dplyr',ds4Dash','plotly','readr','DT','shiny','shinyWidgets','fresh','igraph','networkD3', 'tidyr','htmlwidgets'))
```
#### 3. Run the file app.R
