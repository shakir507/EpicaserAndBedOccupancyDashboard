# RAPIDE EpiCASER and Bed Occupancy Dashboard

An R Shiny dashboard for running epidemiological simulations,
generating synthetic cases, and exploring hospital arrivals
and patient pathways using the EpiCASER engine. 
The patient pathway is then used to run bed occupancy prediction simulations using python code.

## Requirements

- R installed on your computer.
- Bed occupancy python package installed (github repo available at https://github.com/VRPloeg/bed-occupancy-prediction-eph2026)
- RStudio is optional.

## Installation

Run the following in your R console:

```r
# Install the installer if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install the dashboard and its required R packages
remotes::install_github(
  "shakir507/EpicaserAndBedOccupancyDashboard",
  dependencies = NA,
  upgrade = "never"
)
```

Required R dependencies are declared in DESCRIPTION.
EpiCASER is obtained from JimDuggan/epicaser through the
Remotes field.

If this is a private repository, you need repository access
and GitHub authentication.

## Launch the dashboard

```r
EpicaserAndBedOccupancyDashboard::run_app()
```

The app runs on your computer and opens in a browser or
RStudio viewer. Keep the R session running while using it.

To open it explicitly in your browser:

```r
EpicaserAndBedOccupancyDashboard::run_app(launch.browser = TRUE)
```

To stop the app, press Esc in RStudio or Ctrl+C in a terminal.

## Using the dashboard

1. Choose a starting point:
   - Run the model and generate stochastic cases.
   - Upload deterministic model output.
   - Upload existing stochastic cases.
2. Adjust the available parameters.
3. Select whether to generate hospital arrivals and pathways.
4. Click Run pipeline.
5. Inspect the result tabs.
6. Go to Twente:Bed Occupancy tab to run the short term models.

Changing inputs does not rerun the model until you click
Run pipeline. When multiple runs are generated, use
View completed run to select the run to inspect.

## CSV upload formats

Column names are case-sensitive.

### Deterministic output

Use the complete output exported from
generate_epi_cases_deterministic(). Required columns are:

- Day: unique numeric simulation times.
- Cases: non-negative numeric values; decimals are allowed.

A Date column is not required.

Example:

```r
cases_d <- epicaser::generate_epi_cases_deterministic(
  N = 100000,
  I0 = 10,
  RF = 0.3
)

write.csv(
  cases_d,
  "deterministic_cases.csv",
  row.names = FALSE
)
```

This upload skips the deterministic model run and continues
with stochastic case generation.

### Stochastic cases

Required columns are:

- Date: dates in YYYY-MM-DD format, with one row per date.
- Cases: non-negative integer daily case counts.

Cases must represent daily incidence, not cumulative totals.

Example:

```csv
Date,Cases
2026-01-01,10
2026-11-02,15
2026-11-03,22
```

This upload skips both deterministic model execution and
stochastic case generation. It continues with age-cohort
and subsequent data generation.

## Summary metrics

The summary cards describe the selected run:

- Synthetic cases: sum of daily reported case counts.
- Peak daily cases: maximum daily reported case count.
- Hospital arrivals: total number of generated admission records.

Hospital arrivals represent cumulative admissions, not
simultaneous hospital bed occupancy.

## Updating the dashboard

Stop the running app, then reinstall:

```r
remotes::install_github(
  "shakir507/EpicaserAndBedOccupancyDashboard",
  dependencies = NA,
  upgrade = "never",
  force = TRUE
)
```

Restart R after installation, then launch the app again.

## Troubleshooting

### Installation fails

Check the first error in the installation output.
Confirm that you have internet access and permission to
write to your R package library.

Some dependencies may require system libraries or build
tools if a suitable binary package is unavailable.

### An EpiCASER function is not exported

Restart R and confirm that the installed EpiCASER version
contains and exports the function required by the dashboard.

### The app cannot find its files

Check:

```r
system.file(
  "shiny",
  "app_GalwayTwenteJoint.R",
  package = "EpicaserAndBedOccupancyDashboard"
)
```

This should return a path to the installed app.R file.

## Development

Open EpicaserAndBedOccupancyDashboard.Rproj in RStudio.

The dashboard source is in inst/shiny/app_GalwayTwenteJoint.R.
Static assets are in inst/shiny/www.
The package launcher is in R/run_app.R.

To test the source dashboard:

```r
shiny::runApp("inst/shiny")
```

To install the package from its source folder:

```r
remotes::install_local(
  ".",
  dependencies = NA,
  upgrade = "never"
)
```

## Example patient pathway output

Each row represents a stage in a patient's hospital pathway.
The same admission identifier can therefore appear in multiple rows.
Duration is measured in days.

| admission | Origin | Destination | startdatetime | enddatetime | ICU admittance | Duration |
| --------: | :----- | :---------- | :------------ | :---------- | :------------- | -------: |
| 14 | Other hospital | ICU | 2023-01-14 10:49:33 | 2023-01-17 09:49:33 | No | 2.96 |
| 14 | Ward | Home | 2023-01-17 09:49:33 | 2023-02-12 12:49:33 | Yes | 26.12 |
| 50 | Home | Home | 2023-01-28 19:32:20 | 2023-02-07 17:32:20 | No | 9.92 |
| 57 | Home | Home | 2023-01-29 17:49:43 | 2023-02-04 06:49:43 | No | 5.54 |

Time-stamps are displayed in UTC using YYYY-MM-DD HH:MM:SS.
ICU admittance indicates whether the current pathway stage is an
ICU stay; Destination identifies where the patient goes next.
## Short-Term Bed Occupancy Model

Patient pathways generated by EpiCASER feed into the University of Twente’s short-term bed occupancy model.

The **Twente: Bed Occupancy** tab allows users to select prediction settings, run the Python model, and 
view Ward and ICU length-of-stay distributions, historical bed occupancy, and occupancy forecasts with 
confidence intervals.

## Author

Shakir Bilal

## Licence

Copyright © 2026 RAPIDE Project. All rights reserved.

Permission to use, copy, modify, reproduce, or redistribute this
software must be obtained in writing from the copyright holder.

For permission requests, contact Jim Duggan, University of Galway, Ireland.
