# Restaurant Customer Satisfaction Analysis — R Shiny Dashboard

Professional multi-page dashboard for analyzing restaurant guest satisfaction, service operations, and customer recommendations.

## Features
- Executive KPIs: survey responses, average satisfaction, recommendation rate, average waiting time
- Satisfaction score distribution and service-type comparisons
- Food quality, service quality, ambience, and value-for-money analysis
- Waiting time and spend analysis
- Data-driven improvement suggestions based on available fields
- Searchable customer records with CSV export
- Demo data fallback so the dashboard can run before you add your own dataset

## Project structure
```
Restaurant_Customer_Satisfaction_Analysis/
├── app.R
├── data/
│   └── restaurant_customer_satisfaction.csv
├── www/
├── README.md
└── requirements.txt
```

## Run in VS Code
1. Install R for Windows from https://cran.r-project.org/
2. Extract the ZIP and open this folder in VS Code.
3. In R Interactive, install the required packages:
```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse",
  "ggplot2", "DT", "scales", "plotly"
), repos = "https://cloud.r-project.org")
```
4. Put your dataset in `data/restaurant_customer_satisfaction.csv`.
5. Run from the project folder:
```r
shiny::runApp()
```
The dashboard opens in your browser.

## Expected data
The dashboard supports common columns such as `OverallSatisfaction`, `SatisfactionScore`, `Rating`, `FoodQuality`, `ServiceQuality`, `AmbienceRating`, `WaitingTimeMinutes`, `SpendPerPerson`, `ServiceType`, `RecommendRestaurant`, and `ValueForMoney`. Missing optional fields are handled by hiding or messaging on the relevant charts.

If the dataset is not present, the app uses clearly labelled generated sample data. Replace it with your actual CSV for real analysis.
