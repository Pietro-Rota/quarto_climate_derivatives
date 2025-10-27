# Weather Derivatives: Temperature Option Analysis

This repository contains a comprehensive analysis of temperature-based weather derivatives, with a focus on practical applications for climate risk management in finance and energy markets. The project is implemented in **R** and presented as a **Quarto document**, producing an interactive HTML report with detailed data exploration, modeling methodology, and results.

## Overview

Weather derivatives are financial instruments that help businesses hedge against climate-related risks, such as unusually hot summers or cold winters. This project focuses on **Heating Degree Day (HDD)** and **Cooling Degree Day (CDD)** options, using historical temperature data to simulate future scenarios and estimate derivative prices.

The main objectives are to:  
- Explore and visualize historical temperature data (NASA POWER Data Access Viewer (DAV)) to identify seasonal patterns and long-term trends.  
- Model temperature dynamics using mean-reverting stochastic processes, incorporating both deterministic and stochastic components.  
- Simulate future temperature paths via Monte Carlo methods.  
- Price temperature options and estimate sensitivities (Greeks) for practical risk management insights.  
- Provide an interactive, visually rich report to communicate findings to both technical and non-technical audiences.  

## Project Structure

- `quarto_climate_derivatives.qmd` – Main Quarto notebook generating the HTML report with full workflow, charts, and results.  
- `weather_derivatives.R` – Standalone R script for running the key analyses and simulations without Quarto.  
- `quarto_climate_derivatives.html` – Results from the Main Quarto notebook and key file for people interested, useful for a quick glance at what this report is about.  

## Key Features

- **Data Exploration**: Seasonal decomposition, trend analysis, and visual summaries of historical temperatures.  
- **Modeling**: Brief, intuitive explanation of stochastic modeling for temperature (mean-reverting processes, volatility modeling) without overwhelming non-specialists.  
- **Monte Carlo Simulations**: Generate forward-looking temperature scenarios for pricing and risk assessment.  
- **Option Pricing**: Compute expected payouts under risk-neutral assumptions.  
- **Greeks Analysis**: Sensitivity measures for temperature, volatility, seasonality, and time — demonstrating practical applications for hedging.  
- **Interactive Visualizations**: Charts, heatmaps, and ribbons to clearly communicate seasonal patterns, model performance, and pricing outputs.  

## Usage

The project follows a clear workflow, from data acquisition to interactive reporting:

1. **Download Climate Data**  
   Historical temperature data is obtained from the [NASA Prediction Of Worldwide Energy Resources (POWER) Data Access Viewer (DAV)](https://power.larc.nasa.gov/data-access-viewer/). This dataset, from the Agroclimatology community, provides high-resolution and reliable meteorological measurements, ensuring reproducibility for climate research.

2. **Data Preparation and Cleaning**  
   The raw dataset is processed to handle missing values and compute key temperature metrics, including daily maximum, minimum, and average temperatures. Seasonal periods are defined to distinguish between winter and summer trends.

3. **Exploratory Data Analysis**  
   A series of visualizations and statistical summaries are produced to explore seasonal patterns, long-term trends, and variability across years. This includes ribbon charts, seasonal decomposition, and volatility analysis.

4. **Modeling Temperature Dynamics**  
   The project uses a mean-reverting stochastic process to model temperature behavior, incorporating both deterministic seasonal trends and stochastic volatility components. Multiple modeling approaches are explored (Fourier, splines, GAMs) with a focus on interpretability and practical relevance.

5. **Monte Carlo Simulations**  
   Simulated temperature paths are generated to project potential future scenarios. These simulations form the basis for derivative pricing and risk assessment.

6. **Option Pricing and Sensitivities**  
   Heating and Cooling Degree Day options are priced under risk-neutral assumptions. Sensitivity measures (Greeks) are computed to illustrate how payouts respond to changes in temperature, volatility, seasonality, and time.

7. **Reporting and Visualization**  
   All results, visualizations, and analyses are compiled into an interactive **Quarto HTML report**. The report can be rendered directly from the Quarto document, or the standalone script `climate_derivatives_just_code.R` can be used to reproduce the key calculations and outputs.

