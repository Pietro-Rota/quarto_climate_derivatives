# Weather Derivatives: Temperature Option Analysis

This repository contains a comprehensive analysis of temperature-based weather derivatives, with a focus on practical applications for climate risk management in finance and energy markets. The project is implemented in **R** and presented as a **Quarto document**, producing an interactive HTML report with detailed data exploration, modeling methodology, and results.

## Overview

Weather derivatives are financial instruments that help businesses hedge against climate-related risks, such as unusually hot summers or cold winters. This project focuses on **Heating Degree Day (HDD)** and **Cooling Degree Day (CDD)** options, using historical temperature data to simulate future scenarios and estimate derivative prices.

The main objectives are to:  
- Explore and visualize historical temperature data (NASA POWER dataset) to identify seasonal patterns and long-term trends.  
- Model temperature dynamics using mean-reverting stochastic processes, incorporating both deterministic and stochastic components.  
- Simulate future temperature paths via **Monte Carlo methods**.  
- Price temperature options and estimate sensitivities (Greeks) for practical risk management insights.  
- Provide an interactive, visually rich report to communicate findings to both technical and non-technical audiences.  

## Project Structure

- `quarto_weather_analysis.qmd` – Main Quarto notebook generating the HTML report with full workflow, charts, and results.  
- `weather_derivatives.R` – Standalone R script for running the key analyses and simulations without Quarto.  
- `README.md` – This overview and instructions for usage.  
- `figures/` – Folder containing generated charts and visualizations from the analysis.  

## Key Features

- **Data Exploration**: Seasonal decomposition, trend analysis, and visual summaries of historical temperatures.  
- **Modeling**: Brief, intuitive explanation of stochastic modeling for temperature (mean-reverting processes, volatility modeling) without overwhelming non-specialists.  
- **Monte Carlo Simulations**: Generate forward-looking temperature scenarios for pricing and risk assessment.  
- **Option Pricing**: Compute expected payouts under risk-neutral assumptions.  
- **Greeks Analysis**: Sensitivity measures for temperature, volatility, seasonality, and time — demonstrating practical applications for hedging.  
- **Interactive Visualizations**: Charts, heatmaps, and ribbons to clearly communicate seasonal patterns, model performance, and pricing outputs.  

## Usage
Open
To reproduce the HTML report download the `.qmd` file and render it 
