---
TEAM: Guardians of the Gradient
AUTHORS: Sam Lee, Everett Andrew
---

# CO2-FDA
A functional data analysis (FDA) project analyzing carbon emissions over time by country.

# DATA

#### Total Yearly Per-Capita $CO_2$ Emissions within Country [CO2.xlsx](data/energy/emissions/CO2.xlsx)

This data set was downloaded from the Emissions Database for Global Atmospheric Research (EDGAR). Data comes from this report from the European Commission at [https://edgar.jrc.ec.europa.eu/](https://edgar.jrc.ec.europa.eu/).

#### Macroeconomic Indicators

Link to World Population dataset: https://data.worldbank.org/indicator/SP.POP.TOTL

#### Emissions Data [emissions.csv](data/clean/emissions.csv)

This dataset contains annual macroeconomic and governance indicators for various countries from 1990 to 2023. It integrates data on carbon emissions, GDP per capita, Human Development Index (HDI), inflation, (real) interest rates, unemployment, renewable energy share, and institutional stability indicators derived from the Worldwide Governance Indicators (WGI).

#### **Variables**
- **CC**: ISO 3166-1 alpha-3 country code.
- **Country**: Country name.
- **Year**: Year of observation (1990–2023).
- **CO2**: Per capita carbon dioxide (CO₂) emissions (Metric tons per capita).
- **GDPPC**: Gross Domestic Product per capita (current US dollars).
- **Renewable_Share**: Percentage of total energy consumption in a country derived from renewable sources.
- **HDI**: Human Development Index (0–1 scale), measuring health, education, and living standards.
- **Inflation**: Annual percentage change in GDP (GDP Deflator).
- **Interest**: Short-term (real) lending interest rate.
- **UE**: Unemployment rate (percentage of total population).

#### **Institutional Stability Indicators (WGI)**
- **Corruption**: Control of Corruption, capturing perceptions of corruption within society and government.
- **Government**: Government Effectiveness, assessing public service quality and policy implementation.
- **Stability**: Political Stability and Absence of Violence/Terrorism, reflecting political stability and security.
- **Law**: Rule of Law, measuring the enforcement of laws, property rights, and contract reliability.
- **Regulation**: Regulatory Quality, assessing the ability to formulate and enforce sound policies.
- **Voice**: Voice and Accountability, reflecting citizens' ability to participate in governance and freedom of expression.

#### **Data Characteristics**
- **Timeframe**: 1990–2023.
- **Geographic Scope**: Multiple countries globally.
- **Missing Values**: Several variables contain missing observations, particularly institutional indicators and interest rates.
- **Variable Scales**:
  - WGI indicators range from approximately -2.5 to +2.5, where higher values indicate better governance outcomes.
  - Economic indicators such as CO2, GDPPC, and Inflation are reported in their natural units.
 
## Scripts

### `scripts/clean.R`
This script loads, merges, and cleans the raw carbon emissions and covariate data from various sources (e.g., EDGAR, World Bank). It performs the following steps:
- Sources individual cleaning scripts for each dataset (`clean-carbon.R`, `clean-gdp.R`, etc.)
- Joins all datasets into a unified panel by country (`CC`) and year
- Constructs functional data matrices for each variable over the specified time ranges
- Restricts the dataset to countries with complete data across all series
- Saves a cleaned emissions object as `emissions.rds` in `data/clean/`

### `scripts/pffr.R`
This script performs the core modeling and visualization tasks:
- Loads the cleaned data (`emissions.rds`) and defines time grids for modeling
- Applies mean imputation for missing values in select covariates
- Fits both a default function-on-function model and a historical functional linear model using the `pffr()` function from the `refund` package
- Extracts fitted values and plots country-specific trajectories
- Computes time-varying $R^2(t)$ statistics to compare model performance
- Extracts and visualizes the estimated coefficient surface for renewable energy using a bivariate color scale

All visualizations and fitted models are saved or rendered for inclusion in the final report.

