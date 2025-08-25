# The Stigma Effect: County-Level SNAP Participation After High-Profile Fraud Events

This project uses a **Difference-in-Difference** event study to measure how high-profile SNAP fraud news stories impact program participation and issuance at the county level. The central hypothesis is that such events increase local stigma, affecting enrollment trends. The analysis covers five states: Alabama, Arizona, New Jersey, New York, and Virginia.

---

## 📂 Project Structure
- `Raw_Data/` – Source data (State SNAP reports, Census Population, Economic Controls, Fraud Story PDFs)
- `Scripts/` – R scripts for each state, organized into subfolders (e.g., `Alabama/`, `Arizona/`)
- `Data_Outputs/` – Cleaned, compiled, and population-weighted county-level datasets
- `Plots/` – Preliminary visualizations, including USDA data comparisons and raw trend comparisons
- `Event_Study_Plots/` – Final event study graphs showing the differenced trends post-event
- `Docs/` – Project write-ups, data dictionaries, and methodology notes

---

## ⚙️ Methodology & Workflow
The analysis leverages the exogenous shock of a prominent SNAP fraud news story to estimate its causal impact on program stigma and participation.

1.  **Data Collection**: County-level, year-month SNAP data is collected from individual state agency websites. Economic control data (BEA, BLS, ACS) is also gathered.
2.  **Event Coding**: For each state, a single high-profile fraud event is identified from news sources, establishing the "treatment" date and county.
3.  **Treatment & Control Groups**: Counties are grouped for comparison. The primary "treatment" group is the county where the event occurred, while "control" groups include surrounding counties or the rest of the state.
4.  **Data Cleaning & Weighting**: R scripts clean and merge the datasets. All county-level metrics are weighted by annual population estimates from the U.S. Census Bureau to ensure accurate comparisons.
5.  **Difference-in-Difference Analysis**: An event study is conducted to compare the change in trends between the treatment and control groups before and after the event date.

---

## 🚀 Running the Project
The analysis is self-contained within each state's script folder.

- Navigate to a state's folder in the `Scripts/` directory (e.g., `Scripts/Virginia/`).
- Run the R scripts in sequence to perform data cleaning, weighting, and the final event study analysis.
- **Key Outputs**:
    - Final datasets are saved in `Data_Outputs/`.
    - Final event study visualizations are saved in `Event_Study_Plots/`.

---

## 📊 Key Data Sources
- **SNAP Participation Data**: State-level websites for Alabama, Arizona, New Jersey, New York, and Virginia.
- **Population Data**: U.S. Census Bureau Annual County Population Estimates.
- **Fraud Events**: Sourced from local and national news reports.
- **Economic Controls**:
    - **BEA**: GDP and Personal Income by county.
    - **BLS**: Local Area Unemployment Statistics (LAUS) and Quarterly Census of Employment and Wages (QCEW).
    - **Census**: SAIPE (Poverty/Income Estimates) and ACS (Health Insurance Estimates).

---

## ✅ Quick Check
After running the scripts for a state:
- A final, population-weighted `.dta` file should be in `Data_Outputs/`.
- The `Event_Study_Plots/` folder should contain the final visualization of the differenced trends.
