# Fundamental Analysis for Value Investors

This repository contains R code to perform fundamental analysis for value investors. It leverages the `dplyr` and `tidyverse` libraries along with `fmpcloudr` to retrieve and analyze financial data from Financial Modeling Prep.

## Features

- **Analysis**: Analyze key financials of a company.
- **Visualization**: Chart the cash ratio to visualize cash usage and sources.
- **Benchmarking**: Compare a company’s financial metrics with selected competitors.

## Requirements

- R (version >= 4.0.0)
- RStudio (optional, but recommended)
- Packages: `dplyr`, `tidyverse`, `fmpcloudr`, `ggplot2`, `shiny` (for interactive visualizations)

## Installation

1. Clone the repository:
    ```bash
    git clone https://github.com/yourusername/fundamental_analysis.git
    cd fundamental_analysis
    ```

2. Run the script on run_analysis.R:
    

3. Set up your API Token on : https://www.financialmodelingprep.com

## Usage

1. **Run the Analysis**:
    ```R
    source('scripts/main.R')
    ```

2. **Select the Company and Competitors**:
    Modify the `run_analysis.R` script to include the ticker of the company and its competitors.

## Project Structure

- `data/`: Contains raw and processed data files.
- `scripts/`: Contains the main script to run the analysis (run_analysis.R) and for data retrieval, ratio analysis, an.
- `reports/`: Contains example reports and R Markdown files.
- `tests/`: Contains test scripts for the functions.

## Contributing

Contributions are welcome! Please create a pull request with a detailed description of your changes.

## License

This project is licensed under the MIT License.
