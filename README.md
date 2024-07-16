# PostEddyPro

[![R](https://img.shields.io/badge/Made%20with-R-1f425f.svg)](https://www.r-project.org/)

**PostEddyPro** is an R package developed to ease and streamline eddy covariance flux data processing following raw flux calculation in EddyPro. The package includes various functions for quality control, gap filling with machine learning, unit conversions, and Monte Carlo simulations.

## Features

- **Quality Control**: Comprehensive tools to ensure data integrity.
- **Gap Filling**: Utilizes machine learning algorithms like XGBoost and Random Forest for accurate gap filling.
- **Unit Conversions**: Simplifies the conversion process between different units.
- **Monte Carlo Simulation**: Enables robust statistical analysis.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("bravemaster3/PostEddyPro")
```

For more detailed usage, you can refer to the [PostEddyPro_Temp_Vignette](https://github.com/bravemaster3/PostEddyPro_Temp_Vignette).

## Contributions
Contributions are welcome! Feel free to fork the repository and submit pull requests. For major changes, please open an issue first to discuss what you would like to change.

License
This project is licensed under the MIT License.
