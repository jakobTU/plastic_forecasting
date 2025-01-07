# Forecasting Plastic Waste Fractions: A Recycling Perspective

This repository supplements the code to the paper *Forecasting Plastic Waste Fractions: A Recycling Perspective* available at TBA.

### Structure

- `forecasting.R`: Contains the `R` code the paper is based on.
- `create_data_set.R`: Creates an exemplary data set as it is not possible to provide the original data set due to data protection concerns. The data set only comprises the endogenous variables as well as the data of the input scale.
- `input_output_data.csv`: Contains the data set generated by `create_data_set.R`.
- `exogenous.csv`: Stores the exogenous variables also needed to execute the `forecasting.R` file.
- `boxplot.R`: Takes the output of the `forecasting.R` file as input and generates the boxplot in *Figure 3* in the paper.
- `boxplot.pdf`: Is the output of the `boxplot.R` file.
