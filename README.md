# spatial_airbnb

Main program to replicate results: **spatial_airbnb_program**.

We work with database obtained from www.insideairbnb.com, a website on which web scraped datasets of “snapshots” of cities are published. We decided to work with the files of Barcelona of the situation on 2022.

Code structure:

1. Data exploration of **Airbnb** dataset
2. Pre-processing: data extraction using NLP, cleaning and imputation.
3. Preparing maps with spatial data
4. Spatial interpolation methods: predictions of price per square meter in a continuous space within Barcelona boundaries.
5. Cross validation analysis
