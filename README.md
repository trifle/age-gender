# Age and gender on TV: Analysis
This repository contains analysis code for the paper {TBD}.
It is the second part of the replication package: The first repository (`face`) contains the python-based code for extracting faces from video files and classifying age and gender.

## Prerequisites
The datasets used here are fairly large (with approximately 50 million lines). Our data preparation code is mostly optimized for legibility instead of efficiency. As a result, more than 32GB of memory (ram) are needed to run the entire pipeline.

## Non-public data
Our analysis contains propietary content analysis coding from ALM (`ALM-2012-2018.sav`), which we cannot publish in full. This omission unfortunately renders a complete replication impossible. We have, however, retained the entire code used to prepare, transform, merge and analyze all datasets, so that these steps may be scrutinized.

The two final aggregated datasets used in the analysis are included in this repository: `data/agm_program-aggregates.tsv.gz` contains per-show aggregates of age, gender share, gender counts and counts per age bracket, along with the station, date, ALM genre classification and a cryptographic hash of the title.
The second file, `data/hourly_age_gender.tsv.gz`, contains per-hour aggregates of age and gender plus date and station; these are used to present a broad overview of variation in the data.

## Reproducibility
Changing versions of R packages can lead to differences between published and repeated results. We therefore include a snapshot of all package versions from the R library `packrat`.

## Data preparation
As the R code might not be self-explanatory to all readers, we offer a brief high-level description.

There are two main input files originating from the previous neural network analysis: `data/2021_age_gender_combined.tsv.gz` contains classification results for all identified faces. For robustness, we employ two distinct classifiers (one trained on IBDM-Wiki data, the other on the UTK dataset); each face therefore has one line with age and gender estimates for each of them.

The second file, `data/2021_meta_combined.tsv.gz`, contains extraction metadata, specifically the size of the face in pixels and the detection confidence.

The first script, `1_prepare_data.R` loads both files, joins them and pivots the data so that each line represents a single face, with classifier-specific age and gender estimates in columns.

It then parses the face filenames - which contain station, date, and precise time information - into their own columns. We then perform a date range join, attaching program information from the content analysis dataset to each face. This creates additional columns for title, genre, start and end date of the program.

Next, the individual datapoints for each face are annotated with age and gender groups and aggregated, to produce per-show entries with mean age, mean gender share etc. We then recode genre information into english, descriptive and coherent categories.

## Analysis
Our analysis primarily comprises lme4 multilevel models and ggplot visualizations, with some aggregated raw data added to visualize variance. Figure 6, a heatmap, is derived directly from raw data.


