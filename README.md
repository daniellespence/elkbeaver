# elkbeaver
---
title: 'ReadMe: Collaborative valuation of ecosystem services to inform lake remediation (Spence, Baulch, Lloyd-Smith, ES&P)'

author: "Danielle Spence"

date: "December 2022"

output:
  pdf_document: default
  html_document: default
  
---

Overview
--------

The code in this replication package uses R to construct all analyses in *Collaborative valuation of ecosystem services to inform lake remediation*. Here, one master file runs all of the code to generate the data for 1 table and 2 figures in the paper. Scripts for data analysis are available at https://github.com/daniellespence/elkbeaver

Data Availability and Provenance Statements
----------------------------

- [ ] This paper does not involve analysis of external data (i.e., no data are used or the only data are generated by the authors via simulation in their code).

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 


### License for Data

Licence: Open Government Licence - Canada

### Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

### Details on each Data Source

The data dictionary is also provided at this link. Copies of the data and data dictionary are provided as part of this archive.

Data were prepared using scripts 1—4 available at https://github.com/daniellespence/elkbeaver/tree/main/Code

Datafile used:  `Elk_Beaver_Lake_Completes_Excel.xlsx`

Computational requirements
---------------------------

### Software Requirements

- R 3.6.3
  - `pacman`
  - `tidyverse`
  - `purrrlyr`
  - `anesrake`
  - `apollo`
  - `plyr`
  - `ggplot2`
  - `cowplot`


### Memory and Runtime Requirements

#### Summary

Approximate time needed to reproduce the analyses on a standard (2020) desktop machine:

- [ ] <10 minutes
- [x] 10-60 minutes
- [ ] 1-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days
- [ ] Not feasible to run on a desktop machine, as described below.

#### Details

The code was run on a **7-core Intel-based desktop with 32 GB of RAM and a Windows 10 operating system**. 

Description of programs/code
----------------------------

Data preparation and welfare analysis scripts are available at https://github.com/daniellespence/elkbeaver
The code is licensed under a MIT/BSD/GPL/Creative Commons license.

Instructions to Replicators
---------------------------

- You will need to install the following R packages

`install.packages("pacman", "tidyverse", "mice", "fastDummies", "furrr", "micemd", "randtoolbox", "rstan", "rmdcev")`

- Prepare the data using scripts 1—4. 
- Run the `5.1. MIXL_Linear.R` and '5.2. MIXL_Log.R' R scripts to generate model outputs and calculate welfare
- Run the 'CS visualization_mixl.R' and 'Likert scales.R' to generate the two figures presented as results in the paper.

List of tables and programs
---------------------------

The provided code reproduces:

- [x] All numbers provided in text in the paper
- [ ] All tables and figures in the paper
- [x] Selected tables and figures in the paper, as explained and justified below.


| Figure/Table  | Program             | Line Number | Output file                      | Note   |
|-----------|-------------------------|-------------|----------------------------------|-------|
| Table 3 | 5.1. MIXL_Linear.R & 5.2. MIXL_Log.R |             |   ||
| Figure 2 | Likert scales.R |          |    ||
| Figure 3 | CS visualization_mixl.R|         |    ||

---
