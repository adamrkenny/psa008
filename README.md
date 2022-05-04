# README #

Repository for the Psychological Science Accelerator's (PSA) project
008 "Minimal Groups".

For more information on the project, visit the [PSA 008
webpage](https://psysciacc.org/psa-008-project-information/).

## about [analysis](./analysis) ##

The Rmd script
 [analysis-pipeline.Rmd](./analysis/analysis-pipeline.Rmd) takes the
 artificial data created in
 [data-simulation.R](./analysis/data-simulation.R) and runs through
 the main analyses described in the manuscript. The purpose of the
 analysis pipeline is to troubleshoot models --- some errors might be
 an artifact of the artificial data.

The Rmd script [power-analysis.Rmd](./analysis/power-analysis.Rmd)
contains the power analysis, using the artificial data created in
[data-simulation.R](./analysis/data-simulation.R) and the models
specified in the analysis pipeline.

Most users will find it easiest to open the Rmd script in rstudio, and
possibly knit the file. This will compile a html file with all the
text, code chunks, and output by default. It might take a couple of
minutes, especially if packages have to be installed (which should be
done automatically if necessary upon compilation).

We acknowledge the use of the University of Oxford Advanced Research
Computing (ARC) facility in carrying out the power analysis
(http://dx.doi.org/10.5281/zenodo.22558).

## about [data](./data) ##

Pilot data were collected 04 April 2022 on Prolific
[prolific.co](www.prolific.co), with a sample from the United Kingdom
(n = 200) and the United States of America (n = 200). The csv file
[data-pilot-raw.csv](./data/data-pilot-raw.csv) contains raw pilot
data. Specifically, this is the data downloaded from Qualtrics after
the pilot study was completed, minus columns with identifying
information (e.g. IP address recorded by Qualtrics) and rows
corresponding to troubleshooting attempts by the author(s) before the
pilot study was launched. The csv file
[data-pilot-processed.csv](./data/data-pilot-processed.csv) contains
processed pilot data. There is an associated codebook, as the csv file
[codebook-data-pilot-processed.csv](./data/codebook-data-pilot-processed.csv).
The code for processing the raw data is in
[wrangle-pilot-data.R](./analysis/wrangle-pilot-data.R).

The csv file [exchange-rates.csv](./data/exchange-rates.csv) contains
purchasing power adjusted exchange rates for countries across the
world. We use this to pay participants from different countries an
equivalent amount. The specific indicator is the "PPP conversion
factor, GDP (LCU per international $)", which according to the
original data source: "Purchasing power parity (PPP) conversion factor
is a spatial price deflator and currency converter that controls for
price level differences between countries, thereby allowing volume
comparisons of gross domestic product (GDP) and its expenditure
components. This conversion factor is for GDP." This file is a
modified version of the one shared among the PSA team on 2022-03-24,
whose last updated date was 2022-02-15. Data can be obtained from
[https://data.worldbank.org/indicator/PA.NUS.PPP](https://data.worldbank.org/indicator/PA.NUS.PPP).

## about [matching](./matching) ##

The R script [random-allocator.R](./matching/random-allocator.R) takes
 the pilot data (from 2021-09) and randomly samples a
 participant. This participant becomes the random allocator for those
 who are completing the questionnaire on Qualtrics and are randomly
 assigned by Qualtrics to be recipients.

The random participant's minimal group, nationality, and allocation
 decisions are written to
 [random-allocator.json](./matching/random-allocator.json). The json
 file is called by Qualtrics, and the information embedded. Qualtrics
 then use the following logic to determine the payoff:

1. if the random allocator is from the recipient's minimal in-group,
 then randomly choose either `dg_min_in_self` or `dg_min_in_out` to
 determine payoff

2. if not 1, then if the random allocator is from the recipient's
 minimal out-group (i.e. allocator is Group J and recipient Group H),
 then randomly choose either `dg_min_out_self` or `dg_min_in_out` to
 determine payoff

3. if not 2, then if the random allocator is from the recipient's
 national in-group, then randomly choose either `dg_nat_in_self` or
 `dg_nat_in_out` to determine payoff

4. if not 3, then if the random allocator is from the recipient's
 national out-group, then randomly choose either `dg_nat_out_self` or
 `dg_nat_in_out` to determine payoff

This procedure overcomes deception (every recipient payoff is based on
an actual decision;in the unlikely event that the random allocator
didn't make a decision (e.g. due to an unanswered question, or
dropping-out), the random allocator will default to allocating 10
points allocation), is robust (step 4 acts as a backstop and should
always work), and minimises the amount of embedded data imported by
Qualtrics (e.g. if there were multiple random allocators, then the
survey flow would be inflated). To avoid repeatedly using the same
random allocator, the R script will be regularly run. When data are
being collected, the R script will use downloaded data from the active
study (instead of the pilot data), and the R script will be more
regularly run.