# README #

Repository for the Psychological Science Accelerator's (PSA) project
008 "Minimal Groups".

For more information on the project, visit the [PSA 008
webpage](https://psysciacc.org/psa-008-project-information/).

## about [analysis](./analysis) ##

The Rmd script
 [analysis-pipeline.Rmd](./analysis/analysis-pipeline.Rmd) takes the
 artificial data and runs through the main analyses described in the
 manuscript. It does not include the power analysis, which is in a
 separate document. The purpose of the analysis pipeline is to
 troubleshoot models --- some errors might be an artifact of the
 artificial data.

Most users will find it easiest to open the Rmd script in rstudio and
knit the file. This will compile a html file with all the text, code
chunks, and output by default. It might take a couple of minutes,
especially if packages have to be installed (which should be done
automatically if necessary upon compilation).

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