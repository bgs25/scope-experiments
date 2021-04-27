# SCOPE experiments

This folder contains the code and scripts that were used in the experimental sections of [Modelling high-dimensional categorical data using nonconvex fusion penalties](https://arxiv.org/abs/2002.12606). The accompanying R package [CatReg](https://cran.r-project.org/package=CatReg) implements the methodology used in the paper.

For each experiment, we have created a folder which contains an R script, data required to run the test, and results data. 

Originally these experiments would be run across multiple computers, with each running the computation saving at checkpoints. After enough experiments are run, there would be a results-gathering process for (in some instances deriving, and) compiling the results together. Where nontrivial, such files are seperate scripts and end in compile_results. Due to this organisation, in the results files there are sometimes more results than were used in the paper (e.g. for Section 6.1.1 there are 163 rows in the results matrices, of which the first 150 were used).

The reason for this is that the experiments are very slow to run in some cases. In particular, the CasANOVA code is slow, as is the logistic version of SCOPE (in CatReg the logistic version is 'experimental'). Some of the computation times can be found in the appendix of the paper.

The scripts have been lightly abridged for:
* Readability
* Compatibility with updated versions of [CatReg](https://cran.r-project.org/package=CatReg) (so are consistent with the syntax in Version 2.0.1 and later)
* Running on a single machine (wrappers to send jobs to multiple machines have been removed and now these can _in theory_ all be run on a single machine, if one were to have enough time and patience.)

The abbreviations for the various objects in the results files refer to the method used to obtain them. Note that in some of the results files, SCOPE computation results will be saved with the prefix 'mb...', which refers to the 'monkey bar' nickname given to the dynamic programming algorithm used for SCOPE computation (see Figure 3 in the paper). 

## Section 6.1.1

These are among the longest to run. The \_tests code fits all of the models, then the \_compile\_results code will generate test samples, compute predictions and errors. These were originally run on many computers in order to build up enough results to compile into the paper. For the paper, the first 150 replicates were used (from a total of 163 that can be found in the results file).

## Section 6.1.2

These were faster to run owing to not including CasANOVA among the comparisons. These were originally run on a smaller number of computers. Predictions and errors are computed in the \_tests script so the \_compile\_results script is not necessary when the edited \_tests script is run on just one machine.

## Section 6.2

This experiment used data from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/adult), and the processed dataset used is contained in the folder as full_adult_data.Rdata. These are again slow due to the proximal newton loop in both the SCOPE and CasANOVA implementations. The results can be found in Section_6_2_full_results_250.Rdata.

## Section 6.3

These are faster due to the smaller number of settings used, since this is an illustration of the clustering behaviour and not an all-out performance test. The computations for the four datasets are done in parallel and results combined in a list of lists. This used the same processed dataset as in Section 6.2

## Section 6.4

This experiment used data available on [Kaggle](https://www.kaggle.com/c/prudential-life-insurance-assessment/overview). A number of machines were used in this experiment for speed. After running the main code there is no need for further processing of the results. Results obtained are included in the folder; the first 1000 were used.

## Section 6.5

This exploratory experiment used data compiled at the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub). The file process_covidhub.R contains information for how to extract and process data from this source into a form that can then be used in the experiment given in the paper. We also include the processed data that was used for the experiment. The file section_6_5_tests.R performs the experiments and includes script for generating the plot used in the paper.

# Section S.1.2

These can be done on a single machine and are fast to run.
