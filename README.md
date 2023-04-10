Code for the article Cherp, A., Vinichenko, V., Tosun, J., Gordon, J., and Jewell J. 
National growth dynamics of wind and solar power compared to the growth required for 
global climate targets. Nature Energy (2021) 

DOI: https://doi.org/10.1038/s41560-021-00863-0

fitting.R - R code for growth model fitting

functions.R – definition of functions for growth model fitting

sample_data.csv - source data file for growth model fitting

file_format.txt – description of input and output file formats

experiment.R – R code for the computational experiment

experiment_doc.txt – description of the computational experiment and 
format of the generated files

When running R files, working directory should be set to one containing the respective 
data files.

Packages necessary for running R code are listed within fitting.R.

Warning: (1) growths rates cannot be reliably estimated  when the inflection point is 
in the future (indicated by "1" under the **Future** column - see file_format.txt). 
(2) The code has not been tested for data series starting around or above the inflection 
point (50% and 37% of the  asymptote for Logistic and Gompertz models respectively). 
Growth model parameters estimates for such data series may be unreliable. 
