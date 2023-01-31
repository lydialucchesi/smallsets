Dear CRAN Team,

Thank you for taking the time to review our smallsets submission to CRAN. Below is a note about Gurobi and output from the R CMD check. Kind regards, Lydia Lucchesi

### Note about Gurobi
We include the gurobi package in suggests. This package provides an interface to the Gurobi optimisation software (<https://www.gurobi.com/documentation/9.1/quickstart_windows/r_ins_the_r_package.html>) but is not available on CRAN, as noted here in a vignette for the prioritizr package on CRAN (they also include gurobi in suggests): <https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html>. In the smallsets package, users can run two optimisation problems using the Gurobi solver, if they'd like. However, they will first need to obtain a license and install the solver/package on their own. Instructions are provided to users if they try to use one of the optional optimisation problems but do not have gurobi installed. **The smallsets package can be used without installation of gurobi.**

### R CMD check
Duration: 48.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

