# Response to reviewer comments for CRAN re-submission (5 December 2023)
Thank you, Uwe Ligges, for reviewing smallsets. As suggested, more information regarding the gurobi installation has been added to the Description field in the DESCRIPTION file.

# Second CRAN submission (4 December 2023)
Dear CRAN Team,

Thank you for taking the time to review this smallsets v2.0.0 submission to CRAN. The first smallsets CRAN release (v1.0.0) was on 3 February 2023. Since then, several new features have been added and changes made, including a breaking change. These updates are described in the NEWS.md file. The R CMD check succeeded in 1m 11.8s without any errors or warnings. There will be one note about the Gurobi R package (as was the case in the first CRAN release); please see an explanation of this note [below](#Note_about_gurobi). Kind regards, Lydia Lucchesi

### R CMD check
Duration: 1m 11.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded




# Response to reviewer comments for CRAN re-submission (2 Feb 2023)
Thank you, Benjamin Altmann, for reviewing smallsets and providing comments. Below we address the review comments.

#### Adding `\value` to .Rd files
We added the missing `\value` information to the help documentation for `sets_labelling()`, `sets_sizing()` and `sets_spacing()`, including the output class and what it means. We also added info to the "Details" section of these three pages, pointing back to `Smallset_Timeline()`.

#### Adding gurobi to Additional_repositories
We are unable to add an URL to a source package for gurobi, as it depends on where the Gurobi software is installed on one's machine. In the smallsets vignette, we point users to Richard Schuster's [Gurobi installation guide](https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html) in the prioritizr R package, as it provides step-by-step instructions on Gurobi installation in R.

#### Removing `print()` commands
We have removed all `print()` commands, replacing them with `warning()`/`stop()`/`cat()`, depending on which is most appropriate.

### R CMD check after revisions
Duration: 50.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

***Thanks again for the helpful comments. Kind regards, Lydia Lucchesi***


# First CRAN submission (31 Jan 2023)
Dear CRAN Team,

Thank you for taking the time to review our smallsets submission to CRAN. Below is a note about Gurobi and output from the R CMD check. Kind regards, Lydia Lucchesi

### Note about Gurobi {#Note_about_gurobi}
We include the gurobi package in suggests. This package provides an interface to the Gurobi optimisation software (<https://www.gurobi.com/documentation/9.1/quickstart_windows/r_ins_the_r_package.html>) but is not available on CRAN, as noted here in a vignette for the prioritizr package on CRAN (they also include gurobi in suggests): <https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html>. In the smallsets package, users can run two optimisation problems using the Gurobi solver, if they'd like. However, they will first need to obtain a license and install the solver/package on their own. Instructions are provided to users if they try to use one of the optional optimisation problems but do not have gurobi installed. **The smallsets package can be used without installation of gurobi.**

### R CMD check
Duration: 48.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

