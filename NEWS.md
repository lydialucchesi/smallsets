# smallsets 2.0.0
### CRAN submission

December 2023

CRAN submission. New features and updates:

* smallsets now builds Smallset Timelines for preprocessing code in R Markdown and Jupyter Notebook files. Example Rmd and ipynb files have been added to the package.
* The format of structured comments has changed. There are no longer start and end instructions. Instead all snapshot comments use the snap instruction but with a place argument as well. **This change will break existing smallsets code.**
* Users can now ignore any column in the dataset in the Smallset Timeline (i.e., the ignoreCols argument in Smallset_Timeline() has been changed).
* Smallset Timelines can now be vertically aligned, meaning snapshots are plotted from top to bottom (instead of left to right) and captions are located to the right of snapshots (instead of below). There is a new "align" argument in Smallset_Timeline().
* The Smallset Timeline colour legend labels have been simplified.
* A bug (introduced with the release of R 4.2.0 and changes to as.vector()) in the second optimisation Smallset selection problem is fixed.
* All documentation--including the help pages, vignette, pkgdown website, and README page--has been updated to reflect these package changes.

# smallsets 1.0.0
### CRAN submission

31 January 2023

CRAN submission. Major revisions to package structure and user workflow. New documentation, including a vignette.

# smallsets 0.0.1.9000
### FAccT paper

3 November 2022

Version released several months after FAccT publication (June 2022). However, there have been no major changes to the software since then. The structure and functionality mirror what was described in the paper.
