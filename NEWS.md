# wcep (development version 1.0.3.900)

## Major updates and breaking changes
* Adding ability to handle censoring.
* Update to survival and variance estimators to run in C++.
* Addition of C++ dependencies and removal of progress bar dependencies.
* New progress bar for each estimate component (survival, variance) directly in C++
* Update to R dependency (>= 4.1) to allow for use of pipe `|>`.
* Removal of global Wilcoxon and t-tests.


# wcep 1.0.3

## Minor improvements and fixes

* Adding missing package anchors and bug reporting url.

# wcep 1.0.2

## Minor improvements and fixes

* Fixed incorrect coding for sum of squared survival estimates in variance estimator.
* Updated methods reference in description file.

# wcep 1.0.1

## Minor improvements and fixes

* Update to `left_join()` to remove `stringsAsFactors` argument to comply with 
  with dplyr 1.1.0 (@DavisVaughan).

## Comments
* References updated/added. Variance estimator reference now available in `wcep()` help file.
* Package help file added with instructions for reporting bugs/issues.
* Licence updated to reflect copyright of all package contributors.

# wcep 1.0.0

* Added a `NEWS.md` file to track changes to the package.

## Comments
This is the first version of this package. There is no news.
