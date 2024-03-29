Shiny\_TOSTER app
=================

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5750034.svg)](https://doi.org/10.5281/zenodo.5750034)

This app adds a user interface to the [TOSTER](https://cran.r-project.org/web/packages/TOSTER/index.html) package, demonstrating the usefulness of Two One-Sided Test (TOST) equivalence testing. In the current version only raw SESOI (smallest effect size of interest) input is implemented.

The TOSTER R package can be installed from CRAN using install.packages (TOSTER). Detailed example vignettes are available from: <https://cran.rstudio.com/web/packages/TOSTER/vignettes/IntroductionToTOSTER.html>

Getting Started
---------------

You can copy the app file app.R to your system and run it locally. Alternatively, you can launch it from GitHub without installation.

### Prerequisites

In both cases, the following packages need to be installed in order for the app to run:

`shiny` from <https://cran.r-project.org/web/packages/shiny/index.html>

`TOSTER` from <https://cran.r-project.org/web/packages/TOSTER/index.html>

`MASS` from <https://cran.r-project.org/web/packages/MASS/index.html>

`tidyverse` from <https://cran.r-project.org/web/packages/tidyverse/index.html>

### Launching the app locally

You can launch the app by giving the name of its directory to the function runApp. If the app is in a folder of the working directory called "Shiny\_TOSTER", run it with the following code:

    library(shiny)
    runApp("Shiny_TOSTER")

Alternatively, you can open the file app.R in the RStudio editor and click the "Run App" button at the top of the editor.

### Launching the app from GitHub without installation

From the R console run the following commands to launch the App:

    # Assuming 'shiny' package and all other prerequisites are already installed 
    library(shiny)     
    #name of the app dir and username
    runGitHub("Shiny_TOSTER", "ontogenerator")

Your browser should launch the shiny app directly on your machine.

Usage
-----

The interface allows the user to play around with the inputs and observe how the values change the output of the TOST tests. An additional feature is to let the inputs represent the hypothesized true parameters of interest, e.g. what the experimenter expects the results to look like and bootstrap from those values. A given `Number of iterations` is performed, resampling from the corresponding distributions.
This feature is activated by first selecting the radiobutton `simulated`, which changes the input and the output view. The simulations are only performed when the button `Perform simulations` is clicked, in order to avoid constant recalculation when the input is changed.
The output is displayed as a stack of all (e.g. 1000) iterations in a "dance of the confidence intervals" style. Both the 95% (for alpha = 0.05) and the 90% confidence intervals are plotted with the same width on top of each other, coded in different colors depending on statistical significance. Non-significant results are plotted in dark gray, significant results from NHST are plotted in orchid, and significant results from TOST are plotted in lime green. Additionally the plot displays the proportion of iterations that yielded all possible outcomes. These outcomes are labeled and color-coded as follows:

| Full name           | Label & Color                                                                    | Outcome of TOST | Outcome of NHST |
|---------------------|----------------------------------------------------------------------------------|-----------------|-----------------|
| inconclusive        | inc ![\#A9A9A9](gray.png) `darkgray`         | non-equivalent  | non-significant |
| non-equivalent      | nonequiv ![\#FF83FA](orchid.png) `orchid1`     | non-equivalent  | significant     |
| strictly equivalent | str\_equiv ![\#32CD32](limegreen.png) `limegreen` | equivalent      | non-significant |
| trivial             | triv ![\#32CD32](limegreen.png) `limegreen`       | equivalent      | significant     |
| non-zero            | nonzero ![\#FF83FA](orchid.png) `orchid1`      | irrelevant      | significant     |
| equivalent          | equiv ![\#32CD32](limegreen.png) `limegreen`      | significant     | irrelevant      |

The input options for the simulations (when the radiobutton `simulated` is selected) are in most cases identical to the ones for the standard input (when the radiobutton `observed` is selected), with the exception of the paired two-sample test and the correlations.

### Paired two-sample test

In the paired two-sample test only one standard deviation is given as an input, the standard deviation of a typical group. With repeated measures the observed total variance can be decomposed to the variance between individuals(or groups) and within individuals (or groups). The larger the variance between individuals (or groups), the higher the so-called repeatability is. For example, the repeatability of height in adult people is very high, because a group of people vary in height much more than every individual varies from day to day.
The repeatability can be given values between 0 and 1 and corresponds to the proportion of the between-individual (or group) variance from the total variance.

### Correlations

The correlation test is simulated as follows: first a virtual population of a certain size (`Total number of individuals in the virtual population`) is chosen. Then two random vectors are generated, so that they have the chosen correlation (actually a value very close to it, because of the algorithm). Then the desired number of paired observations are sampled at random, and the correlations of these samples are calculated and taken as input for the statistical tests in each iteration.

License
-------

Copyright (C) 2022 Vladislav Nachev

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.  

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.  

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
