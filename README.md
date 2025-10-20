# GCubeR: Estimation of Forest Volume, Biomass, and Carbon

The goal of GCubeR is to provide a reliable, modular, and scientifically tool for estimating key forest metrics (volume, biomass, and carbon) by applying allometric equations. It builds upon the coding standards of the Dagnelie package, allowing users and scientists to easily audit and extend the internal database of equations.

## Installation

You can install the development version of GCubeR directly from your GitLab repository using the `devtools` package like so:

```r
# Assurez-vous d'avoir 'devtools' installé
# install.packages("devtools") 

devtools::install_gitlab("David.Linchant/gcuber") 
# Cette commande télécharge et installe le package depuis votre dépôt.
```
## Instructions for Developers (ULiège Team)

This section is addressed to the project's developers and details how to retrieve the team's configured project.

1.  Clone the Project:Use RStudio: `File` > `New Project` > `Version Control` > `Git`. Paste your GitLab repository URL: `https://gitlab.uliege.be/David.Linchant/gcuber`
2.  Open:** Open the `GCubeR.Rproj` file.
3.  Synchronization:
    * Before Coding:Perform a Pull to fetch the latest changes from the team.
    * After Each Session:Complete a Commit followed by a Push to update the central repository.