# GCubeR: Estimation of Forest Volume, Biomass, and Carbon

![](articles/img/logo_verysmall.png)

GCubeR is an R package designed to provide reliable, modular, and
scientifically robust tools for estimating key forest attributes,
including tree volume, biomass, and carbon stocks through the
application of allometric equations.

The package is primarily intended for forest researchers, practitioners,
and managers working in Southern Belgium, while its flexible framework
also facilitates adaptation to other regions and contexts.

## Acknowledgement

GCubeR is the result of a collaborative effort involving several
generations of contributors:

- Gauthier Ligot developed the first version of the package, named
  Dagnelie.
- Anthonin Gaussin (Master’s student in Gembloux Agro-Bio Tech, ULiege)
  developed the second version, named Gcuber.
- Pierre Bosman, Juliette Defontaine, Samuel Douin, David Linchant, and
  Timon Luizi (Master’s students in Gembloux Agro-Bio Tech, ULiege)
  developed the third version, named GCubeR, which became the first
  release published on CRAN.
- Gauthier Ligot and Nathéo Beauchamp subsequently enhanced and expanded
  the package, improving its functionality, documentation, and
  maintainability.

## Installation

You can install GCubeR directly from the CRAN :

``` r

# install.packages("GCubeR")
```

You can also install the lastest development version (for developers
mostly) :

``` r

# It requires to install the devtools package
# install.packages("devtools")
devtools::install_git("https://github.com/GauthierLigot/GCubeR.git")
```

## Instructions for Developers (ULiège Team)

This section is addressed to the project’s developers and details how to
retrieve the team’s configured project.

1.  Clone the Project:Use RStudio: `File` \> `New Project` \>
    `Version Control` \> `Git`. Paste your GitLab repository URL:
    `https://github.com/GauthierLigot/GCubeR`
2.  Open:\*\* Open the `GCubeR.Rproj` file.
3.  Synchronization:
    - Before Coding:Perform a Pull to fetch the latest changes from the
      team.
    - After Each <Session:Complete> a Commit followed by a Push to
      update the central repository.
