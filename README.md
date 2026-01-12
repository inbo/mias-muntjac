<!-- badges: start -->
![GitHub](https://img.shields.io/github/license/inbo/mias-muntjac)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/inbo/mias-muntjac/check-project)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/mias-muntjac)
<!-- badges: end -->

# Monitoring invasive alien species of Union concern: Chinese muntjac

[Adolf, Janne![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-6064-9803)[^aut][^cre][^inbo.be]
Research Institute for Nature and Forest (INBO)[^cph][^fnd]

[^cph]: copyright holder
[^fnd]: funder
[^aut]: author
[^cre]: contact person
[^inbo.be]: Research Institute for Nature and Forest (INBO)

**keywords**: invasive alien species; monitoring schemes; Chinese muntjac

<!-- community: inbo -->

<!-- description: start -->
Repository accompanying the development of monitoring schemes for the Chinese muntjac in Flanders.\
For the overarching project repository, see the repository [mias-general](https://github.com/inbo/mias-general).


<!-- description: end -->

<!-- Anything below here is visible in the README but not in the citation. -->


## Repository structure 

The repository is organised as follows:

```
.
├── data/
├── inst/
├── media/
├── renv/
└── source/
```


### `data/`
Contains input and processed data used in the project's analyses and visualizations.

### `media/`
Contains static media files used in documentation or report, such as:
- bibliography

### `renv/`
Contains files created by the **renv** R package to manage the R package environment and ensure reproducibility.

### `source/`
Contains the source code for the project, including:
- `_functions`: R functions used throughout the project
- `config`: R scripts used to set up, configure and maintain the repository
- `docu_report`: Quarto project and Quarto files used to generate the dynamic report for this project
- `gbif_occ_maps`: R scripts for occurrence maps based on GBIF data
- `sample_design`: R scripts for sampling design and power-based sample size planning

## Reproducibility

This project uses **renv** to manage R package dependencies.
The `renv.lock` file (and its commit history) provides detailed information on the R version and all R packages used.
To restore the project dependencies from the lockfile, use the indicated R version and run:

```r
renv::restore()
```
