## biobtreeR 

R package for genomic research via [biobtree](https://github.com/tamerh/biobtree). It aims to provide an alternative
to existing packages such as biomaRt with abilty process large and diverse datasets effectievly and allows 
executing simple or complex queries between these datasets.


## Install
```r
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("tamerh/biobtreeR")
```

## Usage

```r
  
  library(biobtreeR)
  
  # directory for the tool files 
  bbUseOutDir("specify your directory")
  
  # default database for most studied dataset and organism genomes 
  # once it is retrieved it is saved to your directory for later reuse
  # check document for included dataset or other builtin databases or build custom data
  bbBuiltInDB()

  # starts server for executing queries inside R pipelines and provide web interface for expolaration with example queries
  # web interface address http://localhost:8888/ui/
  bbStart()

```
