## biobtreeR 

Bioconductor R package for genomic research via [biobtree](https://github.com/tamerh/biobtree). It aims to provide an alternative to existing packages with abilty process large and diverse datasets effectievly and allows 
executing simple or complex queries between these datasets.

For more detail and documentation check the Bioconductor [page](https://bioconductor.org/packages/3.11/bioc/html/biobtreeR.html)


## Install
```r

if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("tamerh/biobtreeR")

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

  # starts server for executing queries inside R pipelines and provide web ui for expolaration with examples
  # web interface address http://localhost:8888/ui/
  bbStart()

```
