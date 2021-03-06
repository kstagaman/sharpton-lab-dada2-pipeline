# The Sharpton Lab dada2 pipeline

This package is designed to make it easy to replicably conduct the dada2 pipeline as run by the Sharpton lab at Oregon State University.

To begin, if you want to build phylogenetic trees, you need the [mothur](https://mothur.org/) and [FastTree](http://www.microbesonline.org/fasttree/) programs installed on the machine on which you will be conducting the pipeline.

Additionally, it relies on three packages that will not auto-install if you first try to install it right away. Make sure you run the following code for package you don't yet have installed:

```
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
for (pkg in c("dada2", "phyloseq", "ALDEx2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    BiocManager::install(pkg)
  }
}
devtools::install_github("ggloor/CoDaSeq/CoDaSeq")
devtools::install_github("kstagaman/phyloseqCompanion")
```

This package can be installed by running the following

```
devtools::install_github("kstagaman/sharpton-lab-dada2-pipeline")
```

To get started, first generate the template script by running in your working directory.

```
library(dada2.pipeline)
generate.template()
```

