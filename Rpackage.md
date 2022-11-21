# Introduction to BipotentR package

Below, we describe instruction to setup R package of BipotentR. 
The following tutorial is designed to BipotentR pacakge overview of the kinds of comparative analyses on complex cell types that are possible using the Seurat integration procedure. Here, we address a few key goals:

* Install BipotentR R package
* Download reference data
* Identify bipotent targets of CITRATE CYCLE and immune response using BipotentR 

## Install
```r
library(devtools)
install_github("vinash85/TRIM")
```

## Download reference dataset
The hdf5 file for cistrome dataset is downloaded by `download.data`.
```r
library(TRIM)
download.data()
```

## Run BipotentR
Here we describe running the package for CITRATE CYCLE  as a input pathway. The genes within TCA cycle is stored in the variable `c2.kegg`. `BipotentR` is main function of the package.
The outputs will be stored in `bipotent.targets`
```r
tca.pathway =  c2.kegg[c2.kegg$term=="KEGG_CITRATE_CYCLE_TCA_CYCLE",]$gene  
## default location download.data store file
cistrome.hdf5.path =  sprintf("%s/data//human_100kRP.hd5", system.file(package = "TRIM")) 
bipotent.targets = BipotentR(tca.pathway, cistrome.hdf5.path)
```

## Github 
The R package is available in github at [BipotentR](https://github.com/vinash85/TRIM) package. 
