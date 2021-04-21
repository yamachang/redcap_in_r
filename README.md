# REDCAP IN R REPOSITORY
This a project that use R to deal with RedCap data.

To install the 'bsrc' package, you will need the package 'devtools'
```
library("devtools")
devtools::install_github("PROTECT-lab/redcap_in_r")
objects("package:bsrc")
```
**Before use of the package, you will need a profile for each RedCap connection**

(Skip this part if you have a startup script, which should include making the following)
You can use the following function to create one using necessary information:
```
bsrc.switcher(name=NULL,      #Any name you wish to give it to the profile, just easy way to identify
	      redcap_uri=NULL,#RedCap URL for the redcap system *required
	      token=NULL,     #RedCap token.                    *required
	      rdpath=NULL,    #local .rdata path, let it remain NULL if wish to use fully online mode
	      protocol.cur=F  #do not use global variable protocol.cur to generate profile
	      ) 
```

**To attach data**

To attach data as an environment or list, you can use the following function. The argument "returnas" can be set to either "list" or "envir." By default it should use "envir" for compatibility with other functions
```
curdb<-bsrc.attachngrab(protocol=ptcs$bsocial,returnas="envir")
```
The environment (or list) object should include the following objects: 
- data #Main data all in one data frame
- metadata #Meta data for mapping purposes
- eventmapping #Event mapping data for mapping purposes 
- update.date #When is this updated 
- update.time #What time is this updated
- success     #If the update was successful

Most of the function within this package will accept this environment (or list [limited support]) object as their data source. If not supplied, they will pull from RedCap server always using the credential. To minimize time cause, always supply this to a function when applicable. 

**Required Packages**

Since now this repo can be loaded as package, the required packages are automatically loaded during installation of 'bsrc'
But here's a list of the packages:
```
library("data.table")
library("lubridate")
library("ggplot2")
library("zoo")
library("plyr")
library("ggrepel")
library("redcapAPI")
library("RCurl")
library("data.table")
library("REDCapR")
library(pracma)
library(rprime)
library(tidyr)
library(stringdist)
```
