## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

 devtools::check()
Updating creditmodel documentation
Writing NAMESPACE
Loading creditmodel
Writing NAMESPACE
Writing lasso_filter.Rd
-- Building ----------------------------------------------------- creditmodel --
Setting env vars:
* CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
* CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
* CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
--------------------------------------------------------------------------------
√  checking for file 'C:\Users\28142\Documents\automodel/DESCRIPTION' (367ms)
-  preparing 'creditmodel': (1s)
√  checking DESCRIPTION meta-information ... 
-  installing the package to build vignettes (551ms)
√  creating vignettes (27.4s)
-  checking for LF line-endings in source and make files and shell scripts (364ms)
-  checking for empty or unneeded directories
-  looking to see if a 'data/datalist' file should be added
-  building 'creditmodel_1.0.tar.gz' (1.6s)
  

-- Checking ----------------------------------------------------- creditmodel --
Setting env vars:
* _R_CHECK_CRAN_INCOMING_USE_ASPELL_: TRUE
* _R_CHECK_CRAN_INCOMING_REMOTE_    : FALSE
* _R_CHECK_CRAN_INCOMING_           : FALSE
* _R_CHECK_FORCE_SUGGESTS_          : FALSE
-- R CMD check -----------------------------------------------------------------------
-  using log directory 'C:/Users/28142/AppData/Local/Temp/Rtmpa6OGfx/creditmodel.Rcheck' (467ms)
-  using R Under development (unstable) (2019-04-21 r76409)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: CP936
-  using options '--no-manual --as-cran' (776ms)
√  checking for file 'creditmodel/DESCRIPTION'
-  this is package 'creditmodel' version '1.0'
-  package encoding: UTF-8
√  checking package namespace information ... 
√  checking package dependencies (1.9s)
√  checking if this is a source package ... 
√  checking if there is a namespace
√  checking for executable files (549ms)
√  checking for hidden files and directories ... 
√  checking for portable file names ... 
√  checking serialization versions ... 
√  checking whether package 'creditmodel' can be installed (31.8s)
√  checking installed package size ... 
√  checking package directory (793ms)
√  checking for future file timestamps (668ms)
√  checking 'build' directory
√  checking DESCRIPTION meta-information (474ms)
√  checking top-level files ...
√  checking for left-over files ... 
√  checking index information (450ms)
√  checking package subdirectories (427ms)
√  checking R files for non-ASCII characters ... 
√  checking R files for syntax errors ... 
√  checking whether the package can be loaded (3.3s)
√  checking whether the package can be loaded with stated dependencies (3s)
√  checking whether the package can be unloaded cleanly (2.8s)
√  checking whether the namespace can be loaded with stated dependencies (2.8s)
√  checking whether the namespace can be unloaded cleanly (2.9s)
√  checking loading without being on the library search path (3.7s)
√  checking dependencies in R code (3.3s)
√  checking S3 generic/method consistency (4.3s)
√  checking replacement functions (2.8s)
√  checking foreign function calls (3.6s)
√  checking R code for possible problems (26.9s)
√  checking Rd files (1.4s)
√  checking Rd metadata ... 
√  checking Rd line widths (425ms)
√  checking Rd cross-references (429ms)
√  checking for missing documentation entries (2.9s)
√  checking for code/documentation mismatches (8.7s)
√  checking Rd \usage sections (4.9s)
√  checking Rd contents (534ms)
√  checking for unstated dependencies in examples (1.2s)
√  checking contents of 'data' directory ...
√  checking data for non-ASCII characters (1.9s)
√  checking data for ASCII and uncompressed saves (433ms)
√  checking installed files from 'inst/doc' ... 
√  checking files in 'vignettes' ... 
√  checking examples (1m 8.4s)
√  checking for unstated dependencies in 'tests' ... 
-  checking tests ...
√  Running 'testthat.R'
√  checking for unstated dependencies in vignettes (792ms)
√  checking package vignettes in 'inst/doc' ...
√  checking re-building of vignette outputs (1.6s)
  
   

Warning in readChar(path, nchars = file.info(path)$size, ...) :
  can only read in bytes in a non-UTF-8 MBCS locale
-- R CMD check results ------------------------------------------ creditmodel 1.0 ----
Duration: 3m 17.5s

0 errors √ | 0 warnings √ | 0 notes √