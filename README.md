GlobalCurve
===========

This is script-driven global non-linear least squares parameter estimation written in Fortran 90. The software was originally written to analyze time resolved fluorescence data (decay). The observed fluorescence decay is the intrinsic decay convolved with the *instrument resonse function (IRF)*. 

### Quirks ###
- One must have global.in file (a .conf or .ini file) in the working directory of the .ans file. This controls the nature of the analysis, like the target change in chi^2 for stopping the analysis. For this reason, the .in file must be **local**.
- When data needs to be pre- or post-processed in some way during the anslysis, this is specified in the data file itself. This specification is in the header of the data file. 

### Limitations ###
- This is compiled softare. Additional models must be compiled. 
- This was written using the Digital/Compac/Fujitsu/Intel F90 compiler, which uses digital extensions of the F90 standared. This restricts one to development on DOS.
- Srcripts (usually called .ans files) are written in a custom format. These days, one would write in YAML or similar.
- Software is **very unforgiving** of errors in data format or script files.



Installation
============
- install globalcurve in ``c:\bin\``
- install pgplot plotting library (dependency) in ``c:\``

Set [system Path (environement variable)](http://www.computerhope.com/issues/ch000549.htm) to recognize the globalcurve executable. Value on my computer is 
```
%SystemRoot%\system32;%SystemRoot%;%SystemRoot%\System32\Wbem;%SYSTEMROOT%\System32\WindowsPowerShell\v1.0\;C:\bin\GlobalCurve
```


### pgplot
install at c:


Usage
=====

In windows, use PowerShell. To access documents, which are shared using VMWARE, `cd z:`. cd to directory of interest. 

```
> globalcurve
```


## Analysis examples

### 2 exponential fitting of TCSPC data
- data file is usually given a .glo extension.
- The IRF and the # counts in each channel as two columns. The header contains, successively, 
    * time/channel (ns/chan) 
    * start channel of the convolution
    * stop channel of chi^2 calculation
    * start channel of the chi^2 calculation

