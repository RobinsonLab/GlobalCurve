GlobalCurve
===========

This is script-driven global non-linear least squares parameter estimation written in Fortran 90. The software was originally written to analyzed time resolved fluorescence data (decay). The observed fluorescence decay is the intrinsic decay convolved with the *instrument resonse function (IRF)*.


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


## Example of analysis

2 exponential fitting
