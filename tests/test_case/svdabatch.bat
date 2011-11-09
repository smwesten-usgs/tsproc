@echo off
REM This part of batch file added by SVDAPREP
REM
REM Delete model input files.
del par2par_base.dat > nul
REM
REM Run PARCALC to compute base parameters from super parameters.
parcalc > nul
REM
REM Run PICALC to compute base parameter prior information.
picalc > nul
REM
REM The following is copied directly from file modelcmd.bat
REM
IF NOT EXIST par2par_base.dat ECHO Can't find par2par_base.dat.  Exiting...

ECHO Running PAR2PAR...
par2par.exe par2par_base.dat

ECHO Running GSFLOW...

gsflow.exe -Cgsflow_pest.control

REM IF NOT EXIST statvar.dat ECHO Something is wrong with GSFLOW model setup: no statvar.dat file exists...
REM IF NOT EXIST statvar.dat EXIT

IF NOT EXIST gis.nssr ECHO Something is wrong with GSFLOW model setup: no gis.nssr file exists...
IF NOT EXIST gis.nhru ECHO Something is wrong with GSFLOW model setup: no gis.nnhru file exists...
IF NOT EXIST gis.ngw ECHO Something is wrong with GSFLOW model setup: no gis.ngw file exists...

IF NOT EXIST gis.nssr EXIT
IF NOT EXIST gis.nhru EXIT
IF NOT EXIST gis.ngw EXIT

ECHO Running the munge data extraction program and creating SSF files...
munge.exe

ECHO Running TSPROC...
tsproc.exe <tsproc_console.inp >nul

ECHO ^<END of modelcmd.bat script^>
