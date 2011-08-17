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
tsproc.exe all tsproc_hi.inp tsproc_hi.rec

ECHO ^<END of modelcmd.bat script^>
