# Makefile created by mkmf.pl $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 



include ../../mkmf_templates/mingw_gfortran.tpl


.DEFAULT:
	-echo $@ does not exist.
all: libwdm.a
tsbufr.o: wdm/tsbufr.f wdm/.\ctsbuf.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/tsbufr.f
wdatm2.o: wdm/wdatm2.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	wdm/wdatm2.f
wdatrb.o: wdm/wdatrb.f wdm/.\cfbuff.inc wdm/.\cdrloc.inc wdm/.\fversn.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdatrb.f
wdatru.o: wdm/wdatru.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	wdm/wdatru.f
wdbtch.o: wdm/wdbtch.f wdm/.\cfbuff.inc wdm/.\cdrloc.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdbtch.f
wddlg.o: wdm/wddlg.f wdm/.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wddlg.f
wdexpt.o: wdm/wdexpt.f wdm/.\cfbuff.inc wdm/.\ctblab.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdexpt.f
wdimpt.o: wdm/wdimpt.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	wdm/wdimpt.f
wdmid.o: wdm/wdmid.f wdm/.\cwdmid.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdmid.f
wdsptm.o: wdm/wdsptm.f wdm/.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdsptm.f
wdtbl2.o: wdm/wdtbl2.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	wdm/wdtbl2.f
wdtble.o: wdm/wdtble.f wdm/.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdtble.f
wdtms1.o: wdm/wdtms1.f wdm/.\cfbuff.inc wdm/.\cwtsds.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdtms1.f
wdtms2.o: wdm/wdtms2.f wdm/.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdtms2.f
wdtms3.o: wdm/wdtms3.f wdm/.\cfbuff.inc wdm/.\cwtsds.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -Iwdm/.\	wdm/wdtms3.f
./ctsbuf.inc: wdm/.\ctsbuf.inc
	cp wdm/.\ctsbuf.inc .
./wdexpt.f: wdm/wdexpt.f
	cp wdm/wdexpt.f .
./wdtbl2.f: wdm/wdtbl2.f
	cp wdm/wdtbl2.f .
./wdimpt.f: wdm/wdimpt.f
	cp wdm/wdimpt.f .
./wdatrb.f: wdm/wdatrb.f
	cp wdm/wdatrb.f .
./wdsptm.f: wdm/wdsptm.f
	cp wdm/wdsptm.f .
./cdrloc.inc: wdm/.\cdrloc.inc
	cp wdm/.\cdrloc.inc .
./wddlg.f: wdm/wddlg.f
	cp wdm/wddlg.f .
./wdmid.f: wdm/wdmid.f
	cp wdm/wdmid.f .
./wdtble.f: wdm/wdtble.f
	cp wdm/wdtble.f .
./cwtsds.inc: wdm/.\cwtsds.inc
	cp wdm/.\cwtsds.inc .
./wdbtch.f: wdm/wdbtch.f
	cp wdm/wdbtch.f .
./wdatru.f: wdm/wdatru.f
	cp wdm/wdatru.f .
./cfbuff.inc: wdm/.\cfbuff.inc
	cp wdm/.\cfbuff.inc .
./ctblab.inc: wdm/.\ctblab.inc
	cp wdm/.\ctblab.inc .
./cwdmid.inc: wdm/.\cwdmid.inc
	cp wdm/.\cwdmid.inc .
./wdtms3.f: wdm/wdtms3.f
	cp wdm/wdtms3.f .
./wdtms2.f: wdm/wdtms2.f
	cp wdm/wdtms2.f .
./tsbufr.f: wdm/tsbufr.f
	cp wdm/tsbufr.f .
./wdtms1.f: wdm/wdtms1.f
	cp wdm/wdtms1.f .
./fversn.inc: wdm/.\fversn.inc
	cp wdm/.\fversn.inc .
./wdatm2.f: wdm/wdatm2.f
	cp wdm/wdatm2.f .
SRC = wdm/wdtble.f wdm/wdtms3.f wdm/wdtms2.f wdm/tsbufr.f wdm/wdsptm.f wdm/wdimpt.f wdm/wddlg.f wdm/wdatru.f wdm/wdbtch.f wdm/wdatm2.f wdm/wdtms1.f wdm/wdtbl2.f wdm/wdmid.f wdm/wdatrb.f wdm/wdexpt.f wdm/.\ctsbuf.inc wdm/.\cdrloc.inc wdm/.\cwtsds.inc wdm/.\cwdmid.inc wdm/.\ctblab.inc wdm/.\fversn.inc wdm/.\cfbuff.inc
OBJ = wdtble.o wdtms3.o wdtms2.o tsbufr.o wdsptm.o wdimpt.o wddlg.o wdatru.o wdbtch.o wdatm2.o wdtms1.o wdtbl2.o wdmid.o wdatrb.o wdexpt.o
OFF = wdm/.\ctsbuf.inc wdm/wdexpt.f wdm/wdtbl2.f wdm/wdimpt.f wdm/wdatrb.f wdm/wdsptm.f wdm/.\cdrloc.inc wdm/wddlg.f wdm/wdmid.f wdm/wdtble.f wdm/.\cwtsds.inc wdm/wdbtch.f wdm/wdatru.f wdm/.\cfbuff.inc wdm/.\ctblab.inc wdm/.\cwdmid.inc wdm/wdtms3.f wdm/wdtms2.f wdm/tsbufr.f wdm/wdtms1.f wdm/.\fversn.inc wdm/wdatm2.f
clean: neat
	-rm -f .libwdm.a.cppdefs $(OBJ) libwdm.a
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
libwdm.a: $(OBJ)
	$(AR) $(ARFLAGS) libwdm.a $(OBJ)
