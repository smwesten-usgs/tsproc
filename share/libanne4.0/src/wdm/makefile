# Makefile created by mkmf.pl $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 



include ../../../mkmf_templates/mingw_gfortran.tpl


.DEFAULT:
	-echo $@ does not exist.
all: libwdm.a
tsbufr.o: ./tsbufr.f ./.\ctsbuf.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./tsbufr.f
wdatm2.o: ./wdatm2.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./wdatm2.f
wdatrb.o: ./wdatrb.f ./.\cfbuff.inc ./.\cdrloc.inc ./.\fversn.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdatrb.f
wdatru.o: ./wdatru.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./wdatru.f
wdbtch.o: ./wdbtch.f ./.\cfbuff.inc ./.\cdrloc.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdbtch.f
wddlg.o: ./wddlg.f ./.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wddlg.f
wdexpt.o: ./wdexpt.f ./.\cfbuff.inc ./.\ctblab.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdexpt.f
wdimpt.o: ./wdimpt.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./wdimpt.f
wdmid.o: ./wdmid.f ./.\cwdmid.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdmid.f
wdsptm.o: ./wdsptm.f ./.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdsptm.f
wdtbl2.o: ./wdtbl2.f
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./wdtbl2.f
wdtble.o: ./wdtble.f ./.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdtble.f
wdtms1.o: ./wdtms1.f ./.\cfbuff.inc ./.\cwtsds.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdtms1.f
wdtms2.o: ./wdtms2.f ./.\cfbuff.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdtms2.f
wdtms3.o: ./wdtms3.f ./.\cfbuff.inc ./.\cwtsds.inc
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c -I./.\	./wdtms3.f
./fversn.inc: ./.\fversn.inc
	cp ./.\fversn.inc .
./cdrloc.inc: ./.\cdrloc.inc
	cp ./.\cdrloc.inc .
./ctsbuf.inc: ./.\ctsbuf.inc
	cp ./.\ctsbuf.inc .
./ctblab.inc: ./.\ctblab.inc
	cp ./.\ctblab.inc .
./cwtsds.inc: ./.\cwtsds.inc
	cp ./.\cwtsds.inc .
./cwdmid.inc: ./.\cwdmid.inc
	cp ./.\cwdmid.inc .
./cfbuff.inc: ./.\cfbuff.inc
	cp ./.\cfbuff.inc .
SRC = ./wdtble.f ./wdtms3.f ./wdtms2.f ./tsbufr.f ./wdsptm.f ./wdimpt.f ./wddlg.f ./wdatru.f ./wdbtch.f ./wdatm2.f ./wdtms1.f ./wdtbl2.f ./wdmid.f ./wdatrb.f ./wdexpt.f ./.\fversn.inc ./.\cdrloc.inc ./.\ctsbuf.inc ./.\ctblab.inc ./.\cwtsds.inc ./.\cwdmid.inc ./.\cfbuff.inc
OBJ = wdtble.o wdtms3.o wdtms2.o tsbufr.o wdsptm.o wdimpt.o wddlg.o wdatru.o wdbtch.o wdatm2.o wdtms1.o wdtbl2.o wdmid.o wdatrb.o wdexpt.o
OFF = ./.\fversn.inc ./.\cdrloc.inc ./.\ctsbuf.inc ./.\ctblab.inc ./.\cwtsds.inc ./.\cwdmid.inc ./.\cfbuff.inc
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
