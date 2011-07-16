add_library(wdm STATIC
    tsbufr.f 
    wdatm2.f
    wdatrb.f
    wdatru.f
    wdbtch.f
    wddlg.f
    wdexpt.f
    wdimpt.f
    wdmid.f
    wdsptm.f
    wdtbl2.f 
    wdtble.f
    wdtms1.f
    wdtms2.f
    wdtms3.f )

#set_target_properties(swblib
#          PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

target_link_libraries(wdm ${EXTRA_LIBS})
