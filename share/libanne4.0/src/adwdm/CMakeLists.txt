add_library(adwdm STATIC
  adutil.f
  ehutlf90.f
  ehutlh.f
  ehutot.f
  msexpt.f
  msimpt.f
  msopen.f
  prtfil.f
  usyslh.f
  usysot.f
  usyspc.f
  usyspr.f
  usysux.f
  usysvx.f
  utwdmd.f
  utwdmf.f
  utwdt1.f
  wdatm1.f
  wdmchk.f
  wdmess.f
  wdoppr.f
  wdopux.f
  ztwdmf.f )

#set_target_properties(swblib
#          PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

target_link_libraries(wdm ${EXTRA_LIBS})
