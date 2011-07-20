add_library(util STATIC
  dirlis_.c
  FileName.c
  getdir_.c
  Malloc.c
  utcpc.c
  ckfsdg.f
  ckfspc.f
  dttm90.f
  dttmdc.f
  dttmdg.f
  dttmpc.f
  dttmsg.f
  dttmsn.f
  uscnpc.f
  uscnux.f
  uscnvt.f
  utchar.f
  utcp90.f
  utcpgn.f
  utdate.f
  utgnrl.f
  utnumb.f
  utscdg.f
  utscdg_m.f
  utscin.f
  utscpc.f
  utscxx.f
  utscxxdl.f
  utscxxin.f
  utsort.f )

message ("platform: " ${PLATFORM_SUBDIR} )


