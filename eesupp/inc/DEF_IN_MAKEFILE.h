C=====  WARNING: DO NOT include this file in any source code =====
CBOP
C     !DESCRIPTION:
C     *==========================================================*
C     | This file provides a list of CPP-options that can be set
C     | by the Makefile, through the variable $DEFINES which is
C     | passed directly to the pre-processor command (CPP).
C     *==========================================================*
CEOP

C-- options set in Makefile by genmake2:
#undef ALLOW_AIM
#undef IGNORE_TIME
#undef ALLOW_USE_MPI
#undef USE_OMP_THREADING
#undef LET_RS_BE_REAL4
#undef TIME_PER_TIMESTEP
#undef TIME_PER_TIMESTEP_SFP
#undef USE_PAPI
#undef USE_PAPI_FLOPS
#undef USE_PAPI_FLOPS_SFP
#undef USE_PCL
#undef USE_PCL_FLOPS
#undef USE_PCL_FLOPS_SFP
#undef USE_LIBHPM
#undef USE_GSL_IEEE
#undef HAVE_SYSTEM
#undef HAVE_FDATE
#undef HAVE_ETIME_FCT
#undef HAVE_ETIME_SBR
#undef HAVE_CLOC
#undef HAVE_SETRLSTK
#undef HAVE_SIGREG
#undef HAVE_STAT
#undef HAVE_NETCDF
#undef HAVE_LAPACK
#undef HAVE_FLUSH

C-- options that can be set in Makefile by the OPTFILE
#undef HAVE_PTHREADS
C-  platform specific options:
#undef TARGET_AIX
#undef TARGET_BGL
#undef TARGET_CRAY_VECTOR
#undef TARGET_DEC
#undef TARGET_HP
#undef TARGET_LAM
#undef TARGET_NEC_SX
#undef TARGET_NEC_VECTOR
#undef TARGET_PWR3
#undef TARGET_SGI
#undef TARGET_SUN
#undef TARGET_T3E
C-  compiler/platform I/O specific options:
#undef _BYTESWAPIO
#undef EXCLUDE_OPEN_ACTION
#undef NML_EXTENDED_F77
#undef NML_TERMINATOR
#undef WORDLENGTH
C-  others options found in optfiles in dir tools/build_options:
#undef ALWAYS_USE_MPI
#undef AUTODIFF_USE_MDSFINDUNITS
#undef PROFILES_USE_MDSFINDUNITS
#undef CG2D_OUTERLOOPITERS
#undef CG3D_OUTERLOOPITERS
#undef INTEL_COMMITQQ

C-----  WARNING: DO NOT include this file in any source code -----
