C $Header: /u/gcmpack/MITgcm/eesupp/inc/DEF_IN_MAKEFILE.h,v 1.3 2012/10/26 18:50:17 jmc Exp $
C $Name:  $

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
#undef TIME_PER_TIMESTEP_SFP
#undef TIME_PER_TIMESTEP
#undef USE_PAPI_FLOPS_SFP
#undef USE_PAPI_FLOPS
#undef USE_PCL_FLOPS_SFP
#undef USE_PCL_FLOPS
#undef USE_PAPI
#undef USE_PCL
#undef USE_LIBHPM
#undef USE_GSL_IEEE
#undef HAVE_SYSTEM
#undef HAVE_FDATE
#undef HAVE_ETIME
#undef HAVE_CLOC
#undef HAVE_SETRLSTK
#undef HAVE_SIGREG
#undef HAVE_STAT
#undef HAVE_NETCDF
#undef HAVE_LAPACK
#undef HAVE_FLUSH

C-- options that can be set in Makefile by the OPTFILE
C-  platform specific options:
#undef TARGET_AIX
#undef TARGET_BGL
#undef TARGET_CRAY_VECTOR
#undef TARGET_CRAYXT
#undef TARGET_DEC
#undef TARGET_LAM
#undef TARGET_NEC_SX
#undef TARGET_NEC_VECTOR
#undef TARGET_PWR3
#undef TARGET_SGI
#undef TARGET_T3E
C-  others options found in optfiles in dir tools/build_options:
#undef _BYTESWAPIO
#undef ALWAYS_USE_MPI
#undef CG2D_OUTERLOOPITERS
#undef CG3D_OUTERLOOPITERS
#undef IFORT
#undef NML_EXTENDED_F77
#undef NML_TERMINATOR
#undef SEAICE_VECTORIZE_LSR
#undef WORDLENGTH

C-----  WARNING: DO NOT include this file in any source code -----
