!BOP
! !ROUTINE: MAIN_PDIRECTIVES1.h
! !INTERFACE:
! include "MAIN_PDIRECTIVES1.h"
! !DESCRIPTION:
! *==========================================================*
! | MAIN\_PDIRECTIVES1.h
! *==========================================================*
! | Parallel directives to generate multithreaded code for
! | various different compilers. The master preprocessor
! | file CPP\_OPTIONS is used to select which of these
! | options is included in the code.
! *==========================================================*
!EOP

#ifdef USE_SOLARIS_THREADING
!--
!--  Parallel directives for SUN/Pro compiler.
!--
! Parallel compiler directives for Solaris
!$PAR  DOALL
!$PAR& SHARED(nThreads)
!$PAR& ,PRIVATE(myThid)
!$PAR& ,SCHEDTYPE(SELF(1))
!
#endif

#define USE_KAP_THREADING
#ifdef USE_KAP_THREADING
!--
!--  Parallel directives for Kuck and Associates compiler.
!--  This is used to generate multi-threaded code on Digital
!--  systems. It can also be used under NT.
!--
 ! Parallel compiler directives for Digital with kap compiler.
!*KAP* PARALLEL REGION
!*KAP*& SHARED(nThreads,eeBootError,threadIsComplete)
!*KAP*& LOCAL(myThid,I)
!*KAP*  PARALLEL DO
!*KAP*& BLOCKED(1)
!
#endif

#ifdef USE_IRIX_THREADING
!--
!--  Parallel directives for MIPS Pro Fortran compiler
!--
 ! Parallel compiler directives for SGI with IRIX
!$PAR  PARALLEL DO
!$PAR&  CHUNK=1,MP_SCHEDTYPE=INTERLEAVE,
!$PAR&  SHARE(nThreads),LOCAL(myThid,I)
!
#endif

#ifdef USE_EXEMPLAR_THREADING
!--
!--  Parallel directives for HP Exemplar Fortran compiler
!--
 ! Parallel compiler directives for HP Exemplar
!$DIR loop_parallel
!$DIR loop_private (I,myThid)
#endif

#ifdef USE_C90_THREADING
!--
!--  Parallel directives for CRAY/SGI Fortan 90 compiler.
!--
!MIC$ DO ALL PRIVATE (I, myThid ) SHARED(nThreads)
!MIC$& SINGLE
#endif


#ifdef USE_OMP_THREADING
!$OMP PARALLEL  SHARED(nThreads), PRIVATE(I,myThid)
#endif

