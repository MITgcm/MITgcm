C $Header: /u/gcmpack/MITgcm/eesupp/inc/MAIN_PDIRECTIVES1.h,v 1.7 2005/11/07 18:16:08 cnh Exp $
C $Name:  $
CBOP
C     !ROUTINE: MAIN_PDIRECTIVES1.h
C     !INTERFACE:
C     include "MAIN_PDIRECTIVES1.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | MAIN\_PDIRECTIVES1.h                                       
C     *==========================================================*
C     | Parallel directives to generate multithreaded code for    
C     | various different compilers. The master preprocessor      
C     | file CPP\_OPTIONS is used to select which of these 
C     | options is included in the code.
C     *==========================================================*
CEOP

#ifdef USE_SOLARIS_THREADING
C--
C--  Parallel directives for SUN/Pro compiler.
C--
C     Parallel compiler directives for Solaris
C$PAR  DOALL
C$PAR& SHARED(nThreads)
C$PAR& ,PRIVATE(myThid)
C$PAR& ,SCHEDTYPE(SELF(1))
C
#endif

#define USE_KAP_THREADING
#ifdef USE_KAP_THREADING
C--
C--  Parallel directives for Kuck and Associates compiler.
C--  This is used to generate multi-threaded code on Digital 
C--  systems. It can also be used under NT.
C--
C      Parallel compiler directives for Digital with kap compiler.
C*KAP* PARALLEL REGION
C*KAP*& SHARED(nThreads,eeBootError,threadIsComplete)
C*KAP*& LOCAL(myThid,I)
C*KAP*  PARALLEL DO
C*KAP*& BLOCKED(1)
C
#endif

#ifdef USE_IRIX_THREADING
C--
C--  Parallel directives for MIPS Pro Fortran compiler
C--
C      Parallel compiler directives for SGI with IRIX
C$PAR  PARALLEL DO
C$PAR&  CHUNK=1,MP_SCHEDTYPE=INTERLEAVE,
C$PAR&  SHARE(nThreads),LOCAL(myThid,I)
C
#endif

#ifdef USE_EXEMPLAR_THREADING
C--
C--  Parallel directives for HP Exemplar Fortran compiler
C--
C      Parallel compiler directives for HP Exemplar
C$DIR loop_parallel
C$DIR loop_private (I,myThid)
#endif

#ifdef USE_C90_THREADING
C--
C--  Parallel directives for CRAY/SGI Fortan 90 compiler.
C--
CMIC$ DO ALL PRIVATE (I, myThid ) SHARED(nThreads)
CMIC$& SINGLE
#endif             


#ifdef USE_OMP_THREADING
C$OMP PARALLEL  SHARED(nThreads), PRIVATE(I,myThid)
#endif

