C $Header: /u/gcmpack/MITgcm/verification/hs94.128x64x5/code/Attic/CPP_EEOPTIONS.h,v 1.3 2001/02/04 14:38:52 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | CPP_EEOPTIONS.h                                          |
C     |==========================================================|
C     | C preprocessor "execution environment" supporting        |
C     | flags. Use this file to set flags controlling the        |
C     | execution environment in which a model runs - as opposed |
C     | to the dynamical problem the model solves.               |
C     | Note: Many options are implemented with both compile time|
C     |       and run-time switches. This allows options to be   |
C     |       removed altogether, made optional at run-time or   |
C     |       to be permanently enabled. This convention helps   |
C     |       with the data-dependence analysis performed by the |
C     |       adjoint model compiler. This data dependency       |
C     |       analysis can be upset by runtime switches that it  |
C     |       is unable to recoginise as being fixed for the     |
C     |       duration of an integration.                        |
C     |       A reasonable way to use these flags is to          |
C     |       set all options as selectable at runtime but then  |
C     |       once an experimental configuration has been        |
C     |       identified, rebuild the code with the appropriate  |
C     |       options set at compile time.                       |
C     \==========================================================/

#ifndef _CPP_EEOPTIONS_H_
#define _CPP_EEOPTIONS_H_

C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C     Flag used to indicate whether Fortran formatted write
C     and read are threadsafe. On SGI the routines can be thread
C     safe, on Sun it is not possible - if you are unsure then
C     undef this option.
#undef  FMTFTN_IO_THREADSAFE

C     Flag used to indicate which flavour of multi-threading
C     compiler directives to use. Only set one of these.
C     USE_SOLARIS_THREADING  - Takes directives for SUN Workshop
C                              compiler.
C     USE_KAP_THREADING      - Takes directives for Kuck and 
C                              Associates multi-threading compiler
C                              ( used on Digital platforms ).
C     USE_IRIX_THREADING     - Takes directives for SGI MIPS
C                              Pro Fortran compiler.
C     USE_EXEMPLAR_THREADING - Takes directives for HP SPP series
C                              compiler.
C     USE_C90_THREADING      - Takes directives for CRAY/SGI C90
C                              system F90 compiler.
#ifdef TARGET_SUN
#define USE_SOLARIS_THREADING
#endif

#ifdef TARGET_DEC
#define USE_KAP_THREADING
#endif

#ifdef TARGET_SGI
#define USE_IRIX_THREADING
#endif

#ifdef TARGET_HP
#define USE_EXEMPLAR_THREADING
#endif

#ifdef TARGET_CRAY_VECTOR
#define USE_C90_THREADING
#endif

C--   Define the mapping for the _BARRIER macro
C     On some systems low-level hardware support can be accessed through
C     compiler directives here.
#define _BARRIER CALL BARRIER(myThid)

C--   Define the mapping for the BEGIN_CRIT() and  END_CRIT() macros. 
C     On some systems we simply execute this section only using the
C     master thread i.e. its not really a critical section. We can
C     do this because we do not use critical sections in any critical
C     sections of our code!
#define _BEGIN_CRIT(a) _BEGIN_MASTER(a)
#define _END_CRIT(a)   _END_MASTER(a)

C--   Define the mapping for the BEGIN_MASTER_SECTION() and
C     END_MASTER_SECTION() macros. These are generally implemented by
C     simply choosing a particular thread to be "the master" and have
C     it alone execute the BEGIN_MASTER..., END_MASTER.. sections.
#define _BEGIN_MASTER(a)  IF ( a .EQ. 1 ) THEN
#define _END_MASTER(a)    ENDIF

C--   Control MPI based parallel processing
#undef  ALLOW_USE_MPI
#undef  ALWAYS_USE_MPI
 
C--   Control use of communication that might overlap computation.
C     Under MPI selects/deselects "non-blocking" sends and receives.
#define ALLOW_ASYNC_COMMUNICATION
#undef  ALLOW_ASYNC_COMMUNICATION
#undef  ALWAYS_USE_ASYNC_COMMUNICATION
C--   Control use of communication that is atomic to computation.
C     Under MPI selects/deselects "blocking" sends and receives.
#define ALLOW_SYNC_COMMUNICATION
#undef  ALWAYS_USE_SYNC_COMMUNICATION

#undef  LETS_MAKE_JAM
#undef  JAM_WITH_TWO_PROCS_PER_NODE
#ifdef LETS_MAKE_JAM
#define _JAMEXT _jam
#else
#define _JAMEXT
#endif

C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working
C     set size. However, on vector CRAY systems this degrades
C     performance.
#define REAL4_IS_SLOW
 
#ifdef REAL4_IS_SLOW
#define _RS  Real*8
#define _RL  Real*8
#define RS_IS_REAL8
#define _EXCH_XY_R4(a,b)       CALL EXCH_XY_R8 _JAMEXT ( a, b )
#define _EXCH_XYZ_R4(a,b)      CALL EXCH_XYZ_R8 _JAMEXT ( a, b )
#define _GLOBAL_SUM_R4(a,b)    CALL GLOBAL_SUM_R8 _JAMEXT ( a, b )
#define _GLOBAL_MAX_R4(a,b)    CALL GLOBAL_MAX_R8 ( a, b )
#endif

#ifndef REAL4_IS_SLOW
#define _RS  Real*4
#define _RL  Real*8
#define RS_IS_REAL4
#define _EXCH_XY_R4(a,b)       CALL EXCH_XY_R4 ( a, b )
#define _EXCH_XYZ_R4(a,b)      CALL EXCH_XYZ_R4 ( a, b )
#define _GLOBAL_SUM_R4(a,b)    CALL GLOBAL_SUM_R4( a, b )
#define _GLOBAL_MAX_R4(a,b)    CALL GLOBAL_MAX_R4( a, b )
#endif
 
#define _EXCH_XY_R8(a,b)       CALL EXCH_XY_R8 _JAMEXT ( a, b )
#define _EXCH_XYZ_R8(a,b)      CALL EXCH_XYZ_R8 _JAMEXT ( a, b )
#define _GLOBAL_SUM_R8(a,b)    CALL GLOBAL_SUM_R8 _JAMEXT ( a, b )
#define _GLOBAL_MAX_R8(a,b)    CALL GLOBAL_MAX_R8 ( a, b )
 
C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16
#define D0 d0
#ifdef REAL_D0_IS_16BYTES
#define D0
#endif

C--   Control XY periodicity in processor to grid mappings
C     Note: Model code does not need to know whether a domain is 
C           periodic because it has overlap regions for every box.
C           Model assume that these values have been
C           filled in some way.
#undef  ALWAYS_PREVENT_X_PERIODICITY
#undef  ALWAYS_PREVENT_Y_PERIODICITY
#define CAN_PREVENT_X_PERIODICITY
#define CAN_PREVENT_Y_PERIODICITY

C--   Substitue for 1.D variables
C     Sun compilers do not use 8-byte precision for literals
C     unless .Dnn is specified. CRAY vector machines use 16-byte
C     precision when they see .Dnn which runs very slowly!
#ifdef REAL_D0_IS_16BYTES
#define _d
#define _F64( a ) a
#endif
#ifndef REAL_D0_IS_16BYTES
#define _d D
#define _F64( a ) DFLOAT( a )
#endif

#endif /* _CPP_EEOPTIONS_H_ */
