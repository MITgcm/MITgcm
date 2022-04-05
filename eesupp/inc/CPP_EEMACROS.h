CBOP
C     !ROUTINE: CPP_EEMACROS.h
C     !INTERFACE:
C     include "CPP_EEMACROS.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP_EEMACROS.h
C     *==========================================================*
C     | C preprocessor "execution environment" supporting
C     | macros. Use this file to define macros for  simplifying
C     | execution environment in which a model runs - as opposed
C     | to the dynamical problem the model solves.
C     *==========================================================*
CEOP

#ifndef _CPP_EEMACROS_H_
#define _CPP_EEMACROS_H_

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
#define USING_THREADS
#endif

#ifdef TARGET_DEC
#define USE_KAP_THREADING
#define USING_THREADS
#endif

#ifdef TARGET_SGI
#define USE_IRIX_THREADING
#define USING_THREADS
#endif

#ifdef TARGET_HP
#define USE_EXEMPLAR_THREADING
#define USING_THREADS
#endif

#ifdef TARGET_CRAY_VECTOR
#define USE_C90_THREADING
#define USING_THREADS
#endif

#ifdef USE_OMP_THREADING
#define USING_THREADS
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

#define _BEGIN_MASTER(a) IF ( a .EQ. 1 ) THEN
#define _END_MASTER(a)   ENDIF
CcnhDebugStarts
C      Alternate form to the above macros that increments (decrements) a counter each
C      time a MASTER section is entered (exited). This counter can then be checked in barrier
C      to try and detect calls to BARRIER within single threaded sections.
C      Using these macros requires two changes to Makefile - these changes are written
C      below.
C      1 - add a filter to the CPP command to kill off commented _MASTER lines
C      2 - add a filter to the CPP output the converts the string N EWLINE to an actual newline.
C      The N EWLINE needs to be changes to have no space when this macro and Makefile changes
C      are used. Its in here with a space to stop it getting parsed by the CPP stage in these
C      comments.
C      #define _BEGIN_MASTER(a)  IF ( a .EQ. 1 ) THEN  N EWLINE      CALL BARRIER_MS(a)
C      #define _END_MASTER(a)    CALL BARRIER_MU(a) N EWLINE        ENDIF
C      'CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  grep -v '^[cC].*_MASTER' | cpp  -traditional -P'
C      .F.f:
C      $(CPP) $(DEFINES) $(INCLUDES) |  sed 's/N EWLINE/\n/' > $@
CcnhDebugEnds

C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working
C     set size. However, on vector CRAY systems this degrades
C     performance.
C- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
C  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
C  enable to call the corresponding R4 or R8 S/R.
#ifdef REAL4_IS_SLOW
#define _RS Real*8
#define RS_IS_REAL8
#define _GLOBAL_SUM_RS(a,b) CALL GLOBAL_SUM_R8 ( a, b)
#define _GLOBAL_MAX_RS(a,b) CALL GLOBAL_MAX_R8 ( a, b )
#define _MPI_TYPE_RS MPI_DOUBLE_PRECISION
#else
#define _RS Real*4
#define RS_IS_REAL4
#define _GLOBAL_SUM_RS(a,b) CALL GLOBAL_SUM_R4 ( a, b )
#define _GLOBAL_MAX_RS(a,b) CALL GLOBAL_MAX_R4 ( a, b )
#define _MPI_TYPE_RS MPI_REAL
#endif

#define _RL Real*8
#define RL_IS_REAL8
#define _GLOBAL_SUM_RL(a,b) CALL GLOBAL_SUM_R8 ( a, b )
#define _GLOBAL_MAX_RL(a,b) CALL GLOBAL_MAX_R8 ( a, b )
#define _MPI_TYPE_RL MPI_DOUBLE_PRECISION

#define _MPI_TYPE_R4 MPI_REAL
#if (defined (TARGET_SGI) || defined (TARGET_AIX) || defined (TARGET_LAM))
#define _MPI_TYPE_R8 MPI_DOUBLE_PRECISION
#else
#define _MPI_TYPE_R8 MPI_REAL8
#endif
#define _R4 Real*4
#define _R8 Real*8

C- Note: a) exch macros were used to switch to  JAM routines (obsolete)
C        b) exch R4 & R8 macros are not practically used ; if needed,
C           will directly call the corrresponding S/R.
#define _EXCH_XY_RS(a,b) CALL EXCH_XY_RS ( a, b )
#define _EXCH_XY_RL(a,b) CALL EXCH_XY_RL ( a, b )
#define _EXCH_XYZ_RS(a,b) CALL EXCH_XYZ_RS ( a, b )
#define _EXCH_XYZ_RL(a,b) CALL EXCH_XYZ_RL ( a, b )

C--   Control use of JAM routines for Artic network (no longer supported)
C     These invoke optimized versions of "exchange" and "sum" that
C     utilize the programmable aspect of Artic cards.
CXXX No longer supported ; started to remove JAM routines.
CXXX #ifdef LETS_MAKE_JAM
CXXX #define _GLOBAL_SUM_RS(a,b) CALL GLOBAL_SUM_R8_JAM ( a, b)
CXXX #define _GLOBAL_SUM_RL(a,b) CALL GLOBAL_SUM_R8_JAM ( a, b )
CXXX #define _EXCH_XY_RS(a,b) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define _EXCH_XY_RL(a,b) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define _EXCH_XYZ_RS(a,b) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #define _EXCH_XYZ_RL(a,b) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #endif

C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16
#ifdef REAL_D0_IS_16BYTES
#define D0
#endif

C--   Substitue for 1.D variables
C     Sun compilers do not use 8-byte precision for literals
C     unless .Dnn is specified. CRAY vector machines use 16-byte
C     precision when they see .Dnn which runs very slowly!
#ifdef REAL_D0_IS_16BYTES
#define _F64( a ) a
#endif
#ifndef REAL_D0_IS_16BYTES
#define _F64( a ) DFLOAT( a )
#endif

C--   Set the format for writing processor IDs, e.g. in S/R eeset_parms
C     and S/R open_copy_data_file. The default of I9.9 should work for
C     a long time (until we will use 10e10 processors and more)
#define FMT_PROC_ID 'I9.9'

C--   Set the format for writing ensemble task IDs in S/R eeset_parms
C     and S/R open_copy_data_file.
#define FMT_TSK_ID 'I6.6'

C--   Set ACTION= in OPEN instruction for input file (before doing IO)
C     leave it empty (if EXCLUDE_OPEN_ACTION) or set it to proper value
#ifdef EXCLUDE_OPEN_ACTION
# define _READONLY_ACTION
#else
# define _READONLY_ACTION ACTION='read',
#endif

#endif /* _CPP_EEMACROS_H_ */
