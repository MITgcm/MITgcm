!BOP
! !ROUTINE: CPP_EEMACROS.h
! !INTERFACE:
! include "CPP_EEMACROS.h"
! !DESCRIPTION:
! *==========================================================*
! | CPP_EEMACROS.h
! *==========================================================*
! | C preprocessor "execution environment" supporting
! | macros. Use this file to define macros for  simplifying
! | execution environment in which a model runs - as opposed
! | to the dynamical problem the model solves.
! *==========================================================*
!EOP

#ifndef _CPP_EEMACROS_H_
#define _CPP_EEMACROS_H_

! In general the following convention applies:
! ALLOW  - indicates an feature will be included but it may
! CAN      have a run-time flag to allow it to be switched
!          on and off.
!          If ALLOW or CAN directives are "undef'd" this generally
!          means that the feature will not be available i.e. it
!          will not be included in the compiled code and so no
!          run-time option to use the feature will be available.
!
! ALWAYS - indicates the choice will be fixed at compile time
!          so no run-time option will be present

! Flag used to indicate which flavour of multi-threading
! compiler directives to use. Only set one of these.
! USE_SOLARIS_THREADING  - Takes directives for SUN Workshop
!                          compiler.
! USE_KAP_THREADING      - Takes directives for Kuck and
!                          Associates multi-threading compiler
!                          ( used on Digital platforms ).
! USE_IRIX_THREADING     - Takes directives for SGI MIPS
!                          Pro Fortran compiler.
! USE_EXEMPLAR_THREADING - Takes directives for HP SPP series
!                          compiler.
! USE_C90_THREADING      - Takes directives for CRAY/SGI C90
!                          system F90 compiler.
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

!--   Define the mapping for the _BARRIER macro
! On some systems low-level hardware support can be accessed through
! compiler directives here.
#define _BARRIER CALL BARRIER(myThid)

!--   Define the mapping for the BEGIN_CRIT() and  END_CRIT() macros.
! On some systems we simply execute this section only using the
! master thread i.e. its not really a critical section. We can
! do this because we do not use critical sections in any critical
! sections of our code!
#define _BEGIN_CRIT(a) _BEGIN_MASTER(a)
#define _END_CRIT(a)   _END_MASTER(a)

!--   Define the mapping for the BEGIN_MASTER_SECTION() and
! END_MASTER_SECTION() macros. These are generally implemented by
! simply choosing a particular thread to be "the master" and have
! it alone execute the BEGIN_MASTER..., END_MASTER.. sections.

#define _BEGIN_MASTER(a) IF ( a .EQ. 1 ) THEN
#define _END_MASTER(a)   ENDIF
!cnhDebugStarts
 ! Alternate form to the above macros that increments (decrements) a counter each
 ! time a MASTER section is entered (exited). This counter can then be checked in barrier
 ! to try and detect calls to BARRIER within single threaded sections.
 ! Using these macros requires two changes to Makefile - these changes are written
 ! below.
 ! 1 - add a filter to the CPP command to kill off commented _MASTER lines
 ! 2 - add a filter to the CPP output the converts the string N EWLINE to an actual newline.
 ! The N EWLINE needs to be changes to have no space when this macro and Makefile changes
 ! are used. Its in here with a space to stop it getting parsed by the CPP stage in these
 ! comments.
 ! #define _BEGIN_MASTER(a)  IF ( a .EQ. 1 ) THEN  N EWLINE      CALL BARRIER_MS(a)
 ! #define _END_MASTER(a)    CALL BARRIER_MU(a) N EWLINE        ENDIF
 ! 'CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  grep -v '^[cC].*_MASTER' | cpp  -traditional -P'
 ! .F.f:
 ! $(CPP) $(DEFINES) $(INCLUDES) |  sed 's/N EWLINE/\n/' > $@
!cnhDebugEnds

!--   Control storage of floating point operands
! On many systems it improves performance only to use
! 8-byte precision for time stepped variables.
! Constant in time terms ( geometric factors etc.. )
! can use 4-byte precision, reducing memory utilisation and
! boosting performance because of a smaller working
! set size. However, on vector CRAY systems this degrades
! performance.
!- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
!  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
!  enable to call the corresponding R4 or R8 S/R.
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

!- Note: a) exch macros were used to switch to  JAM routines (obsolete)
   ! b) exch R4 & R8 macros are not practically used ; if needed,
   !    will directly call the corrresponding S/R.
#define _EXCH_XY_RS(a,b) CALL EXCH_XY_RS ( a, b )
#define _EXCH_XY_RL(a,b) CALL EXCH_XY_RL ( a, b )
#define _EXCH_XYZ_RS(a,b) CALL EXCH_XYZ_RS ( a, b )
#define _EXCH_XYZ_RL(a,b) CALL EXCH_XYZ_RL ( a, b )

!--   Control use of JAM routines for Artic network (no longer supported)
! These invoke optimized versions of "exchange" and "sum" that
! utilize the programmable aspect of Artic cards.
!XXX No longer supported ; started to remove JAM routines.
!XXX #ifdef LETS_MAKE_JAM
!XXX #define _GLOBAL_SUM_RS(a,b) CALL GLOBAL_SUM_R8_JAM ( a, b)
!XXX #define _GLOBAL_SUM_RL(a,b) CALL GLOBAL_SUM_R8_JAM ( a, b )
!XXX #define _EXCH_XY_RS(a,b) CALL EXCH_XY_R8_JAM ( a, b )
!XXX #define _EXCH_XY_RL(a,b) CALL EXCH_XY_R8_JAM ( a, b )
!XXX #define _EXCH_XYZ_RS(a,b) CALL EXCH_XYZ_R8_JAM ( a, b )
!XXX #define _EXCH_XYZ_RL(a,b) CALL EXCH_XYZ_R8_JAM ( a, b )
!XXX #endif

!--   Control use of "double" precision constants.
! Use D0 where it means REAL*8 but not where it means REAL*16
#ifdef REAL_D0_IS_16BYTES
#define D0
#endif

!--   Substitue for 1.D variables
! Sun compilers do not use 8-byte precision for literals
! unless .Dnn is specified. CRAY vector machines use 16-byte
! precision when they see .Dnn which runs very slowly!
#ifdef REAL_D0_IS_16BYTES
#define _F64( a ) a
#endif
#ifndef REAL_D0_IS_16BYTES
#define _F64( a ) DFLOAT( a )
#endif

!--   Set the format for writing processor IDs, e.g. in S/R eeset_parms
! and S/R open_copy_data_file. The default of I9.9 should work for
! a long time (until we will use 10e10 processors and more)
#define FMT_PROC_ID 'I9.9'

!--   Set the format for writing ensemble task IDs in S/R eeset_parms
! and S/R open_copy_data_file.
#define FMT_TSK_ID 'I6.6'

!--   Set ACTION= in OPEN instruction for input file (before doing IO)
! leave it empty (if EXCLUDE_OPEN_ACTION) or set it to proper value
#ifdef EXCLUDE_OPEN_ACTION
# define _READONLY_ACTION
#else
# define _READONLY_ACTION ACTION='read',
#endif

#endif /* _CPP_EEMACROS_H_ */
