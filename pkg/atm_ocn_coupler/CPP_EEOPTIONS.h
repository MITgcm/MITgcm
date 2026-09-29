#ifndef _CPP_EEOPTIONS_H_
#define _CPP_EEOPTIONS_H_

CBOP
C     !ROUTINE: CPP_EEOPTIONS.h
C     !INTERFACE:
C     include "CPP_EEOPTIONS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP\_EEOPTIONS.h                                         |
C     *==========================================================*
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
C     *==========================================================*
CEOP

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

C=== Macro related options ===
C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working set size.
C     However, on vector CRAY systems this degrades performance.
C     Enable to switch REAL4_IS_SLOW from genmake2 (with LET_RS_BE_REAL4):
#ifdef LET_RS_BE_REAL4
#undef REAL4_IS_SLOW
#else /* LET_RS_BE_REAL4 */
#define REAL4_IS_SLOW
#endif /* LET_RS_BE_REAL4 */

C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16
#define D0 d0

C=== IO related options ===
C--   Flag used to indicate whether Fortran formatted write
C     and read are threadsafe. On SGI the routines can be thread
C     safe, on Sun it is not possible - if you are unsure then
C     undef this option.
#undef FMTFTN_IO_THREAD_SAFE

C--   Flag used to indicate whether Binary write to Local file (i.e.,
C     a different file for each tile) and read are thread-safe.
#undef LOCBIN_IO_THREAD_SAFE

C--   Flag to turn off the writing of error message to ioUnit zero
#undef DISABLE_WRITE_TO_UNIT_ZERO

C--   Flag to turn on old default of opening scratch files with the
C     STATUS='SCRATCH' option. This method, while perfectly FORTRAN-standard,
C     caused filename conflicts on some multi-node/multi-processor platforms
C     in the past and has been replace by something (hopefully) more robust.
#undef USE_FORTRAN_SCRATCH_FILES

C--   Flag defined for eeboot_minimal.F, eeset_parms.F and open_copy_data_file.F
C     to write STDOUT, STDERR and scratch files from process 0 only.
C WARNING: to use only when absolutely confident that the setup is working
C     since any message (error/warning/print) from any proc <> 0 will be lost.
#undef SINGLE_DISK_IO

C=== MPI, EXCH and GLOBAL_SUM related options ===
C--   Flag turns off MPI_SEND ready_to_receive polling in the
C     gather_* subroutines to speed up integrations.
#undef DISABLE_MPI_READY_TO_RECEIVE

C--   Control use of communication that might overlap computation.
C     Under MPI selects/deselects "non-blocking" sends and receives.
#undef  ALLOW_ASYNC_COMMUNICATION
#undef  ALWAYS_USE_ASYNC_COMMUNICATION
C--   Control use of communication that is atomic to computation.
C     Under MPI selects/deselects "blocking" sends and receives.
#define ALLOW_SYNC_COMMUNICATION
#undef  ALWAYS_USE_SYNC_COMMUNICATION

C--   Control XY periodicity in processor to grid mappings
C     Note: Model code does not need to know whether a domain is
C           periodic because it has overlap regions for every box.
C           Model assume that these values have been
C           filled in some way.
#undef  ALWAYS_PREVENT_X_PERIODICITY
#undef  ALWAYS_PREVENT_Y_PERIODICITY
#define CAN_PREVENT_X_PERIODICITY
#define CAN_PREVENT_Y_PERIODICITY

C--   disconnect tiles (no exchange between tiles, just fill-in edges
C     assuming locally periodic subdomain)
#undef DISCONNECTED_TILES

C--   Always cumulate tile local-sum in the same order by applying MPI allreduce
C     to array of tiles ; can get slower with large number of tiles (big set-up)
#define GLOBAL_SUM_ORDER_TILES

C--   Alternative way of doing global sum without MPI allreduce call
C     but instead, explicit MPI send & recv calls. Expected to be slower.
#undef GLOBAL_SUM_SEND_RECV

C--   Alternative way of doing global sum on a single CPU
C     to eliminate tiling-dependent roundoff errors. Note: This is slow.
#undef CG2D_SINGLECPU_SUM

C=== Other options (to add/remove pieces of code) ===
C--   Flag to turn on checking for errors from all threads and procs
C     (calling S/R STOP_IF_ERROR) before stopping.
#define USE_ERROR_STOP

C--   Control use of communication with other component:
C     allow to import and export from/to Coupler interface.
#undef COMPONENT_MODULE

C--   Activate some pieces of code for coupling to GEOS AGCM
#undef HACK_FOR_GMAO_CPL

C=== And define Macros ===
c#include "CPP_EEMACROS.h"
C--- Taken from file "CPP_EEMACROS.h":

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
#define _BEGIN_MASTER(a)  IF ( a .EQ. 1 ) THEN
#define _END_MASTER(a)    ENDIF

C- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
C  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
C  enable to call the corresponding R4 or R8 S/R.
#ifdef REAL4_IS_SLOW

#define _RS  Real*8
#define RS_IS_REAL8
#define _GLOBAL_SUM_RS(a,b)    CALL GLOBAL_SUM_R8( a, b )
#define _GLOBAL_MAX_RS(a,b)    CALL GLOBAL_MAX_R8( a, b )
#define _MPI_TYPE_RS MPI_DOUBLE_PRECISION

#else /* REAL4_IS_SLOW */

#define _RS  Real*4
#define RS_IS_REAL4
#define _GLOBAL_SUM_RS(a,b)    CALL GLOBAL_SUM_R4( a, b )
#define _GLOBAL_MAX_RS(a,b)    CALL GLOBAL_MAX_R4( a, b )
#define _MPI_TYPE_RS MPI_REAL

#endif /* REAL4_IS_SLOW */

#define _RL  Real*8
#define RL_IS_REAL8
#define _GLOBAL_SUM_RL(a,b)    CALL GLOBAL_SUM_R8( a, b )
#define _GLOBAL_MAX_RL(a,b)    CALL GLOBAL_MAX_R8( a, b )
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

#endif /* _CPP_EEOPTIONS_H_ */
