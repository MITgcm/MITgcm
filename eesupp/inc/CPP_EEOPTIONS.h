#ifndef _CPP_EEOPTIONS_H_
#define _CPP_EEOPTIONS_H_

!BOP
! !ROUTINE: CPP_EEOPTIONS.h
! !INTERFACE:
! include "CPP_EEOPTIONS.h"
!
! !DESCRIPTION:
! *==========================================================*
! | CPP\_EEOPTIONS.h                                         |
! *==========================================================*
! | C preprocessor "execution environment" supporting        |
! | flags. Use this file to set flags controlling the        |
! | execution environment in which a model runs - as opposed |
! | to the dynamical problem the model solves.               |
! | Note: Many options are implemented with both compile time|
! |       and run-time switches. This allows options to be   |
! |       removed altogether, made optional at run-time or   |
! |       to be permanently enabled. This convention helps   |
! |       with the data-dependence analysis performed by the |
! |       adjoint model compiler. This data dependency       |
! |       analysis can be upset by runtime switches that it  |
! |       is unable to recoginise as being fixed for the     |
! |       duration of an integration.                        |
! |       A reasonable way to use these flags is to          |
! |       set all options as selectable at runtime but then  |
! |       once an experimental configuration has been        |
! |       identified, rebuild the code with the appropriate  |
! |       options set at compile time.                       |
! *==========================================================*
!EOP

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

!=== Macro related options ===
!--   Control storage of floating point operands
! On many systems it improves performance only to use
! 8-byte precision for time stepped variables.
! Constant in time terms ( geometric factors etc.. )
! can use 4-byte precision, reducing memory utilisation and
! boosting performance because of a smaller working set size.
! However, on vector CRAY systems this degrades performance.
! Enable to switch REAL4_IS_SLOW from genmake2 (with LET_RS_BE_REAL4):
#ifdef LET_RS_BE_REAL4
#undef REAL4_IS_SLOW
#else /* LET_RS_BE_REAL4 */
#define REAL4_IS_SLOW
#endif /* LET_RS_BE_REAL4 */

!--   Control use of "double" precision constants.
! Use D0 where it means REAL*8 but not where it means REAL*16
#define D0 d0

!=== IO related options ===
!--   Flag used to indicate whether Fortran formatted write
! and read are threadsafe. On SGI the routines can be thread
! safe, on Sun it is not possible - if you are unsure then
! undef this option.
#undef FMTFTN_IO_THREAD_SAFE

!--   Flag used to indicate whether Binary write to Local file (i.e.,
! a different file for each tile) and read are thread-safe.
#undef LOCBIN_IO_THREAD_SAFE

!--   Flag to turn off the writing of error message to ioUnit zero
#undef DISABLE_WRITE_TO_UNIT_ZERO

!--   Flag to turn on old default of opening scratch files with the
! STATUS='SCRATCH' option. This method, while perfectly FORTRAN-standard,
! caused filename conflicts on some multi-node/multi-processor platforms
! in the past and has been replace by something (hopefully) more robust.
#undef USE_FORTRAN_SCRATCH_FILES

!--   Flag defined for eeboot_minimal.F, eeset_parms.F and open_copy_data_file.F
! to write STDOUT, STDERR and scratch files from process 0 only.
! WARNING: to use only when absolutely confident that the setup is working
! since any message (error/warning/print) from any proc <> 0 will be lost.
#undef SINGLE_DISK_IO

!=== MPI, EXCH and GLOBAL_SUM related options ===
!--   Flag turns off MPI_SEND ready_to_receive polling in the
! gather_* subroutines to speed up integrations.
#undef DISABLE_MPI_READY_TO_RECEIVE

!--   Control MPI based parallel processing
!XXX We no longer select the use of MPI via this file (CPP_EEOPTIONS.h)
!XXX To use MPI, use an appropriate genmake2 options file or use
!XXX genmake2 -mpi .
!XXX #undef  ALLOW_USE_MPI

!--   Control use of communication that might overlap computation.
! Under MPI selects/deselects "non-blocking" sends and receives.
#undef  ALLOW_ASYNC_COMMUNICATION
#undef  ALWAYS_USE_ASYNC_COMMUNICATION
!--   Control use of communication that is atomic to computation.
! Under MPI selects/deselects "blocking" sends and receives.
#define ALLOW_SYNC_COMMUNICATION
#undef  ALWAYS_USE_SYNC_COMMUNICATION

!--   Control XY periodicity in processor to grid mappings
! Note: Model code does not need to know whether a domain is
!       periodic because it has overlap regions for every box.
!       Model assume that these values have been
!       filled in some way.
#undef  ALWAYS_PREVENT_X_PERIODICITY
#undef  ALWAYS_PREVENT_Y_PERIODICITY
#define CAN_PREVENT_X_PERIODICITY
#define CAN_PREVENT_Y_PERIODICITY

!--   disconnect tiles (no exchange between tiles, just fill-in edges
! assuming locally periodic subdomain)
#undef DISCONNECTED_TILES

!--   Always cumulate tile local-sum in the same order by applying MPI allreduce
! to array of tiles ; can get slower with large number of tiles (big set-up)
#define GLOBAL_SUM_ORDER_TILES

!--   Alternative way of doing global sum without MPI allreduce call
! but instead, explicit MPI send & recv calls. Expected to be slower.
#undef GLOBAL_SUM_SEND_RECV

!--   Alternative way of doing global sum on a single CPU
! to eliminate tiling-dependent roundoff errors. Note: This is slow.
#undef CG2D_SINGLECPU_SUM

!=== Other options (to add/remove pieces of code) ===
!--   Flag to turn on checking for errors from all threads and procs
! (calling S/R STOP_IF_ERROR) before stopping.
#define USE_ERROR_STOP

!--   Control use of communication with other component:
! allow to import and export from/to Coupler interface.
#undef COMPONENT_MODULE

!--   Activate some pieces of code for coupling to GEOS AGCM
#undef HACK_FOR_GMAO_CPL

!=== And define Macros ===
#include "CPP_EEMACROS.h"

#endif /* _CPP_EEOPTIONS_H_ */
