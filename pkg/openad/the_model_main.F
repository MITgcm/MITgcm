#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THE_MODEL_MAIN

C     !INTERFACE:
      SUBROUTINE THE_MODEL_MAIN(myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THE_MODEL_MAIN
C     | o This is the OpenAD local version of S/R THE_MODEL_MAIN
C     |   see ref. version: model/src/the_model_main.F
C     |   for details about what this routine does.
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     THE_MODEL_MAIN()
C       |
C       |
C       |--INITIALISE_FIXED
C       |   o Set model configuration (fixed arrays)
C       |     Topography, hydrography, timestep, grid, etc..
C       |
C       |--CTRL_UNPACK      o Derivative mode. Unpack control vector.
C       |
C       |--ADTHE_MAIN_LOOP  o Main timestepping loop for combined
C       |                     prognostic and reverse mode integration.
C       |
C       |--THE_MAIN_LOOP    o Main timestepping loop for pure prognostic
C       |                     integration.
C       |
C       |--CTRL_PACK        o Derivative mode. Unpack control vector.
C       |
C       |--GRDCHK_MAIN      o Gradient check control routine.
C       |
C       |--TIMER_PRINTALL   o Print out timing statistics.
C       |
C       |--COMM_STATS       o Print out communication statistics.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C -->> OpenAD
      use OAD_active
      use OAD_rev
      use OAD_tape
      use OAD_cp
#include "cost.h"
C <<-- OpenAD
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "OPTIMCYCLE.h"
# include "CTRL_GENARR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Note: Under the multi-threaded model myIter and myTime are local
C           variables passed around as routine arguments.
C           Although this is fiddly it saves the need to impose
C           additional synchronisation points when they are updated.
C     myTime :: Time counter for this thread
C     myIter :: Iteration counter for this thread
      INTEGER myIter
      _RL     myTime
      LOGICAL costFinalExist
      LOGICAL lastdiva
C -->> OpenAD
      _RL foo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL foo2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fname
      INTEGER il
c     INTEGER ik
C <<-- OpenAD
CEOP

C--   set default:
      costFinalExist = .TRUE.
      lastdiva = .TRUE.
C -->> OpenAD
C-    Set the execution mode
      our_rev_mode%arg_store=.FALSE.
      our_rev_mode%arg_restore=.FALSE.
      our_rev_mode%res_store=.FALSE.
      our_rev_mode%res_restore=.FALSE.
      our_rev_mode%plain=.TRUE.
      our_rev_mode%tape=.FALSE.
      our_rev_mode%adjoint=.FALSE.
      our_rev_mode%switchedToCheckpoint=.FALSE.
C-    Initialize the tape
      CALL OAD_TAPE_INIT()
C-    Initialize the checkpoint areas
      CALL CP_INIT()
C <<-- OpenAD

#ifdef ALLOW_PETSC
      CALL STREAMICE_INITIALIZE_PETSC
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MODEL_MAIN',myThid)
#endif

#if defined(USE_PAPI) || defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
      CALL TIMER_CONTROL('','INIT','THE_MODEL_MAIN',myThid)
#endif
C--   This timer encompasses the whole code
      CALL TIMER_START('ALL                    [THE_MODEL_MAIN]',myThid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_FIXED',myThid)
#endif
C--   Set model configuration (fixed arrays)
      CALL TIMER_START('INITIALISE_FIXED       [THE_MODEL_MAIN]',myThid)
C -->> OpenAD
c     CALL INITIALISE_FIXED( myThid )
      CALL OpenAD_INITIALISE_FIXED( myThid )
C <<-- OpenAD
      CALL TIMER_STOP ('INITIALISE_FIXED       [THE_MODEL_MAIN]',myThid)

      myTime = startTime
      myIter = nIter0

#if ( defined (ALLOW_ADMTLM) )

      STOP 'should never get here; ADMTLM_DSVD calls ADMTLM_DRIVER'

#elif ( defined (ALLOW_AUTODIFF))

# ifdef ALLOW_CTRL
      IF ( useCTRL ) THEN
#  ifndef EXCLUDE_CTRL_PACK
        INQUIRE( file='costfinal', exist=costFinalExist )
        IF ( .NOT. costFinalExist ) THEN
          IF ( (optimcycle.NE.0 .OR. .NOT.doinitxx)
     &         .AND. doMainUnpack ) THEN
            CALL TIMER_START('CTRL_UNPACK   [THE_MODEL_MAIN]',myThid)
            CALL CTRL_UNPACK( .TRUE. , myThid )
            CALL TIMER_STOP ('CTRL_UNPACK   [THE_MODEL_MAIN]',myThid)
          ENDIF
        ENDIF
# endif /* EXCLUDE_CTRL_PACK */
      ENDIF
# endif /* ALLOW_CTRL */

# ifdef ALLOW_COST
      CALL COST_DEPENDENT_INIT ( myThid )
# endif

# if defined( ALLOW_TANGENTLINEAR_RUN )

#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('G_THE_MAIN_LOOP',myThid)
#  endif
      CALL TIMER_START('G_THE_MAIN_LOOP           [TANGENT RUN]',myThid)
      CALL G_THE_MAIN_LOOP ( myTime, myIter, myThid )
      CALL TIMER_STOP ('G_THE_MAIN_LOOP           [TANGENT RUN]',myThid)

# elif defined( ALLOW_ADJOINT_RUN )

#  ifdef ALLOW_DIVIDED_ADJOINT
      STOP 'In (OpenAD) THE_MODEL_MAIN: ALLOW_DIVIDED_ADJOINT not coded'
#  else /* ALLOW_DIVIDED_ADJOINT undef */
#   ifndef ALLOW_OPENAD
#    ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ADTHE_MAIN_LOOP',myThid)
#    endif
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', myThid)
      CALL ADTHE_MAIN_LOOP ( myThid )
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', myThid)
#   else /* ALLOW_OPENAD defined */
C -->> OpenAD
#   ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
#   endif
      CALL TIMER_START('THE_MAIN_LOOP (F)      [THE_MODEL_MAIN]',myThid)
      our_rev_mode%plain=.FALSE.
      our_rev_mode%tape=.TRUE.
      CALL TIMERATIO()
      CALL OpenAD_THE_MAIN_LOOP( myTime, myIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP (F)      [THE_MODEL_MAIN]',myThid)
      CALL TIMER_START('THE_MAIN_LOOP (A)      [THE_MODEL_MAIN]',myThid)
      our_rev_mode%arg_store=.FALSE.
      our_rev_mode%arg_restore=.FALSE.
      our_rev_mode%plain=.FALSE.
      our_rev_mode%tape=.FALSE.
      our_rev_mode%adjoint=.TRUE.
      IF (myProcID .EQ. 0) THEN
        fc%d=1.0
      ENDIF
      CALL TIMERATIO()
      CALL OpenAD_THE_MAIN_LOOP( myTime, myIter, myThid )
      CALL TIMERATIO()
      our_rev_mode%arg_store=.FALSE.
      our_rev_mode%arg_restore=.FALSE.
      our_rev_mode%plain=.TRUE.
      our_rev_mode%tape=.FALSE.
      our_rev_mode%adjoint=.FALSE.
      WRITE(suff,'(I10.10)') optimcycle
#   ifndef ALLOW_OPENAD_ACTIVE_READ_XYZ
#   ifdef ALLOW_DIFFKR_CONTROL
      foo=diffkr%d
      il=ILNBLNK( xx_diffkr_file )
      write(fname,'(4a)') 'ad', xx_diffkr_file(1:il), '.', suff
      CALL WRITE_REC_3D_RL(fname,ctrlprec,Nr,foo,1,myIter,myThid)
#   endif
#   endif /* ALLOW_OPENAD_ACTIVE_READ_XYZ */

cc#   ifdef ALLOW_GENARR2D_CONTROL
cc      do ik = 1, maxCtrlArr2D
cc        foo2d=xx_genarr2d(:,:,:,:,ik)%d
cc        write(fname,'(A,I2.2,A)') 'adxx_genarr2d_',ik,'.'
cc        call write_fld_xy_rl(fname,suff,foo2D,myIter,1)
cc      enddo
cc#   endif
cc#   ifdef ALLOW_GENTIM2D_CONTROL
cc      do ik = 1, maxCtrlTim2D
cc        foo2d=xx_gentim2d(:,:,:,:,ik)%d
cc        write(fname,'(A,I2.2,A)') 'adxx_gentim2d_',ik,'.'
cc        call write_fld_xy_rl(fname,suff,foo2D,myIter,1)
cc      enddo
cc#   endif
cc#   ifdef ALLOW_GENARR3D_CONTROL
cc      do ik = 1, maxCtrlArr3D
cc        foo=xx_genarr3d(:,:,:,:,:,ik)%d
cc        write(fname,'(A,I2.2,A)') 'adxx_genarr3d_',ik,'.'
cc        call write_fld_xyz_rl(fname,suff,foo,myIter,1)
cc      enddo
cc#   endif
      our_rev_mode%plain=.TRUE.
      our_rev_mode%tape=.FALSE.
      our_rev_mode%adjoint=.FALSE.
      CALL TIMER_STOP ('THE_MAIN_LOOP (A)      [THE_MODEL_MAIN]',myThid)

C <<-- OpenAD
#   endif /* ALLOW_OPENAD */
#  endif /* ALLOW_DIVIDED_ADJOINT */

# else /* forward run only within AD setting */

#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
#  endif
C--   Call time stepping loop of full model
      CALL TIMER_START('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP( myTime, myIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)

# endif /* forward run only within AD setting */

# ifdef ALLOW_CTRL
# ifndef EXCLUDE_CTRL_PACK
#  ifdef ALLOW_OPENAD
cph-- ad hoc fix for OpenAD time stepping counter lagging one step
cph-- after final adjoint step
      myIter=nIter0
#  endif
      IF ( useCTRL .AND. lastdiva .AND. doMainPack ) THEN
        CALL TIMER_START('CTRL_PACK           [THE_MODEL_MAIN]',myThid)
        CALL CTRL_PACK( .FALSE. , myThid )
        CALL TIMER_STOP ('CTRL_PACK           [THE_MODEL_MAIN]',myThid)
        IF ( ( optimcycle.EQ.0 .OR. (.NOT. doMainUnpack) )
     &       .AND. myIter.EQ.nIter0 ) THEN
          CALL TIMER_START('CTRL_PACK     [THE_MODEL_MAIN]',myThid)
          CALL CTRL_PACK( .TRUE. , myThid )
          CALL TIMER_STOP ('CTRL_PACK     [THE_MODEL_MAIN]',myThid)
        ENDIF
      ENDIF
# endif /* EXCLUDE_CTRL_PACK */
# endif /* ALLOW_CTRL */

# ifdef ALLOW_GRDCHK
      IF ( useGrdchk .AND. lastdiva ) THEN
        CALL TIMER_START('GRDCHK_MAIN         [THE_MODEL_MAIN]',myThid)
        CALL GRDCHK_MAIN( myThid )
        CALL TIMER_STOP ('GRDCHK_MAIN         [THE_MODEL_MAIN]',myThid)
      ENDIF
# endif

#else /* ALL AD-related undef */

# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
# endif
C--   Call time stepping loop of full model
      CALL TIMER_START('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP( myTime, myIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)

#endif /* ALLOW_TANGENTLINEAR_RUN ALLOW_ADJOINT_RUN ALLOW_ADMTLM */

#ifdef ALLOW_PETSC
      CALL STREAMICE_FINALIZE_PETSC
#endif

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Close all open NetCDF files
        _BEGIN_MASTER( myThid )
        CALL MNC_FILE_CLOSE_ALL( myThid )
        _END_MASTER( myThid )
      ENDIF
#endif

C--   This timer encompasses the whole code
      CALL TIMER_STOP ('ALL                    [THE_MODEL_MAIN]',myThid)

C--   Write timer statistics
      IF ( myThid .EQ. 1 ) THEN
        CALL TIMER_PRINTALL( myThid )
        CALL COMM_STATS
      ENDIF

C--   Check threads synchronization :
      CALL BAR_CHECK( 9, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MODEL_MAIN',myThid)
#endif

      RETURN
      END
