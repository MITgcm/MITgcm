#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_INIT_CTRLVAR

C     !INTERFACE: ======================================================
      SUBROUTINE CTRL_INIT_CTRLVAR(
     I       xx_fname,
     I       ivar,
     I       varIndex,
     I       varRecs,
     I       varStartRec,
     I       varEndRec,
     I       varNxMax,
     I       varNyMax,
     I       varNrMax,
     I       varGrid,
     I       varType,
     I       costfinal_exists,
     I       myThid )

C     !DESCRIPTION:
C
C     Generic routine to initialize control variable xx_fname and store
C     specifier in global arrays for later reference.
C
C     Called from S/R CTRL_INIT_FIXED, this is where the crucial
C     information for a given control variable is set.
C
C     started: heimbach@mit.edu 28-Oct-2003

C     !USES: ===========================================================
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "OPTIMCYCLE.h"

C     !INPUT PARAMETERS: ===============================================
C     ivar   :: if > 0, index in ctrl-var list to store ctrl-var settings
C               if == 0, skip storing of ctrl-var settings
C     myThid ::  my Thread Id number
      CHARACTER*(MAX_LEN_FNAM) xx_fname
      INTEGER ivar
      INTEGER varIndex
      INTEGER varRecs
      INTEGER varStartRec
      INTEGER varEndRec
      INTEGER varNxMax
      INTEGER varNyMax
      INTEGER varNrMax
      CHARACTER*(1) varGrid
      CHARACTER*(5) varType
      LOGICAL costfinal_exists
      INTEGER myThid

C     !OUTPUT PARAMETERS: ==============================================
C     none

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER il,ilDir
      CHARACTER*(MAX_LEN_FNAM) fname(3), gfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL g_exst
CEOP

c     _BEGIN_MASTER( myThid )
C     First check if the control variable index is within the allowed range.
      IF ( ivar .LT. 0 .OR. ivar .GT. maxcvars ) THEN
        WRITE(msgBuf,'(A,I4,A)') 'S/R CTRL_INIT_CTRLVAR: ivar = ',
     &       ivar, ' is not valid.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I4)')
     &       'S/R CTRL_INIT_CTRLVAR: allowed values: ',
     &       '0 < ivar <= maxcvars = ', maxcvars
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CTRL_INIT_CTRLVAR'
      ELSEIF ( ivar .NE. 0 ) THEN
C     Save this ctrl-var setting in ctrl-var list:
        ncvarindex(ivar)    = varIndex
        ncvarrecs (ivar)    = varRecs
        ncvarrecstart(ivar) = varStartRec
        ncvarrecsend(ivar)  = varEndRec
        ncvarxmax (ivar)    = varNxMax
        ncvarymax (ivar)    = varNyMax
        ncvarnrmax(ivar)    = varNrMax
        ncvargrd  (ivar)    = varGrid
        ncvartype (ivar)    = varType
        ncvarfname(ivar)    = xx_fname
      ENDIF
c     _END_MASTER( myThid )

cph add following flag to make pack/unpack only less error-prone
#ifndef CTRL_DO_PACK_UNPACK_ONLY

      ilDir = ILNBLNK( ctrlDir )
      CALL CTRL_SET_FNAME( ctrlDir(1:ilDir)//xx_fname, fname, myThid )

C     In an adjoint run, adxx_ files should always be initialized with zeros
C     For TLM, all zeros means zero sensitivity, so check
C     for existing perturbation file g_xx_, otherwise
C     initialize to zero and warn user
      IF (yadprefix.EQ.'g_') THEN
        il = ILNBLNK(fname(2))
        WRITE(gfname,'(2a)') fname(2)(1:il),'.data'
        INQUIRE(file=gfname,exist=g_exst)
        IF (.NOT. g_exst) THEN
            WRITE(msgBuf,'(3A)')'** WARNING ** CTRL_INIT_CTRLVAR: ',
     &          'could not find ', gfname(1:il+5)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(3A)')'** WARNING ** ',
     &          'for TLM perturbation ',
     &          'so will initialize this file with all zeros'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
        ENDIF
      ELSE
        g_exst = .FALSE.
      ENDIF

      IF ( .NOT. costfinal_exists ) THEN

        IF ( varType .EQ. 'Arr3D' ) THEN
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
          IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &      CALL CTRL_SET_GLOBFLD_XYZ( fname(2),
     &                                 varRecs, ctrlprec, myThid )
#endif
          IF ( ( doInitXX .AND. optimcycle.EQ.0 ) .OR. doAdmTlm )
     &      CALL CTRL_SET_GLOBFLD_XYZ( fname(1),
     &                                 varRecs, ctrlprec, myThid )
        ELSEIF ( varType(4:5) .EQ. '2D' ) THEN
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
          IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &      CALL CTRL_SET_GLOBFLD_XY( fname(2),
     &                                 varRecs, ctrlprec, myThid )
#endif
          IF ( ( doInitXX .AND. optimcycle.EQ.0 ) .OR. doAdmTlm )
     &      CALL CTRL_SET_GLOBFLD_XY( fname(1),
     &                                 varRecs, ctrlprec, myThid )
        ELSEIF ( varType .EQ. 'SecXZ' ) THEN
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
          IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &      CALL CTRL_SET_GLOBFLD_XZ( fname(2),
     &                                 varRecs, ctrlprec, myThid )
#endif
          IF ( ( doInitXX .AND. optimcycle.EQ.0 ) .OR. doAdmTlm )
     &      CALL CTRL_SET_GLOBFLD_XZ( fname(1),
     &                                 varRecs, ctrlprec, myThid )
        ELSEIF ( varType .EQ. 'SecYZ' ) THEN
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
          IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &      CALL CTRL_SET_GLOBFLD_YZ( fname(2),
     &                                 varRecs, ctrlprec, myThid )
#endif
          IF ( ( doInitXX .AND. optimcycle.EQ.0 ) .OR. doAdmTlm )
     &      CALL CTRL_SET_GLOBFLD_YZ( fname(1),
     &                                 varRecs, ctrlprec, myThid )
        ELSE
          STOP 'CTRL_INIT_CTRLVAR: varType option not implemented'
        ENDIF

      ENDIF

#endif /* CTRL_DO_PACK_UNPACK_ONLY */

      RETURN
      END
