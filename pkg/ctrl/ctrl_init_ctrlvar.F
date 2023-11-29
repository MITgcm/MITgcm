#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_INIT_CTRLVAR

C     !INTERFACE: ======================================================
      SUBROUTINE CTRL_INIT_CTRLVAR(
     I       xx_fname,
     I       ivarindex,
     I       ncvarindex_loc,
     I       ncvarrecs_loc,
     I       ncvarrecstart_loc,
     I       ncvarrecsend_loc,
     I       ncvarxmax_loc,
     I       ncvarymax_loc,
     I       ncvarnrmax_loc,
     I       ncvargrd_loc,
     I       whichxyz,
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
#include "CTRL.h"
#include "OPTIMCYCLE.h"

C     !INPUT PARAMETERS: ===============================================
C     myThid ::  my Thread Id number
      CHARACTER*(MAX_LEN_FNAM) xx_fname
      INTEGER ivarindex
      INTEGER ncvarindex_loc
      INTEGER ncvarrecs_loc
      INTEGER ncvarrecstart_loc
      INTEGER ncvarrecsend_loc
      INTEGER ncvarxmax_loc
      INTEGER ncvarymax_loc
      INTEGER ncvarnrmax_loc
      CHARACTER*(1) ncvargrd_loc
      CHARACTER*(5) whichxyz
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
      IF ( ivarindex .LE. 0 .OR. ivarindex .GT. maxcvars ) THEN
       WRITE(msgBuf,'(A,I4,A)') 'S/R CTRL_INIT_CTRLVAR: ivarindex = ',
     &      ivarindex, ' is not valid.'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,I4)')
     &      'S/R CTRL_INIT_CTRLVAR: allowed values: '//
     &      '0 < ivarindex <= maxcvars = ', maxcvars
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R CTRL_INIT_CTRLVAR'
      ENDIF
C
      ncvarindex(ivarindex)    = ncvarindex_loc
      ncvarrecs (ivarindex)    = ncvarrecs_loc
      ncvarrecstart(ivarindex) = ncvarrecstart_loc
      ncvarrecsend(ivarindex)  = ncvarrecsend_loc
      ncvarxmax (ivarindex)    = ncvarxmax_loc
      ncvarymax (ivarindex)    = ncvarymax_loc
      ncvarnrmax(ivarindex)    = ncvarnrmax_loc
      ncvargrd  (ivarindex)    = ncvargrd_loc
      ncvartype (ivarindex)    = whichxyz
      ncvarfname(ivarindex)    = xx_fname
c     _END_MASTER( myThid )

cph add following flag to make pack/unpack only less error-prone
#ifndef CTRL_DO_PACK_UNPACK_ONLY

      ilDir=ilnblnk( ctrlDir )
      CALL CTRL_SET_FNAME( ctrlDir(1:ilDir)//xx_fname, fname, myThid )

C     In an adjoint run, adxx_ files should always be initialized with zeros
C     For TLM, all zeros means zero sensitivity, so check
C     for existing perturbation file g_xx_, otherwise
C     initialize to zero and warn user
      IF (yadprefix.eq.'g_') then
        il = ILNBLNK(fname(2))
        WRITE(gfname(1:il+5),'(2a)') fname(2)(1:il),'.data'
        INQUIRE(file=gfname,exist=g_exst)
        IF (.not. g_exst) then
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

         IF ( whichxyz .EQ. 'Arr3D') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xyz( fname(2), ivarindex, myThid)
#endif
            IF ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xyz( fname(1), ivarindex, myThid)
         ELSEIF ( whichxyz(4:5) .EQ. '2D' ) THEN
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xy( fname(2), ivarindex, myThid)
#endif
            IF ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xy( fname(1), ivarindex, myThid )
         ELSEIF ( whichxyz .EQ. 'SecXZ') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_xz( fname(2), ivarindex, myThid)
#endif
            IF ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_xz( fname(1), ivarindex, myThid )
         ELSEIF ( whichxyz .EQ. 'SecYZ') then
#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
            IF ( yadprefix.EQ.'ad' .OR. .NOT.(g_exst .OR. doAdmTlm) )
     &           call ctrl_set_globfld_yz( fname(2), ivarindex, myThid)
#endif
            IF ( ( doInitXX .AND. optimcycle.eq.0 ) .OR. doAdmTlm )
     &           call ctrl_set_globfld_yz( fname(1), ivarindex, myThid )
         ELSE
            STOP 'whichxyz option not implemented'
         ENDIF

      ENDIF

#endif /* CTRL_DO_PACK_UNPACK_ONLY */

      RETURN
      END
