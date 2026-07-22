#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_NH_FIELDS
C     !INTERFACE:
      SUBROUTINE INI_NH_FIELDS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_NH_FIELDS
C     | o Set model initial non-hydrostatic fields.
C     *==========================================================*
C     | Note: If using NH form,
C     |  call this S/R whether starting or restarting simulation.
C     | This is different from other "true" ini_fields type S/R
C     |  (e.g., INI_VEL) which are called only when starting.
C     | Reason: no real physical field to initialise (since wVel
C     |  is diagnose from continuity) but needs to set few arrays
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "RESTART.h"
#include "NH_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER ks
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ' ) THEN
C--   Case where starting from initial conditions

C--   Read an initial non-hydrostatic pressure field
c       IF (phiNHinitFile .NE. ' ') THEN
c        CALL READ_FLD_XYZ_RL( phiNHinitFile, ' ', phi_nh, 0, myThid )
c        _EXCH_XYZ_RL(phi_nh, myThid)
c       ENDIF

      ELSE
C--   Case where restarting from a pickup

       _BEGIN_MASTER(myThid)
       WRITE(msgBuf,'(A,I4)')
     &   'INI_NH_FIELDS: dPhiNHstatus=', dPhiNHstatus
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER(myThid)

C-- if solveForDeltaP=F:
C   a) case selectNHfreeSurf=0 : dPhiNH is not in pickup (and dPhiNHstatus=0)
C        and we set dPhiNH from phi_nh(k=1) as done in S/R POST_CG3D)
C   b) case selectNHfreeSurf=1 : dPhiNH is written to pickup so should have been
C        loaded (-> dPhiNHstatus=1), and in this case, nothing special to do.
C        if not loaded (e.g., restarting from run with selectNHfreeSurf=0),
C        best approach is to do the same copy as for the case (a) above
C-- if solveForDeltaP=T and pickup written from run using solveForDeltaP=F:
C   a) case selectNHfreeSurf=0 : need to substract phi_nh(k=1) from phi_nh
C        to get the true NH kinematic pressure.
C   b) case selectNHfreeSurf=1 : dPhiNH is written to pickup so should have been
C        loaded (-> dPhiNHstatus=1), and in this case, can be substracted from
C        phi_nh to get the true NH kinematic pressure.
C        if not loaded from pickup, best approach is to proceed as in (a) above
C-- if solveForDeltaP=T and pickup written from run using solveForDeltaP=T:
C        dPhiNH is not used (and will be zero); just make sure processing above
C        does not affect this restart.

       IF ( ( exactConserv .AND. dPhiNHstatus.EQ.0 )
     &       .OR. solveForDeltaP ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

          IF ( exactConserv .AND. dPhiNHstatus.EQ.0
     &                      .AND. uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj) = phi_nh(i,j,1,bi,bj)
c             dPhiNH(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
          ELSEIF ( exactConserv .AND. dPhiNHstatus.EQ.0 ) THEN
C-       Other than Z coordinate: no assumption on surface level index
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             ks = kSurfC(i,j,bi,bj)
             IF ( ks.LE.Nr ) THEN
              dPhiNH(i,j,bi,bj) = phi_nh(i,j,ks,bi,bj)
             ELSE
              dPhiNH(i,j,bi,bj) = 0.
             ENDIF
            ENDDO
           ENDDO
          ENDIF

          IF ( solveForDeltaP ) THEN
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              phi_nh(i,j,k,bi,bj) = ( phi_nh(i,j,k,bi,bj)
     &                              - dPhiNH(i,j,bi,bj)
     &                              )*maskC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF

         ENDDO
        ENDDO
C-     end of if-block: dPhiNH_status OR solveForDeltaP
       ENDIF

      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */
      RETURN
      END
