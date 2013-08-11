C $Header: /u/gcmpack/MITgcm/model/src/ini_nh_fields.F,v 1.6 2013/08/11 20:23:39 jmc Exp $
C $Name:  $

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
C     == Routine arguments ==
C     myThid  :: My Thread Id number
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j
      INTEGER ks
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef NONLIN_FRSURF
      INTEGER k
#endif
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
       IF ( exactConserv .AND. dPhiNHstatus.EQ.0 ) THEN
c      IF ( exactConserv ) THEN
C--   Separate the Hydrostatic Surface Pressure adjusment (=> put it in dPhiNH)
C     from the Non-hydrostatic pressure (since cg3d_x contains both contribution)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          IF ( select_rStar.EQ.0 .AND. uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj) = phi_nh(i,j,1,bi,bj)
c             dPhiNH(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
          ELSEIF ( select_rStar.EQ.0 ) THEN
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
#ifdef NONLIN_FRSURF
          ELSE
C        rStar : take vertical average of P_NH as Hyd.Surf.Press adjustment
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj) = dPhiNH(i,j,bi,bj)
     &          + phi_nh(i,j,k,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj) = dPhiNH(i,j,bi,bj)
     &                           *recip_Rcol(i,j,bi,bj)
            ENDDO
           ENDDO
#endif /* NONLIN_FRSURF */
          ENDIF
         ENDDO
        ENDDO
C-     end of if-block: dPhiNH_status
       ENDIF

      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */
      RETURN
      END
