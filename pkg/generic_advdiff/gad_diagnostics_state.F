C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diagnostics_state.F,v 1.2 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: GAD_DIAGNOSTICS_STATE

C     !INTERFACE:
      SUBROUTINE GAD_DIAGNOSTICS_STATE( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in the diagnostics array for GAD state variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "GAD_SOM_VARS.h"

C     !INPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
#ifdef GAD_ALLOW_TS_SOM_ADV
C     ! FUNCTIONS:
      CHARACTER*4  GAD_DIAG_SUFX
      EXTERNAL     GAD_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C     !LOCAL VARIABLES:
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      INTEGER   i,j,k,bi,bj,n
      _RL       locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL       recipVol
      _RL       oneThird, oneFifth, oneNinth

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      oneThird = 1. _d 0 / 3. _d 0
      oneFifth = 1. _d 0 / 5. _d 0
      oneNinth = 1. _d 0 / 9. _d 0

C---  Temperature:
      diagSufx = GAD_DIAG_SUFX( GAD_TEMPERATURE, myThid )

      DO n=1,nSOM
        diagName  = 'SM'//somSfx(n)//diagSufx
        CALL DIAGNOSTICS_FILL( som_T(1-OLx,1-OLy,1,1,1,n), diagName,
     &                         0, Nr, 0, 1, 1, myThid )
      ENDDO

      diagName  = 'SM_v'//diagSufx
      IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C--   Compute Internal (sub-grid) variance :
C     [Sx^2 + Sy^2 + Sz^2]/3 +[Sxx^2 + Syy^2 + Szz^2]/5 +[Sxy^2 + Sxz^2 + Syz^2]/9
           DO k=1,Nr
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              recipVol = recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &                  *recip_rhoFacC(k)
              locVar(i,j,k) = (
     &               (  som_T(i,j,k,bi,bj,1)*som_T(i,j,k,bi,bj,1)
     &                + som_T(i,j,k,bi,bj,2)*som_T(i,j,k,bi,bj,2)
     &                + som_T(i,j,k,bi,bj,3)*som_T(i,j,k,bi,bj,3)
     &               )*oneThird
     &             + (  som_T(i,j,k,bi,bj,4)*som_T(i,j,k,bi,bj,4)
     &                + som_T(i,j,k,bi,bj,5)*som_T(i,j,k,bi,bj,5)
     &                + som_T(i,j,k,bi,bj,6)*som_T(i,j,k,bi,bj,6)
     &               )*oneFifth
     &             + (  som_T(i,j,k,bi,bj,7)*som_T(i,j,k,bi,bj,7)
     &                + som_T(i,j,k,bi,bj,8)*som_T(i,j,k,bi,bj,8)
     &                + som_T(i,j,k,bi,bj,9)*som_T(i,j,k,bi,bj,9)
     &               )*oneNinth
     &                        )*recipVol*recipVol

             ENDDO
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL( locVar, diagName,
     &                            0, Nr, 2, bi, bj, myThid )
         ENDDO
        ENDDO
      ENDIF

C---  Salinity:
      diagSufx = GAD_DIAG_SUFX( GAD_SALINITY, myThid )

      DO n=1,nSOM
        diagName  = 'SM'//somSfx(n)//diagSufx
        CALL DIAGNOSTICS_FILL( som_S(1-OLx,1-OLy,1,1,1,n), diagName,
     &                         0, Nr, 0, 1, 1, myThid )
      ENDDO

      diagName  = 'SM_v'//diagSufx
      IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C--   Compute Internal (sub-grid) variance :
C     [Sx^2 + Sy^2 + Sz^2]/3 +[Sxx^2 + Syy^2 + Szz^2]/5 +[Sxy^2 + Sxz^2 + Syz^2]/9
           DO k=1,Nr
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              recipVol = recip_rA(i,j,bi,bj)*recip_deepFac2C(k)
     &                  *recip_drF(k)*_recip_hFacC(i,j,k,bi,bj)
     &                  *recip_rhoFacC(k)
              locVar(i,j,k) = (
     &               (  som_S(i,j,k,bi,bj,1)*som_S(i,j,k,bi,bj,1)
     &                + som_S(i,j,k,bi,bj,2)*som_S(i,j,k,bi,bj,2)
     &                + som_S(i,j,k,bi,bj,3)*som_S(i,j,k,bi,bj,3)
     &               )*oneThird
     &             + (  som_S(i,j,k,bi,bj,4)*som_S(i,j,k,bi,bj,4)
     &                + som_S(i,j,k,bi,bj,5)*som_S(i,j,k,bi,bj,5)
     &                + som_S(i,j,k,bi,bj,6)*som_S(i,j,k,bi,bj,6)
     &               )*oneFifth
     &             + (  som_S(i,j,k,bi,bj,7)*som_S(i,j,k,bi,bj,7)
     &                + som_S(i,j,k,bi,bj,8)*som_S(i,j,k,bi,bj,8)
     &                + som_S(i,j,k,bi,bj,9)*som_S(i,j,k,bi,bj,9)
     &               )*oneNinth
     &                        )*recipVol*recipVol
             ENDDO
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL( locVar, diagName,
     &                            0, Nr, 2, bi, bj, myThid )
         ENDDO
        ENDDO
      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
