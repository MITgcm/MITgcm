C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_aim2dyn.F,v 1.9 2014/05/12 01:32:41 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_AIM2DYN
C     !INTERFACE:
      SUBROUTINE AIM_AIM2DYN(
     I                        bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_AIM2DYN
C     | o Remap AIM outputs to dynamics conforming arrays.
C     |==========================================================*
C     | Currently AIM exports to the dynmaics
C     |  - PBL drag coefficient
C     |  - Net tendency for temperature
C     |  - Net tendency for water vapor
C     | Exporting drag has the nice property that it is a scalar.
C     | This means that the exchanges on the AIM exported fields
C     | do not need special piaring on the cube. It may not be
C     | a good idea in the long term as it makes assumptions
C     | about the momentum schemes within AIM.
C     *==========================================================*
C     \ev
C-------
C  Note: Except LSC tendency, all others need to be /dpFac.
C-------

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

#include "AIM2DYN.h"
#include "com_physvar.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: Tile index
C     myTime :: Current time of simulation ( s )
C     myIter :: Current iteration number in simulation
C     myThid :: Number of this instance of the routine
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,k        :: loop counters
C     I2,Katm      :: loop counters
C     conv_T2theta :: conversion factor from (absolute) Temp. to Pot.Temp.
      _RL conv_T2theta
      INTEGER i,j,k
      INTEGER I2, Katm
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  physTendDiag
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Physics tendency term

#ifdef ALLOW_DIAGNOSTICS
      physTendDiag = .FALSE.
      IF (useDiagnostics) THEN
        physTendDiag = DIAGNOSTICS_IS_ON( 'DIABT   ', myThid )
     &            .OR. DIAGNOSTICS_IS_ON( 'DIABQ   ', myThid )
      ENDIF
#endif

C-    Planetary boundary layer drag coeff.
      DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
         aim_drag(i,j,bi,bj) = DRAG(I2,0,myThid)
        ENDDO
      ENDDO
#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
C-    Near surface wind speed
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
         aim_surfWind(i,j,bi,bj) = SPEED0(I2,myThid)
        ENDDO
       ENDDO
      ENDIF
#endif /* COMPONENT_MODULE */

      DO k=1,Nr
       Katm = _KD2KA( k )
       conv_T2theta = (atm_Po/rC(k))**atm_kappa

C-     Add all tendencies (ignoring partial cell factor) for T & Q
C          and convert Temp. tendency to Pot.Temp. tendency
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
C        temperature tendency
         aim_dTdt(i,j,k,bi,bj) = ( TT_CNV(I2,Katm,myThid)
     &                            +TT_PBL(I2,Katm,myThid)
     &                            +TT_RSW(I2,Katm,myThid)
     &                            +TT_RLW(I2,Katm,myThid)
     &                            +TT_LSC(I2,Katm,myThid)
     &                           )*conv_T2theta
C        water vapor tendency
         aim_dSdt(i,j,k,bi,bj) =   QT_CNV(I2,Katm,myThid)
     &                            +QT_PBL(I2,Katm,myThid)
     &                            +QT_LSC(I2,Katm,myThid)
        ENDDO
       ENDDO
#ifdef NONLIN_FRSURF
       IF ( select_rStar.GE.1 ) THEN
         DO j=1,sNy
          DO i=1,sNx
            aim_dTdt(i,j,k,bi,bj) = aim_dTdt(i,j,k,bi,bj)
     &                            / pStarFacK(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DIAGNOSTICS
       IF ( physTendDiag ) THEN
         CALL DIAGNOSTICS_FILL( aim_dTdt, 'DIABT   ',
     &                          k, Nr, 1,bi,bj, myThid )
         CALL DIAGNOSTICS_FILL( aim_dSdt, 'DIABQ   ',
     &                          k, Nr, 1,bi,bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     Account for partial cell filling:
#ifdef NONLIN_FRSURF
       IF ( staggerTimeStep .AND. nonlinFreeSurf.GT.0 ) THEN
        IF ( select_rStar.GT.0 ) THEN
         DO j=1,sNy
          DO i=1,sNx
            aim_dTdt(i,j,k,bi,bj) = aim_dTdt(i,j,k,bi,bj)
     &                          *recip_hFacC(i,j,k,bi,bj)
     &                              /rStarExpC(i,j,bi,bj)
            aim_dSdt(i,j,k,bi,bj) = aim_dSdt(i,j,k,bi,bj)
     &                          *recip_hFacC(i,j,k,bi,bj)
     &                              /rStarExpC(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
            aim_dTdt(i,j,k,bi,bj) = aim_dTdt(i,j,k,bi,bj)
     &                             /hFac_surfC(i,j,bi,bj)
            aim_dSdt(i,j,k,bi,bj) = aim_dSdt(i,j,k,bi,bj)
     &                             /hFac_surfC(i,j,bi,bj)
           ELSE
            aim_dTdt(i,j,k,bi,bj) = aim_dTdt(i,j,k,bi,bj)
     &                          *recip_hFacC(i,j,k,bi,bj)
            aim_dSdt(i,j,k,bi,bj) = aim_dSdt(i,j,k,bi,bj)
     &                          *recip_hFacC(i,j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ELSE
#else /* ndef NONLIN_FRSURF */
       IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
         DO j=1,sNy
          DO i=1,sNx
            aim_dTdt(i,j,k,bi,bj) =  aim_dTdt(i,j,k,bi,bj)
     &                           *recip_hFacC(i,j,k,bi,bj)
            aim_dSdt(i,j,k,bi,bj) =  aim_dSdt(i,j,k,bi,bj)
     &                           *recip_hFacC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF

C--- end of k loop.
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
