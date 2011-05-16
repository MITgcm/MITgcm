C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_surfcor.F,v 1.14 2011/05/16 22:04:15 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_SURFCOR

C     !INTERFACE:
      SUBROUTINE MON_SURFCOR(
     I                        myThid )

C     !DESCRIPTION:
C     Compute and write area-mean surface expansion term (also called
C     ``surface correction'' with Linear FS).
C
C     Diagnose mean surface expansion term
C     \begin{equation}
C       \mbox{with r coordinate} = (\mbox{w surf})(\mbox{Tracer})
C     \end{equation}
C     \begin{equation}
C       \mbox{units} = (\mbox{W units})(\mbox{Tracer units})
C       \ ; \ \mbox{+ = out}
C     \end{equation}
C     \begin{equation}
C       \mbox{with r* coord}) = \frac{d\eta}{dt} \frac{dz}{H}
C       (\mbox{Tracer})
C     \end{equation}
C
C     Atmosphere: convert surf.cor(Theta) to surface heating,
C     \begin{equation}
C       \mbox{units} = \frac{W}{m^2}, \mbox{+ = out}
C     \end{equation}
C     compute mean conversion term Temp -> PE , units= W/m2,
C     + = decreasing heat content, increasing PE

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "GRID.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
C     myThid    :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,k,ks,bi,bj
      _RL theArea, wT_Mean, wS_Mean
      _RL wT_Heat, theta2PE
      _RL tmpVal, ddPI
      _RL tileArea(nSx,nSy)
      _RL tile_wT (nSx,nSy)
      _RL tile_wS (nSx,nSy)
      _RL tileWHeat(nSx,nSy)
      _RL tileTh2pe(nSx,nSy)
#ifdef NONLIN_FRSURF
      _RL tmpVol, conv_th2Heat
      _RL vT_Mean, vS_Mean, vT_Heat
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       theArea = 0.
       wT_Mean = 0.
       wS_Mean = 0.
       wT_Heat = 0.
       theta2PE = 0.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          tileArea(bi,bj) = 0.
          tile_wT(bi,bj) = 0.
          tile_wS(bi,bj) = 0.
          tileWHeat(bi,bj) = 0.
          tileTh2pe(bi,bj) = 0.
C-- Compute surface "expansion" term & do the integral
          DO j=1,sNy
           DO i=1,sNx
            ks = ksurfC(i,j,bi,bj)
            IF (ks.LE.Nr) THEN
             tileArea(bi,bj) = tileArea(bi,bj)
     &                       + rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
             tmpVal = rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
     &               *wVel(i,j,ks,bi,bj)*theta(i,j,ks,bi,bj)
             tile_wT(bi,bj) = tile_wT(bi,bj) + tmpVal
             tile_wS(bi,bj) = tile_wS(bi,bj)
     &                      + rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
     &                       *wVel(i,j,ks,bi,bj)*salt(i,j,ks,bi,bj)
C-  Atmos in Pot.Temp => convert Omega*Theta to heat flux :
             IF ( fluidIsAir ) THEN
               tileWHeat(bi,bj) = tileWHeat(bi,bj)
     &                 + tmpVal*atm_cp*((rC(ks)/atm_po)**atm_kappa)
             ENDIF
            ENDIF
           ENDDO
          ENDDO
#ifdef ALLOW_AIM
          IF ( useAIM ) THEN
           tile_wS(bi,bj) = 0.
           DO j=1,sNy
            DO i=1,sNx
             ks = ksurfC(i,j,bi,bj)
             IF (ks.LE.Nr) THEN
              tmpVal = salt(i,j,ks,bi,bj)
     &               + salt(i,j,Nr,bi,bj)*drF(Nr)*recip_drF(ks)
     &                *hFacC(i,j,Nr,bi,bj)*_recip_hFacC(i,j,ks,bi,bj)
              tile_wS(bi,bj) = tile_wS(bi,bj)
     &                       + rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
     &                        *wVel(i,j,ks,bi,bj)*tmpVal
             ENDIF
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_AIM */


C-- Atmos in Pot.Temp => conmpute energy conversion Temp -> PE
C    = Omega*Theta*DeltaPI
          IF ( fluidIsAir ) THEN
           DO k=2,Nr
            ddPI=atm_cp*( (rC(K-1)/atm_po)**atm_kappa
     &                   -(rC( K )/atm_po)**atm_kappa )
            DO j=1,sNy
             DO i=1,sNx
              tileTh2pe(bi,bj) = tileTh2pe(bi,bj)
     &         - ddPI*rA(i,j,bi,bj)*wVel(i,j,k,bi,bj)
     &           *(theta(i,j,k,bi,bj)+theta(i,j,k-1,bi,bj))*0.5 _d 0
     &           *maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
     &           *maskInC(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF

#ifdef NONLIN_FRSURF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
        IF (select_rStar.NE.0) THEN
C-- Compute Volume expansion term & do the integral
          vT_Mean = 0.
          vS_Mean = 0.
          vT_Heat = 0.
          conv_th2Heat = 0.
          DO k=1,Nr
           IF (fluidIsAir) conv_th2Heat =
     &                            atm_cp*((rC(k)/atm_po)**atm_kappa)
           DO j=1,sNy
            DO i=1,sNx
              tmpVol  = rA(i,j,bi,bj)*h0FacC(i,j,k,bi,bj)*drF(k)
     &                 *maskInC(i,j,bi,bj)
              tmpVal  = rStarDhCDt(i,j,bi,bj)*theta(i,j,k,bi,bj)
              vT_Mean = vT_Mean + tmpVol*tmpVal
              vS_Mean = vS_Mean
     &          +tmpVol*rStarDhCDt(i,j,bi,bj)*salt(i,j,k,bi,bj)
C-  Atmos in Pot.Temp => convert Omega*Theta to heat flux :
              IF (fluidIsAir) vT_Heat = vT_Heat
     &                                + tmpVol*tmpVal*conv_th2Heat
            ENDDO
           ENDDO
          ENDDO
          tile_wT(bi,bj) = tile_wT(bi,bj) + vT_Mean
          tile_wS(bi,bj) = tile_wS(bi,bj) + vS_Mean
          tileWHeat(bi,bj) = tileWHeat(bi,bj) + vT_Heat
        ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

C--    end bi,bj loop
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( tileArea, theArea, myThid )
       CALL GLOBAL_SUM_TILE_RL( tile_wT , wT_Mean, myThid )
       CALL GLOBAL_SUM_TILE_RL( tile_wS , wS_Mean, myThid )
       IF ( fluidIsAir ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileWHeat , wT_Heat , myThid )
        CALL GLOBAL_SUM_TILE_RL( tileTh2pe , theta2PE, myThid )
       ENDIF
       IF (theArea.GT.0.) THEN
         wT_Mean = wT_Mean / theArea
         wS_Mean = wS_Mean / theArea
         wT_Heat = wT_Heat / theArea
         theta2PE = theta2PE / theArea
         wT_Heat  = wT_Heat  * rUnit2mass
         theta2PE = theta2PE * rUnit2mass
       ENDIF

C-    Print the Average value (monitor type output)

       CALL MON_SET_PREF('surfExpan',myThid)
       CALL MON_OUT_RL( '_theta', wT_Mean, mon_foot_mean ,myThid)
       CALL MON_OUT_RL( '_salt' , wS_Mean, mon_foot_mean ,myThid)
      IF ( fluidIsAir ) THEN
       CALL MON_OUT_RL( '_Heat' , wT_Heat, mon_foot_mean ,myThid)
       CALL MON_SET_PREF('En_Budget',myThid)
       CALL MON_OUT_RL('_T2PE',theta2PE, mon_foot_mean ,myThid)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
