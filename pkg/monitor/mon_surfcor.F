C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_surfcor.F,v 1.1 2002/11/22 02:33:24 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_SURFCOR(
     I                       myThid )
C     |==========================================================
C     | SUBROUTINE MON_SURFCOR                                   
C     | o Compute and write area-mean surface correction term    
C     |==========================================================
C     | Diagnose mean surface correction term  = w_surf * Tracer
C     |                 units = W_units * Tracer units ; + = out 
C     | Atmosphere :                                               
C     |   convert surf.cor(Theta) to surface heating, 
C     |                 units= W/m2, + = out
C     |   compute mean conversion term Temp -> PE , units= W/m2, 
C     |               + = decreasing heat content, increasing PE
C     |==========================================================
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#include "GRID.h"
#include "MONITOR.h"

C     === Routine arguments ===
      INTEGER myThid

C     === Local variables ====
      INTEGER i,j,k,ks,bi,bj 
      _RL theArea, wT_Mean, wS_Mean, tmp_wS_M
      _RL tmpVal, ddPI, wT_Heat, theta2PE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c-- Compute surface correction term & do the integral
       theArea = 0.
       wT_Mean = 0.
       wS_Mean = 0.
       wT_Heat = 0.
       theta2PE = 0.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid) 
          tmp_wS_M = wS_Mean
          DO j=1,sNy
           DO i=1,sNx
            ks = ksurfC(i,j,bi,bj)
            IF (ks.LE.Nr) THEN
             theArea = theArea + rA(i,j,bi,bj) 
             tmpVal =
     &          rA(i,j,bi,bj)*wVel(i,j,ks,bi,bj)*theta(i,j,ks,bi,bj)
             wT_Mean = wT_Mean + tmpVal
             wS_Mean = wS_Mean
     &        + rA(i,j,bi,bj)*wVel(i,j,ks,bi,bj)* salt(i,j,ks,bi,bj)
C-- Atmos in Pot.Temp => convert Omega*Theta to heat flux :
             IF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
               wT_Heat = wT_Heat 
     &                 + tmpVal*atm_cp*((rC(ks)/atm_po)**atm_kappa) 
             ENDIF
            ENDIF
           ENDDO
          ENDDO
C-- Atmos in Pot.Temp => conmpute energy conversion Temp -> PE 
C    = Omega*Theta*DeltaPI
          IF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
           DO k=2,Nr
            ddPI=atm_cp*( (rC(K-1)/atm_po)**atm_kappa
     &                   -(rC( K )/atm_po)**atm_kappa )
            DO j=1,sNy
             DO i=1,sNx
              theta2PE = theta2PE
     &         - ddPI*rA(i,j,bi,bj)*wVel(i,j,k,bi,bj)
     &           *(theta(i,j,k,bi,bj)+theta(i,j,k-1,bi,bj))*0.5 _d 0
     &           *maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#ifdef ALLOW_AIM
          IF ( useAIM ) THEN
           DO j=1,sNy
            DO i=1,sNx
             ks = ksurfC(i,j,bi,bj)
             IF (ks.LE.Nr) THEN 
              tmpVal = salt(i,j,ks,bi,bj)
     &               + salt(i,j,Nr,bi,bj)*drF(Nr)*recip_drF(ks)
     &                *hFacC(i,j,Nr,bi,bj)*recip_hFacC(i,j,ks,bi,bj)
              tmp_wS_M = tmp_wS_M
     &         + rA(i,j,bi,bj)*wVel(i,j,ks,bi,bj)*tmpVal
             ENDIF
            ENDDO
           ENDDO
           wS_Mean = tmp_wS_M
          ENDIF
#endif /* ALLOW_AIM */
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(theArea,myThid) 
       _GLOBAL_SUM_R8(wT_Mean,myThid) 
       _GLOBAL_SUM_R8(wS_Mean,myThid) 
       IF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
        _GLOBAL_SUM_R8(wT_Heat,myThid) 
        _GLOBAL_SUM_R8(theta2PE,myThid) 
       ENDIF
       IF (theArea.GT.0.) THEN
         wT_Mean = wT_Mean / theArea
         wS_Mean = wS_Mean / theArea
         wT_Heat = wT_Heat / theArea
         theta2PE = theta2PE / theArea
         wT_Heat  = wT_Heat  * rhoConst*recip_horiVertRatio
         theta2PE = theta2PE * rhoConst*recip_horiVertRatio
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Print the Average value (monitor type output)

       CALL MON_SET_PREF('surf_Corr',myThid)
       CALL MON_OUT_RL( '_theta', wT_Mean, mon_foot_mean ,myThid)
       CALL MON_OUT_RL( '_salt' , wS_Mean, mon_foot_mean ,myThid)
      IF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
       CALL MON_OUT_RL( '_Heat' , wT_Heat, mon_foot_mean ,myThid)
       CALL MON_SET_PREF('En_Budget',myThid)
       CALL MON_OUT_RL('_T2PE',theta2PE, mon_foot_mean ,myThid)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
