C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_suflux_post.F,v 1.3 2004/06/24 23:43:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: SUFLUX_POST
C     !INTERFACE:
      SUBROUTINE SUFLUX_POST(
     I                   FMASK, EMISloc, 
     I                   TLAND, TSEA, TSICE, dTskin, SLRD,
     I                   T0, Q0, DENVV,
     U                   DRAG, SHF, EVAP, SLRup,
     O                   SLRU, TSFC, TSKIN,
     I                   bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SUFLUX_POST
C     | o finish surface flux calculation
C     *==========================================================*
C     | o contain 2nd part of original S/R SUFLUX (Speedy code)
C     *==========================================================*
C--
C--   SUBROUTINE SUFLUX (PSA,UA,VA,TA,QA,RH,PHI,
C--  &                   PHI0,FMASK,TLAND,TSEA,SWAV,SSR,SLRD,
C--  &                   USTR,VSTR,SHF,EVAP,SLRU,
C--  &                   TSFC,TSKIN,U0,V0,T0,Q0)
C--
C--   Purpose: Compute surface fluxes of momentum, energy and moisture,
C--            and define surface skin temperature from energy balance
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "GRID.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Surface flux constants
#include "com_sflcon.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C--   Input:
C    FMASK  :: fraction land - sea - sea-ice (2.5-dim)
C    EMISloc:: longwave surface emissivity
C    TLAND  :: land-surface temperature        (2-dim)
C    TSEA   ::  sea-surface temperature        (2-dim)
C    TSICE  ::  sea-ice surface temperature    (2-dim)
C    dTskin :: temp. correction for daily-cycle heating [K]
C    SLRD   :: sfc lw radiation (downward flux)(2-dim)
C    SSR    :: sfc sw radiation (net flux)     (2-dim)
C    T0     :: near-surface air temperature    (2-dim)
C    Q0     :: near-surface sp. humidity [g/kg](2-dim)
C    DENVV  :: surface flux (sens,lat.) coeff. (=Rho*|V|) [kg/m2/s]
C--   Output:
C    DRAG   :: surface Drag term (= Cd*Rho*|V|)(2-dim)
C    SHF    :: sensible heat flux              (2-dim)
C    EVAP   :: evaporation [g/(m^2 s)]         (2-dim)
C    SLRU   :: sfc lw radiation (upward flux)  (2-dim)
C    SLRup  :: same, for each surface type     (2-dim)
C    TSFC   :: surface temperature (clim.)     (2-dim)
C    TSKIN  :: skin surface temperature        (2-dim)
C--   Input:
C    bi,bj  :: tile index
C    myThid :: Thread number for this instance of the routine
C--
      _RL  FMASK(NGP,3), EMISloc 
      _RL  TLAND(NGP), TSEA(NGP), TSICE(NGP), dTskin(NGP), SLRD(NGP)
      _RL  T0(NGP), Q0(NGP), DENVV(NGP)

      _RL  DRAG(NGP,0:3), SHF(NGP,0:3), EVAP(NGP,0:3), SLRup(NGP,3)
      _RL  SLRU(NGP), TSFC(NGP), TSKIN(NGP)

      INTEGER bi,bj,myThid
CEOP

#ifdef ALLOW_AIM

C-- Local variables:
C     J,i1,j1 :: Loop counters
C     msgBuf  :: Informational/error message buffer
      INTEGER J,i1,j1
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Extrapolation of wind, temp, hum. and density to the surface

C--   2. Computation of fluxes over land and sea

C--   3. Adjustment of skin temperature and fluxes over land
C--      based on energy balance (to be implemented)


C--   4. Weighted average of surface fluxes and temperatures
C--      according to land-sea mask

      DO J=1,NGP
c       USTR(J,3) = USTR(J,2)+FMASK(J,1)*(USTR(J,1)-USTR(J,2))
c       VSTR(J,3) = VSTR(J,2)+FMASK(J,1)*(VSTR(J,1)-VSTR(J,2))
c       DRAG(J,0) = DRAG(J,2)+FMASK(J,1)*(DRAG(J,1)-DRAG(J,2))
c        SHF(J,0) =  SHF(J,2)+FMASK(J,1)*( SHF(J,1)- SHF(J,2))
c       EVAP(J,0) = EVAP(J,2)+FMASK(J,1)*(EVAP(J,1)-EVAP(J,2))
c       SLRU(J)  = SLRup(J,2)+FMASK(J,1)*(SLRup(J,1)-SLRup(J,2))
        DRAG(J,0) = (FMASK(J,1)*DRAG(J,1)+FMASK(J,2)*DRAG(J,2)
     &                                   +FMASK(J,3)*DRAG(J,3))
        SHF (J,0) = (FMASK(J,1)*SHF(J,1) +FMASK(J,2)*SHF(J,2)
     &                                   +FMASK(J,3)*SHF(J,3) )
        EVAP(J,0) = (FMASK(J,1)*EVAP(J,1)+FMASK(J,2)*EVAP(J,2)
     &                                   +FMASK(J,3)*EVAP(J,3))
        SLRU(J)  = (FMASK(J,1)*SLRup(J,1)+FMASK(J,2)*SLRup(J,2)
     &                                   +FMASK(J,3)*SLRup(J,3))
      ENDDO

      DO J=1,NGP
c       TSFC(J)  = TSEA(J)+FMASK(J,1)*(TLAND(J)-TSEA(J))
        TSFC(J)  = (FMASK(J,1)*TLAND(J) + FMASK(J,2)*TSEA(J)
     &                                  + FMASK(J,3)*TSICE(J))
        TSKIN(J) = TSFC(J)+FMASK(J,1)*dTskin(J)
      ENDDO

C-    Compute Net LW surf flux (+=upward) for each surface type:
C      (for diagnostic only)
      DO J=1,NGP
        SLRup(J,1)=EMISloc*SLRup(J,1)-SLRD(J)
        SLRup(J,2)=EMISloc*SLRup(J,2)-SLRD(J)
        SLRup(J,3)=EMISloc*SLRup(J,3)-SLRD(J)
        SLRU(J)   =EMISloc*SLRU(J)
      ENDDO

C-    Check that Temp is OK for LW Radiation scheme :
       DO J=1,NGP
        IF (  TSFC(J).LT.lwTemp1 .OR.
     &        TSFC(J).GT.lwTemp2 ) THEN
         i1 = 1 + mod((J-1),sNx)
         j1 = 1 + int((J-1)/sNx)
         WRITE(msgBuf,'(A,1PE20.13,A,2I4)')
     &    'SUFLUX_POST: TS=', TSFC(J),
     &    ' out of range ',lwTemp1,lwTemp2
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A,1P3E10.3,A,0P3F8.5)')
     &    'SUFLUX_POST: T_Lnd,Sea,Sic=',TLAND(J),TSEA(J),TSICE(J),
     &                ' Mask:',FMASK(J,1),FMASK(J,2),FMASK(J,3)
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(A,2I4,3I3,I6,2F9.3)')
     &    'SUFLUX_POST: Pb in i,j,bi,bj,myThid,IJ,X,Y=',
     &        i1,j1,bi,bj,myThid,J,xC(i1,j1,bi,bj),yC(i1,j1,bi,bj)
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R SUFLUX_POST'
        ENDIF
       ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END
