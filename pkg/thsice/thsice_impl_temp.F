C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_impl_temp.F,v 1.1 2004/04/07 23:40:34 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
 
C     !ROUTINE: THSICE_IMPL_TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_IMPL_TEMP( 
     I                netSW, sFlx,
     O                dTsurf,
     I                bi, bj, myTime, myIter, myThid)
C     *==========================================================*
C     | S/R  THSICE_IMPL_TEMP             
C     | o Calculate sea-ice and surface temp. implicitly
C     *==========================================================*
C     | o return surface fluxes for atmosphere boundary layer 
C     |  physics (and therefore called within atmospheric physics)
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     netSW   :: net Short Wave surf. flux (+=down) [W/m2]
C     sFlx    :: surf. heat flux (+=down) except SW, function of surf. temp Ts:
C                0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C     dTsurf  :: surf. temp adjusment: Ts^n+1 - Ts^n
C     bi,bj   :: Tile index
C     myIter  :: iteration counter for this thread
C     myTime  :: time counter for this thread
C     myThid  :: thread number for this instance of the routine.
      _RL netSW  (sNx,sNy)
      _RL sFlx   (sNx,sNy,0:2)
      _RL dTsurf (sNx,sNy)
      INTEGER bi,bj
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     flxSW    :: net Short-Wave (+=down) at surface [W/m2]
C     flxExcSw :: surf. heat flux (+=down) except SW, function of surf. temp Ts:
C                0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C     sHeating :: surf heating left to melt snow or ice (= Atmos-conduction)
C     flxCnB   :: heat flux conducted through the ice to bottom surface
C     flxtmp   :: net heat flux from the atmosphere ( >0 downward)
C     evptmp   :: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
      INTEGER i,j
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL flxSW
      _RL Tf
      _RL flxtmp, evptmp
      _RL  flxExcSw(0:2)
      _RL hIce, hSnow, Tsf, Tice(nlyr), qicen(nlyr)

      LOGICAL dBug

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy
      dBug = .FALSE.
 1010 FORMAT(A,1P4E11.3)

      DO j = jMin, jMax
       DO i = iMin, iMax
c       dBug = ( bi.EQ.3 .AND. i.EQ.13 .AND. j.EQ.13 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ; 
C     Solve for surface and ice temperature (implicitly) ; compute surf. fluxes
C-------
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN 
          Tf      = -mu_Tf*sOceMxL(i,j,bi,bj)
          hIce    = iceHeight(i,j,bi,bj)
          hSnow   = snowHeight(i,j,bi,bj)
          flxExcSw(0) = sFlx(i,j,0)
          flxExcSw(1) = sFlx(i,j,1)
          flxExcSw(2) = sFlx(i,j,2)
          flxSW   = netSW(i,j)
          Tsf     = Tsrf(i,j,bi,bj)
          qicen(1)= Qice1(i,j,bi,bj)
          qicen(2)= Qice2(i,j,bi,bj)
          CALL THSICE_SOLVE4TEMP(
     I          useBulkforce, flxExcSw, Tf, hIce, hSnow,
     U          flxSW, Tsf, qicen,
     O          Tice, sHeating(i,j,bi,bj), flxCndBt(i,j,bi,bj),
     O          dTsurf(i,j), flxtmp, evptmp,
     I          i,j, bi,bj, myThid)
C--    Update Fluxes :
          Qsw(i,j,bi,bj) = -flxSW
C--    Update Sea-Ice state :
          Tsrf(i,j,bi,bj) =Tsf
          Tice1(i,j,bi,bj)=Tice(1)
          Tice2(i,j,bi,bj)=Tice(2)
          Qice1(i,j,bi,bj)=qicen(1)
          Qice2(i,j,bi,bj)=qicen(2)
        ELSE
          dTsurf(i,j) = 0. _d 0
          Qsw(i,j,bi,bj) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
