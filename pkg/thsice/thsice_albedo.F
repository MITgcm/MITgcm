C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_albedo.F,v 1.3 2004/12/17 03:44:52 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_READPARMS
C     !INTERFACE:
      SUBROUTINE THSICE_ALBEDO(
     I                         hi, hs, Tsf, age, 
     O                         albedo,
     I                         myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_ALBEDO
C     *==========================================================*
C     | Compute surface albedo
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "EEPARAMS.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      _RL  hi                  ! ice height
      _RL  hs                  ! snow height
      _RL  Tsf                 ! surface temperature
      _RL  age                 ! snow age
      _RL  albedo              ! surface albedo
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
C     frsnow     :: fractional snow cover 
C     albsno     :: albedo of snow
C     albice     :: albedo of ice
C     albNewSnow :: albedo of new (fresh) snow
c     _RL  frsnow
      _RL albsno, albice
      _RL albNewSnow

C--   Albedo of Bare Sea-Ice
      albice = albIceMax + (albIceMin-albIceMax)*exp(-hi/hAlbIce)

C--   LANL albedo calculation
c     frsnow = 0.
c     if (hs .gt. 0.) frsnow = 1.
c     if (Tsf .lt. 0.) then
c        albedo = frsnow*albColdSnow + (1.-frsnow)*albice
c     else
c        albedo = frsnow*albWarmSnow + (1.-frsnow)*albice
c     endif
C-end LANL albedo calculation 

C--   GISS model albedo calculation
c     albice = 0.7 _d 0

C-    New snow: (linear) transition between -10.oC and 0.oC
C      from cold/dry snow albedo to warm/wet snow albedo
      albNewSnow = albColdSnow
     &      + (albWarmSnow - albColdSnow)
     &       *MAX( 0. _d 0, 1. _d 0 + MIN(Tsf/10. _d 0, 0. _d 0) )
C-    albedo of snow is function of snow-age (make age units into days):
      albsno = albOldSnow
     &       +(albNewSnow-albOldSnow)*exp(-0.2 _d 0*age/86400. _d 0)
C-    layer of snow over the ice:
      albedo = albsno + (albice-albsno)*exp(-hs/hAlbSnow)

      if (albedo.gt.1. _d 0 .or. albedo.lt. .2 _d 0) then
        print*,'QQ - albedo problem', albedo, age, hs, albsno
        stop
      endif

#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
