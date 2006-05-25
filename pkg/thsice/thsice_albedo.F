C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_albedo.F,v 1.4 2006/05/25 18:03:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_ALBEDO
C     !INTERFACE:
      SUBROUTINE THSICE_ALBEDO(
     I                  bi, bj, siLo, siHi, sjLo, sjHi,
     I                  iMin,iMax, jMin,jMax,
     I                  iceMask, hIce, hSnow, tSrf, ageSnw,
     O                  sAlb,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_ALBEDO
C     *==========================================================*
C     | Compute surface albedo over sea-ice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "EEPARAMS.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     siLo,siHi   :: size of input/output array: 1rst dim. lower,higher bounds
C     sjLo,sjHi   :: size of input/output array: 2nd  dim. lower,higher bounds
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  hIce    (hi)   :: ice height [m]
C  hSnow   (hs)   :: snow height [m]
C  tSrf    (Tsf)  :: surface (ice or snow) temperature [oC]
C  ageSnw  (age)  :: snow age [s]
C---  Output
C  sAlb  (albedo) :: surface albedo [0-1]
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER siLo, siHi, sjLo, sjHi
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL iceMask(siLo:siHi,sjLo:sjHi)
      _RL hIce   (siLo:siHi,sjLo:sjHi)
      _RL hSnow  (siLo:siHi,sjLo:sjHi)
      _RL tSrf   (siLo:siHi,sjLo:sjHi)
      _RL ageSnw (siLo:siHi,sjLo:sjHi)
      _RL sAlb   (siLo:siHi,sjLo:sjHi)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
      _RL  hi                  ! ice height
      _RL  hs                  ! snow height
      _RL  Tsf                 ! surface temperature
      _RL  age                 ! snow age
      _RL  albedo              ! surface albedo
C     == Local variables ==
C     frsnow     :: fractional snow cover
C     albsno     :: albedo of snow
C     albice     :: albedo of ice
C     albNewSnow :: albedo of new (fresh) snow
C     albNewSnow :: albedo of new (fresh) snow
C     msgBuf     :: Informational/error meesage buffer
c     _RL  frsnow
      _RL albsno, albice
      _RL albNewSnow
      INTEGER i,j
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO j = jMin, jMax
       DO i = iMin, iMax
        IF ( iceMask(i,j).GT.0. _d 0 ) THEN
          hi  = hIce(i,j)
          hs  = hSnow(i,j)
          Tsf = tSrf(i,j)
          age = ageSnw(i,j)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Albedo of Bare Sea-Ice
      albice = albIceMax + (albIceMin-albIceMax)*EXP(-hi/hAlbIce)

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
     &       +(albNewSnow-albOldSnow)*EXP(-0.2 _d 0*age/86400. _d 0)
C-    layer of snow over the ice:
      albedo = albsno + (albice-albsno)*EXP(-hs/hAlbSnow)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
          IF (albedo.GT.1. _d 0 .OR. albedo.LT. .2 _d 0) THEN
c       print*,'QQ - albedo problem', albedo, age, hs, albsno
            WRITE(msgBuf,'(A,I10,4I6)')
     &       'THSICE_ALBEDO: Problem at:', myIter,i,j,bi,bj
            CALL PRINT_ERROR( msgBuf , myThid)
            WRITE(msgBuf,'(A,1P4E17.9)')
     &       'THSICE_ALBEDO: albedo=', albedo, age, hs, albsno
            CALL PRINT_ERROR( msgBuf , myThid)
            STOP 'THSICE_ALBEDO: albedo out of range'
          ENDIF
          sAlb(i,j) = albedo
        ELSE
          sAlb(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
