C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_get_snow.F,v 1.2 2003/12/31 17:44:32 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

      SUBROUTINE THSICE_GET_SNOW(
     I                         Tsf, hi,
     U                         hs, 
     O                         frsnow,
     I                         i,j,bi,bj,myThid )
C     *==========================================================*
C     | S/R  THSICE_GET_SNOW                                       
C     *==========================================================*
C     | Interface S/R : get Snow from ?
C     |  and update snow-thickness and snow-age
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#ifdef ALLOW_BULK_FORCE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "BULKF.h"
#endif
c #include "THSICE_SIZE.h"
#include "THSICE.h"
#include "THSICE_PARAMS.h"

C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER i,j, bi,bj
      INTEGER myThid
      _RL  Tsf           ! surface (ice or snow) temperature
      _RL  hi            ! ice height
      _RL  hs            ! snow height
      _RL  frsnow        ! fractional snow cover

#ifdef ALLOW_THSICE
#ifdef ALLOW_BULK_FORCE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
      _RL  agingTime     ! aging time scale (s)

        agingTime = 50. _d 0 * 86400. _d 0

        frsnow = 0. _d 0  ! temporarily indicates whether fresh snow accumulates
        IF (Tsf.lt.0. _d 0 .and. Tair(i,j,bi,bj).le.Tf0kel
     &                     .and. hi.gt.0. _d 0) THEN
          frsnow = 1. _d 0
cQQ change if reading in snow -- including left over value of rain
          snow(i,j,bi,bj)=rain(i,j,bi,bj)*rhofw/rhos
          rain(i,j,bi,bj)=0. _d 0
          sage(i,j,bi,bj)= 
     &        ( thSIce_deltaT
     &         +sage(i,j,bi,bj)*(1. _d 0-thSIce_deltaT/agingTime)
     &        )*exp( -snow(i,j,bi,bj)*thSIce_deltaT/hNewSnowAge )
          hs = hs + snow(i,j,bi,bj)*thSIce_deltaT
        ELSE
          snow(i,j,bi,bj)=0. _d 0
          sage(i,j,bi,bj)= 
     &        ( thSIce_deltaT
     &         +sage(i,j,bi,bj)*(1. _d 0-thSIce_deltaT/agingTime)
     &        )
        ENDIF

        IF (sage(i,j,bi,bj).gt.agingTime) THEN
           print*,'QQ sage',i,j,sage(i,j,bi,bj)
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_BULK_FORCE */
#endif /* ALLOW_THSICE */

      RETURN
      END
