C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_start.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

C     !ROUTINE: THSICE_START
C     !INTERFACE:
      SUBROUTINE THSICE_START(myThid,
     I                     esurp, qicAv, Tf,
     O                     qleft, fresh, fsalt,
     U                     sst, compact, iceThick, snowThick )
C     *==========================================================*
C     | S/R  THSICE_START                                              
C     | o Check water temperature and start ice layer if
C     | appropriate.                                            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
c #include "SIZE.h"
c #include "EEPARAMS.h"
c #include "PARAMS.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     esurp    :: energy available for freezing
C     qicAv    :: mean enthalphy of ice (layer 1 & 2)
C     Tf       :: freezing temperature
C     qleft    :: (additional) heat flux to ocean        (W/m^2)
C     fsalt    :: (additional) salt flux to ocean        ( g/m^2/s)
C     fresh    :: (additional) fresh water flux to ocean (kg/m^2/s)
C     sst      :: Sea Surf Temp. (oC)
C     compact  :: fraction of grid area covered in ice
C     iceThick :: ice height  (m)
C     snowThick:: snow height (m)
      INTEGER myThid
      _RL esurp
      _RL qicAv
      _RL Tf
      _RL qleft
      _RL fresh
      _RL fsalt
      _RL sst
      _RL compact
      _RL iceThick
      _RL snowThick

#ifdef ALLOW_THSICE

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL deltaTice ! time-step for ice model
c     _RL qi0av, hi0new, q1, q2
      _RL newIce
      _RL newIceFrac
      _RL iceFraction

C     start ice
           deltaTice = thSIce_deltaT
           iceFraction = compact
           newIceFrac = 0. _d 0
           newIce = esurp*deltaTice/qicAv
           IF (iceFraction.EQ.0. _d 0) THEN
c           IF (newIce.GE.himin*iceMaskmax) THEN
C- jmc: above is the original version, but below seems more logical:
            IF (newIce.GE.himin0*iceMaskmin) THEN
C-   if there is no ice in grid and enough ice to form:
              iceThick   = MAX(himin0,newIce/iceMaskmax)
              newIceFrac = MIN(newIce/himin0,iceMaskmax)
              compact = newIceFrac
              sst=Tf
            ENDIF
           ELSE
C- if there is already some ice
              newIceFrac=MIN(newIce/iceThick,iceMaskmax-iceFraction) 
              compact = iceFraction + newIceFrac
C- spread snow out over ice
              snowThick = snowThick*iceFraction/compact
              sst=(1. _d 0-newIceFrac)*sst+newIceFrac*Tf      
           ENDIF
           qleft= iceThick*newIceFrac*qicAv/deltaTice
           fresh=-(rhoi*iceThick)*newIceFrac/deltaTice
           fsalt=-(rhoi*iceThick*saltice)*newIceFrac/deltaTice

#endif /* ALLOW_THSICE */

      RETURN
      END
