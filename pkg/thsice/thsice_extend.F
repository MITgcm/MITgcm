C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_extend.F,v 1.3 2006/02/10 00:30:32 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_EXTEND
C     !INTERFACE:
      SUBROUTINE THSICE_EXTEND(
     I                  esurp, Tf,
     U                  sst, compact, iceThick, snowThick, qicen,
     O                  qleft, fresh, fsalt,
     I                  dBugFlag, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_EXTEND                                              
C     | o Extend sea-ice area incresing ice fraction
C     *==========================================================*
C     | o incorporate surplus of energy to 
C     |   make new ice or make ice grow laterally 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     esurp    :: energy available for freezing           [W/m2]
C     Tf       :: freezing temperature  [oC]
C     sst      :: Sea Surf Temp. [oC]
C     compact  :: fraction of grid area covered in ice
C     iceThick :: ice height  [m]
C     snowThick:: snow height [m]
C     qicen    :: ice enthalpy [J/kg]
C     qleft    :: (additional) heat flux to ocean         [W/m2]
C     fsalt    :: (additional) salt flux to ocean        [ g/m2/s]
C     fresh    :: (additional) fresh water flux to ocean [kg/m2/s]
C     dBugFlag :: allow to print debugging stuff (e.g. on 1 grid point).
C     myThid   :: thread number for this instance of the routine.
      _RL esurp
      _RL Tf
      _RL sst
      _RL compact
      _RL iceThick
      _RL snowThick
      _RL qicen(nlyr)
      _RL qleft
      _RL fresh
      _RL fsalt
      LOGICAL dBugFlag
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     qicAv    :: mean enthalpy of ice (layer 1 & 2) [J/m^3]
      _RL deltaTice ! time-step for ice model
      _RL newIce
      _RL newIceFrac
      _RL iceFraction
      _RL qicAv
      LOGICAL dBug

 1010 FORMAT(A,I3,3F8.3)
 1020 FORMAT(A,1P4E11.3)
      dBug = .FALSE.
c     dBug = dBugFlag

C--   start ice
        deltaTice = thSIce_deltaT
        iceFraction = compact
        newIceFrac = 0. _d 0

C-    enthalpy of new ice to form :
        IF ( compact.LE.0. _d 0 ) THEN
          qicen(1)= -cpwater*Tmlt1
     &             + cpice *(Tmlt1-Tf) + Lfresh*(1. _d 0-Tmlt1/Tf)
          qicen(2)= -cpice *Tf + Lfresh
        ENDIF
        qicAv = rhoi*(qicen(1)+qicen(2))*0.5 _d 0
        newIce = esurp*deltaTice/qicAv

        IF (iceFraction.EQ.0. _d 0) THEN
c         IF (newIce.GE.himin*iceMaskmax) THEN
C- jmc: above is the original version, but below seems more logical:
          IF (newIce.GE.himin0*iceMaskmin) THEN
C-    if there is no ice in grid and enough ice to form:
            iceThick   = MAX(himin0,newIce/iceMaskmax)
            newIceFrac = MIN(newIce/himin0,iceMaskmax)
            compact = newIceFrac
            sst=Tf
          ENDIF
        ELSE
C-    if there is already some ice
            newIceFrac=MIN(newIce/iceThick,iceMaskmax-iceFraction) 
            compact = iceFraction + newIceFrac
C-    spread snow out over ice
            snowThick = snowThick*iceFraction/compact
            sst=(1. _d 0-newIceFrac)*sst+newIceFrac*Tf      
        ENDIF
        qleft= iceThick*newIceFrac*qicAv/deltaTice
        fresh=-(rhoi*iceThick)*newIceFrac/deltaTice
        fsalt=-(rhoi*iceThick*saltice)*newIceFrac/deltaTice

        IF (dBug) WRITE(6,1020) 'ThSI_EXT: iceH, newIce, newIceFrac=',
     &     iceThick, newIce, newIceFrac

#endif /* ALLOW_THSICE */

      RETURN
      END
