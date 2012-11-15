C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_r_star.F,v 1.8 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_APPLY_R_STAR

C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_R_STAR(
     I                bi, bj, etaFld,
     U                rStarFldC, rStarFldW, rStarFldS,
     I                myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_R_STAR
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: tile indices
C     etaFld    :: current eta field used to update the hFactor
C     rStarFldC :: r* thickness-factor (grid-cell center)
C     hFac_FldW :: r* thickness-factor (grid-cell Western -Edge)
C     hFac_FldS :: r* thickness-factor (grid-cell Southern-Edge)
C     myTime    :: current time in simlation
C     myIter    :: current time-step number
C     myThid    :: my Thread Id number
      INTEGER bi, bj
      _RL etaFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef NONLIN_FRSURF
#ifndef DISABLE_RSTAR_CODE

C     !LOCAL VARIABLES:
      INTEGER i,j
      LOGICAL useOBeta

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Eta OB values corresponding to previous iteration are not available when
C   calc_r_star is called for the 1rst time (myIter=-1) form initialise_varia.
C   Use current "etaFld" values instead, only for this 1rst call (myIter=-1).
      useOBeta = myIter.NE.-1

C- Set model rStar_Factor to OB values on North/South Boundaries
      IF ( tileHasOBN(bi,bj) ) THEN
C  Northern boundary
       DO i=1-OLx,sNx+OLx
        IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
         j = OB_Jn(i,bi,bj)
         IF (kSurfS(i,j,bi,bj).LE.Nr) THEN
          IF ( useOBeta ) THEN
           rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + OBNeta(  j,bi,bj) / (rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
          ELSE
           rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + etaFld(i,j,bi,bj) / (rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
          ENDIF
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBS(bi,bj) ) THEN
C  Southern boundary
       DO i=1-OLx,sNx+OLx
        IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
         j = OB_Js(i,bi,bj)+1
         IF (kSurfS(i,j,bi,bj).LE.Nr) THEN
          IF ( useOBeta ) THEN
           rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + OBSeta(  j,bi,bj) / (rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
          ELSE
           rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + etaFld(i,j-1,bi,bj)/(rSurfS(i,j,bi,bj)-rLowS(i,j,bi,bj))
          ENDIF
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C- Set model rStar_Factor to OB values on East/West Boundaries
      IF ( tileHasOBE(bi,bj) ) THEN
C  Eastern boundary
       DO j=1-OLy,sNy+OLy
        IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
         i = OB_Ie(j,bi,bj)
         IF (kSurfW(i,j,bi,bj).LE.Nr) THEN
          IF ( useOBeta ) THEN
           rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + OBEeta(  j,bi,bj) / (rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
          ELSE
           rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + etaFld(i,j,bi,bj) / (rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
          ENDIF
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBW(bi,bj) ) THEN
C  Western boundary
       DO j=1-OLy,sNy+OLy
        IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
         i = OB_Iw(j,bi,bj)+1
         IF (kSurfW(i,j,bi,bj).LE.Nr) THEN
          IF ( useOBeta ) THEN
           rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + OBWeta(  j,bi,bj) / (rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
          ELSE
           rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + etaFld(i-1,j,bi,bj)/(rSurfW(i,j,bi,bj)-rLowW(i,j,bi,bj))
          ENDIF
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* DISABLE_RSTAR_CODE */
#endif /* NONLIN_FRSURF */

      RETURN
      END
