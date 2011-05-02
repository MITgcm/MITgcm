C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_r_star.F,v 1.4 2011/05/02 21:04:12 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_APPLY_R_STAR

C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_R_STAR(
     I                bi, bj,
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
#include "OBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: tile indices
C     rStarFldC :: r* thickness-factor (grid-cell center)
C     hFac_FldW :: r* thickness-factor (grid-cell Western -Edge)
C     hFac_FldS :: r* thickness-factor (grid-cell Southern-Edge)
C     myTime    :: current time in simlation
C     myIter    :: current time-step number
C     myThid    :: my Thread Id number
      INTEGER bi,bj
      _RL rStarFldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF
#ifndef DISABLE_RSTAR_CODE

C     !LOCAL VARIABLES:
      INTEGER i,j

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- Set model rStar_Factor to OB values on North/South Boundaries
      IF ( tileHasOBN(bi,bj) ) THEN
C  Northern boundary
       DO i=1-Olx,sNx+Olx
        j = OB_Jn(i,bi,bj)
        IF (j.NE.0) THEN
         rStarFldS(i,j,bi,bj) = 1. _d 0
         IF (ksurfS(i,j,bi,bj).LE.Nr) THEN
          rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + OBNeta(i,bi,bj)
     &       *MAX(recip_Rcol(i,j-1,bi,bj),recip_Rcol(i,j,bi,bj))
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBS(bi,bj) ) THEN
C  Southern boundary
       DO i=1-Olx,sNx+Olx
        j = 1+OB_Js(i,bi,bj)
        IF (j.NE.1) THEN
         rStarFldS(i,j,bi,bj) = 1. _d 0
         IF (ksurfS(i,j,bi,bj).LE.Nr) THEN
          rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + OBSeta(i,bi,bj)
     &       *MAX(recip_Rcol(i,j-1,bi,bj),recip_Rcol(i,j,bi,bj))
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C- Set model rStar_Factor to OB values on East/West Boundaries
      IF ( tileHasOBE(bi,bj) ) THEN
C  Eastern boundary
       DO j=1-Oly,sNy+Oly
        i = OB_Ie(J,bi,bj)
        IF (i.NE.0) THEN
         rStarFldW(i,j,bi,bj) = 1. _d 0
         IF (ksurfW(i,j,bi,bj).LE.Nr) THEN
          rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + OBEeta(j,bi,bj)
     &       *MAX(recip_Rcol(i-1,j,bi,bj),recip_Rcol(i,j,bi,bj))
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBW(bi,bj) ) THEN
C  Western boundary
       DO j=1-Oly,sNy+Oly
        i = 1+OB_Iw(j,bi,bj)
        IF (i.NE.1) THEN
          rStarFldW(i,j,bi,bj) = 1. _d 0
         IF (ksurfW(i,j,bi,bj).LE.Nr) THEN
          rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + OBWeta(j,bi,bj)
     &       *MAX(recip_Rcol(i-1,j,bi,bj),recip_Rcol(i,j,bi,bj))
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* DISABLE_RSTAR_CODE */
#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */
      RETURN
      END
