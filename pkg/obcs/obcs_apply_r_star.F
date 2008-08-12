C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_r_star.F,v 1.3 2008/08/12 22:42:27 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_R_STAR(
     I                bi, bj,
     U                rStarFldC, rStarFldW, rStarFldS,
     I                myTime, myIter, myThid )
C     *==========================================================*
C     | S/R OBCS_APPLY_R_STAR
C     *==========================================================*
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj
      _RL rStarFldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rStarFldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF
#ifndef DISABLE_RSTAR_CODE

C     == Local variables ==
      INTEGER i,j

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- Set model rStar_Factor to OB values on North/South Boundaries
      DO i=1-Olx,sNx+Olx
C  Northern boundary
       j = OB_Jn(i,bi,bj)
       IF (j.NE.0) THEN
         rStarFldS(i,j,bi,bj) = 1. _d 0
        IF (ksurfS(i,j,bi,bj).LE.Nr) THEN
         rStarFldS(i,j,bi,bj) = 1. _d 0
     &      + OBNeta(i,bi,bj)
     &       *MAX(recip_Rcol(i,j-1,bi,bj),recip_Rcol(i,j,bi,bj)) 
        ENDIF
       ENDIF
C  Southern boundary
       j = OB_Js(i,bi,bj)
       IF (j.NE.0) THEN
         rStarFldS(i,j+1,bi,bj) = 1. _d 0
        IF (ksurfS(i,j+1,bi,bj).LE.Nr) THEN
         rStarFldS(i,j+1,bi,bj) = 1. _d 0
     &      + OBSeta(i,bi,bj)
     &       *MAX(recip_Rcol(i,j+1,bi,bj),recip_Rcol(i,j,bi,bj)) 
        ENDIF
       ENDIF
      ENDDO

C- Set model rStar_Factor to OB values on East/West Boundaries
      DO j=1-Oly,sNy+Oly
C  Eastern boundary
       i = OB_Ie(J,bi,bj)
       IF (i.NE.0) THEN
         rStarFldW(i,j,bi,bj) = 1. _d 0
        IF (ksurfW(i,j,bi,bj).LE.Nr) THEN
         rStarFldW(i,j,bi,bj) = 1. _d 0
     &      + OBEeta(i,bi,bj)
     &       *MAX(recip_Rcol(i-1,j,bi,bj),recip_Rcol(i,j,bi,bj)) 
        ENDIF
       ENDIF
C  Western boundary
       i = OB_Iw(j,bi,bj)
       IF (i.NE.0) THEN
         rStarFldW(i+1,j,bi,bj) = 1. _d 0
        IF (ksurfW(i+1,j,bi,bj).LE.Nr) THEN
         rStarFldW(i+1,j,bi,bj) = 1. _d 0
     &      + OBWeta(i,bi,bj)
     &       *MAX(recip_Rcol(i+1,j,bi,bj),recip_Rcol(i,j,bi,bj)) 
        ENDIF
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* DISABLE_RSTAR_CODE */
#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */
      RETURN
      END
