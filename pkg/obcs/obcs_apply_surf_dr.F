C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_surf_dr.F,v 1.1 2002/01/30 04:22:30 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_APPLY_SURF_DR(
     I                bi, bj, ksurfC, ksurfW, ksurfS,
     U                hFac_FldC, hFac_FldW, hFac_FldS,
     I                myThid )
C     /==========================================================\
C     | S/R OBCS_APPLY_SURF_DR                                   |
C     \==========================================================/
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER bi,bj
      INTEGER ksurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
      INTEGER ksurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
      INTEGER ksurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)  
      _RS hFac_FldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF

C     == Local variables ==
      INTEGER i,j,ks
      _RS hFacInfMOM, hFactmp

      hFacInfMOM = hFacInf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- Set model h_Factor to OB values on North/South Boundaries
      DO i=1-Olx,sNx+Olx
C  Northern boundary
       j = OB_Jn(i,bi,bj)
       IF (j.NE.0) THEN
        ks = ksurfS(i,j,bi,bj)
        IF (ks.LE.Nr) THEN
         hFactmp = OBNhFac0(i,bi,bj) + OBNeta(i,bi,bj)*recip_drF(ks)
         hFac_FldS(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
        ENDIF
       ENDIF
C  Southern boundary
       j = OB_Js(i,bi,bj)
       IF (j.NE.0) THEN
        ks = ksurfS(i,j+1,bi,bj)
        IF (ks.LE.Nr) THEN
         hFactmp = OBShFac0(i,bi,bj) + OBSeta(i,bi,bj)*recip_drF(ks)
         hFac_FldS(i,j+1,bi,bj) = MAX( hFacInfMOM, hFactmp )
        ENDIF
       ENDIF
      ENDDO

C- Set model h_Factor to OB values on East/West Boundaries
      DO j=1-Oly,sNy+Oly
C  Eastern boundary
       i = OB_Ie(J,bi,bj)
       IF (i.NE.0) THEN
        ks = ksurfW(i,j,bi,bj)
        IF (ks.LE.Nr) THEN
         hFactmp = OBEhFac0(j,bi,bj) + OBEeta(j,bi,bj)*recip_drF(ks)
         hFac_FldW(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
        ENDIF
       ENDIF
C  Western boundary
       i = OB_Iw(j,bi,bj)
       IF (i.NE.0) THEN
        ks = ksurfW(i+1,j,bi,bj)
        IF (ks.LE.Nr) THEN
         hFactmp = OBWhFac0(j,bi,bj) + OBWeta(j,bi,bj)*recip_drF(ks)
         hFac_FldW(i+1,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
        ENDIF
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */
      RETURN
      END
