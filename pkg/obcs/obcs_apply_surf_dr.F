C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_surf_dr.F,v 1.3 2011/03/02 20:51:58 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_APPLY_SURF_DR

C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_SURF_DR(
     I                bi, bj,
     U                hFac_FldC, hFac_FldW, hFac_FldS,
     I                myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_SURF_DR
C     |  update surface-level thickness factor at Open-Boundaries
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "OBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: tile indices
C     hFac_FldC ::  surface-level new thickness factor (grid-cell center)
C     hFac_FldW ::  idem, West  interface (U point)
C     hFac_FldS ::  idem, South interface (V point)
C     myThid    :: my Thread Id number
      INTEGER bi,bj
      _RS hFac_FldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
      INTEGER i,j,ks
      _RS hFacInfMOM, hFactmp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      hFacInfMOM = hFacInf

C- Set model surface h_Factor to OB values on North/South Boundaries
      IF ( tileHasOBN(bi,bj) ) THEN
C  Northern boundary
       DO i=1-Olx,sNx+Olx
        j = OB_Jn(i,bi,bj)
        IF (j.NE.0) THEN
         ks = ksurfS(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          hFactmp = h0FacS(i,j,ks,bi,bj) + OBNeta(i,bi,bj)*recip_drF(ks)
          hFac_FldS(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBS(bi,bj) ) THEN
C  Southern boundary
       DO i=1-Olx,sNx+Olx
        j = 1+OB_Js(i,bi,bj)
        IF (j.NE.1) THEN
         ks = ksurfS(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          hFactmp = h0FacS(i,j,ks,bi,bj) + OBSeta(i,bi,bj)*recip_drF(ks)
          hFac_FldS(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C- Set model surface h_Factor to OB values on East/West Boundaries
      IF ( tileHasOBE(bi,bj) ) THEN
C  Eastern boundary
       DO j=1-Oly,sNy+Oly
        i = OB_Ie(J,bi,bj)
        IF (i.NE.0) THEN
         ks = ksurfW(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          hFactmp = h0FacW(i,j,ks,bi,bj) + OBEeta(j,bi,bj)*recip_drF(ks)
          hFac_FldW(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBW(bi,bj) ) THEN
C  Western boundary
       DO j=1-Oly,sNy+Oly
        i = 1+OB_Iw(j,bi,bj)
        IF (i.NE.1) THEN
         ks = ksurfW(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          hFactmp = h0FacW(i,j,ks,bi,bj) + OBWeta(j,bi,bj)*recip_drF(ks)
          hFac_FldW(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */
      RETURN
      END
