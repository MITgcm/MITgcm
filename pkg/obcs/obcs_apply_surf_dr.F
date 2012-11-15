C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_surf_dr.F,v 1.8 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_APPLY_SURF_DR

C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_SURF_DR(
     I                bi, bj, etaFld,
     U                hFac_FldC, hFac_FldW, hFac_FldS,
     I                myTime, myIter, myThid )

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
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: tile indices
C     etaFld    :: current eta field used to update the hFactor
C     hFac_FldC :: surface-level new thickness factor (grid-cell center)
C     hFac_FldW ::  idem, West  interface (U point)
C     hFac_FldS ::  idem, South interface (V point)
C     myTime    :: current time in simlation
C     myIter    :: current time-step number
C     myThid    :: my Thread Id number
      INTEGER bi,bj
      _RL etaFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS hFac_FldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
      INTEGER i,j,ks
      LOGICAL useOBeta
      _RS hFacInfMOM, hFactmp

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      hFacInfMOM = hFacInf

C-- Eta OB values corresponding to previous iteration are not available when
C   calc_surf_dr is called for the 1rst time (myIter=-1) form initialise_varia.
C   Use current "etaFld" values instead, only for this 1rst call (myIter=-1).
      useOBeta = myIter.NE.-1

C- Set model surface h_Factor to OB values on North/South Boundaries
      IF ( tileHasOBN(bi,bj) ) THEN
C  Northern boundary
       DO i=1-OLx,sNx+OLx
        IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
         j = OB_Jn(i,bi,bj)
         ks = kSurfS(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          IF ( useOBeta ) THEN
           hFactmp = h0FacS(i,j,ks,bi,bj)
     &             + OBNeta(i,bi,bj)*recip_drF(ks)
          ELSE
           hFactmp = h0FacS(i,j,ks,bi,bj)
     &             + etaFld(i,j,bi,bj)*recip_drF(ks)
          ENDIF
          hFac_FldS(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBS(bi,bj) ) THEN
C  Southern boundary
       DO i=1-OLx,sNx+OLx
        IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
         j = OB_Js(i,bi,bj)+1
         ks = kSurfS(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          IF ( useOBeta ) THEN
           hFactmp = h0FacS(i,j,ks,bi,bj)
     &             + OBSeta(i,bi,bj)*recip_drF(ks)
          ELSE
           hFactmp = h0FacS(i,j,ks,bi,bj)
     &             + etaFld(i,j-1,bi,bj)*recip_drF(ks)
          ENDIF
          hFac_FldS(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C- Set model surface h_Factor to OB values on East/West Boundaries
      IF ( tileHasOBE(bi,bj) ) THEN
C  Eastern boundary
       DO j=1-OLy,sNy+OLy
        IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
         i = OB_Ie(j,bi,bj)
         ks = kSurfW(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          IF ( useOBeta ) THEN
           hFactmp = h0FacW(i,j,ks,bi,bj)
     &             + OBEeta(j,bi,bj)*recip_drF(ks)
          ELSE
           hFactmp = h0FacW(i,j,ks,bi,bj)
     &             + etaFld(i,j,bi,bj)*recip_drF(ks)
          ENDIF
          hFac_FldW(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF
      IF ( tileHasOBW(bi,bj) ) THEN
C  Western boundary
       DO j=1-OLy,sNy+OLy
        IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
         i = OB_Iw(j,bi,bj)+1
         ks = kSurfW(i,j,bi,bj)
         IF (ks.LE.Nr) THEN
          IF ( useOBeta ) THEN
           hFactmp = h0FacW(i,j,ks,bi,bj)
     &             + OBWeta(j,bi,bj)*recip_drF(ks)
          ELSE
           hFactmp = h0FacW(i,j,ks,bi,bj)
     &             + etaFld(i-1,j,bi,bj)*recip_drF(ks)
          ENDIF
          hFac_FldW(i,j,bi,bj) = MAX( hFacInfMOM, hFactmp )
         ENDIF
        ENDIF
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* NONLIN_FRSURF */

      RETURN
      END
