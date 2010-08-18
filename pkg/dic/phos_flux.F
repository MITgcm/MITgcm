C $Header: /u/gcmpack/MITgcm/pkg/dic/phos_flux.F,v 1.16 2010/08/18 12:20:13 mlosch Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: PHOS_FLUX

C !INTERFACE: ==========================================================
      SUBROUTINE PHOS_FLUX( BIOac, pflux, exportflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C !DESCRIPTION:
C Calculate the PO4 flux to depth from bio activity

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  BIOac                :: biological productivity
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  BIOac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER imin, imax, jmin, jmax, bi, bj

C !OUTPUT PARAMETERS: ===================================================
C  pflux                :: changes to PO4 due to flux and reminerlization
      _RL pflux     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL exportflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#if (defined ALLOW_PTRACERS && defined DIC_BIOTIC)

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
c  ko                     :: loop-within-loop index
c  bexport                :: flux of phosphorus from base each "productive"
c                            layer
c  depth_l                :: depth and lower interface
c  flux_u, flux_l         :: flux through upper and lower interfaces
c  reminFac               :: abbreviation
c  zbase                  :: depth of bottom of current productive layer
      INTEGER I,J,k, ko, kop1
      _RL zbase
      _RL bexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL reminFac
      _RL depth_l
      _RL flux_u (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flux_l
CEOP

C- Calculate PO4 flux from base of each layer
      DO k=1,nlev
       DO j=jmin,jmax
        DO i=imin,imax
         bexport(i,j) = 0. _d 0
         IF ( _hFacC(i,j,k,bi,bj).gt.0. _d 0 ) THEN
C--   If no layer below initial layer (because of bottom or
C--   topography), then remineralize in here
          IF (k.EQ.Nr) THEN
           pflux(i,j,k)=pflux(i,j,k)+BIOac(i,j,k)*(1. _d 0-DOPfraction) 
          ELSEIF (hFacC(i,j,k+1,bi,bj).EQ.0. _d 0) THEN
           pflux(i,j,k)=pflux(i,j,k)+BIOac(i,j,k)*(1. _d 0-DOPfraction)
          ELSE
C- flux out of layer k
           bexport(i,j)=BIOac(i,j,k)*(1. _d 0-DOPfraction)
     &          *drF(k) * _hFacC(i,j,k,bi,bj)
          ENDIF
         ENDIF
        ENDDO
       ENDDO
C--   If available, flux phosphate downward;
C--   calculate flux to each layer from base of k
       zbase=-rF(k+1)
C--   Upper flux
       DO j=jmin,jmax
        DO i=imin,imax
         flux_u(i,j)  = bexport(i,j)
        ENDDO
       ENDDO
C     Instead of running the loop to ko=Nr and masking the last
C     flux_l, let ko reach only Nr-1 and do a special loop for ko=Nr,
C     in order to save a few expensive exp-function calls
       DO ko=k+1,Nr-1
        kop1   = MIN(Nr,ko+1)
#ifndef NONLIN_FRSURF
C     For the linear free surface, hFacC can be omitted, buying another
C     performance increase of a factor of six on a vector computer.
C     For now this is not implemented via run time flags, in order to 
C     avoid making this code too complicated.
        depth_l  = -rF(ko) + drF(ko)
C       reminFac = (depth_l/zbase)**(-Kremin)
C     The following form does the same, but is faster 
        reminFac = exp(-Kremin*log(depth_l/zbase))
#endif
        DO j=jmin,jmax
         DO i=imin,imax
          IF ( bexport(i,j) .NE. 0. _d 0 ) THEN
C--   Lower flux (no flux to ocean bottom)
#ifdef NONLIN_FRSURF
           depth_l  = -rF(ko) + drF(ko) * _hFacC(i,j,ko,bi,bj)
C          reminFac = (depth_l/zbase)**(-Kremin)
C     The following form does the same, but is faster 
           reminFac = exp(-Kremin*log(depth_l/zbase))
#endif
           flux_l   = bexport(i,j)*reminFac
     &          *maskC(i,j,kop1,bi,bj)
C     
           pflux(i,j,ko)=pflux(i,j,ko) + (flux_u(i,j)-flux_l)
     &          *recip_drF(ko) * _recip_hFacC(i,j,ko,bi,bj)
           exportflux(i,j,ko)=exportflux(i,j,ko)+flux_u(i,j)
C--   Store flux through upper layer for the next k-level
           flux_u(i,j) = flux_l
C     endif bexport .ne. 0
          ENDIF
C     i,j-loops
         ENDDO
        ENDDO
C     ko-loop
       ENDDO
C     now do ko = Nr
       ko = Nr
       flux_l = 0. _d 0
       DO j=jmin,jmax
        DO i=imin,imax
         pflux(i,j,ko)=pflux(i,j,ko) + (flux_u(i,j)-flux_l)
     &        *recip_drF(ko) * _recip_hFacC(i,j,ko,bi,bj)
         exportflux(i,j,ko)=exportflux(i,j,ko)+flux_u(i,j)
        ENDDO
       ENDDO
C     k-loop
      ENDDO
c
#endif /* defined ALLOW_PTRACERS && defined DIC_BIOTIC */
      RETURN
      END
