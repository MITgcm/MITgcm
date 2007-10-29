C $Header: /u/gcmpack/MITgcm/pkg/dic/car_flux.F,v 1.9 2007/10/29 16:49:49 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: CAR_FLUX

C !INTERFACE: ==========================================================
      SUBROUTINE CAR_FLUX( CAR_S, cflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C !DESCRIPTION:
C Calculate carbonate fluxes

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_BIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  CAR_S                :: carbonate source
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  CAR_S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER imin, imax, jmin, jmax, bi, bj

C !OUTPUT PARAMETERS: ===================================================
C  cflux                :: carbonate flux
      _RL cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
c  ko                     :: loop-within-loop index
c  caexport               :: flux of carbonate from base each "productive"
c                            layer
c  depth_u, depth_l       :: depths of upper and lower interfaces
c  flux_u, flux_l         :: flux through upper and lower interfaces
c  zbase                  :: depth of bottom of current productive layer
       _RL zbase
       _RL caexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER I,J,k, ko, kop1, maskp1
       _RL depth_u, depth_l
       _RL flux_u, flux_l
CEOP

C- Calculate carbonate flux from base of each layer
       DO j=jmin,jmax
        DO i=imin,imax
         DO k=1,nlev
          IF (hFacC(i,j,k,bi,bj).GT.0. _d 0) THEN
C- flux out of layer k
           caexport(i,j) = CAR_S(i,j,k)*drF(k)*hFacC(i,j,k,bi,bj) 
C- calculate flux to each layer from base of k

C- if no layer below initial layer (because of bottom or
C- topography), then remineralize in place

           IF (k.EQ.Nr) THEN
             cflux(i,j,k)=cflux(i,j,k) + CAR_S(i,j,k) 
           ELSEIF (hFacC(i,j,k+1,bi,bj).EQ.0. _d 0) THEN
             cflux(i,j,k)=cflux(i,j,k) + CAR_S(i,j,k) 
           ELSE

C- else, let's flux it downward

             zbase=-rF(k+1)
             DO ko=k+1,Nr

C- Upper flux
cdfer              depth_u=-rF(ko)
cdfer              flux_u=caexport(i,j)*exp(-(depth_u-zbase)/zca)
              IF (ko.EQ.k+1) THEN
                 depth_u = zbase
                 flux_u  = caexport(i,j)
              ELSE
                 depth_u = depth_l
                 flux_u  = flux_l
              ENDIF

C- Lower flux (no flux to ocean bottom)
              depth_l=depth_u+
     &                  drF(ko)*hFacC(i,j,ko,bi,bj)
              kop1 = MIN(Nr,ko+1)
              maskp1=1.
              IF (ko.GE.Nr) maskp1=0.
              flux_l=caexport(i,j)*exp(-(depth_l-zbase)/zca)
     &              *maskp1*maskC(i,j,kop1,bi,bj)

              cflux(i,j,ko)=cflux(i,j,ko) +
     &          (flux_u-flux_l)*recip_drF(ko)*recip_hFacC(i,j,ko,bi,bj)

             ENDDO

           ENDIF
          ENDIF
         ENDDO
        ENDDO
       ENDDO
c
#endif
#endif
       RETURN
       END
