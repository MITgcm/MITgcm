#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: PHOS_FLUX

C !INTERFACE: ==========================================================
      SUBROUTINE PHOS_FLUX( bioac, pflux,
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
#include "DIC_BIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  bioac                :: biological productivity
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)


C !OUTPUT PARAMETERS: ===================================================
C  pflux               :: changes to PO4 due to flux and reminerlization
      _RL  pflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER imin, imax, jmin, jmax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
c  ko                     :: loop-within-loop index
c bexport                :: flux of phosphurous from base each "productive"
c                            layer
c depth_u, depth_l        :: depths of upper and lower interfaces
c flux_u, flux_l          :: flux through upper and lower interfaces
c zbase                   :: depth of bottom of current productive layer
       _RL  zbase
       _RL bexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER I,J,k, ko
       _RL depth_u, depth_l
       _RL flux_u, flux_l
CEOP


c
c calculate PO4 flux from base of each layer
       DO j=jmin,jmax
        DO i=imin,imax
         DO k=1,nlev
          if (hFacC(i,j,k,bi,bj).gt.0.d0) then
c flux out of layer k
           bexport(i,j)=bioac(i,j,k)*(1.0-DOPfraction)*
     &                  drF(k)*hFacC(i,j,k,bi,bj)
c calculate flux to each layer from base of k
           zbase=-rF(k+1)
           Do ko=k+1,Nr
            if (hFacC(i,j,ko,bi,bj).gt.0.d0) then
              depth_u=-rF(ko)
              depth_l=depth_u+
     &                  drF(ko)*hFacC(i,j,ko,bi,bj)
c
              flux_u=bexport(i,j)*((depth_u/zbase)**(-Kremin))
c no flux to ocean bottom
              if (ko.eq.Nr) then
                 flux_l=0.d0
              else
                 if (hFacC(i,j,ko+1,bi,bj).eq.0.d0) then
                   flux_l=0.d0
                 else
                   flux_l=bexport(i,j)*((depth_l/zbase)**(-Kremin))
                 endif
              endif
              pflux(i,j,ko)=pflux(i,j,ko) +
     &          ( (Flux_u-Flux_l)/(drF(ko)*hFacC(i,j,ko,bi,bj)) ) 
            else
c if no layer below initial layer, remineralize in place
               if (ko.eq.k+1) pflux(i,j,k)=pflux(i,j,k)
     &                  +bioac(i,j,k)*(1.0-DOPfraction)
            endif
           ENDDO
          endif
         ENDDO 
        ENDDO
       ENDDO
c
#endif
#endif
       RETURN
       END
