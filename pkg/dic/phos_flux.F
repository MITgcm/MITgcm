#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE PHOS_FLUX( bioac, pflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE PHOS_FORCING                                   |
C     | o Calculate the PO4 flux to depth from bio activity
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_BIOTIC.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  pflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  zbase
      INTEGER imin, imax, jmin, jmax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC
C     == Local variables ==
       _RL bexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER I,J,k, ko
       _RL depth_u, depth_l
       _RL flux_u, flux_l
c
c calculate PO4 flux from base of each layer
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx 
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
