#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CAR_FLUX( bioac, cflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE CAR_FLUX                                      |
C     | o Calculate carbonate fluxes                              |
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
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       INTEGER imin, imax, jmin, jmax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC
C     == Local variables ==
       _RL caexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER I,J,k, ko
       _RL depth_u, depth_l
       _RL flux_u, flux_l
       _RL zbase
c
c calculate carbonate flux from base of each nlev 
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx 
         DO k=1,nLev
          if (hFacC(i,j,k,bi,bj).gt.0.d0) then
           caexport(i,j)= R_cp*rain_ratio(i,j,bi,bj)*bioac(i,j,k)*
     &           (1.0-DOPfraction)*drF(k)*hFacC(i,j,k,bi,bj)
c calculate flux to each layer from base of k
           zbase=-rF(k+1)
           Do ko=k+1,Nr
            if (hFacC(i,j,ko,bi,bj).gt.0.d0) then
             depth_u=-rF(ko)
             depth_l=depth_u+
     &                  drF(ko)*hFacC(i,j,ko,bi,bj)
             flux_u=caexport(i,j)*exp(-(depth_u-zbase)/zca)
c no flux to ocean bottom
             if (ko.eq.Nr) then
                flux_l=0.d0
             else
               if (hFacC(i,j,ko+1,bi,bj).eq.0.d0) then
                 flux_l=0.d0
               else
                 flux_l=caexport(i,j)*exp(-(depth_l-zbase)/zca)
               endif
             endif
             cflux(i,j,ko)=cflux(i,j,ko) +
     &          ( (Flux_u-Flux_l)/(drF(ko)*hFacC(i,j,ko,bi,bj)) ) 
           else
c if no layer below initial layer, remineralize in place
               if (ko.eq.k+1) cflux(i,j,k)=cflux(i,j,k)
     &                  +bioac(i,j,k)*(1.0-DOPfraction)*
     &                    R_cp*rain_ratio(i,j,bi,bj)
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
