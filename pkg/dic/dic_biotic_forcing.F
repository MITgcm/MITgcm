C modified for external_forcing_DIC.F  August 1999
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c modified swd Oct 01 and Feb 02, for use as package for c40_patch1
c modified to use with c44 and ptracers: swd May 2002
c modified to have carbonate and biological influences: swd June 2002
C

#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_BIOTIC_FORCING( PTR_DIC, PTR_ALK, PTR_PO4,
     &                            PTR_DOP, PTR_O2, 
#ifdef ALLOW_FE
     &                            PTR_FE,
#endif
     &                            bi,bj,imin,imax,jmin,jmax,
     &                             myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE DIC_BIOTIC_FORCING                                   |
C     | o Calculate the changes to DIC through air-sea  fluxes   |  
C     |   carbonates, and biology (parameterizes by PO4)         |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_BIOTIC.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER bi, bj, imin, imax, jmin, jmax

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC
C     == Local variables ==
      _RL  GDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GPO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  SURA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  BIO(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  CAR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  pflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_FE
      _RL  GFE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
       INTEGER I,J,k

       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GDIC(i,j,k)=0.d0
           GALK(i,j,k)=0.d0
           GPO4(i,j,k)=0.d0
           GDOP(i,j,k)=0.d0
           GO2(i,j,k)=0.d0
           SURA(i,j)=0.d0
           SURC(i,j)=0.d0
           CAR(i,j,k)=0.d0
           BIO(i,j,k)=0.d0
           bioac(i,j,k)=0.d0
           pflux(i,j,k)=0.d0
           cflux(i,j,k)=0.d0
#ifdef ALLOW_FE
           GFE(i,j,k)=0.d0
           freefe(i,j,k)=0.d0
#endif
          ENDDO
         ENDDO
       ENDDO

c carbon air-sea interaction
       CALL DIC_SURFFORCING( PTR_DIC, SURC,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c alkalinity air-sea interaction
       CALL ALK_SURFFORCING( PTR_ALK, SURA,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c carbon air-sea interaction
       CALL O2_SURFFORCING( PTR_O2, SURO,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

#ifdef ALLOW_FE
c find free iron
       call fe_chem(bi,bj,iMin,iMax,jMin,jMax, PTR_FE, freefe,
     &                myIter, mythid)
#endif


c biological activity
       CALL BIO_EXPORT( PTR_PO4 , 
#ifdef ALLOW_FE
     I           PTR_FE, 
#endif 
     I           bioac,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

c flux of po4 from layers with biological activity
       CALL PHOS_FLUX( bioac, pflux,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c carbonate
        CALL CAR_FLUX( bioac, cflux,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c add all tendencies for PO4, DOP, ALK, DIC
       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           bio(i,j,k)=-bioac(i,j,k)+pflux(i,j,k)
     &         + maskC(i,j,k,bi,bj)*Kdopremin*PTR_DOP(i,j,k)
           car(i,j,k)=-bioac(i,j,k)* R_cp*rain_ratio(i,j,bi,bj)*
     &                (1.0-DOPfraction)+cflux(i,j,k)
           GPO4(i,j,k)=bio(i,j,k)
           GDOP(i,j,k)=+bioac(i,j,k)*DOPfraction
     &         - maskC(i,j,k,bi,bj)*Kdopremin*PTR_DOP(i,j,k)
           GALK(i,j,k)=+2.d0*car(i,j,k)-R_NP*bio(i,j,k)
           GDIC(i,j,k)=car(i,j,k)+R_CP*bio(i,j,k)
           if (PTR_O2(i,j,k).gt.o2crit) then
             GO2(i,j,k)=R_OP*bio(i,j,k)
           else
             GO2(i,j,k)=0.d0
           endif
#ifdef ALLOW_FE
           GFE(i,j,k)=R_FeP*bio(i,j,k)
     &             -Kscav*freefe(i,j,k)
#endif
           IF (K.eq.1) then
               GALK(i,j,1)=GALK(i,j,1)+SURA(i,j)
               GDIC(i,j,1)=GDIC(i,j,1)+SURC(i,j)
               GO2(i,j,1)=GO2(i,j,1)+SURO(i,j)
#ifdef ALLOW_FE
               GFE(i,j,1)=GFE(i,j,1)+alpfe*
     &                    InputFe(i,j,bi,bj)/delz(1)
#endif
           ENDIF
          ENDDO
         ENDDO
       ENDDO


C update
       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           PTR_DIC(i,j,k)=
     &      PTR_DIC(i,j,k)+GDIC(i,j,k)*deltaTtracer
           PTR_ALK(i,j,k)=
     &      PTR_ALK(i,j,k)+GALK(i,j,k)*deltaTtracer
           PTR_PO4(i,j,k)=
     &      PTR_PO4(i,j,k)+GPO4(i,j,k)*deltaTtracer
           PTR_DOP(i,j,k)=
     &      PTR_DOP(i,j,k)+GDOP(i,j,k)*deltaTtracer
           PTR_O2(i,j,k)=
     &      PTR_O2(i,j,k)+GO2(i,j,k)*deltaTtracer
#ifdef ALLOW_FE
           PTR_FE(i,j,k)=
     &      PTR_FE(i,j,k)+GFE(i,j,k)*deltaTtracer
#endif
          ENDDO
         ENDDO
       ENDDO

#ifdef ALLOW_TIMEAVE 
c save averages
      DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            BIOave(i,j,k,bi,bj)=BIOave(i,j,k,bi,bj)+
     &                          BIOac(i,j,k)*deltaTclock
            CARave(i,j,k,bi,bj)=CARave(i,j,k,bi,bj)+
     &                          CAR(i,j,k)*deltaTclock
            if (k.eq.1) then
              SURave(i,j,bi,bj)=SURave(i,j,bi,bj)+
     &                          SURC(i,j)*deltaTclock
              SUROave(i,j,bi,bj)=SUROave(i,j,bi,bj)+
     &                           SURO(i,j)*deltaTclock
              pCO2ave(i,j,bi,bj)=pCO2ave(i,j,bi,bj)+
     &                           pCO2(i,j,bi,bj)*deltaTclock
              pHave(i,j,bi,bj)=pHave(i,j,bi,bj)+
     &                           pH(i,j,bi,bj)*deltaTclock
              fluxCO2ave(i,j,bi,bj)=fluxCO2ave(i,j,bi,bj)+
     &                           fluxCO2(i,j,bi,bj)*deltaTclock
            endif
          ENDDO
         ENDDO
      ENDDO
      do k=1,Nr
       dic_timeave(bi,bj,k)=dic_timeave(bi,bj,k)+deltaTclock
      enddo
#endif

#endif
#endif

c
       RETURN
       END
