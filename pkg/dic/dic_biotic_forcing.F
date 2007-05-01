C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_biotic_forcing.F,v 1.14 2007/05/01 21:45:33 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: DIC_BIOTIC_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_BIOTIC_FORCING( PTR_DIC, PTR_ALK, PTR_PO4,
     &                            PTR_DOP, 
#ifdef ALLOW_O2
     &                            PTR_O2, 
#endif
#ifdef ALLOW_FE
     &                            PTR_FE,
#endif
     &                            bi,bj,imin,imax,jmin,jmax,
     &                             myIter,myTime,myThid)

C !DESCRIPTION:
C updates all the tracers for the effects of air-sea exchange, biological
c activity and remineralization

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_BIOTIC.h"
#include "DIC_ABIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  PTR_DIC              :: dissolced inorganic carbon
C  PTR_ALK              :: alkalinity
C  PTR_PO4              :: phosphate
c  PTR_DOP              :: dissolve organic phosphurous
c  PTR_O2               :: oxygen
C  PTR_FE               :: iron
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_O2
      _RL  PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER bi, bj, imin, imax, jmin, jmax

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
C  G*                     :: tendency term for the tracers
C  SURA                   :: tendency of alkalinity due to freshwater
C  SURC                   :: tendency of DIC due to air-sea exchange
C                            and virtual flux
C  SURO                   :: tendency of O2 due to air-sea exchange
C  BIO                    :: tendency of PO4 due to biological productivity,
C                            exchange with DOP pool and reminerization
C  CAR                    :: carbonate changes due to biological 
C                             productivity and reminerization
C  bioac                  :: biological productivity
C  pflux                  :: changes to PO4 due to flux and reminerlization
c  cflux                  :: carbonate changes due to flux and reminerlization
c  freefe                 :: iron not bound to ligand
      _RL  GDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GPO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  SURA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  BIO(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  BIO_kar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  CAR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  pflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  exportflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_O2
      _RL  GO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ALLOW_FE
      _RL  GFE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
       INTEGER I,J,k
       INTEGER nCALCITEstep
CEOP
       jmin=1
       jmax=sNy
       imin=1
       imax=sNx

       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GDIC(i,j,k)=0.d0
           GALK(i,j,k)=0.d0
           GPO4(i,j,k)=0.d0
           GDOP(i,j,k)=0.d0
           SURA(i,j)=0.d0
           SURC(i,j)=0.d0
           CAR(i,j,k)=0.d0
           BIO(i,j,k)=0.d0
           BIO_kar(i,j,k)=0.d0
           bioac(i,j,k)=0.d0
           pflux(i,j,k)=0.d0
           exportflux(i,j,k)=0.d0
           cflux(i,j,k)=0.d0
#ifdef ALLOW_O2
           GO2(i,j,k)=0.d0
#endif
#ifdef ALLOW_FE
           GFE(i,j,k)=0.d0
           freefe(i,j,k)=0.d0
#endif
          ENDDO
         ENDDO
       ENDDO

c carbon air-sea interaction
       CALL DIC_SURFFORCING( PTR_DIC, PTR_ALK, PTR_PO4, SURC,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c alkalinity air-sea interaction
       CALL ALK_SURFFORCING( PTR_ALK, SURA,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

#ifdef ALLOW_O2
c oxygen air-sea interaction
       CALL O2_SURFFORCING( PTR_O2, SURO,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)
#endif

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
       CALL PHOS_FLUX( bioac, pflux, exportflux,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

c carbonate
#ifdef CAR_DISS
c dissolution only below saturation horizon
c code following methid by Karsten Friis
         nCALCITEstep = 3600
         IF(myIter .lt. (nIter0+5) .or.
     &               mod(myIter,nCALCITEstep) .eq. 0)THEN
           CALL CALCITE_SATURATION(PTR_DIC, PTR_ALK, PTR_PO4,
     I                    bi,bj,imin,imax,jmin,jmax,
     I                    myIter,myTime,myThid)
        ENDIF
c
        CALL CAR_FLUX_OMEGA_TOP( bioac, cflux,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)
#else
c old OCMIP way
        CALL CAR_FLUX( bioac, cflux,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)
#endif

c add all tendencies for PO4, DOP, ALK, DIC
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           bio(i,j,k)=-bioac(i,j,k)+pflux(i,j,k)
     &         + maskC(i,j,k,bi,bj)*Kdopremin*PTR_DOP(i,j,k)
           car(i,j,k)=-bioac(i,j,k)* R_cp*rain_ratio(i,j,bi,bj)*
     &                (1.0-DOPfraction)+cflux(i,j,k)
           GPO4(i,j,k)=bio(i,j,k)
           GDOP(i,j,k)=+bioac(i,j,k)*DOPfraction
     &         - maskC(i,j,k,bi,bj)*Kdopremin*PTR_DOP(i,j,k)
           GALK(i,j,k)=+2.d0*car(i,j,k)-R_NP*bio(i,j,k)
           BIO_kar(i,j,k)=R_NP*bio(i,j,k)
           GDIC(i,j,k)=car(i,j,k)+R_CP*bio(i,j,k)
#ifdef ALLOW_O2
           if (PTR_O2(i,j,k).gt.o2crit) then
             GO2(i,j,k)=R_OP*bio(i,j,k)
           else
             GO2(i,j,k)=0.d0
           endif
#endif
#ifdef ALLOW_FE
           GFE(i,j,k)=R_FeP*bio(i,j,k)
     &             -Kscav*freefe(i,j,k)
#endif
           IF (K.eq.1) then
               GALK(i,j,1)=GALK(i,j,1)+SURA(i,j)
               GDIC(i,j,1)=GDIC(i,j,1)+SURC(i,j)
#ifdef ALLOW_O2
               GO2(i,j,1)=GO2(i,j,1)+SURO(i,j)
#endif
#ifdef ALLOW_FE
               GFE(i,j,1)=GFE(i,j,1)+alpfe*
     &                    InputFe(i,j,bi,bj)*recip_drF(1)
     &                       *recip_hFacC(i,j,1,bi,bj)
#endif
           ENDIF
          ENDDO
         ENDDO
       ENDDO


C update
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           PTR_DIC(i,j,k)=
     &      PTR_DIC(i,j,k)+GDIC(i,j,k)*dTtracerLev(k)
           PTR_ALK(i,j,k)=
     &      PTR_ALK(i,j,k)+GALK(i,j,k)*dTtracerLev(k)
           PTR_PO4(i,j,k)=
     &      PTR_PO4(i,j,k)+GPO4(i,j,k)*dTtracerLev(k)
           PTR_DOP(i,j,k)=
     &      PTR_DOP(i,j,k)+GDOP(i,j,k)*dTtracerLev(k)
#ifdef ALLOW_O2
           PTR_O2(i,j,k)=
     &      PTR_O2(i,j,k)+GO2(i,j,k)*dTtracerLev(k)
#endif
#ifdef ALLOW_FE
           PTR_FE(i,j,k)=
     &      PTR_FE(i,j,k)+GFE(i,j,k)*dTtracerLev(k)
#endif
          ENDDO
         ENDDO
       ENDDO

#ifdef ALLOW_FE
#ifdef MINFE
c find free iron and get rid of insoluble part
       call fe_chem(bi,bj,iMin,iMax,jMin,jMax, PTR_FE, freefe,
     &                myIter, mythid)
#endif 
#endif


#ifdef ALLOW_TIMEAVE 
c save averages
      DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
            BIOave(i,j,k,bi,bj)=BIOave(i,j,k,bi,bj)+
     &                          BIOac(i,j,k)*deltaTclock
            CARave(i,j,k,bi,bj)=CARave(i,j,k,bi,bj)+
     &                          CAR(i,j,k)*deltaTclock
            OmegaCave(i,j,k,bi,bj)= OmegaCave(i,j,k,bi,bj)+
     &                           OmegaC(i,j,k,bi,bj)*deltaTclock
            pfluxave(i,j,k,bi,bj)= pfluxave(i,j,k,bi,bj) +
     &                           pflux(i,j,k)*deltaTclock
            epfluxave(i,j,k,bi,bj)= epfluxave(i,j,k,bi,bj) +
     &                           exportflux(i,j,k)*deltaTclock
            cfluxave(i,j,k,bi,bj)= cfluxave(i,j,k,bi,bj) +
     &                           cflux(i,j,k)*deltaTclock
            if (k.eq.1) then
              SURave(i,j,bi,bj)=SURave(i,j,bi,bj)+
     &                          SURC(i,j)*deltaTclock
#ifdef ALLOW_O2
              SUROave(i,j,bi,bj)=SUROave(i,j,bi,bj)+
     &                           SURO(i,j)*deltaTclock
#endif
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
