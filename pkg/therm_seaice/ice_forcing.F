cswdice ---- takes place of external_forcing_surf when
c            the ice model is working

#include "CPP_OPTIONS.h"
 
C     !ROUTINE: ICE_FORCING
C     !INTERFACE:
      SUBROUTINE ICE_FORCING( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )
C     *==========================================================*
C     | SUBROUTINE  ICE_FORCING             
C     | o Determines forcing terms based on external fields       
C     |   relaxation terms etc, including effects of ice.                                   
C     |   cswd - Apr 02 -- add influence of partial ice cover
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_BULK_FORCE
#include "BULKF_ICE_CONSTANTS.h"
#include "BULKF.h"
#include "BULKF_DIAG.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#include "ICE_DIAGS.h"
#endif
#endif

 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER myThid
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
      _RL qleft
      _RL fsalt
      _RL ffresh
      _RL Tf, cphm, frzmlt, compact

#ifdef ALLOW_THERM_SEAICE


      DO j = jMin, jMax
         DO i = iMin, iMax

         Tf=-mu_Tf*salt(i,j,1,bi,bj)
cQQ from v3.0
         cphm = cpwater*rhosw*drF(1)*hFacC(i,j,1,bi,bj)
         frzmlt = (Tf-theta(i,j,1,bi,bj))*cphm/deltaTtracer
cQQ from v2
cQQ   frzmlt = (Tf-theta(i,j,1,bi,bj))*cpwater*30.d0/dt
         frzmlt = min(max(frzmlt,-1.d4),1.d4)

      IF (iceMask(i,j,bi,bj).eq.0.and.frzmlt.lt.1e-6) THEN
c no freezing
c Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = fu(i,j,bi,bj)
     &         *horiVertRatio*recip_rhoNil*recip_dRf(1)
c Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = fV(i,j,bi,bj)
     &         *horiVertRatio*recip_rhoNil*recip_dRf(1)
c Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = -Qnet(i,j,bi,bj)
     &        *recip_Cp*recip_rhoNil*recip_dRf(1)
c relax to sst equatorward of relaxlat
          if (abs(yC(i,j,bi,bj)).le.relaxlat) then
            surfaceTendencyT(i,j,bi,bj) = 
     &            surfaceTendencyT(i,j,bi,bj)
     &         - lambdaThetaClimRelax*
     &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))
          endif

c Net salt flux
#ifdef USE_NATURAL_BCS
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         *recip_dRf(1)*salt(i,j,1,bi,bj)
#else 
c     Freshwater flux EmPmR:
          surfaceTendencyS(i,j,bi,bj) = EmPmR(i,j,bi,bj)
     &         *recip_dRf(1)*35.
#endif          
       if (abs(yC(i,j,bi,bj)).le.relaxlat) then
           surfaceTendencyS(i,j,bi,bj) =
     &          surfaceTendencyS(i,j,bi,bj)
     &         - lambdaSaltClimRelax*
     &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
       endif
       iceMask(i,j,bi,bj)=0.0
       iceHeight(i,j,bi,bj)=0.0
       snowHeight(i,j,bi,bj)=0.0
       snow(i,j,bi,bj)=0.0
       Tsrf(i,j,bi,bj)=theta(i,j,1,bi,bj)
       Tice1(i,j,bi,bj)=0.0
       Tice2(i,j,bi,bj)=0.0
       Qice1(i,j,bi,bj)=0.0
       Qice2(i,j,bi,bj)=0.0

#ifdef ALLOW_TIMEAVE
        ICE_qleft_AVE(i,j,bi,bj)=ICE_qleft_AVE(i,j,bi,bj)
     &                   +Qnet(i,j,bi,bj)*deltaTclock
        ICE_fresh_AVE(i,j,bi,bj)=ICE_fresh_AVE(i,j,bi,bj)
     &                   +(EmPmR(i,j,bi,bj))*deltaTclock
#endif /*ALLOW_TIMEAVE*/
c
      ELSE
        qleft=0.D0
        fsalt=0.D0
        ffresh=0.D0
        compact=0.d0
        if (hfacC(i,j,1,bi,bj).gt.0.d0) then
c if new ice should have formed last timestep
          if (icemask(i,j,bi,bj).eq.0.d0) then
            call ice_start(i,j,bi, bj, myThid, 
     &           frzmlt,ffresh,fsalt,Tf, compact)
             qleft=0.d0
             theta(i,j,1,bi,bj)=Tf
             salt(i,j,1,bi,bj)=salt(i,j,1,bi,bj)+
     &                     (ffresh-fsalt)/RHONIL
     &                      *recip_dRf(1)*35.*deltaTtracer
          endif
          CALL ICE_THERM(i,j,bi,bj,qleft,fsalt,ffresh,
     &                      compact, myThid)
c create more ice
cQQ       if (compact.gt.0.d0.and.compact.lt.1.d0.and.
cQQ  &                                       frzmlt.ge.1e-6) then
cQQ           call ice_extrastart(i,j,bi, bj, myThid,qleft,
cQQ  &            frzmlt,ffresh,fsalt,Tf,Tair(i,j,bi,bj), compact)
cQQ       endif
        endif
c redistribute ice if too thin
        if (compact.gt.0.d0.and.iceHeight(i,j,bi,bj).lt.hicemin) then
              compact=compact*iceHeight(i,j,bi,bj)/hicemin
              iceHeight(i,j,bi,bj)=hicemin
        endif
c
c Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = 0.D0
c Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = 0.D0
c Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = 
     &       -( compact*qleft + (1.d0-compact)*Qnet(i,j,bi,bj)  )
     &        *recip_Cp*recip_rhoNil*recip_dRf(1)
cQQQQ relaxing below ice
c relax to sst equatorward of relaxlat
c         if (abs(yC(i,j,bi,bj)).le.relaxlat) then
c           surfaceTendencyT(i,j,bi,bj) = 
c    &            surfaceTendencyT(i,j,bi,bj)
c    &         - lambdaThetaClimRelax*
c    &           (theta(i,j,1,bi,bj)-SST(i,j,bi,bj))
c         endif
cQQQQQ

c Freshwater flux:
         ffresh=-(ffresh-fsalt)/RHONIL
         surfaceTendencyS(i,j,bi,bj) = 
     &    ( compact*(ffresh+rain(i,j,bi,bj)) 
     &     + (1.d0-compact)*EmPmR(i,j,bi,bj)) 
     &         *recip_dRf(1)*35.
cQQQQQ relax below ice
c relax to SSS equatorward of relaxlat
c      if (abs(yC(i,j,bi,bj)).le.relaxlat) then
c          surfaceTendencyS(i,j,bi,bj) =
c    &          surfaceTendencyS(i,j,bi,bj)
c    &         - lambdaSaltClimRelax*
c    &           (salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
c      endif
cQQQQQQ 

#ifdef ALLOW_TIMEAVE
             ICE_qleft_AVE(i,j,bi,bj)=ICE_qleft_AVE(i,j,bi,bj)
     &             +( compact*qleft + 
     &              (1.d0-compact)*Qnet(i,j,bi,bj))*deltaTclock
             ICE_fresh_AVE(i,j,bi,bj)=ICE_fresh_AVE(i,j,bi,bj)
     &             +( compact*(ffresh+rain(i,j,bi,bj)) + 
     &                (1.d0-compact)*EmPmR(i,j,bi,bj))*deltaTclock
#endif /*ALLOW_TIMEAVE*/
            
           iceMask(i,j,bi,bj)=compact

      ENDIF !iceMask
cswdice --- end add ---

         ENDDO
      ENDDO

#endif  /*ALLOW_THERM_SEAICE*/

      RETURN
      END
