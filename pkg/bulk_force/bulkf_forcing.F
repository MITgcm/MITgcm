C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_forcing.F,v 1.7 2006/01/22 15:51:35 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"
#undef ALLOW_THSICE

      SUBROUTINE BULKF_FORCING(
     I                           myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE BULKF_FORCING
c     ==================================================================
c
c     o Get the surface fluxes used to force ocean model
c       Output:
c       ------
c       ustress, vstress - wind stress
c       qnet             - net heat flux
c       empmr            - freshwater flux
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)     at height hu (m)
c       Tair  - mean air temperature (K)  at height ht (m)
c       Qair  - mean air humidity (kg/kg) at height hq (m)
c       theta(k=1) - sea surface temperature (K)
c       rain  - precipitation
c       runoff - river(ice) runoff
c
c     ==================================================================
c     SUBROUTINE bulkf_forcing
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#include "BULKF_INT.h"
#include "BULKF_DIAG.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
c     == routine arguments ==

      INTEGER myThid
      INTEGER myIter
      _RL     myTime

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j,k

      _RL     df0dT, hfl, evp, dEvdT
#ifdef ALLOW_FORMULA_AIM
      _RL     SHF(1), EVPloc(1), SLRU(1)
      _RL     dEvp(1), sFlx(0:2)
#endif

c variables to include seaice effect
      _RL     tmp
      _RL     albedo
      INTEGER iceornot

c     == external functions ==


c     determine forcing field records

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1-Oly,sNy+Oly
          DO i = 1-Olx,sNx+Olx
           IF (hFacC(i,j,1,bi,bj).NE.0. _d 0) THEN
#ifdef ALLOW_THSICE
             IF (ICEMASK(i,j,bi,bj).GT.0. _d 0) THEN
                 tmp=Tsrf(i,j,bi,bj)
                IF (snowheight(i,j,bi,bj).GT.3. _d -1) THEN
                   iceornot=2
                 ELSE
                   iceornot=1
                 ENDIF
             ELSE
                 tmp=theta(i,j,1,bi,bj)
                 iceornot=0
             ENDIF
#else
             tmp=theta(i,j,1,bi,bj)
             iceornot=0
#endif

#ifdef ALLOW_FORMULA_AIM
             IF ( useFluxFormula_AIM ) THEN
               CALL BULKF_FORMULA_AIM(
     I            theta(i,j,1,bi,bj), flwdwn(i,j,bi,bj),
     I            thAir(i,j,bi,bj), Tair(i,j,bi,bj),
     I            Qair(i,j,bi,bj), wspeed(i,j,bi,bj),
     O            SHF, EVPloc, SLRU,
     O            dEvp, sFlx,
     I            iceornot, myThid )

                  flwup(i,j,bi,bj)= ocean_emissivity*SLRU(1)
C-    reverse sign (AIM convention -> BULKF convention):
                  fsh(i,j,bi,bj) = -SHF(1)
                  flh(i,j,bi,bj) = -Lvap*EVPloc(1)
C-    Convert from g/m2/s to m/s
                  evap(i,j,bi,bj) = EVPloc(1) * 1. _d -3 / rhofw
                  dEvdT = dEvp(1) * 1. _d -3
                  df0dT = sFlx(2)
             ELSE
#else  /* ALLOW_FORMULA_AIM */
             IF ( .TRUE. ) THEN
#endif /* ALLOW_FORMULA_AIM */
               CALL BULKF_FORMULA_LANL(
     I            uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     I            wspeed(i,j,bi,bj), Tair(i,j,bi,bj), Qair(i,j,bi,bj),
     I            cloud(i,j,bi,bj), tmp,
     O            flwup(i,j,bi,bj), flh(i,j,bi,bj),
     O            fsh(i,j,bi,bj), df0dT,
     O            ustress(i,j,bi,bj), vstress(i,j,bi,bj),
     O            evp, savssq(i,j,bi,bj), dEvdT,
     I            iceornot, myThid )

C               Note that the LANL flux conventions are opposite
C               of what they are in the model.

C-             Convert from kg/m2/s to m/s
               evap(i,j,bi,bj) = evp/rhofw

             ENDIF

cQQ use down long wave data
               flwupnet(i,j,bi,bj)=flwup(i,j,bi,bj)-flwdwn(i,j,bi,bj)
cQQ using down solar, need to have water albedo -- .1
#ifdef ALLOW_THSICE
             IF (ICEMASK(i,j,bi,bj).GT.0. _d 0) THEN
                  CALL THSICE_ALBEDO(
     I                     ICEHEIGHT(i,j,bi,bj), SNOWHEIGHT(i,j,bi,bj),
     I                     Tsrf(i,j,bi,bj), snowAge(i,j,bi,bj),
     O                     albedo,
     I                     myThid )
             ELSE
                  albedo= ocean_albedo
             ENDIF
#else
             albedo=  ocean_albedo
#endif
             fswnet(i,j,bi,bj)=solar(i,j,bi,bj)*(1. _d 0-albedo)
           ElSE
             ustress(i,j,bi,bj) = 0. _d 0
             vstress(i,j,bi,bj) = 0. _d 0
             fsh(i,j,bi,bj)     = 0. _d 0
             flh(i,j,bi,bj)     = 0. _d 0
             flwup(i,j,bi,bj)   = 0. _d 0
             evap(i,j,bi,bj)    = 0. _d 0
             fswnet(i,j,bi,bj)  = 0. _d 0
             savssq(i,j,bi,bj)  = 0. _d 0
           ENDIF
          ENDDO
         ENDDO


         IF ( calcWindStress ) THEN
cswd move wind stresses to u and v points
           DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx+1,sNx+Olx
              fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)
     &          *(ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
           DO j = 1-Oly+1,sNy+Oly
            DO i = 1-Olx,sNx+Olx
              fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)
     &          *(vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
         ENDIF

c
c     Add all contributions.
         k = 1
         DO j = 1-Oly,sNy+Oly
          DO i = 1-Olx,sNx+Olx
            IF (hFacC(i,j,1,bi,bj).NE.0. _d 0) THEN
c        Net downward surface heat flux :
              hfl = 0. _d 0
              hfl = hfl + fsh(i,j,bi,bj)
              hfl = hfl + flh(i,j,bi,bj)
              hfl = hfl - flwupnet(i,j,bi,bj)
              hfl = hfl + fswnet(i,j,bi,bj)
c Heat flux:
              Qnet(i,j,bi,bj) = -hfl
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = df0dT
#endif
c Fresh-water flux from Precipitation and Evaporation.
              EmPmR(i,j,bi,bj) = (evap(i,j,bi,bj)-rain(i,j,bi,bj)
     &                           - runoff(i,j,bi,bj))
cccccccccccCHEATccccccccccccccccccccccccc
c            Qnet(i,j,bi,bj) = Qnetch(i,j,bi,bj)
c            EmPmR(i,j,bi,bj) = EmPch(i,j,bi,bj)
cccccccccccccccccccccccccccccccccccccccccc
            ELSE
              Qnet(i,j,bi,bj) = 0. _d 0
              EmPmR(i,j,bi,bj)= 0. _d 0
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = 0. _d 0
#endif
            ENDIF
          ENDDO
         ENDDO

         IF ( blk_taveFreq.GT.0. _d 0 )
     &    CALL BULKF_AVE(bi,bj,myThid)

C--   end bi,bj loops
       ENDDO
      ENDDO

C--   Update the tile edges.
C jmc: Is it necessary ?
c     _EXCH_XY_R8(Qnet,   myThid)
c     _EXCH_XY_R8(EmPmR,   myThid)
c     CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif  /*ALLOW_BULK_FORCE*/

      RETURN
      END
