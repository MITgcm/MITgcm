#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_FORCING
C     !INTERFACE:
      SUBROUTINE BULKF_FORCING(
     I                          myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE BULKF_FORCING
C     *==========================================================*
C     \ev

C     o Get the surface fluxes used to force ocean model
C       Output:
C       ------
C       ustress, vstress :: wind stress
C       qnet             :: net heat flux
C       empmr            :: freshwater flux
C       ---------
C
C       Input:
C       ------
C       uwind, vwind  :: mean wind speed (m/s)     at height hu (m)
C       Tair  :: mean air temperature (K)  at height ht (m)
C       Qair  :: mean air humidity (kg/kg) at height hq (m)
C       theta(k=1) :: sea surface temperature (K)
C       rain   :: precipitation
C       runoff :: river(ice) runoff
C
C     ==================================================================
C     SUBROUTINE bulkf_forcing
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#include "BULKF_INT.h"
#include "BULKF_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j
      INTEGER ks, iceornot

      _RL     df0dT, hfl, evp, dEvdT
#ifdef ALLOW_FORMULA_AIM
      _RL     SHF(1), EVPloc(1), SLRU(1)
      _RL     dEvp(1), sFlx(0:2)
#endif

C-    surface level index:
      ks = 1
      IF ( usingPcoords ) ks = Nr

C-    Compute surface fluxes over ice-free ocean only:
      iceornot = 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           IF ( maskC(i,j,ks,bi,bj).NE.0. _d 0 ) THEN

#ifdef ALLOW_FORMULA_AIM
             IF ( useFluxFormula_AIM ) THEN
               CALL BULKF_FORMULA_AIM(
     I            gcmSST(i,j,bi,bj), flwdwn(i,j,bi,bj),
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
                  evap(i,j,bi,bj) = EVPloc(1) * 1. _d -3 / rhoFW
                  dEvdT = dEvp(1) * 1. _d -3
                  df0dT = sFlx(2)

             ELSEIF ( blk_nIter.EQ.0 ) THEN
#else  /* ALLOW_FORMULA_AIM */
             IF ( blk_nIter.EQ.0 ) THEN
#endif /* ALLOW_FORMULA_AIM */
               CALL BULKF_FORMULA_LANL(
     I            uwind(i,j,bi,bj),vwind(i,j,bi,bj),wspeed(i,j,bi,bj),
     I            Tair(i,j,bi,bj), Qair(i,j,bi,bj),
     I            cloud(i,j,bi,bj), gcmSST(i,j,bi,bj),
     O            flwup(i,j,bi,bj), flh(i,j,bi,bj),
     O            fsh(i,j,bi,bj), df0dT,
     O            ustress(i,j,bi,bj), vstress(i,j,bi,bj),
     O            evp, savssq(i,j,bi,bj), dEvdT,
     I            iceornot, myThid )
C               Note that the LANL flux conventions are opposite
C               of what they are in the model.

C-             Convert from kg/m2/s to m/s
               evap(i,j,bi,bj) = evp/rhoFW

             ELSE
               CALL BULKF_FORMULA_LAY(
     I            uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     I            wspeed(i,j,bi,bj), Tair(i,j,bi,bj),
     I            Qair(i,j,bi,bj), gcmSST(i,j,bi,bj),
     O            flwup(i,j,bi,bj), flh(i,j,bi,bj),
     O            fsh(i,j,bi,bj), df0dT,
     O            ustress(i,j,bi,bj), vstress(i,j,bi,bj),
     O            evp, savssq(i,j,bi,bj), dEvdT,
     I            iceornot, i,j,bi,bj,myThid )

C-             Convert from kg/m2/s to m/s
               evap(i,j,bi,bj) = evp/rhoFW

             ENDIF

C- use down long wave data
             flwupnet(i,j,bi,bj)=flwup(i,j,bi,bj)-flwdwn(i,j,bi,bj)
C- using down solar, need to have water albedo -- .1
             fswnet(i,j,bi,bj) = solar(i,j,bi,bj)
     &                         *( 1. _d 0 - ocean_albedo )
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
C-  move wind stresses to u and v points
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx+1,sNx+OLx
              fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)
     &          *(ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
           DO j = 1-OLy+1,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)
     &          *(vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
         ENDIF

C-    Add all contributions.
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            IF ( maskC(i,j,ks,bi,bj).NE.0. _d 0 ) THEN
C-       Net downward surface heat flux :
              hfl = 0. _d 0
              hfl = hfl + fsh(i,j,bi,bj)
              hfl = hfl + flh(i,j,bi,bj)
              hfl = hfl - flwupnet(i,j,bi,bj)
              hfl = hfl + fswnet(i,j,bi,bj)
C- Heat flux:
              Qnet(i,j,bi,bj) = -hfl
              Qsw (i,j,bi,bj) = -fswnet(i,j,bi,bj)
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = df0dT
#endif
C- Fresh-water flux from Precipitation and Evaporation.
              EmPmR(i,j,bi,bj) = (evap(i,j,bi,bj)-rain(i,j,bi,bj)
     &                           - runoff(i,j,bi,bj))*rhoConstFresh
C---- cheating: now done in S/R BULKF_FLUX_ADJUST, over ice-free ocean only
c            Qnet(i,j,bi,bj) = Qnetch(i,j,bi,bj)
c            EmPmR(i,j,bi,bj) = EmPch(i,j,bi,bj)
C----
            ELSE
              Qnet(i,j,bi,bj) = 0. _d 0
              Qsw (i,j,bi,bj) = 0. _d 0
              EmPmR(i,j,bi,bj)= 0. _d 0
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = 0. _d 0
#endif
            ENDIF
          ENDDO
         ENDDO

         IF ( temp_EvPrRn .NE. UNSET_RL ) THEN
C--   Account for energy content of Precip + RunOff & Evap. Assumes:
C     1) Rain has same temp as Air
C     2) Snow has no heat capacity (consistent with seaice & thsice pkgs)
C     3) No distinction between sea-water Cp and fresh-water Cp
C     4) Run-Off comes at the temp of surface water (with same Cp)
C     5) Evap is released to the Atmos @ surf-temp (=SST); should be using
C        the water-vapor heat capacity here and consistently in Bulk-Formulae;
C        Could also be put directly into Latent Heat flux.
c         IF ( SnowFile .NE. ' ' ) THEN
C--   Melt snow (if provided) into the ocean and account for rain-temp
c          DO j = 1-OLy,sNy+OLy
c           DO i = 1-OLx,sNx+OLx
c             Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
c    &              + Lfresh*snowPrecip(i,j,bi,bj)*rhoConstFresh
c    &              - HeatCapacity_Cp
c    &               *( Tair(i,j,bi,bj) - Tf0kel - temp_EvPrRn )
c    &               *( rain(i,j,bi,bj)- snowPrecip(i,j,bi,bj) )
c    &               *rhoConstFresh
c           ENDDO
c          ENDDO
c         ELSE
C--   Make snow (according to Air Temp) and melt it in the ocean
C     note: here we just use the same criteria as over seaice but would be
C           better to consider a higher altitude air temp, e.g., 850.mb
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             IF ( Tair(i,j,bi,bj).LE.Tf0kel ) THEN
               Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + Lfresh*rain(i,j,bi,bj)*rhoConstFresh
              ELSE
C--   Account for rain-temp
               Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              - HeatCapacity_Cp
     &               *( Tair(i,j,bi,bj) - Tf0kel - temp_EvPrRn )
     &               *rain(i,j,bi,bj)*rhoConstFresh
              ENDIF
            ENDDO
           ENDDO
c         ENDIF
C--   Account for energy content of Evap and RunOff:
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
c             Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
c    &              + ( theta(i,j,ks,bi,bj) - temp_EvPrRn )
c    &               *( evap(i,j,bi,bj)*cpwv
c    &                - runoff(i,j,bi,bj)*HeatCapacity_Cp
c    &                )*rhoConstFresh
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &              + ( theta(i,j,ks,bi,bj) - temp_EvPrRn )
     &               *( evap(i,j,bi,bj) - runoff(i,j,bi,bj) )
     &               *HeatCapacity_Cp*rhoConstFresh
              Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)*maskC(i,j,ks,bi,bj)
            ENDDO
          ENDDO
         ENDIF

         IF ( blk_taveFreq.GT.0. _d 0 )
     &     CALL BULKF_AVE( bi, bj, myThid )

C--   end bi,bj loops
       ENDDO
      ENDDO

C--   Update the tile edges.
C jmc: Is it necessary ?
c     _EXCH_XY_RS(Qnet,   myThid)
c     _EXCH_XY_RS(EmPmR,   myThid)
c     CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif  /*ALLOW_BULK_FORCE*/

      RETURN
      END
