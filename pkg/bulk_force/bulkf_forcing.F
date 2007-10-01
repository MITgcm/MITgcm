C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_forcing.F,v 1.12 2007/10/01 13:33:24 jmc Exp $
C $Name:  $

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

C-    Compute surface fluxes over ice-free ocean only:
      iceornot = 0

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j = 1-Oly,sNy+Oly
          DO i = 1-Olx,sNx+Olx
           IF ( maskC(i,j,ks,bi,bj).NE.0. _d 0 ) THEN

#ifdef ALLOW_FORMULA_AIM
             IF ( useFluxFormula_AIM ) THEN
               CALL BULKF_FORMULA_AIM(
     I            theta(i,j,ks,bi,bj), flwdwn(i,j,bi,bj),
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
     I            cloud(i,j,bi,bj),theta(i,j,ks,bi,bj),
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
     I            Qair(i,j,bi,bj), theta(i,j,ks,bi,bj),
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

C-    Add all contributions.
         DO j = 1-Oly,sNy+Oly
          DO i = 1-Olx,sNx+Olx
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
