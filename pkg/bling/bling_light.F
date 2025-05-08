#include "BLING_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: BLING_LIGHT

C !INTERFACE: ==========================================================
      SUBROUTINE BLING_LIGHT(
     I           mld,
     U           irr_inst, irr_eff,
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C !DESCRIPTION:
C     o calculate effective light for phytoplankton growth
C       There are multiple types of light.
C     - irr_inst is the instantaneous irradiance field.
C     - irr_mix is the same, but with the irr_inst averaged throughout
C       the mixed layer. This quantity is intended to represent the
C       light to which phytoplankton subject to turbulent transport in
C       the mixed-layer would be exposed.
C     - irr_eff is the effective irradiance for photosynthesis,
C       given either by irr_inst or irr_mix, depending on model
C       options and location.
C     o instantaneous light is calculated either from
C     - date and latitude, then exponentially attenuated down the
C       water column, or
C     - short-wave radiation read from external forcing file,
C       attenuated down the water column, or
C     - short-wave radiation distributed through the water column
C       according to SWFRAC routine

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GCHEM.h"
#include "BLING_VARS.h"
#ifdef ALLOW_EXF
#include "FFIELDS.h"
#include "EXF_PARAM.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
      _RL mld       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !OUTPUT PARAMETERS: ==================================================
C     irr_inst      :: instantaneous light
C     irr_eff       :: effective light for photosynthesis
      _RL irr_inst  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_eff   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j,k
      LOGICAL QSW_underice
#ifdef ALLOW_CAL
      INTEGER mydate(4)
#endif
      _RL localTime
      _RL utcTime, diffutc
      _RL sat_atten
      _RL sat_atten_sum(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL chl_sat_sum  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL atten
      _RL irr_surf  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ML_MEAN_LIGHT
      _RL irr_mix   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SumMLIrr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmp_ML    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
c#ifndef USE_QSW
      _RL solar, albedo
      _RL dayfrac, yday, delta
      _RL lat, sun1, dayhrs
      _RL cosz, frac, fluxi
      _RL sfac      (1-OLy:sNy+OLy)
c#endif
#ifdef PHYTO_SELF_SHADING
      _RL k0_rd, chi_rd, e_rd
      _RL k0_bg, chi_bg, e_bg
      _RL kChl_rd   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kChl_bg   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL atten_rd
      _RL atten_bg
      _RL irr_rd    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL irr_bg    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif /* PHYTO_SELF_SHADING */
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
C     kkey :: tape key (tile and level dependent)
      INTEGER tkey, kkey
#endif
CEOP

#ifndef ALLOW_EXF
      CHARACTER*(128) swdownfile
#endif

c  Remove light under ice
c  If using Qsw and seaice/thsice, then ice fraction is already
c  taken into account
      QSW_underice = .FALSE.
#ifdef USE_QSW
      IF ( useSEAICE ) QSW_underice = .TRUE.
      IF ( useThSIce ) QSW_underice = .TRUE.
#endif

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        chl_sat_sum(i,j)   = 0. _d 0
        sat_atten_sum(i,j) = 0. _d 0
#ifdef ML_MEAN_LIGHT
        SumMLIrr(i,j) = 0. _d 0
        tmp_ML(i,j)   = 0. _d 0
#endif
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         irr_eff(i,j,k) = 0. _d 0
#ifdef PHYTO_SELF_SHADING
         irr_rd(i,j,k)        = 0. _d 0
         irr_bg(i,j,k)        = 0. _d 0
#endif
        ENDDO
       ENDDO
      ENDDO

#ifdef PHYTO_SELF_SHADING
c  Specify co-efficients for bio-optical model {kChl = k0 +chi[chl]^e}
c  in red and blue-green fractions (Morel 1988; Foujols et al. 2000)
      k0_rd  = 0.225 _d 0
      k0_bg  = 0.0232 _d 0
      chi_rd = 0.037 _d 0
      chi_bg = 0.074 _d 0
      e_rd   = 0.629 _d 0
      e_bg   = 0.674 _d 0
#endif

c ---------------------------------------------------------------------
c  Surface insolation

#ifndef ALLOW_EXF
      swdownfile = ' '
#else
      IF ( .NOT. gchem_useEXF) THEN
        swdownfile = ' '
      ENDIF
#endif

C If no EXF shortwave, calculate from time and latitude
      IF (swdownfile.EQ.' ') THEN

C  From pkg/dic/dic_insol
C  find light as function of date and latitude
C  based on paltridge and parson

       solar  = 1360. _d 0   !solar constant
       albedo = 0.6 _d 0     !planetary albedo

C Case where a 2-d output array is needed: for now, stop here.
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        STOP 'ABNORMAL END: S/R INSOL: 2-D output not implemented'
       ENDIF

C Find day (****NOTE for year starting in winter*****)
C fraction of year
       dayfrac=mod(myTime,360. _d 0*86400. _d 0)
     &                 /(360. _d 0*86400. _d 0)
C convert to radians
       yday = 2. _d 0*PI*dayfrac
C cosine zenith angle (paltridge+platt)
       delta = (0.006918 _d 0
     &       -(0.399912 _d 0*cos(yday))
     &       +(0.070257 _d 0*sin(yday))
     &       -(0.006758 _d 0*cos(2. _d 0*yday))
     &       +(0.000907 _d 0*sin(2. _d 0*yday))
     &       -(0.002697 _d 0*cos(3. _d 0*yday))
     &       +(0.001480 _d 0*sin(3. _d 0*yday)) )
        DO j=1-OLy,sNy+OLy
C latitude in radians
         lat=YC(1,j,1,bj)*deg2rad
C     latitute in radians, backed out from coriolis parameter
C     (makes latitude independent of grid)
         IF ( usingCartesianGrid .OR. usingCylindricalGrid )
     &    lat = asin( fCori(1,j,1,bj)/(2. _d 0*omega) )
          sun1 = -sin(delta)/cos(delta) * sin(lat)/cos(lat)
         IF (sun1.LE.-0.999 _d 0) sun1=-0.999 _d 0
         IF (sun1.GE. 0.999 _d 0) sun1= 0.999 _d 0
          dayhrs = abs(acos(sun1))
C average zenith angle
          cosz = ( sin(delta)*sin(lat)
     &        +(cos(delta)*cos(lat)*sin(dayhrs)/dayhrs) )
              IF (cosz.LE.5. _d -3) cosz= 5. _d -3
C fraction of daylight in day
          frac = dayhrs/PI
C daily average photosynthetically active solar radiation just below surface
          fluxi = solar*(1. _d 0-albedo)*cosz*frac*parfrac

C convert to sfac
          sfac(j) = MAX(1. _d -5,fluxi)
       ENDDO !j

      ENDIF
C get time (in h) within the day:
      utcTime = MOD( myTime/3600. _d 0, 24. _d 0 )
#ifdef ALLOW_CAL
c mydate is utc time
      IF ( useCAL ) THEN
       CALL CAL_GETDATE( myIter, myTime, mydate, myThid )
       i = mydate(2)/10000
       j = mydate(2)/100
       j = MOD(j,100)
       k = MOD(mydate(2),100)
       utcTime = i + j/60. _d 0 + k/3600. _d 0
      ENDIF
#endif

c ---------------------------------------------------------------------
c  instantaneous light, mixed layer averaged light

      DO j=jmin,jmax
       DO i=imin,imax
c  Photosynthetically-available radiations (PAR)
        IF (swdownfile.EQ.' ') THEN
         irr_surf(i,j) = sfac(j)
        ELSE
#ifdef ALLOW_EXF
         irr_surf(i,j) = max(epsln,
     &                 -parfrac*Qsw(i,j,bi,bj)*maskC(i,j,1,bi,bj))
#endif
        ENDIF
c  Remove light under ice
        IF ( .NOT. QSW_underice ) THEN
         irr_surf(i,j) = irr_surf(i,j)*(1. _d 0 - FIce(i,j,bi,bj))
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

      DO k=1,Nr

#ifdef ALLOW_AUTODIFF_TAMC
       kkey = k + (tkey-1)*Nr
# ifdef ML_MEAN_LIGHT
CADJ STORE tmp_ML = comlev1_bibj_k, key=kkey, kind=isbyte
# endif /* ML_MEAN_LIGHT */
#endif

C Top layer
       IF ( k.EQ.1) THEN

        DO j=jmin,jmax
         DO i=imin,imax

          IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

#ifdef PHYTO_SELF_SHADING
c  Use bio-optical model of Manizza et al. (2005) to account for
c  effect of self-shading on ligt available for phytoplankton
c  growth. As written this DOES NOT feedback onto the absorption
c  of shortwave radiation calculated in the physical model, which
c  is instead calculated in the subroutine swfrac

c  Attenuation coefficient adjusted to chlorophyll in top layer
#ifdef ALLOW_AUTODIFF
           IF ( chl(i,j,1,bi,bj) .GT. 0. _d 0 ) THEN
#endif
            kChl_rd(i,j,1) = k0_rd + chi_rd*(chl(i,j,1,bi,bj)**e_rd)
            kChl_bg(i,j,1) = k0_bg + chi_bg*(chl(i,j,1,bi,bj)**e_bg)
#ifdef ALLOW_AUTODIFF
           ELSE
            kChl_rd(i,j,1) = k0_rd
            kChl_bg(i,j,1) = k0_bg
           ENDIF
#endif
c  Light attenuation in middle of top layer
           atten_rd = kChl_rd(i,j,1)*drF(1)/2. _d 0*hFacC(i,j,1,bi,bj)
           atten_bg = kChl_bg(i,j,1)*drF(1)/2. _d 0*hFacC(i,j,1,bi,bj)
c  Irradiance in middle of top layer
           irr_rd(i,j,1) = irr_surf(i,j) * exp(-atten_rd) * 0.5 _d 0
           irr_bg(i,j,1) = irr_surf(i,j) * exp(-atten_bg) * 0.5 _d 0
           irr_inst(i,j,1) = irr_rd(i,j,1) + irr_bg(i,j,1)
#else /* PHYTO_SELF_SHADING */
C SW radiation attenuated exponentially
c  Light attenuation in middle of top layer
           atten = k0*drF(1)/2. _d 0*hFacC(i,j,1,bi,bj)
           irr_inst(i,j,1) = irr_surf(i,j)*exp(-atten)

#endif /* PHYTO_SELF_SHADING */

          ENDIF
         ENDDO
        ENDDO

C k>1: below surface layer
       ELSE

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef PHYTO_SELF_SHADING
CADJ STORE irr_bg(:,:,k-1) = comlev1_bibj_k, key=kkey, kind=isbyte
CADJ STORE irr_rd(:,:,k-1) = comlev1_bibj_k, key=kkey, kind=isbyte
# endif
#endif

        DO j=jmin,jmax
         DO i=imin,imax

          IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

#ifdef PHYTO_SELF_SHADING
c  Attenuation coefficient adjusted to chlorophyll in kth layer
#ifdef ALLOW_AUTODIFF
           IF ( chl(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
#endif
            kChl_rd(i,j,k) = k0_rd + chi_rd*(chl(i,j,k,bi,bj)**e_rd)
            kChl_bg(i,j,k) = k0_bg + chi_bg*(chl(i,j,k,bi,bj)**e_bg)
#ifdef ALLOW_AUTODIFF
           ELSE
            kChl_rd(i,j,k) = k0_rd
            kChl_bg(i,j,k) = k0_bg
           ENDIF
#endif
c  Light attenuation from one more layer
           atten_rd = kChl_rd(i,j,k)*drF(k)/2. _d 0*hFacC(i,j,k,bi,bj)
     &        + kChl_rd(i,j,k-1)*drF(k-1)/2. _d 0*hFacC(i,j,k-1,bi,bj)
           atten_bg = kChl_bg(i,j,k)*drF(k)/2. _d 0*hFacC(i,j,k,bi,bj)
     &        + kChl_bg(i,j,k-1)*drF(k-1)/2. _d 0*hFacC(i,j,k-1,bi,bj)
c  Irradiance in middle of layer k
           irr_rd(i,j,k) = irr_rd(i,j,k-1)*exp(-atten_rd)
           irr_bg(i,j,k) = irr_bg(i,j,k-1)*exp(-atten_bg)
           irr_inst(i,j,k) = irr_rd(i,j,k) + irr_bg(i,j,k)

#else /* PHYTO_SELF_SHADING */
C SW radiation attenuated exponentially
c  Attenuation from one more layer
           atten = k0*drF(k)/2. _d 0*hFacC(i,j,k,bi,bj)
     &           + k0*drF(k-1)/2. _d 0*hFacC(i,j,k-1,bi,bj)
           irr_inst(i,j,k) = irr_inst(i,j,k-1)*exp(-atten)

#endif /* PHYTO_SELF_SHADING */

          ENDIF
         ENDDO
        ENDDO

       ENDIF /* if k=1 then, else */

C Satellite chl
       DO j=jmin,jmax
        DO i=imin,imax
         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

          IF ( irr_surf(i,j).GT.zeroRL ) THEN
c           sat_atten = irr_inst(i,j,k)/irr_surf(i,j)
#ifdef PHYTO_SELF_SHADING
           sat_atten = exp(-2. _d 0 * k0_bg * (-rC(k)))
#else
           sat_atten = exp(-2. _d 0 * k0 * (-rC(k)))
#endif
           chl_sat_sum(i,j) = chl_sat_sum(i,j)
     &                 + chl(i,j,k,bi,bj)*sat_atten
           sat_atten_sum(i,j) = sat_atten_sum(i,j) + sat_atten
          ENDIF

#ifdef ML_MEAN_LIGHT
c  Mean irradiance in the mixed layer
          IF ( (-rF(k+1).LE. mld(i,j)) .AND.
     &         (-rF(k+1).LT.MLmix_max) ) THEN
           SumMLIrr(i,j) = SumMLIrr(i,j)+drF(k)*irr_inst(i,j,k)
           tmp_ML(i,j) = tmp_ML(i,j) + drF(k)
           irr_mix(i,j) = SumMLIrr(i,j)/tmp_ML(i,j)
          ENDIF
#endif

         ENDIF
        ENDDO
       ENDDO

C     end first k loop
      ENDDO

C Satellite chlorophyll
C Update diagnostic only if ~13:30 local time, when satellite observes
      DO j=jmin,jmax
       DO i=imin,imax
        IF ( usingSphericalPolarGrid .OR. usingCurvilinearGrid ) THEN
C       local-time difference (in h) from UTC time (note: 15 = 360/24)
         diffutc = XC(i,j,bi,bj)/15. _d 0
        ELSE
C       for other grid (e.g., cartesian), assumes no difference in time
         diffutc = 0. _d 0
        ENDIF
        localTime = utcTime + diffutc + 24. _d 0
        localTime = MOD( localTime, 24. _d 0 )
        IF ( localTime.GT.chlsat_locTimWindow(1) .AND.
     &       localTime.LT.chlsat_locTimWindow(2) ) THEN
         chl_sat(i,j,bi,bj) = chl_sat_sum(i,j)
     &                      / (sat_atten_sum(i,j) + epsln)
        ENDIF
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax
         IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN

          irr_eff(i,j,k) = irr_inst(i,j,k)
#ifdef ML_MEAN_LIGHT
c  Inside mixed layer, effective light is set to mean mixed layer light
          IF ( (-rF(k+1).LE. mld(i,j)) .AND.
     &         (-rF(k+1).LT.MLmix_max) ) THEN
           irr_eff(i,j,k) = irr_mix(i,j)
          ENDIF
#endif

         ENDIF
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(chl_sat,'BLGCHLSA',0,1,1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
