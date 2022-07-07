#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
# ifdef ALLOW_EXF
#  include "EXF_OPTIONS.h"
# endif
# define ALLOW_AUTODIFF_TAMC_MORE
#endif

CBOP
C     !ROUTINE: THSICE_SOLVE4TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_SOLVE4TEMP(
     I                  bi, bj,
     I                  iMin,iMax, jMin,jMax, dBugFlag,
     I                  useBulkForce, useEXF,
     I                  icMask, hIce, hSnow, tFrz, flxExSW,
     U                  flxSW, tSrf, qIc1, qIc2,
     O                  tIc1, tIc2, dTsrf,
     O                  sHeat, flxCnB, flxAtm, evpAtm,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_SOLVE4TEMP
C     *==========================================================*
C     | Solve (implicitly) for sea-ice and surface temperature
C     *==========================================================*
C     \ev

C ADAPTED FROM:
C LANL CICE.v2.0.2
C-----------------------------------------------------------------------
C.. thermodynamics (vertical physics) based on M. Winton 3-layer model
C.. See Bitz, C. M. and W. H. Lipscomb, 1999:  An energy-conserving
C..       thermodynamic sea ice model for climate study.
C..       J. Geophys. Res., 104, 15669 - 15677.
C..     Winton, M., 1999:  "A reformulated three-layer sea ice model."
C..       Submitted to J. Atmos. Ocean. Technol.
C.. authors Elizabeth C. Hunke and William Lipscomb
C..         Fluid Dynamics Group, Los Alamos National Laboratory
C-----------------------------------------------------------------------
Cc****subroutine thermo_winton(n,fice,fsnow,dqice,dTsfc)
C.. Compute temperature change using Winton model with 2 ice layers, of
C.. which only the top layer has a variable heat capacity.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF
# include "THSICE_SIZE.h"
c#include "THSICE_VARS.h"
# ifdef ALLOW_EXF
#  include "EXF_PARAM.h"
#  include "EXF_CONSTANTS.h"
#  include "EXF_FIELDS.h"
# endif
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C     useBulkForce:: use surf. fluxes from bulk-forcing external S/R
C     useEXF      :: use surf. fluxes from exf          external S/R
C---  Input:
C     icMask      :: sea-ice fractional mask [0-1]
C     hIce        :: ice height [m]
C     hSnow       :: snow height [m]
C     tFrz        :: sea-water freezing temperature [oC] (function of S)
C     flxExSW     :: Net (minus SW) surf. heat flux (+=down) fct of surf temp Ts
C                    0: Flx(Ts=0) ; 1: Flx(Ts=Ts^n) ; 2: d.Flx/dTs(Ts=Ts^n)
C---  Modified (input&output):
C     flxSW       :: net Short-Wave flux (+=down) [W/m2]: input= at surface
C                 ::                 output= below sea-ice, into the ocean
C     tSrf        :: surface (ice or snow) temperature
C     qIc1        :: ice enthalpy [J/kg], 1rst level
C     qIc2        :: ice enthalpy [J/kg], 2nd level
C---  Output
C     tIc1        :: temperature of ice layer 1 [oC]
C     tIc2        :: temperature of ice layer 2 [oC]
C     dTsrf       :: surface temp adjusment: Ts^n+1 - Ts^n
C     sHeat       :: surf heat flux left to melt snow or ice (=Atmos-conduction)
C     flxCnB      :: heat flux conducted through the ice to bottom surface
C     flxAtm      :: net flux of energy from the atmosphere [W/m2] (+=down)
C                    without snow precip. (energy=0 for liquid water at 0.oC)
C     evpAtm      :: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
      LOGICAL useBulkForce
      LOGICAL useEXF
      _RL icMask (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hSnow  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFrz   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tSrf   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc2   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tIc1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tIc2   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sHeat  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxCnB (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evpAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C
      _RL flxExSW(1:sNx,1:sNy,0:2)
      _RL dTsrf  (1:sNx,1:sNy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     == Local Variables ==
C     useBlkFlx    :: use some bulk-formulae to compute fluxes over sea-ice
C                  :: (otherwise, fluxes are passed as argument from AIM)
C     iterate4Tsf  :: to stop to iterate when all icy grid pts Tsf did converged
C     iceFlag      :: True= do iterate for Surf.Temp ; False= do nothing
C     frsnow       :: fractional snow cover
C     fswpen       :: SW penetrating beneath surface [W/m2]
C     fswdn        :: SW absorbed at surface [W/m2]
C     fswint       :: SW absorbed in ice [W/m2]
C     fswocn       :: SW passed through ice to ocean [W/m2]
C     Tsf_mlt      :: surface temperature of snow or ice at melting point
C     flx0exSW     :: net (minus SW) surface heat flux over melting snow/ice
C     flxTexSW     :: net (minus SW) surface heat flux (+=down) [W/m2]
C     dFlxdT       :: derivative of flxNet wrt tSrf [W/m2/K]
C     evap_0       :: evaporation over melting snow/ice (>0 evaporate) [kg/m2/s]
C                     renamed to avoid conflict with EXF evap0 (AUTODIFF)
C     evapT        :: evaporation over snow/ice (>0 if evaporate) [kg/m2/s]
C     dEvdT        :: derivative of evap. with respect to tSrf [kg/m2/s/K]
C     flxNet       :: net surf heat flux (+=down), from atmos to sea-ice [W/m2]
C     netSW        :: net Short-Wave flux at surface (+=down) [W/m2]
C     fct          :: heat conducted to top surface
C     k12, k32     :: thermal conductivity terms
C     a10,b10,c10  :: coefficients in quadratic eqn for T1
C     a1, b1, c1   :: coefficients in quadratic eqn for T1
C     dt           :: timestep
      LOGICAL useBlkFlx
      LOGICAL iterate4Tsf
      INTEGER i, j, k, iterMax
      INTEGER ii, jj, icount, stdUnit
#if ( defined ALLOW_DBUG_THSICE || !defined ALLOW_AUTODIFF )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      _RL  frsnow
      _RL  fswpen
      _RL  fswdn
      _RL  fswint
      _RL  fswocn
      _RL  iceFlag (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  Tsf_mlt (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  flx0exSW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  flxTexSW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  dFlxdT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  evap_0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  evapT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  dEvdT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  flxNet
      _RL  fct
      _RL  k12     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  k32
      _RL  a10     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  b10     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  c10     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  a1, b1, c1
      _RL  dt
      _RL  recip_dhSnowLin
      _RL  TsfTmp, tIc2Tmp
#ifdef ALLOW_DBUG_THSICE
      _RL  netSW
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey, kkey
#endif

#ifdef ALLOW_DBUG_THSICE
C-    Define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"
 1010 FORMAT(A,I3,3F11.6)
 1020 FORMAT(A,1P4E14.6)
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
CADJ STORE flxsw  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hSnow  = comlev1_bibj, key = ticekey, kind = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
        tIc1(i,j) = 0. _d 0
        tIc2(i,j) = 0. _d 0
C-   set these arrays everywhere: overlap are not set and not used,
C     but some arrays are stored and storage includes overlap.
        flx0exSW(i,j) = 0. _d 0
        flxTexSW(i,j) = 0. _d 0
        dFlxdT  (i,j) = 0. _d 0
        evap_0  (i,j) = 0. _d 0
        evapT   (i,j) = 0. _d 0
        dEvdT   (i,j) = 0. _d 0
        iceFlag (i,j) = 0. _d 0
        Tsf_mlt (i,j) = 0. _d 0
       ENDDO
      ENDDO
#endif

      stdUnit = standardMessageUnit
      useBlkFlx = useEXF .OR. useBulkForce
      dt  = thSIce_dtTemp
      IF ( dhSnowLin.GT.0. _d 0 ) THEN
        recip_dhSnowLin = 1. _d 0 / dhSnowLin
      ELSE
        recip_dhSnowLin = 0. _d 0
      ENDIF

      iterate4Tsf = .FALSE.
      icount = 0

      DO j = jMin, jMax
       DO i = iMin, iMax
        IF ( icMask(i,j).GT.0. _d 0) THEN
          iterate4Tsf  = .TRUE.
          iceFlag(i,j) = 1. _d 0
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,'(A,2I4,2I2)')
     &         'ThSI_SOLVE4T: i,j=',i,j,bi,bj
#endif
          IF ( hIce(i,j).LT.hIceMin ) THEN
C     if hi < hIceMin, melt the ice.
C     keep the position of this problem but do the stop later
           ii = i
           jj = j
           icount = icount + 1
          ENDIF

C--   Fractional snow cover:
C     assume a linear distribution of snow thickness, with dhSnowLin slope,
C      from hs-dhSnowLin to hs+dhSnowLin if full ice & snow cover.
C     frsnow = fraction of snow over the ice-covered part of the grid cell
          IF ( hSnow(i,j) .GT. icMask(i,j)*dhSnowLin ) THEN
           frsnow = 1. _d 0
          ELSE
           frsnow = hSnow(i,j)*recip_dhSnowLin/icMask(i,j)
           IF ( frsnow.GT.0. _d 0 ) frsnow = SQRT(frsnow)
          ENDIF

C--   Compute SW flux absorbed at surface and penetrating to layer 1.
          fswpen = flxSW(i,j) * (1. _d 0 - frsnow) * i0swFrac
          fswocn = fswpen * exp(-ksolar*hIce(i,j))
          fswint = fswpen - fswocn
          fswdn  = flxSW(i,j) - fswpen

C     Initialise Atmospheric surf. heat flux with net SW flux at the surface
          flxAtm(i,j) = flxSW(i,j)
C     SW flux at sea-ice base left to the ocean
          flxSW(i,j) = fswocn
C     Initialise surface Heating with SW contribution
          sHeat(i,j) = fswdn

C--   Compute conductivity terms at layer interfaces.
          k12(i,j) = 4. _d 0*kIce*kSnow
     &             / (kSnow*hIce(i,j) + 4. _d 0*kIce*hSnow(i,j))
          k32      = 2. _d 0*kIce  / hIce(i,j)

C--   Compute ice temperatures
          a1 = cpIce
          b1 = qIc1(i,j) + (cpWater-cpIce )*Tmlt1 - Lfresh
          c1 = Lfresh * Tmlt1
          tIc1(i,j) = 0.5 _d 0 *(-b1 - SQRT(b1*b1-4. _d 0*a1*c1))/a1
          tIc2(i,j) = (Lfresh-qIc2(i,j)) / cpIce

#ifdef ALLOW_DBUG_THSICE
          IF (tIc1(i,j).GT.0. _d 0 ) THEN
           WRITE(stdUnit,'(A,I12,1PE14.6)')
     &       ' BBerr: Tice(1) > 0 ; it=', myIter, qIc1(i,j)
           WRITE(stdUnit,'(A,4I5,2F11.4)')
     &      ' BBerr: i,j,bi,bj,Tice = ',i,j,bi,bj,tIc1(i,j),tIc2(i,j)
          ENDIF
          IF ( tIc2(i,j).GT.0. _d 0) THEN
           WRITE(stdUnit,'(A,I12,1PE14.6)')
     &       ' BBerr: Tice(2) > 0 ; it=', myIter, qIc2(i,j)
           WRITE(stdUnit,'(A,4I5,2F11.4)')
     &      ' BBerr: i,j,bi,bj,Tice = ',i,j,bi,bj,tIc1(i,j),tIc2(i,j)
          ENDIF
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1010)
     &     'ThSI_SOLVE4T: k, Ts, Tice=',0,tSrf(i,j),tIc1(i,j),tIc2(i,j)
#endif

C--   Compute coefficients used in quadratic formula.

          a10(i,j) = rhoi*cpIce *hIce(i,j)/(2. _d 0*dt) +
     &          k32*( 4. _d 0*dt*k32 + rhoi*cpIce *hIce(i,j) )
     &           / ( 6. _d 0*dt*k32 + rhoi*cpIce *hIce(i,j) )
          b10(i,j) = -hIce(i,j)*
     &          ( rhoi*cpIce*tIc1(i,j) + rhoi*Lfresh*Tmlt1/tIc1(i,j) )
     &           /(2. _d 0*dt)
     &        - k32*( 4. _d 0*dt*k32*tFrz(i,j)
     &               +rhoi*cpIce*hIce(i,j)*tIc2(i,j) )
     &           / ( 6. _d 0*dt*k32 + rhoi*cpIce *hIce(i,j) )
     &        - fswint
          c10(i,j) = rhoi*Lfresh*hIce(i,j)*Tmlt1 / (2. _d 0*dt)

        ELSE
          iceFlag(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO
#ifndef ALLOW_AUTODIFF
      IF ( icount .gt. 0 ) THEN
       WRITE(stdUnit,'(A,I5,A)')
     &      'THSICE_SOLVE4TEMP: there are ',icount,
     &      ' case(s) where hIce<hIceMin;'
       WRITE(stdUnit,'(A,I3,A1,I3,A)')
     &      'THSICE_SOLVE4TEMP: the last one was at (',ii,',',jj,
     &      ') with hIce = ', hIce(ii,jj)
       WRITE( msgBuf, '(A)')
     &      'THSICE_SOLVE4TEMP: should not enter if hIce<hIceMin'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R THSICE_SOLVE4TEMP'
      ENDIF
#endif /* ALLOW_AUTODIFF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Get surface fluxes over melting surface (assumed to be @ 0 oC)
#ifndef ALLOW_AUTODIFF
      IF ( useBlkFlx .AND. iterate4Tsf  ) THEN
#endif
        DO j = jMin, jMax
         DO i = iMin, iMax
           Tsf_mlt(i,j) = 0.
         ENDDO
        ENDDO
#ifndef ALLOW_AUTODIFF
        IF ( useEXF ) THEN
#endif
           k = 1
           CALL THSICE_GET_EXF(   bi, bj, k,
     I                            iMin, iMax, jMin, jMax,
     I                            iceFlag, hSnow, Tsf_mlt,
     O                            flx0exSW, dFlxdT, evap_0, dEvdT,
     I                            myTime, myIter, myThid )
#ifndef ALLOW_AUTODIFF
C-    could add this "ifdef" to hide THSICE_GET_BULKF from TAF
        ELSEIF ( useBulkForce ) THEN
           CALL THSICE_GET_BULKF( bi, bj,
     I                            iMin, iMax, jMin, jMax,
     I                            iceFlag, hSnow, Tsf_mlt,
     O                            flx0exSW, dFlxdT, evap_0, dEvdT,
     I                            myTime, myIter, myThid )
C--- end if: IF ( useEXF ) THEN
        ENDIF
C--- end if: IF ( useBlkFlx .AND. iterate4Tsf  ) THEN
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  Compute new surface and ice temperatures; iterate until tSrf converges.
      DO j = jMin, jMax
        DO i = iMin, iMax
          dTsrf(i,j) = Terrmax
        ENDDO
      ENDDO
      IF ( useBlkFlx ) THEN
        iterMax = nitMaxTsf
      ELSE
        iterMax = 1
      ENDIF

C ----- begin iteration  -----
      DO k = 1,iterMax
#ifndef ALLOW_AUTODIFF
       IF ( iterate4Tsf ) THEN
        iterate4Tsf = .FALSE.
C
        IF ( useBlkFlx ) THEN
#endif
#ifdef ALLOW_AUTODIFF_TAMC
         kkey = (ticekey-1)*MaxTsf + k
CADJ STORE iceflag  = comlev1_thsice_s4t, key = kkey, kind = isbyte
CADJ STORE tSrf     = comlev1_thsice_s4t, key = kkey, kind = isbyte
CADJ STORE flxTexSW = comlev1_thsice_s4t, key = kkey, kind = isbyte
CADJ STORE dFlxdT   = comlev1_thsice_s4t, key = kkey, kind = isbyte
CADJ STORE dEvdT    = comlev1_thsice_s4t, key = kkey, kind = isbyte
#endif

C--   Compute top surface flux.
#ifndef ALLOW_AUTODIFF
         IF ( useEXF ) THEN
#endif
           CALL THSICE_GET_EXF(   bi, bj, k+1,
     I                            iMin, iMax, jMin, jMax,
     I                            iceFlag, hSnow, tSrf,
     O                            flxTexSW, dFlxdT, evapT, dEvdT,
     I                            myTime, myIter, myThid )
C-    could add this "ifdef" to hide THSICE_GET_BULKF from TAF
#ifndef ALLOW_AUTODIFF
         ELSEIF ( useBulkForce ) THEN
           CALL THSICE_GET_BULKF( bi, bj,
     I                            iMin, iMax, jMin, jMax,
     I                            iceFlag, hSnow, tSrf,
     O                            flxTexSW, dFlxdT, evapT, dEvdT,
     I                            myTime, myIter, myThid )
C--- end if: IF ( useEXF ) THEN
         ENDIF
        ELSE
         DO j = jMin, jMax
          DO i = iMin, iMax
           IF ( iceFlag(i,j).GT.0. _d 0 ) THEN
             flxTexSW(i,j) = flxExSW(i,j,1)
             dFlxdT(i,j) = flxExSW(i,j,2)
           ENDIF
          ENDDO
         ENDDO
C--- end if: IF ( useBlkFlx ) THEN
        ENDIF
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     avoid calling thsice_get_exf in ad-code
CADJ STORE flxTexSW = comlev1_thsice_s4t, key = kkey, kind = isbyte
CADJ STORE dFlxdT   = comlev1_thsice_s4t, key = kkey, kind = isbyte
#endif
C--   Compute new top layer and surface temperatures.
C     If tSrf is computed to be > 0 C, fix tSrf = 0 and recompute tIc1
C     with different coefficients.
        DO j = jMin, jMax
         DO i = iMin, iMax
          IF ( iceFlag(i,j).GT.0. _d 0 ) THEN
           flxNet = sHeat(i,j) + flxTexSW(i,j)
#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1020)
     &     'ThSI_SOLVE4T: flxNet,dFlxdT,k12,D=',
     &      flxNet, dFlxdT(i,j), k12(i,j), k12(i,j)-dFlxdT(i,j)
#endif

           a1 = a10(i,j) - k12(i,j)*dFlxdT(i,j) / (k12(i,j)-dFlxdT(i,j))
           b1 = b10(i,j) - k12(i,j)*(flxNet-dFlxdT(i,j)*tSrf(i,j))
     &                   /(k12(i,j)-dFlxdT(i,j))
           c1 = c10(i,j)
           tIc1(i,j)  = -(b1 + SQRT(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
           dTsrf(i,j) = (flxNet + k12(i,j)*(tIc1(i,j)-tSrf(i,j)))
     &                   /(k12(i,j)-dFlxdT(i,j))
C     this line causes 3 recomputation warings for TAF because tSrf is
C     updated and the updated value is used to update tSrf (and other
C     variables) again; with this intermediate variable, the warnings go
C     away.
C          tSrf(i,j) = tSrf(i,j) + dTsrf(i,j)
           TsfTmp = tSrf(i,j) + dTsrf(i,j)
C
           IF ( TsfTmp .GT. 0. _d 0 ) THEN
#ifdef ALLOW_DBUG_THSICE
            IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1010)
     &     'ThSI_SOLVE4T: k,ts,t1,dTs=',k,TsfTmp,tIc1(i,j),dTsrf(i,j)
#endif
            a1 = a10(i,j) + k12(i,j)
C      note: b1 = b10 - k12*Tf0
            b1 = b10(i,j)
            tIc1(i,j) = (-b1 - SQRT(b1*b1-4. _d 0*a1*c1))/(2. _d 0*a1)
            tSrf(i,j) = 0. _d 0
#ifndef ALLOW_AUTODIFF
            IF ( useBlkFlx ) THEN
#endif /* ALLOW_AUTODIFF */
             flxTexSW(i,j) = flx0exSW(i,j)
             evapT(i,j) = evap_0(i,j)
             dTsrf(i,j) = 0. _d 0
#ifndef ALLOW_AUTODIFF
            ELSE
             flxTexSW(i,j) = flxExSW(i,j,0)
             dTsrf(i,j) = 1000.
             dFlxdT(i,j) = 0.
            ENDIF
#endif /* ALLOW_AUTODIFF */
           ELSE
            tSrf(i,j) = TsfTmp
           ENDIF

C--   Check for convergence.  If no convergence, then repeat.
           IF (ABS(dTsrf(i,j)).GE.Terrmax) THEN
            iceFlag(i,j) = 1. _d 0
           ELSE
            iceFlag(i,j) = 0. _d 0
           ENDIF
#ifndef ALLOW_AUTODIFF
           iterate4Tsf = iterate4Tsf .OR. (iceFlag(i,j).GT.0. _d 0)
#endif

C     Convergence test: Make sure tSrf has converged, within prescribed error.
C     (Energy conservation is guaranteed within machine roundoff, even
C      if tSrf has not converged.)
C     If no convergence, then repeat.

#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1010)
     &  'ThSI_SOLVE4T: k,ts,t1,dTs=', k,tSrf(i,j),tIc1(i,j),dTsrf(i,j)
           IF ( useBlkFlx .AND. k.EQ.nitMaxTsf .AND.
     &                     (iceFlag(i,j).GT.0. _d 0) ) THEN
            WRITE(stdUnit,'(A,4I4,I12,F15.9)')
     &       ' BB: not converge: i,j,it,hi=',i,j,bi,bj,myIter,hIce(i,j)
            WRITE(stdUnit,*)
     &        'BB: not converge: tSrf, dTsrf=', tSrf(i,j), dTsrf(i,j)
            WRITE(stdUnit,*)
     &        'BB: not converge: flxNet,dFlxT=', flxNet, dFlxdT(i,j)
            IF ( tSrf(i,j).LT.-70. _d 0 ) THEN
             WRITE( msgBuf, '(A,2I4,2I3,I10,F12.3)')
     &        'THSICE_SOLVE4TEMP: Too low tSrf in', i, j, bi, bj,
     &                            myIter, tSrf(i,j)
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R THSICE_SOLVE4TEMP'
            ENDIF
           ENDIF
#endif /* ALLOW_DBUG_THSICE */

          ENDIF
         ENDDO
        ENDDO
#ifndef ALLOW_AUTODIFF
C--- end if: IF ( iterate4Tsf ) THEN
       ENDIF
#endif /* ALLOW_AUTODIFF */
C--- end loop DO k = 1,iterMax
      ENDDO
C ------ end iteration ------------
#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     avoid calling thsice_get_exf in ad-code
CADJ STORE tIc1   = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE dTsrf  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE tSrf   = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE dFlxdT = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE dEvdT  = comlev1_bibj, key = ticekey, kind = isbyte
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO j = jMin, jMax
       DO i = iMin, iMax
C     this intermediate variable avoids a recomputation warning
        tIc2Tmp = tIc2(i,j)
        IF ( icMask(i,j).GT.0. _d 0 ) THEN

C--   Compute new bottom layer temperature.
          k32       = 2. _d 0*kIce  / hIce(i,j)
          tIc2(i,j) = ( 2. _d 0*dt*k32*(tIc1(i,j)+2. _d 0*tFrz(i,j))
     &                 + rhoi*cpIce*hIce(i,j)*tIc2Tmp)
     &               /(6. _d 0*dt*k32 + rhoi*cpIce*hIce(i,j))
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1010)
     &  'ThSI_SOLVE4T: k, Ts, Tice=',k,tSrf(i,j),tIc1(i,j),tIc2(i,j)
          netSW   = flxAtm(i,j)
#endif

C--   Compute final flux values at surfaces.
          fct    = k12(i,j)*(tSrf(i,j)-tIc1(i,j))
          flxCnB(i,j) = 4. _d 0*kIce *(tIc2(i,j)-tFrz(i,j))/hIce(i,j)
          flxNet = sHeat(i,j) + flxTexSW(i,j)
          flxNet = flxNet + dFlxdT(i,j)*dTsrf(i,j)
#ifndef ALLOW_AUTODIFF
          IF ( useBlkFlx ) THEN
#endif
C--   need to update also Evap (tSrf changes) since Latent heat has
C     been updated
            evpAtm(i,j) = evapT(i,j) + dEvdT(i,j)*dTsrf(i,j)
#ifndef ALLOW_AUTODIFF
          ELSE
C- WARNING: Evap & +Evap*Lfresh are missing ! (but only affects Diagnostics)
            evpAtm(i,j) = 0.
          ENDIF
#endif
C-    Update energy flux to Atmos with other than SW contributions;
C     use latent heat = Lvap (energy=0 for liq. water at 0.oC)
          flxAtm(i,j) = flxAtm(i,j) + flxTexSW(i,j)
     &                + dFlxdT(i,j)*dTsrf(i,j) + evpAtm(i,j)*Lfresh
C-    excess of energy @ surface (used for surface melting):
          sHeat(i,j) = flxNet - fct

#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,1020)
     &     'ThSI_SOLVE4T: flxNet,fct,Dif,flxCnB=',
     &                    flxNet,fct,flxNet-fct,flxCnB(i,j)
#endif

C--   Compute new enthalpy for each layer.
          qIc1(i,j) = -cpWater*Tmlt1 + cpIce *(Tmlt1-tIc1(i,j))
     &                + Lfresh*(1. _d 0-Tmlt1/tIc1(i,j))
          qIc2(i,j) = -cpIce *tIc2(i,j) + Lfresh

#ifdef ALLOW_DBUG_THSICE
C--   Make sure internal ice temperatures do not exceed Tmlt.
C     (This should not happen for reasonable values of i0swFrac)
          IF (tIc1(i,j) .GE. Tmlt1) THEN
           WRITE(stdUnit,'(A,2I4,2I3,1P2E14.6)')
     &     ' BBerr - Bug: IceT(1) > Tmlt',i,j,bi,bj,tIc1(i,j),Tmlt1
          ENDIF
          IF (tIc2(i,j) .GE. 0. _d 0) THEN
           WRITE(stdUnit,'(A,2I4,2I3,1P2E14.6)')
     &     ' BBerr - Bug: IceT(2) > 0',i,j,bi,bj,tIc2(i,j)
          ENDIF

          IF ( dBug(i,j,bi,bj) ) THEN
           WRITE(stdUnit,1020) 'ThSI_SOLV_4T: tSrf,Tice(1,2), dTsurf=',
     &           tSrf(i,j), tIc1(i,j), tIc2(i,j), dTsrf(i,j)
           WRITE(stdUnit,1020) 'ThSI_SOLV_4T: sHeat, flxCndBt, Qice =',
     &           sHeat(i,j), flxCnB(i,j), qIc1(i,j), qIc2(i,j)
           WRITE(stdUnit,1020) 'ThSI_SOLV_4T: flxA, evpA, fxSW_bf,af=',
     &           flxAtm(i,j), evpAtm(i,j), netSW, flxSW(i,j)
          ENDIF
#endif

        ELSE
C--     ice-free grid point:
c         tIc1  (i,j) = 0. _d 0
c         tIc2  (i,j) = 0. _d 0
          dTsrf (i,j) = 0. _d 0
c         sHeat (i,j) = 0. _d 0
c         flxCnB(i,j) = 0. _d 0
c         flxAtm(i,j) = 0. _d 0
c         evpAtm(i,j) = 0. _d 0

        ENDIF
       ENDDO
      ENDDO
#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
