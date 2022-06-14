#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
# define ALLOW_AUTODIFF_TAMC_MORE
#endif

CBOP
C     !ROUTINE: THSICE_CALC_THICKN
C     !INTERFACE:
      SUBROUTINE THSICE_CALC_THICKN(
     I                  bi, bj,
     I                  iMin,iMax, jMin,jMax, dBugFlag,
     I                  iceMask, tFrz, tOce, v2oc,
     I                  snowP, prcAtm, sHeat, flxCnB,
     U                  icFrac, hIce, hSnow1, tSrf, qIc1, qIc2,
     U                  frwAtm, fzMlOc, flx2oc,
     O                  frw2oc, fsalt, frzSeaWat,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_CALC_THICKN
C     | o Calculate ice & snow thickness changes
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

C---------------------------------
C  parameters that control the partitioning between lateral (ice area) and
C    vertical (ice thickness) ice volume changes.
C a) surface melting and bottom melting (runtime parameter: fracEnMelt):
C  frace is the fraction of available heat that is used for
C  lateral melting (and 1-frace reduces the thickness ) when
C o       hi < hThinIce        & frac > lowIcFrac2 : frace=1 (lateral melting only)
C o hThinIce < hi < hThickIce  & frac > lowIcFrac1 : frace=fracEnMelt
C o            hi > hThickIce or frac < lowIcFrac1 : frace=0 (thinning only)
C b) ocean freezing (and ice forming):
C - conductive heat flux (below sea-ice) always increases thickness.
C - under sea-ice, freezing potential (x iceFraction) is used to increase ice
C                  thickness or ice fraction (lateral growth), according to:
C o       hi < hThinIce       : use freezing potential to grow ice vertically;
C o hThinIce < hi < hThickIce : use partition factor fracEnFreez for lateral growth
c                               and (1-fracEnFreez) to increase thickness.
C o            hi > hThickIce : use all freezing potential to grow ice laterally
C                                (up to areaMax)
C - over open ocean, use freezing potential [x(1-iceFraction)] to grow ice laterally
C - lateral growth forms ice of the same or =hNewIceMax thickness, the less of the 2.
C - starts to form sea-ice over fraction iceMaskMin, as minimum ice-volume is reached
C---------------------------------
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     dBugFlag    :: allow to print debugging stuff (e.g. on 1 grid point).
C---  Input:
C         iceMask :: sea-ice fractional mask [0-1]
C  tFrz           :: sea-water freezing temperature [oC] (function of S)
C  tOce           :: surface level oceanic temperature [oC]
C  v2oc           :: square of ocean surface-level velocity [m2/s2]
C  snowP          :: snow precipitation                [kg/m2/s]
C  prcAtm         :: total precip from the atmosphere [kg/m2/s]
C  sHeat          :: surf heating flux left to melt snow or ice (= Atmos-conduction)
C  flxCnB         :: heat flux conducted through the ice to bottom surface
C---  Modified (input&output):
C  icFrac         :: fraction of grid area covered in ice
C  hIce           :: ice height [m]
C  hSnow1          :: snow height [m]
C  tSrf           :: surface (ice or snow) temperature
C  qIc1   (qicen) :: ice enthalpy (J/kg), 1rst level
C  qIc2   (qicen) :: ice enthalpy (J/kg), 2nd level
C  frwAtm (evpAtm):: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C  fzMlOc         :: ocean mixed-layer freezing/melting potential [W/m2]
C  flx2oc         :: net heat flux to ocean    [W/m2]          (> 0 downward)
C---  Output
C  frw2oc         :: Total fresh water flux to ocean [kg/m2/s] (> 0 downward)
C  fsalt          :: salt flux to ocean        [g/m2/s]        (> 0 downward)
C  frzSeaWat      :: seawater freezing rate (expressed as mass flux) [kg/m^2/s]
C---  Input:
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL dBugFlag
      _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFrz   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tOce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL v2oc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL snowP  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL prcAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sHeat  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxCnB (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL icFrac (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hSnow1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tSrf   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qIc2   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frwAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fzMlOc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flx2oc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frw2oc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fsalt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frzSeaWat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     !LOCAL VARIABLES:
C---  local copy of input/output argument list variables (see description above)
      _RL qicen(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nlyr)

C     == Local Variables ==
C     i,j,k      :: loop indices
C     rec_nlyr   :: reciprocal of number of ice layers (real value)
C     evapLoc    :: evaporation over snow/ice [kg/m2/s] (>0 if evaporate)
C     Fbot       :: oceanic heat flux used to melt/form ice [W/m2]
C     etop       :: energy for top melting    (J m-2)
C     ebot       :: energy for bottom melting (J m-2)
C     etope      :: energy (from top)    for lateral melting (J m-2)
C     ebote      :: energy (from bottom) for lateral melting (J m-2)
C     extend     :: total energy for lateral melting (J m-2)
C     hnew(nlyr) :: new ice layer thickness (m)
C     hlyr       :: individual ice layer thickness (m)
C     dhi        :: change in ice thickness
C     dhs        :: change in snow thickness
C     rq         :: rho * q for a layer
C     rqh        :: rho * q * h for a layer
C     qbot       :: enthalpy for new ice at bottom surf (J/kg)
C     dt         :: timestep
C     esurp      :: surplus energy from melting (J m-2)
C     mwater0    :: fresh water mass gained/lost (kg/m^2)
C     msalt0     :: salt gained/lost  (kg/m^2)
C     freshe     :: fresh water gain from extension melting
C     salte      :: salt gained from extension melting
C     lowIcFrac1 :: ice-fraction lower limit above which partial (lowIcFrac1)
C     lowIcFrac2 :: or full (lowIcFrac2) lateral melting is allowed.
C     from THSICE_RESHAPE_LAYERS
C     f1         :: Fraction of upper layer ice in new layer
C     qh1, qh2   :: qice*h for layers 1 and 2
C     qhtot      :: qh1 + qh2
C     q2tmp      :: Temporary value of qice for layer 2
      INTEGER  i,j,k
      _RL rec_nlyr
      _RL evapLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fbot   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etop   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ebot   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etope  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ebote  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL esurp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL extend
      _RL hnew   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nlyr)
C     help TAF with these variables
      _RL hnewTmp, icFracTmp, hIceTmp, hSnwTmp
      _RL hlyr
      _RL dhi
      _RL dhs
      _RL rq
      _RL rqh
      _RL qbot
      _RL dt
      _RL mwater0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL msalt0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL freshe
      _RL salte
      _RL lowIcFrac1, lowIcFrac2
      _RL  f1
      _RL  qh1, qh2
      _RL  qhtot
      _RL  q2tmp
#ifdef CHECK_ENERGY_CONSERV
      _RL  qaux(nlyr)
#endif /* CHECK_ENERGY_CONSERV */

      _RL  ustar, cpchr
      _RL  chi
      _RL  frace, rs, hq
#ifdef THSICE_FRACEN_POWERLAW
      INTEGER powerLaw
      _RL rec_pLaw
      _RL c1Mlt, c2Mlt, aMlt, hMlt
      _RL c1Frz, c2Frz, aFrz, hFrz
      _RL enFrcMlt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xxMlt
      _RL enFrcFrz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xxFrz
# if ( !defined TARGET_NEC_SX && !defined ALLOW_AUTODIFF )
      _RL tmpMlt, tmpFrz
# endif
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey, ikey_1
#endif
#ifdef THSICE_REGULARIZE_CALC_THICKN
      _RL kScal
      PARAMETER ( kScal = 1.D0 )
#endif
#ifdef ALLOW_DBUG_THSICE
C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1020 FORMAT(A,1P4E11.3)
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
#endif /* ALLOW_AUTODIFF_TAMC */

      rec_nlyr = nlyr
      rec_nlyr = 1. _d 0 / rec_nlyr
      dt  = thSIce_deltaT
      cpchr = cpWater*rhosw*bMeltCoef

C     for now, use hard coded threshold (iceMaskMin +1.% and +10.%)
      lowIcFrac1 = iceMaskMin*1.01 _d 0
      lowIcFrac2 = iceMaskMin*1.10 _d 0
#ifdef THSICE_FRACEN_POWERLAW
      IF ( powerLawExp2 .GE. 1 ) THEN
        powerLaw = 1 + 2**powerLawExp2
        rec_pLaw = powerLaw
        rec_pLaw = 1. _d 0 / rec_pLaw
C-    Coef for melting:
C     lateral-melting energy fraction = fracEnMelt - [ aMlt*(hi-hMlt) ]^powerLaw
        c1Mlt = fracEnMelt**rec_pLaw
        c2Mlt = (1. _d 0 - fracEnMelt)**rec_pLaw
        aMlt = (c1Mlt+c2Mlt)/(hThickIce-hThinIce)
        hMlt = hThinIce+c2Mlt/aMlt
C-    Coef for freezing:
C     thickening energy fraction     = fracEnFreez - [ aFrz*(hi-hFrz) ]^powerLaw
        c1Frz = fracEnFreez**rec_pLaw
        c2Frz = (1. _d 0 -fracEnFreez)**rec_pLaw
        aFrz = (c1Frz+c2Frz)/(hThickIce-hThinIce)
        hFrz = hThinIce+c2Frz/aFrz
      ELSE
C-    Linear relation
        powerLaw = 1
        aMlt = -1. _d 0 /(hThickIce-hThinIce)
        hMlt = hThickIce
        aFrz = -1. _d 0 /(hThickIce-hThinIce)
        hFrz = hThickIce
      ENDIF
#endif /* THSICE_FRACEN_POWERLAW */

C     initialise local arrays
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        evapLoc(i,j) = 0. _d 0
        Fbot   (i,j) = 0. _d 0
        etop   (i,j) = 0. _d 0
        ebot   (i,j) = 0. _d 0
        etope  (i,j) = 0. _d 0
        ebote  (i,j) = 0. _d 0
        esurp  (i,j) = 0. _d 0
        mwater0(i,j) = 0. _d 0
        msalt0 (i,j) = 0. _d 0
#ifdef THSICE_FRACEN_POWERLAW
        enFrcMlt(i,j)= 0. _d 0
        enFrcFrz(i,j)= 0. _d 0
#endif
       ENDDO
      ENDDO
      DO k = 1,nlyr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         qicen(i,j,k) = 0. _d 0
         hnew (i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C     The commented store directives ("cCADJ") are not picked up by TAF,
C     that does not mean that under different circumstances they might,
C     so I leave them here as a reminder.
CADJ STORE hice   = comlev1_bibj, key=ticekey, kind=isbyte
CADJ STORE icfrac = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE frwatm = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE fzmloc = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE hSnow1 = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE qic1   = comlev1_bibj, key=ticekey, kind=isbyte
cCADJ STORE qic2   = comlev1_bibj, key=ticekey, kind=isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax

        IF (iceMask(i,j).GT.0. _d 0) THEN
         qicen(i,j,1)= qIc1(i,j)
         qicen(i,j,2)= qIc2(i,j)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     initialize energies
         esurp(i,j) = 0. _d 0

c     make a local copy of evaporation
         evapLoc(i,j) = frwAtm(i,j)

C------------------------------------------------------------------------
C--   Compute growth and/or melting at the top and bottom surfaces
C------------------------------------------------------------------------

#ifdef THSICE_FRACEN_POWERLAW
         xxMlt = aMlt*(hIce(i,j)-hMlt)
         xxFrz = aFrz*(hIce(i,j)-hFrz)
c--
         IF ( powerLawExp2 .GE. 1 ) THEN
#if ( defined TARGET_NEC_SX || defined ALLOW_AUTODIFF )
C     avoid the short inner loop that cannot be vectorized
          xxMlt = xxMlt**powerLaw
          xxFrz = xxFrz**powerLaw
#else
          tmpMlt = xxMlt
          tmpFrz = xxFrz
          DO k=1,powerLawExp2
           tmpMlt = tmpMlt*tmpMlt
           tmpFrz = tmpFrz*tmpFrz
          ENDDO
          xxMlt = xxMlt*tmpMlt
          xxFrz = xxFrz*tmpFrz
#endif /* TARGET_NEC_SX */
          xxMlt = fracEnMelt -xxMlt
          xxFrz = fracEnFreez-xxFrz
         ENDIF
         enFrcMlt(i,j) = MAX( 0. _d 0, MIN( xxMlt, 1. _d 0 ) )
         enFrcFrz(i,j) = MAX( 0. _d 0, MIN( xxFrz, 1. _d 0 ) )
#endif /* THSICE_FRACEN_POWERLAW */

         IF (fzMlOc(i,j).GE. 0. _d 0) THEN
C     !-----------------------------------------------------------------
C     ! freezing conditions
C     !-----------------------------------------------------------------
          Fbot(i,j) = fzMlOc(i,j)
          IF ( icFrac(i,j).LT.iceMaskMax ) THEN
#ifdef THSICE_FRACEN_POWERLAW
           Fbot(i,j) = enFrcFrz(i,j)*fzMlOc(i,j)
#else /* THSICE_FRACEN_POWERLAW */
           IF (hIce(i,j).GT.hThickIce) THEN
C if higher than hThickIce, use all fzMlOc energy to grow extra ice
            Fbot(i,j) = 0. _d 0
           ELSEIF (hIce(i,j).GE.hThinIce) THEN
C between hThinIce & hThickIce, use partition factor fracEnFreez
            Fbot(i,j) = (1. _d 0 - fracEnFreez)*fzMlOc(i,j)
           ENDIF
#endif /* THSICE_FRACEN_POWERLAW */
          ENDIF
         ELSE
C     !-----------------------------------------------------------------
C     ! melting conditions
C     !-----------------------------------------------------------------
#ifdef THSICE_REGULARIZE_CALC_THICKN
C     smooth regulatization
          ustar = SQRT(0.00536 _d 0 *v2oc(i,j) + 25. _d -6)
#else
C     for no currents:
          ustar = 5. _d -3
C frictional velocity between ice and water
          IF (v2oc(i,j) .NE. 0.)
     &     ustar = SQRT(0.00536 _d 0*v2oc(i,j))
          ustar = MAX(5. _d -3,ustar)
#endif
c         cpchr =cpWater*rhosw*bMeltCoef
          Fbot(i,j) = cpchr*(tFrz(i,j)-tOce(i,j))*ustar
C     fzMlOc < Fbot < 0
#ifdef THSICE_REGULARIZE_CALC_THICKN
C     smooth regulatization
C     we want to have some soft maximum for better differentiability:
C     max(a,b;k) = (a*exp(k*a)+b*exp(k*b))/(exp(k*a)+exp(k*b))
          Fbot(i,j) = (
     &             Fbot(i,j) * EXP(kScal*  Fbot(i,j))
     &         + fzMlOc(i,j) * EXP(kScal*fzMlOc(i,j))
     &         )/( EXP(kScal*Fbot(i,j)) + EXP(kScal*fzMlOc(i,j)) )
#else
          Fbot(i,j) = MAX(Fbot(i,j),fzMlOc(i,j))
#endif
          Fbot(i,j) = MIN(Fbot(i,j),0. _d 0)
         ENDIF

C  mass of fresh water and salt initially present in ice
         mwater0(i,j) = rhos*hSnow1(i,j) + rhoi*hIce(i,j)
         msalt0 (i,j) = rhoi*hIce(i,j)*saltIce

#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &        'ThSI_CALC_TH: evpAtm, fzMlOc, Fbot =',
     &        frwAtm(i,j),fzMlOc(i,j),Fbot(i,j)
#endif
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE icFrac = comlev1_bibj, key = ticekey, kind = isbyte
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN

C     Compute energy available for melting/growth.

#ifdef THSICE_FRACEN_POWERLAW
         IF ( fracEnMelt.EQ.0. _d 0 ) THEN
          frace = 0. _d 0
         ELSE
          frace = (icFrac(i,j) - lowIcFrac1)/(lowIcFrac2-iceMaskMin)
          frace = MIN( enFrcMlt(i,j), MAX( 0. _d 0, frace ) )
         ENDIF
#else /* THSICE_FRACEN_POWERLAW */
         IF ( hIce(i,j).GT.hThickIce .OR. fracEnMelt.EQ.0. _d 0 ) THEN
C above certain height (or when no ice fractionation), only melt from top
          frace = 0. _d 0
         ELSEIF (hIce(i,j).LT.hThinIce) THEN
C below a certain height, all energy goes to changing ice extent
          frace = 1. _d 0
         ELSE
          frace = fracEnMelt
         ENDIF
C     Reduce lateral melting when ice fraction is low : the purpose is to avoid
C     disappearing of (up to hThinIce thick) sea-ice by over doing lateral melting
C     (which would bring icFrac below iceMaskMin).
         IF ( icFrac(i,j).LE.lowIcFrac1 ) THEN
          frace = 0. _d 0
         ELSEIF (icFrac(i,j).LE.lowIcFrac2 ) THEN
          frace = MIN( frace, fracEnMelt )
         ENDIF
#endif /* THSICE_FRACEN_POWERLAW */

c     IF (tSrf(i,j) .EQ. 0. _d 0 .AND. sHeat(i,j).GT.0. _d 0) THEN
         IF ( sHeat(i,j).GT.0. _d 0 ) THEN
          etop(i,j) = (1. _d 0-frace)*sHeat(i,j) * dt
          etope(i,j) = frace*sHeat(i,j) * dt
         ELSE
          etop(i,j) =  0. _d 0
          etope(i,j) = 0. _d 0
C jmc: found few cases where tSrf=0 & sHeat < 0 : add this line to conserv energy:
          esurp(i,j) = sHeat(i,j) * dt
         ENDIF
C--   flux at the base of sea-ice:
C     conduction H.flx= flxCnB (+ =down); oceanic turbulent H.flx= Fbot (+ =down).
C-    ==> energy available(+ => melt)= (flxCnB-Fbot)*dt
c     IF (fzMlOc(i,j).LT.0. _d 0) THEN
c         ebot(i,j) = (1. _d 0-frace)*(flxCnB-Fbot(i,j)) * dt
c         ebote(i,j) = frace*(flxCnB-Fbot(i,j)) * dt
c     ELSE
c         ebot(i,j) = (flxCnB-Fbot(i,j)) * dt
c         ebote(i,j) = 0. _d 0
c     ENDIF
C- original formulation(above): Loose energy when flxCnB < Fbot < 0
         ebot(i,j) = (flxCnB(i,j)-Fbot(i,j)) * dt
         IF (ebot(i,j).GT.0. _d 0) THEN
          ebote(i,j) = frace*ebot(i,j)
          ebot(i,j)  = ebot(i,j)-ebote(i,j)
         ELSE
          ebote(i,j) = 0. _d 0
         ENDIF
#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &        'ThSI_CALC_TH: etop,etope,ebot,ebote=',
     &        etop(i,j),etope(i,j),ebot(i,j),ebote(i,j)
#endif
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

C     Initialize layer thicknesses. Divide total thickness equally between
C     layers
      DO k = 1, nlyr
       DO j = jMin, jMax
        DO i = iMin, iMax
         hnew(i,j,k) = hIce(i,j) * rec_nlyr
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hSnow1 = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE etop   = comlev1_bibj, key = ticekey, kind = isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j) .GT. 0. _d 0 .AND.
     &         etop(i,j) .GT. 0. _d 0 .AND.
     &       hSnow1(i,j) .GT. 0. _d 0) THEN

C     Make sure internal ice temperatures do not exceed Tmlt.
C     If they do, then eliminate the layer.  (Dont think this will happen
C     for reasonable values of i0.)
C     Top melt: snow, then ice.
         rq =  rhos * qsnow
         rqh = rq * hSnow1(i,j)
         IF (etop(i,j) .LT. rqh) THEN
          hSnow1(i,j) = hSnow1(i,j) - etop(i,j)/rq
          etop(i,j) = 0. _d 0
         ELSE
          etop(i,j) = etop(i,j) - rqh
          hSnow1(i,j) = 0. _d 0
         ENDIF
C     endif iceMask > 0, etc.
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
CADJ STORE hSnow1 = comlev1_bibj, key = ticekey, kind = isbyte
#endif
C     two layers of ice
      DO k = 1, nlyr
#ifdef ALLOW_AUTODIFF_TAMC
       ikey_1 = k + (ticekey-1)*nlyr
CADJ STORE etop         = comlev1_thsice_nlyr, key=ikey_1, kind=isbyte
CADJ STORE hnew (:,:,k) = comlev1_thsice_nlyr, key=ikey_1, kind=isbyte
CADJ STORE qicen(:,:,k) = comlev1_thsice_nlyr, key=ikey_1, kind=isbyte
#endif
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j).GT.0. _d 0) THEN
          IF (etop(i,j) .GT. 0. _d 0) THEN
           rq =  rhoi * qicen(i,j,k)
           rqh = rq * hnew(i,j,k)
           IF (etop(i,j) .LT. rqh) THEN
            hnew(i,j,k) = hnew(i,j,k) - etop(i,j) / rq
            etop(i,j) = 0. _d 0
           ELSE
            etop(i,j) = etop(i,j) - rqh
            hnew(i,j,k) = 0. _d 0
           ENDIF
          ELSE
           etop(i,j)=0. _d 0
          ENDIF
C If ice is gone and melting energy remains
c     IF (etop(i,j) .GT. 0. _d 0) THEN
c        WRITE (6,*)  'QQ All ice melts from top  ', i,j
c        hIce(i,j)=0. _d 0
c        go to 200
c     ENDIF

C     endif iceMask > 0
         ENDIF
C     end i/j-loops
        ENDDO
       ENDDO
C     end k-loop
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     if isbyte=4, these change the AD-monitor output a little (11-14
C     digits of agreement)
CADJ STORE ebot  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hnew  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE qicen = comlev1_bibj, key = ticekey, kind = isbyte
#endif

C Bottom growth:
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0 .AND. ebot(i,j) .LT. 0. _d 0) THEN
C Compute enthalpy of new ice growing at bottom surface.
         qbot =  -cpIce *tFrz(i,j) + Lfresh
         dhi = -ebot(i,j) / (qbot * rhoi)
         ebot(i,j) = 0. _d 0
         qicen(i,j,nlyr) =
     &        (hnew(i,j,nlyr)*qicen(i,j,nlyr)+dhi*qbot) /
     &        (hnew(i,j,nlyr)+dhi)
         hnew(i,j,nlyr) = hnew(i,j,nlyr) + dhi
         frzSeaWat(i,j) = rhoi*dhi/dt

C     endif iceMask > 0 and ebot < 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     these change the results (increase gradient error sligthly);
C     they do not improve performance too much, so we leave commented
cCADJ STORE hnew (:,:,nlyr) = comlev1_bibj, key = ticekey, kind = isbyte
cCADJ STORE qicen(:,:,nlyr) = comlev1_bibj, key = ticekey, kind = isbyte
#endif

C Bottom melt:
      DO k = nlyr, 1, -1
#ifdef ALLOW_AUTODIFF_TAMC
       ikey_1 = k + (ticekey-1)*nlyr
CADJ STORE ebot = comlev1_thsice_nlyr, key=ikey_1, kind = isbyte
#endif
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j) .GT. 0. _d 0 .AND.
     &        ebot(i,j)   .GT. 0. _d 0 .AND.
     &        hnew(i,j,k) .GT. 0. _d 0) THEN
          rq =  rhoi * qicen(i,j,k)
          rqh = rq * hnew(i,j,k)
          IF (ebot(i,j) .LT. rqh) THEN
           hnew(i,j,k) = hnew(i,j,k) - ebot(i,j) / rq
           ebot(i,j) = 0. _d 0
          ELSE
           ebot(i,j) = ebot(i,j) - rqh
           hnew(i,j,k) = 0. _d 0
          ENDIF
C     endif iceMask > 0 etc.
         ENDIF
C     end i/j-loops
        ENDDO
       ENDDO
C     end k-loop
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     if isbyte=4, these change the AD-monitor output a little (10-11
C     digits of agreement)
CADJ STORE hnew = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE ebot = comlev1_bibj, key = ticekey, kind = isbyte
#endif
C     If ice melts completely and snow is left, remove the snow with
C     energy from the mixed layer
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j) .GT. 0. _d 0 .AND.
     &       ebot(i,j)   .GT. 0. _d 0 .AND.
     &       hSnow1(i,j) .GT. 0. _d 0) THEN
         rq =  rhos * qsnow
         rqh = rq * hSnow1(i,j)
         IF (ebot(i,j) .LT. rqh) THEN
          hSnow1(i,j) = hSnow1(i,j) - ebot(i,j) / rq
          ebot(i,j) = 0. _d 0
         ELSE
          ebot(i,j) = ebot(i,j) - rqh
          hSnow1(i,j) = 0. _d 0
         ENDIF
c        IF (ebot(i,j) .GT. 0. _d 0) THEN
c           IF (dBug) WRITE(6,*) 'All ice (& snow) melts from bottom'
c           hIce(i,j)=0. _d 0
c           go to 200
c        ENDIF

C     endif iceMask > 0, etc.
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
CADJ STORE hSnow1 = comlev1_bibj, key = ticekey, kind = isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN
C Compute new total ice thickness.
         hIce(i,j) = hnew(i,j,1) + hnew(i,j,2)
#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &        'ThSI_CALC_TH:   etop, ebot, hIce, hSnow1 =',
     &        etop(i,j), ebot(i,j), hIce(i,j), hSnow1(i,j)
#endif

C If hIce < hIceMin, melt the ice.
         IF ( hIce(i,j).LT.hIceMin
     &        .AND. (hIce(i,j)+hSnow1(i,j)).GT.0. _d 0 ) THEN
          esurp(i,j) = esurp(i,j) - rhos*qsnow*hSnow1(i,j)
     &         - rhoi*qicen(i,j,1)*hnew(i,j,1)
     &         - rhoi*qicen(i,j,2)*hnew(i,j,2)
          hIce(i,j)   = 0. _d 0
          hSnow1(i,j)  = 0. _d 0
          tSrf(i,j)   = 0. _d 0
          icFrac(i,j) = 0. _d 0
          qicen(i,j,1) = 0. _d 0
          qicen(i,j,2) = 0. _d 0
#ifdef ALLOW_DBUG_THSICE
          IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &         'ThSI_CALC_TH: -1 : esurp=',esurp(i,j)
#endif
         ENDIF

C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN

C--   do a mass-budget of sea-ice to compute "fresh" = the fresh-water flux
C     that is returned to the ocean ; needs to be done before snow/evap
         frw2oc(i,j) = (mwater0(i,j)
     &        - (rhos*hSnow1(i,j)+rhoi*hIce(i,j)))/dt

         IF ( hIce(i,j) .LE. 0. _d 0 ) THEN
C-    return  snow to the ocean (account for Latent heat of freezing)
          frw2oc(i,j) = frw2oc(i,j) + snowP(i,j)
          flx2oc(i,j) = flx2oc(i,j) - snowP(i,j)*Lfresh
         ENDIF

C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
C-    else: hIce > 0
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN

         IF ( hIce(i,j) .GT. 0. _d 0 ) THEN
C Let it snow
          hSnow1(i,j) = hSnow1(i,j) + dt*snowP(i,j)/rhos
C If ice evap is used to sublimate surface snow/ice or
C if no ice pass on to ocean
          IF (hSnow1(i,j).GT.0. _d 0) THEN
           IF (evapLoc(i,j)/rhos *dt.GT.hSnow1(i,j)) THEN
            evapLoc(i,j)=evapLoc(i,j)-hSnow1(i,j)*rhos/dt
            hSnow1(i,j)=0. _d 0
           ELSE
            hSnow1(i,j) = hSnow1(i,j) - evapLoc(i,j)/rhos *dt
            evapLoc(i,j)=0. _d 0
           ENDIF
          ENDIF
C     endif hice > 0
         ENDIF
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

C-    else: hIce > 0
      DO k = 1, nlyr
#ifdef ALLOW_AUTODIFF_TAMC
       ikey_1 = k + (ticekey-1)*nlyr
CADJ STORE evaploc      = comlev1_thsice_nlyr, key=ikey_1, kind=isbyte
#endif
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j).GT.0. _d 0 ) THEN

           hnewTmp = hnew(i,j,k)
           IF (hIce(i,j).GT.0. _d 0.AND.evapLoc(i,j).GT.0. _d 0) THEN
C            IF (evapLoc(i,j) .GT. 0. _d 0) THEN
C-- original scheme, does not care about ice temp.
C-  this can produce small error (< 1.W/m2) in the Energy budget
c              IF (evapLoc(i,j)/rhoi *dt.GT.hnewTmp) THEN
c                evapLoc(i,j)=evapLoc(i,j)-hnewTmp*rhoi/dt
c                hnew(i,j,k)=0. _d 0
c              ELSE
c                hnew(i,j,k) = hnewTmp - evapLoc(i,j)/rhoi *dt
c                evapLoc(i,j)=0. _d 0
c              ENDIF
C-- modified scheme. taking into account Ice enthalpy
             dhi = evapLoc(i,j)/rhoi*dt
             IF (dhi.GE.hnewTmp) THEN
              evapLoc(i,j)=evapLoc(i,j)-hnewTmp*rhoi/dt
              esurp(i,j) = esurp(i,j)
     &             - hnewTmp*rhoi*(qicen(i,j,k)-Lfresh)
              hnew(i,j,k)=0. _d 0
             ELSE
              hq = hnewTmp*qicen(i,j,k)-dhi*Lfresh
              hnew(i,j,k) = hnewTmp - dhi
              qicen(i,j,k)=hq/hnew(i,j,k)
              evapLoc(i,j)=0. _d 0
             ENDIF
C-------
c     IF (evapLoc(i,j) .GT. 0. _d 0) THEN
c           WRITE (6,*)  'BB All ice sublimates', i,j
c           hIce(i,j)=0. _d 0
c           go to 200
c     ENDIF
C     endif hice > 0 and evaploc > 0
          ENDIF
C     endif iceMask > 0
         ENDIF
C     end i/j-loops
        ENDDO
       ENDDO
C     end k-loop
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     if isbyte=4, these change the AD-monitor output a little (8-10
C     digits of agreement)
CADJ STORE evapLoc = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hSnow1  = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hIce    = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hnew    = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE qicen   = comlev1_bibj, key = ticekey, kind = isbyte
#endif

C     still else: hice > 0
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN
         IF (hIce(i,j) .GT. 0. _d 0) THEN
C Compute new total ice thickness.
          hIce(i,j) = hnew(i,j,1) + hnew(i,j,2)
C If hIce < hIceMin, melt the ice.
          IF ( hIce(i,j).GT.0. _d 0 .AND. hIce(i,j).LT.hIceMin ) THEN
           frw2oc(i,j) = frw2oc(i,j)
     &          + (rhos*hSnow1(i,j) + rhoi*hIce(i,j))/dt
           esurp(i,j) = esurp(i,j) - rhos*qsnow*hSnow1(i,j)
     &          - rhoi*qicen(i,j,1)*hnew(i,j,1)
     &          - rhoi*qicen(i,j,2)*hnew(i,j,2)
           hIce(i,j)   = 0. _d 0
           hSnow1(i,j)  = 0. _d 0
           tSrf(i,j)   = 0. _d 0
           icFrac(i,j) = 0. _d 0
           qicen(i,j,1) = 0. _d 0
           qicen(i,j,2) = 0. _d 0
#ifdef ALLOW_DBUG_THSICE
           IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &          'ThSI_CALC_TH: -2 : esurp,frw2oc=',
     &          esurp(i,j), frw2oc(i,j)
#endif
          ENDIF

C--   else hIce > 0: end
         ENDIF

C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

      DO j = jMin, jMax
       DO i = iMin, iMax
C     These helper variables avoid TAF recomputation warnings
        hIceTmp = hIce(i,j)
        hSnwTmp = hSnow1(i,j)
        hnewTmp = hnew(i,j,1)
        IF (iceMask(i,j).GT.0. _d 0) THEN

         IF ( hIceTmp .GT. 0. _d 0 ) THEN

C If there is enough snow to lower the ice/snow interface below
C freeboard, convert enough snow to ice to bring the interface back
C to sea-level OR if snow height is larger than hsMax, snow is
C converted to ice to bring hSnow1 down to hsMax. Largest change is
C applied and enthalpy of top ice layer adjusted accordingly.

          IF ( hSnwTmp .GT. hIceTmp*floodFac
     &         .OR. hSnwTmp .GT. hsMax ) THEN
cBB               WRITE (6,*)  'Freeboard adjusts'
c          dhi = (hSnwTmp * rhos - hIceTmp * rhoiw) / rhosw
c          dhs = dhi * rhoi / rhos
           dhs = (hSnwTmp - hIceTmp*floodFac) * rhoi / rhosw
           dhs = MAX( hSnwTmp - hsMax, dhs )
           dhi = dhs * rhos / rhoi
           rqh = rhoi*qicen(i,j,1)*hnewTmp + rhos*qsnow*dhs
           hnew(i,j,1)  = hnewTmp + dhi
           qicen(i,j,1) = rqh / (rhoi*hnew(i,j,1))
           hIce(i,j)    = hIceTmp + dhi
           hSnow1(i,j)  = hSnwTmp - dhs
          ENDIF
#ifdef ALLOW_AUTODIFF
C     This is awkward, but makes storing a few variables (hIce, hnew,
C     qicen) unnecessary. Splitting the loop also changes the condition
C     hIce>0 because hIce has been modified above. This could be handled
C     more cleanly, but because hIce>0 is required for the previous
C     block to be executed where snow is turned into ice hIce>0 is still
C     unchanged, so splitting this loop with the if conditions should be
C     safe. If this turns out to be robust, we can use this code also
C     for non-AD simulation.

C-    if hIce > 0 : end
         ENDIF
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
C     if isbyte=4, these change the AD-monitor output a little (8-10
C     digits of agreement), but avoid quite a few recomputations
CADJ STORE qicen       = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hice        = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hnew(:,:,1) = comlev1_bibj, key = ticekey, kind = isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
C     This helper variable avoids TAF recomputation warnings
        hIceTmp = hIce(i,j)
        IF (iceMask(i,j).GT.0. _d 0) THEN
         IF ( hIceTmp .GT. 0. _d 0 ) THEN
C limit ice height
C- NOTE: this part does not conserve Energy ;
C        but surplus of fresh water and salt are taken into account.
          IF (hIceTmp.GT.hiMax) THEN
cBB      print*,'BBerr, hIce>hiMax',i,j,hIce(i,j)
           chi=hIceTmp-hiMax
#else /* ndef ALLOW_AUTODIFF */
C limit ice height
C- NOTE: this part does not conserve Energy ;
C        but surplus of fresh water and salt are taken into account.
          IF (hIce(i,j).GT.hiMax) THEN
cBB      print*,'BBerr, hIce>hiMax',i,j,hIce(i,j)
           chi=hIce(i,j)-hiMax
#endif /* ALLOW_AUTODIFF */
           hnew(i,j,1)=hnew(i,j,1)-chi/2. _d 0
           hnew(i,j,2)=hnew(i,j,2)-chi/2. _d 0
           frw2oc(i,j) = frw2oc(i,j) + chi*rhoi/dt
          ENDIF
c       IF (hSnow1(i,j).GT.hsMax) THEN
cc        print*,'BBerr, hSnow1>hsMax',i,j,hSnow1(i,j)
c         chs=hSnow1(i,j)-hsMax
c         hSnow1(i,j)=hsMax
c         frw2oc(i,j) = frw2oc(i,j) + chs*rhos/dt
c       ENDIF

C Compute new total ice thickness.
          hIce(i,j) = hnew(i,j,1) + hnew(i,j,2)

#ifdef ALLOW_DBUG_THSICE
          IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &         'ThSI_CALC_TH: b-Winton: hnew, qice =',
     &         hnew(i,j,1), hnew(i,j,2),
     &         qicen(i,j,1), qicen(i,j,2)
#endif

          hlyr = hIce(i,j) * rec_nlyr
CML          CALL THSICE_RESHAPE_LAYERS(
CML     U         qicen(i,j,:),
CML     I         hlyr, hnew(i,j,:), myThid )
C     inlined version of S/R THSICE_RESHAPE_LAYERS
C     | Repartition into equal-thickness layers, conserving energy.
C     *==========================================================*
C     | This is the 2-layer version (formerly "NEW_LAYERS_WINTON")
C     |  from M. Winton 1999, JAOT, sea-ice model.
          if (hnew(i,j,1).gt.hnew(i,j,2)) then
C--   Layer 1 gives ice to layer 2
           f1 = (hnew(i,j,1)-hlyr)/hlyr
           q2tmp = f1*qicen(i,j,1) + (1. _d 0-f1)*qicen(i,j,2)
           if (q2tmp.gt.Lfresh) then
            qicen(i,j,2) = q2tmp
           else
C-    Keep q2 fixed to avoid q2<Lfresh and T2>0
            qh2 = hlyr*qicen(i,j,2)
            qhtot = hnew(i,j,1)*qicen(i,j,1) + hnew(i,j,2)*qicen(i,j,2)
            qh1 = qhtot - qh2
            qicen(i,j,1) = qh1/hlyr
           endif
          else
C-    Layer 2 gives ice to layer 1
           f1 = hnew(i,j,1)/hlyr
           qicen(i,j,1) = f1*qicen(i,j,1) + (1. _d 0-f1)*qicen(i,j,2)
          endif
C     end of inlined S/R THSICE_RESHAPE_LAYERS

#ifdef ALLOW_DBUG_THSICE
          IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &         'ThSI_CALC_TH: icFrac,hIce, qtot, hSnow1 =',
     &         icFrac(i,j),hIce(i,j), (qicen(i,j,1)+qicen(i,j,2))*0.5,
     &         hSnow1(i,j)
#endif

C-    if hIce > 0 : end
         ENDIF
c200     CONTINUE

C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC_MORE
CADJ STORE qicen = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE hnew = comlev1_bibj, key = ticekey, kind = isbyte
#endif

      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN
C-  Compute surplus energy left over from melting.

         IF (hIce(i,j).LE.0. _d 0) icFrac(i,j)=0. _d 0

C.. heat fluxes left over for ocean
         flx2oc(i,j) = flx2oc(i,j)
     &        + (Fbot(i,j)+(esurp(i,j)+etop(i,j)+ebot(i,j))/dt)
#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &        'ThSI_CALC_TH: [esurp,etop+ebot]/dt =',
     &        esurp(i,j)/dt,etop(i,j)/dt,ebot(i,j)/dt
#endif

C--   Evaporation left to the ocean :
         frw2oc(i,j) = frw2oc(i,j) - evapLoc(i,j)
C--   Correct Atmos. fluxes for this different latent heat:
C     evap was computed over freezing surf.(tSrf<0), latent heat = Lvap+Lfresh
C     but should be Lvap only for the fraction "evap" that is left to the ocean.
         flx2oc(i,j) = flx2oc(i,j) + evapLoc(i,j)*Lfresh

C fresh and salt fluxes
c     frw2oc(i,j) = (mwater0(i,j) - (rhos*(hSnow1(i,j))
c    &              + rhoi*(hIce(i,j))))/dt-evapLoc(i,j)
c     fsalt = (msalt0(i,j) - rhoi*hIce(i,j)*saltIce)/35. _d 0/dt  ! for same units as frw2oc
C     note (jmc): frw2oc is computed from a sea-ice mass budget that already
C     contains, at this point, snow & evaporation (of snow & ice)
C     but are not meant to be part of ice/ocean fresh-water flux.
C     fix: a) like below or b) by making the budget before snow/evap is added
c     frw2oc(i,j) = (mwater0(i,j) - (rhos*(hSnow1(i,j)) + rhoi*(hIce(i,j))))/dt
c    &      + snow(i,j,bi,bj)*rhos - frwAtm
         fsalt(i,j) = (msalt0(i,j) - rhoi*hIce(i,j)*saltIce)/dt

#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) THEN
          WRITE(6,1020)'ThSI_CALC_TH:dH2O,Ev[kg],frw2oc,fsalt',
     &         (mwater0(i,j)-(rhos*hSnow1(i,j)+rhoi*hIce(i,j)))/dt,
     &         evapLoc(i,j),frw2oc(i,j),fsalt(i,j)
          WRITE(6,1020)'ThSI_CALC_TH: flx2oc,Fbot,extend/dt =',
     &         flx2oc(i,j),Fbot(i,j),(etope(i,j)+ebote(i,j))/dt
         ENDIF
#endif

C--   add remaining liquid Precip (rain+RunOff) directly to ocean:
         frw2oc(i,j) = frw2oc(i,j) + (prcAtm(i,j)-snowP(i,j))

C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO

      DO j = jMin, jMax
       DO i = iMin, iMax
C     this copy really only helps TAF
        icFracTmp = icFrac(i,j)
        IF (iceMask(i,j).GT.0. _d 0) THEN
C--   note: at this point, icFrac has not been changed (unless reset to zero)
C     and it can only be reduced by lateral melting in the following part:

C     calculate extent changes
         extend=etope(i,j)+ebote(i,j)
         IF (icFracTmp.GT.0. _d 0.AND.extend.GT.0. _d 0) THEN
          rq =  rhoi * 0.5 _d 0*(qicen(i,j,1)+qicen(i,j,2))
          rs =  rhos * qsnow
          rqh = rq * hIce(i,j) + rs * hSnow1(i,j)
          freshe=(rhos*hSnow1(i,j)+rhoi*hIce(i,j))/dt
          salte=(rhoi*hIce(i,j)*saltIce)/dt
          IF ( extend.LT.rqh ) THEN
           icFrac(i,j)=(1. _d 0-extend/rqh)*icFracTmp
          ENDIF
          IF ( extend.LT.rqh .AND. icFrac(i,j).GE.iceMaskMin ) THEN
           frw2oc(i,j)=frw2oc(i,j)+extend/rqh*freshe
           fsalt(i,j)=fsalt(i,j)+extend/rqh*salte
          ELSE
           icFrac(i,j)=0. _d 0
           hIce(i,j)  =0. _d 0
           hSnow1(i,j) =0. _d 0
           flx2oc(i,j)=flx2oc(i,j)+(extend-rqh)/dt
           frw2oc(i,j)=frw2oc(i,j)+freshe
           fsalt(i,j)=fsalt(i,j)+salte
          ENDIF
         ELSEIF (extend.GT.0. _d 0) THEN
          flx2oc(i,j)=flx2oc(i,j)+extend/dt
         ENDIF
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Update output variables :

C--   Diagnostic of Atmos. fresh water flux (E-P) over sea-ice :
C     substract precip from Evap (<- stored in frwAtm array)
         frwAtm(i,j) = frwAtm(i,j) - prcAtm(i,j)

C--   update Mixed-Layer Freezing potential heat flux by substracting the
C     part which has already been accounted for (Fbot):
         fzMlOc(i,j) = fzMlOc(i,j) - Fbot(i,j)*iceMask(i,j)

C-- Update Sea-Ice state output:
         qIc1(i,j)   = qicen(i,j,1)
         qIc2(i,j)   = qicen(i,j,2)
#ifdef ALLOW_DBUG_THSICE
         IF (dBug(i,j,bi,bj) ) WRITE(6,1020)
     &        'ThSI_CALC_TH: icFrac,flx2oc,fsalt,frw2oc=',
     &        icFrac(i,j), flx2oc(i,j), fsalt(i,j), frw2oc(i,j)
#endif
C     endif iceMask > 0
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef CHECK_ENERGY_CONSERV
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j).GT.0. _d 0) THEN
         qaux(1)=qIc1(i,j)
         qaux(2)=qIc2(i,j)
         CALL THSICE_CHECK_CONSERV( dBugFlag, i, j, bi, bj, 0,
     I        iceMask(i,j), icFrac(i,j), hIce(i,j), hSnow1(i,j),
     I        qaux,
     I        flx2oc(i,j), frw2oc(i,j), fsalt,
     I        myTime, myIter, myThid )
C     endif iceMask > 0
        ENDIF
C     end i/j-loops
       ENDDO
      ENDDO
#endif /* CHECK_ENERGY_CONSERV */

#endif  /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
