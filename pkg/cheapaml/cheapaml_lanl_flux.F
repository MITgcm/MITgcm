#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_LANL_FLUX
C     !INTERFACE:
      SUBROUTINE CHEAPAML_LANL_FLUX(
     I                    i,j,bi,bj, iceOrNot,
     I                    tSurf, windSq,
     O                    fsha, flha, evp, xolw, ssqt, q100,
     O                    cdq, cdu,
c    O                    dSensdTs, dEvapdTs, dLWdTs, dQAdTs,
     I                    myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     SUBROUTINE cheapaml_LANL_flux
C     ==================================================================
C     o compute surface fluxes using LANL algorithms

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"

C     !INPUT PARAMETERS:
C     i, j     :: local indices of current grid-point
C     bi, bj   :: current tile indices
C     iceOrNot :: 0=open water, 1=ice cover, 2=ice+snow
C     tSurf    :: surface temperature
C     windSq   :: relative wind (vs surface motion) speed square
C     myIter   :: Current iteration number in simulation
C     myThid   :: My Thread Id number
      INTEGER i,j,bi,bj
      INTEGER iceOrNot
      _RL tSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL windSq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myIter, myThid

C     !OUTPUT PARAMETERS:
C     fsha   :: sensible heat flux
C     flha   :: latent heat flux
C     evp    :: evaporation
C     xolw   :: oceanic upwelled long wave radiation
C     ssqt   :: sat. specific humidity at atm layer top
C     cdq    :: evaporation coeff
C     cdu    :: surface drag coeff (for wind stress)
      _RL fsha, flha, evp, xolw, ssqt, q100, cdq, cdu
C     derivative vs surf. temp of Sensible, Evap, LW, q100
c     _RL dSensdTs, dEvapdTs, dLWdTs, dQAdTs
CEOP

C     Other Input (in common block):
C       Tair :: mean air temperature (K)  at height ht (m)
C       qair :: Specific humidity kg/kg

C     !LOCAL VARIABLES:
      _RL deltaTm
      _RL uss, usm
      _RL cheapaml_BulkCdn
      _RL t0, QaR
      _RL ssq, q
      _RL deltap, delq, pt, psx100, z100ol
      _RL rdn, ren, rhn, zice, zref
      _RL rd, re, rh, tta, ttas, toa, ttt
      _RL ustar, tstar, qstar, ht, hu, hq
      _RL aln, cdalton, czol, psim_fac
      _RL huol, stable, xsq, x, psimh, psixh
      _RL clha, csha
      INTEGER niter_bulk,iter

C useful values
C hardwire atmospheric relative humidity at 80%
        QaR = 0.8 _d 0
C factor to compute rainfall from specific humidity
C inverse of time step
        deltaTm = 1. _d 0/deltaT
C reference values to compute turbulent flux
        ht = zt
        hq = zq
        hu = zu
        zref = zt
        zice = .0005 _d 0
        aln = log(ht/zref)
C for iterating on turbulence
        niter_bulk = 5
        cdalton = 0.0346000 _d 0
        czol = zref*xkar*gravity
        psim_fac = 5. _d 0

C     determine wind stress
        IF ( .NOT.useStressOption ) THEN
          uss = SQRT( windSq(i,j) )
          usm = MAX( uss, 1. _d 0 )
          cheapaml_BulkCdn = cdrag_1/usm + cdrag_2 + cdrag_3*usm
          cdu = rhoa*cheapaml_BulkCdn*uss
        ELSE
          usm = 1. _d 0
        ENDIF
C diabatic and freshwater flux forcing
        toa = Celsius2K + tSurf(i,j)
        tta = Celsius2K + Tair(i,j,bi,bj)
        ttas = tta+gamma_blk*zref
        ttt = tta - ( cheaphgrid(i,j,bi,bj) - zref )*gamma_blk
        pt = p0*( oneRL -gamma_blk*cheaphgrid(i,j,bi,bj)/ttas )
     &          **(gravity/gamma_blk/gasR)

C specific humidities
        ssq  = ssq0*exp( lath*(ssq1-ssq2/toa) ) / p0
        ssqt = ssq0*exp( lath*(ssq1-ssq2/ttt) ) / pt
C     saturation no more at the top:
        ssqt = 0.7 _d 0*ssq

        IF (useFreshWaterFlux) THEN
          q = qair(i,j,bi,bj)
        ELSE
          q = QaR * ssq
        ENDIF

C adjust temperature from reference height to formula height
c       deltap = t  - to + gamma_blk*(zref-ht)
        deltap = Tair(i,j,bi,bj) - tSurf(i,j) + gamma_blk*(zref-ht)
        delq   = q - ssq
        ttas   = tta + gamma_blk*(zref-ht)
        t0     = ttas*(oneRL + humid_fac*q)

C initialize estimate exchange coefficients
        rdn = xkar/(log(zref/zice))
        rhn = rdn
        ren = rdn
C calculate turbulent scales
        ustar = rdn*usm
        tstar = rhn*deltap
        qstar = ren*delq

C iteration with psi-functions to find transfer coefficients
        DO iter=1,niter_bulk
          huol  = czol/ustar**2
     &          *( tstar/t0 + qstar/(oneRL/humid_fac+q) )
          huol  = SIGN( MIN(ABS(huol),10. _d 0), huol)
          stable = halfRL + SIGN( halfRL, huol)
          xsq = MAX( SQRT(ABS(oneRL - 16. _d 0*huol)), oneRL )
          x     = SQRT(xsq)
          psimh = -5. _d 0*huol*stable
     &          + (oneRL-stable)*( twoRL*LOG(halfRL*(oneRL+x))
     &                           + twoRL*LOG(halfRL*(oneRL+xsq))
     &                           - twoRL*ATAN(x) + pi*halfRL )
          psixh = -5. _d 0*huol*stable
     &          + (oneRL-stable)*(twoRL*LOG(halfRL*(oneRL+xsq)))

C Update the transfer coefficients
          rd = rdn/(oneRL + rdn*(aln-psimh)/xkar)
          rh = rhn/(oneRL + rhn*(aln-psixh)/xkar)
          re = rh
C  Update ustar, tstar, qstar using updated, shifted coefficients.
          ustar = rd*usm
          qstar = re*delq
          tstar = rh*deltap
        ENDDO

        usm = MAX( uss, 0.5 _d 0 )
        csha = rhoa*cpair*usm*rh*rd
        clha = rhoa*lath*usm*re*rd

        fsha = csha*deltap
        flha = clha*delq
        evp  = -flha/lath
        cdq  =  clha/lath

C the sensible and latent heat fluxes, fsha and flha,
C are computed so that positive values are downward.
C the convention for cheapaml is upward fluxes are positive,
C so they must be multiplied by -1
        fsha = -fsha
        flha = -flha

C oceanic upwelled long wave
        xolw = stefan*(toa)**4
C compute specific humidity at 100m
        huol   = czol/ustar**2
     &         * ( tstar/t0 + qstar/(oneRL/humid_fac+q) )
        huol   = SIGN( MIN(ABS(huol),10. _d 0), huol)
        stable = halfRL + SIGN( halfRL, huol)
        z100ol = 100. _d 0 *xkar*gravity/ustar**2
     &         * ( tstar/t0 + qstar/(oneRL/humid_fac+q) )
        xsq = MAX( SQRT(ABS( oneRL - 16. _d 0*z100ol)), oneRL )
c       x   = SQRT(xsq)
        psx100 = -5. _d 0*z100ol*stable
     &         + (oneRL-stable)*(twoRL*log(halfRL*(oneRL+xsq)))
        q100 = ssq + qstar*( log(100. _d 0/zice) - psx100 )

      RETURN
      END
