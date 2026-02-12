#include "BLING_OPTIONS.h"

C--  File bling_carbon_chem.F:
C--   Contents
C--   o CALC_PCO2
C--   o CALC_PCO2_APPROX
C--   o CARBON_COEFFS
C--   o CARBON_COEFFS_PRESSURE_DEP

CBOP
       subroutine CALC_PCO2(
     I                       donewt,inewtonmax,ibrackmax,
     I                       t,s,diclocal,pt,sit,ta,
     I                       k1local,k2local,
     I                       k1plocal,k2plocal,k3plocal,
     I                       kslocal,kblocal,kwlocal,
     I                       ksilocal,kflocal,
     I                       k0local, fugflocal,
     I                       fflocal,btlocal,stlocal,ftlocal,
     U                       pHlocal,pCO2surfloc,
     I                       i,j,k,bi,bj,myIter,myThid )

C     =================================================================
C     | subroutine bling_carbon_chem
C     | o Calculates ocean inorganic carbon chemistry
C     |   Adapted from pkg/dic/carbon_chem.F
C     |   from OCMIP2 code
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
c#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (g/kg)
        INTEGER donewt
        INTEGER inewtonmax
        INTEGER ibrackmax
        _RL  t, s, pt, sit, ta
        _RL  pCO2surfloc, diclocal, pHlocal
        _RL  fflocal, btlocal, stlocal, ftlocal
        _RL  k1local, k2local
        _RL  k1plocal, k2plocal, k3plocal
        _RL  kslocal, kblocal, kwlocal, ksilocal, kflocal
        _RL  k0local, fugflocal
        INTEGER i,j,k,bi,bj,myIter
        INTEGER myThid
CEOP

C     == Local variables ==
C INPUT
C       phlo= lower limit of pH range
C       phhi= upper limit of pH range
C       atmpres = atmospheric pressure in atmospheres (1 atm==1013.25mbar)
C OUTPUT
C       co2star  = CO2*water (mol/m^3)
C       pco2surf = oceanic pCO2 (ppmv)
C ---------------------------------------------------------------------
C OCMIP NOTE: Some words about units - (JCO, 4/4/1999)
C     - Models carry tracers in mol/m^3 (on a per volume basis)
C     - Conversely, this routine, which was written by
C       observationalists (C. Sabine and R. Key), passes input
C       arguments in umol/kg (i.e., on a per mass basis)
C     - I have changed things slightly so that input arguments are in
C       mol/m^3,
C     - Thus, all input concentrations (diclocal, ta, pt, and st) should be
C       given in mol/m^3; output arguments "co2star" and "dco2star"
C       are likewise be in mol/m^3.
C ---------------------------------------------------------------------

        _RL  phhi
        _RL  phlo
        _RL  c
        _RL  a
        _RL  a2
        _RL  da
        _RL  b
        _RL  b2
        _RL  db
        _RL  fn
        _RL  df
        _RL  deltax
        _RL  x
        _RL  x2
        _RL  x3
        _RL  xmid
        _RL  ftest
        _RL  htotal
        _RL  htotal2
        _RL  co2star
        _RL  phguess
        _RL  fco2
        INTEGER inewton
        INTEGER ibrack
        INTEGER hstep
        _RL  fni(3)
        _RL  xlo
        _RL  xhi
        _RL  xguess
        _RL  k123p
        _RL  k12p
        _RL  k12
c ---------------------------------------------------------------------
c import donewt flag
c set donewt = 1 for newton-raphson iteration
c set donewt = 0 for bracket and bisection
c ---------------------------------------------------------------------
C Change units from the input of mol/m^3 -> mol/kg:
c (1 mol/m^3)  x (1 m^3/1024.5 kg)
c where the ocean mean surface density is 1024.5 kg/m^3
c Note: mol/kg are actually what the body of this routine uses
c for calculations.  Units are reconverted back to mol/m^3 at the
c end of this routine.
c ---------------------------------------------------------------------
c To convert input in mol/m^3 -> mol/kg
        pt=pt*permil
        sit=sit*permil
        ta=ta*permil
        diclocal=diclocal*permil
c ---------------------------------------------------------------------
c set first guess and brackets for [H+] solvers
c first guess (for newton-raphson)
        phguess = phlocal

c bracketing values (for bracket/bisection)
        phhi = 10.0
        phlo = 5.0
c convert to [H+]...
        xguess = 10.0**(-phguess)
        xlo = 10.0**(-phhi)
        xhi = 10.0**(-phlo)
        xmid = (xlo + xhi)*0.5

c----------------------------------------------------------------
c iteratively solve for [H+]
c (i) Newton-Raphson method with fixed number of iterations,
c     use previous [H+] as first guess

c select newton-raphson, inewt=1
c else select bracket and bisection

cQQQQQ
        if( donewt .eq. 1)then
c.........................................................
c NEWTON-RAPHSON METHOD
c.........................................................
          x = xguess
cdiags
c         WRITE(0,*)'xguess ',xguess
cdiags
          do inewton = 1, inewtonmax
c set some common combinations of parameters used in
c the iterative [H+] solvers
            x2=x*x
            x3=x2*x
            k12 = k1local*k2local
            k12p = k1plocal*k2plocal
            k123p = k12p*k3plocal
            c = 1.0 + stlocal/kslocal
            a = x3 + k1plocal*x2 + k12p*x + k123p
            a2=a*a
            da = 3.0*x2 + 2.0*k1plocal*x + k12p
            b = x2 + k1local*x + k12
            b2=b*b
            db = 2.0*x + k1local

c Evaluate f([H+]) and f_prime([H+])
c fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree
c      +hso4+hf+h3po4-ta
            fn = k1local*x*diclocal/b +
     &        2.0*diclocal*k12/b +
     &        btlocal/(1.0 + x/kblocal) +
     &        kwlocal/x +
     &        pt*k12p*x/a +
     &        2.0*pt*k123p/a +
     &        sit/(1.0 + x/ksilocal) -
     &        x/c -
     &        stlocal/(1.0 + kslocal/x/c) -
     &        ftlocal/(1.0 + kflocal/x) -
     &        pt*x3/a -
     &        ta

c df = dfn/dx
cdiags
c      WRITE(0,*)'values',b2,kblocal,x2,a2,c,x
cdiags
            df = ((k1local*diclocal*b) - k1local*x*diclocal*db)/b2 -
     &        2.0*diclocal*k12*db/b2 -
     &        btlocal/kblocal/(1.0+x/kblocal)**2. -
     &        kwlocal/x2 +
     &        (pt*k12p*(a - x*da))/a2 -
     &        2.0*pt*k123p*da/a2 -
     &        sit/ksilocal/(1.0+x/ksilocal)**2. +
     &        1.0/c +
     &        stlocal*(1.0 + kslocal/x/c)**(-2.0)*(kslocal/c/x2) +
     &        ftlocal*(1.0 + kflocal/x)**(-2.)*kflocal/x2 -
     &        pt*x2*(3.0*a-x*da)/a2
c evaluate increment in [H+]
            deltax = - fn/df
c update estimate of [H+]
            x = x + deltax
cdiags
c write value of x to check convergence....
c           write(0,*)'inewton, x, deltax ',inewton, x, deltax
c           write(6,*)
cdiags

          end do
c end of newton-raphson method
c....................................................
        else
c....................................................
C BRACKET AND BISECTION METHOD
c....................................................
c (ii) If first step use Bracket and Bisection method
c      with fixed, large number of iterations
          do ibrack = 1, ibrackmax
            do hstep = 1,3
              if(hstep .eq. 1)x = xhi
              if(hstep .eq. 2)x = xlo
              if(hstep .eq. 3)x = xmid
c set some common combinations of parameters used in
c the iterative [H+] solvers

              x2=x*x
              x3=x2*x
              k12 = k1local*k2local
              k12p = k1plocal*k2plocal
              k123p = k12p*k3plocal
              c = 1.0 + stlocal/kslocal
              a = x3 + k1plocal*x2 + k12p*x + k123p
              a2=a*a
              da = 3.0*x2 + 2.0*k1plocal*x + k12p
              b = x2 + k1local*x + k12
              b2=b*b
              db = 2.0*x + k1local
c evaluate f([H+]) for bracketing and mid-value cases
              fn = k1local*x*diclocal/b +
     &          2.0*diclocal*k12/b +
     &          btlocal/(1.0 + x/kblocal) +
     &          kwlocal/x +
     &          pt*k12p*x/a +
     &          2.0*pt*k123p/a +
     &          sit/(1.0 + x/ksilocal) -
     &          x/c -
     &          stlocal/(1.0 + kslocal/x/c) -
     &          ftlocal/(1.0 + kflocal/x) -
     &          pt*x3/a -
     &          ta
              fni(hstep) = fn
            end do
c now bracket solution within two of three
            ftest = fni(1)/fni(3)
            if(ftest .gt. 0.0)then
              xhi = xmid
            else
              xlo = xmid
            end if
            xmid = (xlo + xhi)*0.5

cdiags
c write value of x to check convergence....
c           WRITE(0,*)'bracket-bisection iteration ',ibrack, xmid
cdiags
          end do
c last iteration gives value
          x = xmid
c end of bracket and bisection method
c....................................
        end if
c iterative [H+] solver finished
c----------------------------------------------------------------

c now determine pCO2 etc...
c htotal = [H+], hydrogen ion conc
        htotal = x
C Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
C ORNL/CDIAC-74, dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)
        htotal2=htotal*htotal
        co2star=diclocal*htotal2/(htotal2 + k1local*htotal
     &            + k1local*k2local)
        phlocal=-log10(htotal)

c ---------------------------------------------------------------
c Add two output arguments for storing pCO2surf
c Should we be using K0 or ff for the solubility here?
c ---------------------------------------------------------------
        fco2 = co2star / k0local
        pCO2surfloc = fco2/fugflocal

C ----------------------------------------------------------------
C Reconvert units back to original values for input arguments
C no longer necessary????
C ----------------------------------------------------------------
c       Reconvert from mol/kg -> mol/m^3
        pt=pt/permil
        sit=sit/permil
        ta=ta/permil
        diclocal=diclocal/permil

        RETURN
        END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      subroutine CALC_PCO2_APPROX(
     I                       t,s,diclocal,pt,sit,ta,
     I                       k1local,k2local,
     I                       k1plocal,k2plocal,k3plocal,
     I                       kslocal,kblocal,kwlocal,
     I                       ksilocal,kflocal,
     I                       k0local, fugflocal,
     I                       fflocal,btlocal,stlocal,ftlocal,
     U                       pHlocal,pCO2surfloc,co3local,
     I                       i,j,k,bi,bj,myIter,myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CALC_PCO2_APPROX                              |
C     *==========================================================*
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     C  New efficient pCO2 solver, Mick Follows         CC
C     C                             Taka Ito             CC
C     C                             Stephanie Dutkiewicz CC
C     C  20 April 2003                                   CC
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      Apr 2011: fix vapour bug (following Bennington)
C      Oct 2011: add CO3 extimation and pass out

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (g/kg)
        _RL  t, s, pt, sit, ta
        _RL  pCO2surfloc, diclocal, pHlocal
        _RL  fflocal, btlocal, stlocal, ftlocal
        _RL  k1local, k2local
        _RL  k1plocal, k2plocal, k3plocal
        _RL  kslocal, kblocal, kwlocal, ksilocal, kflocal
        _RL  k0local, fugflocal
        _RL  co3local
        INTEGER i,j,k,bi,bj,myIter
        INTEGER myThid
CEOP

C     == Local variables ==
        _RL  phguess
        _RL  cag
        _RL  bohg
        _RL  hguess
        _RL  stuff
        _RL  gamm
        _RL  hnew
        _RL  co2s
        _RL  h3po4g, h2po4g, hpo4g, po4g
        _RL  siooh3g
        _RL  fco2

c ---------------------------------------------------------------------
C Change units from the input of mol/m^3 -> mol/kg:
c (1 mol/m^3)  x (1 m^3/1024.5 kg)
c where the ocean mean surface density is 1024.5 kg/m^3
c Note: mol/kg are actually what the body of this routine uses
c for calculations.  Units are reconverted back to mol/m^3 at the
c end of this routine.
c To convert input in mol/m^3 -> mol/kg
        pt=pt*permil
        sit=sit*permil
        ta=ta*permil
        diclocal=diclocal*permil
c ---------------------------------------------------------------------
c set first guess and brackets for [H+] solvers
c first guess (for newton-raphson)
        phguess = phlocal
cmick - new approx method
cmick - make estimate of htotal (hydrogen ion conc) using
cmick   appromate estimate of CA, carbonate alkalinity
        hguess = 10.0**(-phguess)
cmick - first estimate borate contribution using guess for [H+]
        bohg = btlocal*kblocal/(hguess+kblocal)

cmick - first estimate of contribution from phosphate
cmick based on Dickson and Goyet
        stuff = hguess*hguess*hguess
     &           + (k1plocal*hguess*hguess)
     &           + (k1plocal*k2plocal*hguess)
     &           + (k1plocal*k2plocal*k3plocal)
        h3po4g = (pt*hguess*hguess*hguess) / stuff
        h2po4g = (pt*k1plocal*hguess*hguess) / stuff
        hpo4g  = (pt*k1plocal*k2plocal*hguess) / stuff
        po4g   = (pt*k1plocal*k2plocal*k3plocal) / stuff

cmick - estimate contribution from silicate
cmick based on Dickson and Goyet
        siooh3g = sit*ksilocal / (ksilocal + hguess)

cmm - rare, but ice melt can cause prescribed silicate alkalinity to be
cmm   larger than total alkalinity. Crop here to account for this
        IF (siooh3g.GT.0.2 _d 0*ta) siooh3g = 0.2 _d 0*ta

cmick - now estimate carbonate alkalinity
        cag = ta - bohg - (kwlocal/hguess) + hguess
     &           - hpo4g - 2.0 _d 0*po4g + h3po4g
     &           - siooh3g
CMM(
        IF ( cag .LT. (0.1 _d 0*ta) )  THEN
       print *,'CMMcag1', cag,i,j,k,xC(i,j,1,1),yC(i,j,1,1)
C myXGlobalLo-1+(bi-1)*sNx+i,        myYGlobalLo-1+(bj-1)*sNy+j
       print *,'CMMcag2',ta,bohg,(kwlocal/hguess),hguess,hpo4g,
     &           2.0 _d 0*po4g,h3po4g, siooh3g
       cag = 0.1 _d 0*ta
        ENDIF
CMM)
cmick - now evaluate better guess of hydrogen ion conc
cmick   htotal = [H+], hydrogen ion conc
        gamm  = diclocal/cag
        stuff = (1.0 _d 0-gamm)*(1.0 _d 0-gamm)*k1local*k1local
     &          - 4.0 _d 0*k1local*k2local*(1.0 _d 0-2.0 _d 0*gamm)
        hnew  = 0.5 _d 0*( (gamm-1.0 _d 0)*k1local + sqrt(stuff) )
CMM(
        IF ( hnew .GT. (0.00001 _d 0) )  THEN
cmm          print *,'CMMhnew reset:',hnew,i,j,k,xC(i,j,1,1),yC(i,j,1,1)
          hnew = 0.00000001 _d 0
        ENDIF
        IF ( hnew .LT. (0.0000000001 _d 0) )  THEN
cmm          print *,'CMMhnew reset:',hnew,i,j,k,xC(i,j,1,1),yC(i,j,1,1)
          hnew = 0.00000001 _d 0
        ENDIF
CMM)
cmick - now determine [CO2*]
        co2s  = diclocal/
     &   (1.0 _d 0 + (k1local/hnew) + (k1local*k2local/(hnew*hnew)))
cmick - return update pH to main routine
        phlocal = -log10(hnew)

c NOW EVALUATE CO32-, carbonate ion concentration
c used in determination of calcite compensation depth
c Karsten Friis & Mick - Sep 2004
        co3local = k1local*k2local*diclocal /
     &         (hnew*hnew + k1local*hnew + k1local*k2local)

c ---------------------------------------------------------------
c surface pCO2 (following Dickson and Goyet, DOE...)
        fco2 = co2s/k0local
        pco2surfloc = fco2/fugflocal

C ----------------------------------------------------------------
c Reconvert from mol/kg -> mol/m^3
        pt=pt/permil
        sit=sit/permil
        ta=ta/permil
        diclocal=diclocal/permil

        RETURN
        END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
      subroutine CARBON_COEFFS(
     I                   ttemp,stemp,
     I                   bi,bj,iMin,iMax,jMin,jMax,myThid)

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CARBON_COEFFS                                 |
C     | determine coefficients for surface carbon chemistry      |
C     | adapted from OCMIP2:  SUBROUTINE CO2CALC                 |
C     | mick follows, oct 1999                                   |
C     | minor changes to tidy, swd aug 2002                      |
C     *==========================================================*
C INPUT
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (g/kg)
C OUTPUT
C IMPORTANT: Some words about units - (JCO, 4/4/1999)
C     - Models carry tracers in mol/m^3 (on a per volume basis)
C     - Conversely, this routine, which was written by observationalists
C       (C. Sabine and R. Key), passes input arguments in umol/kg
C       (i.e., on a per mass basis)
C     - I have changed things slightly so that input arguments are in mol/m^3,
C     - Thus, all input concentrations (diclocal, ta, pt, and st) should be
C       given in mol/m^3; output arguments "co2star" and "dco2star"
C       are likewise be in mol/m^3.
C
C Apr 2011: fix vapour bug (following Bennington)
C--------------------------------------------------------------------------

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C ttemp and stemp are local theta and salt arrays
C dont really need to pass T and S in, could use theta, salt in
C common block in DYNVARS.h, but this way keeps subroutine more
C general
        _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        INTEGER bi,bj,iMin,iMax,jMin,jMax
        INTEGER myThid
CEOP

C LOCAL VARIABLES
        _RL  t
        _RL  s
        _RL  tk
        _RL  tk100
        _RL  tk1002
        _RL  dlogtk
        _RL  sqrtis
        _RL  sqrts
        _RL  s15
        _RL  scl
        _RL  s2
        _RL  invtk
        _RL  is
        _RL  is2
c add Bennington
        _RL  P1atm
        _RL  Rgas
        _RL  RT
        _RL  delta
        _RL  B1
        _RL  B
#ifdef CARBONCHEM_TOTALPHSCALE
c pH scale converstions
        _RL total2free
        _RL free2sw
        _RL total2sw
#endif
        INTEGER i
        INTEGER j

C.....................................................................
C OCMIP note:
C Calculate all constants needed to convert between various measured
C carbon species. References for each equation are noted in the code.
C Once calculated, the constants are
C stored and passed in the common block "const". The original version
C of this code was based on the code by dickson in Version 2 of
C  Handbook of Methods C for the Analysis of the Various Parameters of
C the Carbon Dioxide System in Seawater , DOE, 1994 (SOP No. 3, p25-26).
C....................................................................

        do i=imin,imax
         do j=jmin,jmax
          if (hFacC(i,j,1,bi,bj).gt.0. _d 0) then
           t = ttemp(i,j)
           s = stemp(i,j)
C terms used more than once for:
C temperature
           tk = 273.15 _d 0 + t
           tk100 = tk/100. _d 0
           tk1002=tk100*tk100
           invtk=1.0 _d 0/tk
           dlogtk=log(tk)
C ionic strength
           is=19.924 _d 0*s/(1000. _d 0-1.005 _d 0*s)
           is2=is*is
           sqrtis=sqrt(is)
C salinity
           s2=s*s
           sqrts=sqrt(s)
           s15=s**1.5 _d 0
           scl=s/1.80655 _d 0
C -----------------------------------------------------------------------
C added by Val Bennington Nov 2010
C Fugacity Factor needed for non-ideality in ocean
C ff used for atmospheric correction for water vapor and pressure
C Weiss (1974) Marine Chemistry
           P1atm = 1.01325 _d 0 ! bars
           Rgas = 83.1451 _d 0 ! bar*cm3/(mol*K)
           RT = Rgas*tk
           delta = (57.7 _d 0 - 0.118 _d 0*tk)
           B1 = -1636.75 _d 0 + 12.0408 _d 0*tk - 0.0327957 _d 0*tk*tk
           B = B1 + 3.16528 _d 0*tk*tk*tk*(0.00001 _d 0)
           fugf(i,j,bi,bj) = exp( (B+2. _d 0*delta) *
     &                            AtmosP(i,j,bi,bj) * P1atm / RT)

C------------------------------------------------------------------------
C f = k0(1-pH2O)*correction term for non-ideality
C Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)
           ff(i,j,bi,bj) = exp(-162.8301 _d 0 + 218.2968 _d 0/tk100  +
     &          90.9241 _d 0*log(tk100) - 1.47696 _d 0*tk1002 +
     &          s * (.025695 _d 0 - .025225 _d 0*tk100 +
     &          0.0049867 _d 0*tk1002))
C------------------------------------------------------------------------
C K0 from Weiss 1974
C Independent of pH scale
           ak0(i,j,bi,bj) = exp(93.4517 _d 0/tk100 - 60.2409 _d 0 +
     &        23.3585 _d 0 * log(tk100) +
     &        s * (0.023517 _d 0 - 0.023656 _d 0*tk100 +
     &        0.0047036 _d 0*tk1002))
C------------------------------------------------------------------------
C k1 = [H][HCO3]/[H2CO3]
C k2 = [H][CO3]/[HCO3]
C Millero p.664 (1995) using Mehrbach et al. data on seawater scale
C Both on seawater pH scale
           ak1(i,j,bi,bj)=10.**(-1. _d 0*(3670.7 _d 0*invtk -
     &          62.008 _d 0 + 9.7944 _d 0*dlogtk -
     &          0.0118 _d 0 * s + 0.000116 _d 0*s2))
           ak2(i,j,bi,bj)=10.**(-1. _d 0*(1394.7 _d 0*invtk +4.777 _d 0
     &          -0.0184 _d 0*s + 0.000118 _d 0*s2))
C------------------------------------------------------------------------
C kb = [H][BO2]/[HBO2]
C Millero p.669 (1995) using data from dickson (1990)
C On total pH scale
           akb(i,j,bi,bj)=exp((-8966.90 _d 0- 2890.53 _d 0*sqrts -
     &          77.942 _d 0*s + 1.728 _d 0*s15 - 0.0996 _d 0*s2)*invtk +
     &          (148.0248 _d 0 + 137.1942 _d 0*sqrts + 1.62142 _d 0*s) +
     &          (-24.4344 _d 0 - 25.085 _d 0*sqrts - 0.2474 _d 0*s) *
     &          dlogtk + 0.053105 _d 0*sqrts*tk)
C------------------------------------------------------------------------
C k1p = [H][H2PO4]/[H3PO4]
C DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
C The constant 115.525 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 115.540 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak1p(i,j,bi,bj) = exp(-4576.752 _d 0*invtk + 115.525 _d 0 -
     &          18.453 _d 0*dlogtk +
     &          (-106.736 _d 0*invtk + 0.69171 _d 0)*sqrts +
     &          (-0.65643 _d 0*invtk - 0.01844 _d 0)*s)
C------------------------------------------------------------------------
C k2p = [H][HPO4]/[H2PO4]
C DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
C The constant 172.0833 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 172.1033 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak2p(i,j,bi,bj) = exp(-8814.715 _d 0*invtk + 172.0883 _d 0 -
     &          27.927 _d 0*dlogtk +
     &          (-160.340 _d 0*invtk + 1.3566 _d 0) * sqrts +
     &          (0.37335 _d 0*invtk - 0.05778 _d 0) * s)
C------------------------------------------------------------------------
C k3p = [H][PO4]/[HPO4]
C DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
C The constant 18.141 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 18.126 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak3p(i,j,bi,bj) = exp(-3070.75 _d 0*invtk - 18.141 _d 0 +
     &          (17.27039 _d 0*invtk + 2.81197 _d 0) *
     &          sqrts + (-44.99486 _d 0*invtk - 0.09984 _d 0) * s)
C------------------------------------------------------------------------
C ksi = [H][SiO(OH)3]/[Si(OH)4]
C Millero p.671 (1995) using data from Yao and Millero (1995)
C The constant 117.385 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 117.400 constant to stay on the seawater
C  pH scale (everything else is the same).
C  Note: converted here from mol/kg-H2O to mol/kg-SW
           aksi(i,j,bi,bj) = exp(-8904.2 _d 0*invtk + 117.385 _d 0 -
     &          19.334 _d 0*dlogtk +
     &          (-458.79 _d 0*invtk + 3.5913 _d 0) * sqrtis +
     &          (188.74 _d 0*invtk - 1.5998 _d 0) * is +
     &          (-12.1652 _d 0*invtk + 0.07871 _d 0) * is2 +
     &          log(1.0 _d 0-0.001005 _d 0*s))
C------------------------------------------------------------------------
C kw = [H][OH]
C Millero p.670 (1995) using composite data
C The constant 148.9652 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 148.9802 constant to stay on the seawater
C  pH scale (everything else is the same).
           akw(i,j,bi,bj) = exp(-13847.26 _d 0*invtk + 148.9652 _d 0 -
     &          23.6521 _d 0*dlogtk +
     &          (118.67 _d 0*invtk - 5.977 _d 0 + 1.0495 _d 0 * dlogtk)
     &          * sqrts - 0.01615 _d 0 * s)
C------------------------------------------------------------------------
C ks = [H][SO4]/[HSO4]
C dickson (1990, J. chem. Thermodynamics 22, 113)
C On the free pH scale
C Note: converted here from mol/kg-H2O to mol/kg-SW
           aks(i,j,bi,bj)=exp(-4276.1 _d 0*invtk + 141.328 _d 0 -
     &          23.093 _d 0*dlogtk +
     &   (-13856. _d 0*invtk + 324.57 _d 0 - 47.986 _d 0*dlogtk)*sqrtis+
     &   (35474. _d 0*invtk - 771.54 _d 0 + 114.723 _d 0*dlogtk)*is -
     &          2698. _d 0*invtk*is**1.5 _d 0 + 1776. _d 0*invtk*is2 +
     &          log(1.0 _d 0 - 0.001005 _d 0*s))
C------------------------------------------------------------------------
C kf = [H][F]/[HF]
C dickson and Riley (1979) -- change pH scale to total (following Dickson & Goyet, 1994)
C Note: converted here from mol/kg-H2O to mol/kg-SW
           akf(i,j,bi,bj)=exp(1590.2 _d 0*invtk - 12.641 _d 0 +
     &   1.525 _d 0*sqrtis + log(1.0 _d 0 - 0.001005 _d 0*s) +
     &   log(1.0 _d 0 + (0.1400 _d 0/96.062 _d 0)*(scl)/aks(i,j,bi,bj)))
C------------------------------------------------------------------------
C Calculate concentrations for borate, sulfate, and fluoride
C Uppstrom (1974)
           bt(i,j,bi,bj) = 0.000232 _d 0 * scl/10.811 _d 0
C Morris & Riley (1966)
           st(i,j,bi,bj) = 0.14 _d 0 * scl/96.062 _d 0
C Riley (1965)
           ft(i,j,bi,bj) = 0.000067 _d 0 * scl/18.9984 _d 0
C------------------------------------------------------------------------
#ifdef CARBONCHEM_TOTALPHSCALE
C Convert between pH scales, we want everything to end up on the total scale
C ak1 and ak2 are on seawater scale and aks is on the free scale
           total2free = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))
           free2sw = 1. _d 0
     &                 + st(i,j,bi,bj)/ aks(i,j,bi,bj)
     &                 + ft(i,j,bi,bj)/(akf(i,j,bi,bj)*total2free)
           total2sw = total2free * free2sw
           ak1(i,j,bi,bj)  = ak1(i,j,bi,bj)/total2sw
           ak2(i,j,bi,bj)  = ak2(i,j,bi,bj)/total2sw
           aks(i,j,bi,bj)  = aks(i,j,bi,bj)/total2free
#endif
         else
c add Bennington
            fugf(i,j,bi,bj)=0. _d 0
            ff(i,j,bi,bj)=0. _d 0
            ak0(i,j,bi,bj)= 0. _d 0
            ak1(i,j,bi,bj)= 0. _d 0
            ak2(i,j,bi,bj)= 0. _d 0
            akb(i,j,bi,bj)= 0. _d 0
            ak1p(i,j,bi,bj) = 0. _d 0
            ak2p(i,j,bi,bj) = 0. _d 0
            ak3p(i,j,bi,bj) = 0. _d 0
            aksi(i,j,bi,bj) = 0. _d 0
            akw(i,j,bi,bj) = 0. _d 0
            aks(i,j,bi,bj)= 0. _d 0
            akf(i,j,bi,bj)= 0. _d 0
            bt(i,j,bi,bj) = 0. _d 0
            st(i,j,bi,bj) = 0. _d 0
            ft(i,j,bi,bj) = 0. _d 0
         endif
         end do
        end do

        RETURN
        END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      subroutine CARBON_COEFFS_PRESSURE_DEP(
     I                   ttemp,stemp,
     I                   bi,bj,iMin,iMax,jMin,jMax,
     I                   Klevel,myThid)

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CARBON_COEFFS                                 |
C     | determine coefficients for surface carbon chemistry      |
C     | adapted from OCMIP2:  SUBROUTINE CO2CALC                 |
C     | mick follows, oct 1999                                   |
C     | minor changes to tidy, swd aug 2002                      |
C     | MODIFIED FOR PRESSURE DEPENDENCE                         |
C     | Karsten Friis and Mick Follows 2004                      |
C     *==========================================================*
C INPUT
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (g/kg)
C OUTPUT
C IMPORTANT: Some words about units - (JCO, 4/4/1999)
C     - Models carry tracers in mol/m^3 (on a per volume basis)
C     - Conversely, this routine, which was written by observationalists
C       (C. Sabine and R. Key), passes input arguments in umol/kg
C       (i.e., on a per mass basis)
C     - I have changed things slightly so that input arguments are in mol/m^3,
C     - Thus, all input concentrations (diclocal, ta, pt, and st) should be
C       given in mol/m^3; output arguments "co2star" and "dco2star"
C       are likewise be in mol/m^3.
C
C NOW INCLUDES:
C PRESSURE DEPENDENCE of K1, K2, solubility product of calcite
C based on Takahashi, GEOSECS Atlantic Report, Vol. 1 (1981)
C
C--------------------------------------------------------------------------

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "BLING_VARS.h"

C     == Routine arguments ==
C ttemp and stemp are local theta and salt arrays
C dont really need to pass T and S in, could use theta, salt in
C common block in DYNVARS.h, but this way keeps subroutine more
C general
        _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        INTEGER bi,bj,iMin,iMax,jMin,jMax
C K is depth index
        INTEGER Klevel
        INTEGER myThid
CEOP

C LOCAL VARIABLES
        _RL  t
        _RL  s
        _RL  tk
        _RL  tk100
        _RL  tk1002
        _RL  dlogtk
        _RL  sqrtis
        _RL  sqrts
        _RL  s15
        _RL  scl
        _RL  s2
        _RL  invtk
        _RL  is
        _RL  is2
c add Bennington
        _RL  P1atm
        _RL  Rgas
        _RL  RT
        _RL  delta
        _RL  B1
        _RL  B
        INTEGER i
        INTEGER j
        INTEGER k
        _RL  bdepth
        _RL  cdepth
        _RL  pressc
        _RL  Ksp_T_Calc
        _RL  Ksp_T_Arag
        _RL  xvalue
        _RL  zdum
        _RL  tmpa1
        _RL  tmpa2
        _RL  tmpa3
        _RL  logKspc
        _RL  logKspa
        _RL  dv
        _RL  dk
        _RL  pfactor
        _RL  bigR
#ifdef CARBONCHEM_TOTALPHSCALE
c pH scale converstions
        _RL total2free_surf
        _RL free2sw_surf
        _RL total2sw_surf
        _RL total2free
        _RL free2sw
        _RL total2sw
#endif

C.....................................................................
C OCMIP note:
C Calculate all constants needed to convert between various measured
C carbon species. References for each equation are noted in the code.
C Once calculated, the constants are
C stored and passed in the common block "const". The original version
C of this code was based on the code by dickson in Version 2 of
C  Handbook of Methods C for the Analysis of the Various Parameters of
C the Carbon Dioxide System in Seawater , DOE, 1994 (SOP No. 3, p25-26).
C....................................................................

c determine pressure (bar) from depth
c 1 BAR at z=0m (atmos pressure)
c use UPPER surface of cell so top layer pressure = 0 bar
c for surface exchange coeffs

        bdepth = 0.0d0
        cdepth = 0.0d0
        pressc = 1.0d0
        do k = 1,Klevel
            cdepth = bdepth + 0.5d0*drF(k)
            bdepth = bdepth + drF(k)
            pressc = 1.0d0 + 0.1d0*cdepth
        end do

        bigR = 83.145 _d 0
        P1atm = 1.01325 _d 0 ! bars
        Rgas = 83.1451 _d 0 ! bar*cm3/(mol*K)

        do i=imin,imax
         do j=jmin,jmax
          if (hFacC(i,j,Klevel,bi,bj).gt.0.d0) then
           t = ttemp(i,j)
           s = stemp(i,j)
C terms used more than once for:
C temperature
           tk = 273.15 + t
           tk100 = tk/100.0
           tk1002=tk100*tk100
           invtk=1.0/tk
           dlogtk=log(tk)
C ionic strength
           is=19.924*s/(1000.-1.005*s)
           is2=is*is
           sqrtis=sqrt(is)
C salinity
           s2=s*s
           sqrts=sqrt(s)
           s15=s**1.5
           scl=s/1.80655

C-----------------------------------------------------------------------
C Ariane adding this - copied from non-pressure-dep case
C added by Val Bennington Nov 2010
C Fugacity Factor needed for non-ideality in ocean
C ff used for atmospheric correction for water vapor and pressure
C Weiss (1974) Marine Chemistry
           RT = Rgas*tk
           delta = (57.7 _d 0 - 0.118 _d 0*tk)
           B1 = -1636.75 _d 0 + 12.0408 _d 0*tk - 0.0327957 _d 0*tk*tk
           B = B1 + 3.16528 _d 0*tk*tk*tk*(0.00001 _d 0)
           fugf(i,j,bi,bj) = exp( (B+2. _d 0*delta) *
     &                            AtmosP(i,j,bi,bj) * P1atm / RT)
C------------------------------------------------------------------------
C f = k0(1-pH2O)*correction term for non-ideality
C Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)
           ff(i,j,bi,bj) = exp(-162.8301 + 218.2968/tk100  +
     &          90.9241*log(tk100) - 1.47696*tk1002 +
     &          s * (.025695 - .025225*tk100 +
     &          0.0049867*tk1002))
C------------------------------------------------------------------------
C K0 from Weiss 1974
C Independent of pH scale
           ak0(i,j,bi,bj) = exp(93.4517/tk100 - 60.2409 +
     &        23.3585 * log(tk100) +
     &        s * (0.023517 - 0.023656*tk100 +
     &        0.0047036*tk1002))
C------------------------------------------------------------------------
C k1 = [H][HCO3]/[H2CO3]
C k2 = [H][CO3]/[HCO3]
C Millero p.664 (1995) using Mehrbach et al. data on seawater scale
C Both on seawater pH scale
           ak1(i,j,bi,bj)=10.**(-1.*(3670.7*invtk -
     &          62.008 + 9.7944*dlogtk -
     &          0.0118 * s + 0.000116*s2))
           ak2(i,j,bi,bj)=10.**(-1.*(1394.7*invtk + 4.777 -
     &          0.0184*s + 0.000118*s2))
C NOW PRESSURE DEPENDENCE:
c Following Takahashi (1981) GEOSECS report - quoting Culberson and
c Pytkowicz (1968)
c pressc = pressure in bars
           ak1(i,j,bi,bj) = ak1(i,j,bi,bj)*
     &             exp( (24.2-0.085*t)*(pressc-1.0)/(83.143*tk) )
c FIRST GO FOR K2: According to GEOSECS (1982) report
c          ak2(i,j,bi,bj) = ak2(i,j,bi,bj)*
c    &             exp( (26.4-0.040*t)*(pressc-1.0)/(83.143*tk) )
c SECOND GO FOR K2: corrected coeff according to CO2sys documentation
c                   E. Lewis and D. Wallace (1998) ORNL/CDIAC-105
           ak2(i,j,bi,bj) = ak2(i,j,bi,bj)*
     &             exp( (16.4-0.040*t)*(pressc-1.0)/(83.143*tk) )
C------------------------------------------------------------------------
C kb = [H][BO2]/[HBO2]
C Millero p.669 (1995) using data from dickson (1990)
C On total pH scale
           akb(i,j,bi,bj)=exp((-8966.90 - 2890.53*sqrts - 77.942*s +
     &          1.728*s15 - 0.0996*s2)*invtk +
     &          (148.0248 + 137.1942*sqrts + 1.62142*s) +
     &          (-24.4344 - 25.085*sqrts - 0.2474*s) *
     &          dlogtk + 0.053105*sqrts*tk)
#ifdef CARBONCHEM_TOTALPHSCALE
C JML Not yet, need to account for pH scale conversions
C    (pressure correct on the seawater scale, not the total scale)
C Mick and Karsten - Dec 04
C ADDING pressure dependence based on Millero (1995), p675
C with additional info from CO2sys documentation (E. Lewis and
C D. Wallace, 1998 - see endnotes for commentary on Millero, 95)
C           dv = -29.48 + 0.1622*t + 2.608d-3*t*t
C           dk = -2.84d-3
C           pfactor = - (dv/(bigR*tk))*pressc
C     &               + (0.5*dk/(bigR*tk))*pressc*pressc
C           akb(i,j,bi,bj) = akb(i,j,bi,bj)*exp(pfactor)
#else
C Mick and Karsten - Dec 04
C ADDING pressure dependence based on Millero (1995), p675
C with additional info from CO2sys documentation (E. Lewis and
C D. Wallace, 1998 - see endnotes for commentary on Millero, 95)
           dv = -29.48 _d 0 + 0.1622 _d 0*t + 2.608 _d -3*t*t
           dk = -2.84 _d -3
           pfactor = - (dv/(bigR*tk))*pressc
     &               + (0.5 _d 0*dk/(bigR*tk))*pressc*pressc
           akb(i,j,bi,bj) = akb(i,j,bi,bj)*exp(pfactor)
#endif
C------------------------------------------------------------------------
C k1p = [H][H2PO4]/[H3PO4]
C DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
C The constant 115.525 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 115.540 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak1p(i,j,bi,bj) = exp(-4576.752*invtk + 115.525 -
     &          18.453*dlogtk +
     &          (-106.736*invtk + 0.69171)*sqrts +
     &          (-0.65643*invtk - 0.01844)*s)
C------------------------------------------------------------------------
C k2p = [H][HPO4]/[H2PO4]
C DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
C The constant 172.0833 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 172.1033 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak2p(i,j,bi,bj) = exp(-8814.715*invtk + 172.0883 -
     &          27.927*dlogtk +
     &          (-160.340*invtk + 1.3566) * sqrts +
     &          (0.37335*invtk - 0.05778) * s)
C------------------------------------------------------------------------
C k3p = [H][PO4]/[HPO4]
C DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
C The constant 18.141 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 18.126 constant to stay on the seawater
C  pH scale (everything else is the same).
           ak3p(i,j,bi,bj) = exp(-3070.75*invtk - 18.141 +
     &          (17.27039*invtk + 2.81197) *
     &          sqrts + (-44.99486*invtk - 0.09984) * s)
C------------------------------------------------------------------------
C ksi = [H][SiO(OH)3]/[Si(OH)4]
C Millero p.671 (1995) using data from Yao and Millero (1995)
C The constant 117.385 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 117.400 constant to stay on the seawater
C  pH scale (everything else is the same).
C Note: converted here from mol/kg-H2O to mol/kg-SW
           aksi(i,j,bi,bj) = exp(-8904.2*invtk + 117.385 -
     &          19.334*dlogtk +
     &          (-458.79*invtk + 3.5913) * sqrtis +
     &          (188.74*invtk - 1.5998) * is +
     &          (-12.1652*invtk + 0.07871) * is2 +
     &          log(1.0-0.001005*s))
C------------------------------------------------------------------------
C kw = [H][OH]
C Millero p.670 (1995) using composite data
C The constant 148.9652 is an approximation to convert to the total pH scale
C  (Dickson et al., 2007). Use Milleros 148.9802 constant to stay on the seawater
C  pH scale (everything else is the same).
           akw(i,j,bi,bj) = exp(-13847.26*invtk + 148.9652 -
     &          23.6521*dlogtk +
     &          (118.67*invtk - 5.977 + 1.0495 * dlogtk) *
     &          sqrts - 0.01615 * s)
C------------------------------------------------------------------------
C ks = [H][SO4]/[HSO4]
C dickson (1990, J. chem. Thermodynamics 22, 113)
C On the free pH scale
C Note: converted here from mol/kg-H2O to mol/kg-SW
           aks(i,j,bi,bj)=exp(-4276.1*invtk + 141.328 -
     &          23.093*dlogtk +
     &          (-13856*invtk + 324.57 - 47.986*dlogtk)*sqrtis +
     &          (35474*invtk - 771.54 + 114.723*dlogtk)*is -
     &          2698*invtk*is**1.5 + 1776*invtk*is2 +
     &          log(1.0 - 0.001005*s))
C------------------------------------------------------------------------
C kf = [H][F]/[HF]
C dickson and Riley (1979) -- change pH scale to total (following Dickson & Goyet, 1994)
C Note: converted here from mol/kg-H2O to mol/kg-SW
           akf(i,j,bi,bj)=exp(1590.2*invtk - 12.641 + 1.525*sqrtis +
     &          log(1.0 - 0.001005*s) +
     &          log(1.0 + (0.1400/96.062)*(scl)/aks(i,j,bi,bj)))
C------------------------------------------------------------------------
C Calculate concentrations for borate, sulfate, and fluoride
C Uppstrom (1974)
           bt(i,j,bi,bj) = 0.000232 * scl/10.811
C Morris & Riley (1966)
           st(i,j,bi,bj) = 0.14 * scl/96.062
C Riley (1965)
           ft(i,j,bi,bj) = 0.000067 * scl/18.9984
C------------------------------------------------------------------------

#ifdef CARBONCHEM_TOTALPHSCALE
C JML Convert between pH scales, we want everything to end up on the total scale
C ak1 and ak2 are on seawater scale and are pressure corrected, so they just need converting.
C aks is on the free scale, it needs pressure correcting then converting to the total scale.
C akf needs pressure correcting too, but is on the right (total) pH scale.
C akb, ak1p, ak2p, ak3p, aksi and akw are on the total scale. Convert to the seawater
C  scale before pressure correction, and then convert back to the total scale.

C All the terms for pressure correction are from Table 9, Millero (1995), p675
C Info about pH conversions from Munhoven (2013) and Orr and Epitalon (2015)
           total2free_surf = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

           free2sw_surf = 1. _d 0
     &                 + st(i,j,bi,bj)/ aks(i,j,bi,bj)
     &                 + ft(i,j,bi,bj)/(akf(i,j,bi,bj)*total2free_surf)

           total2sw_surf = total2free_surf * free2sw_surf

C------------------------------------------------------------------------
C Pressure correct aks on native free pH scale
           dv=-18.03 _d 0 + 0.0466 _d 0*t + 0.316 _d -3 *t*t
           dk=-4.53 _d -3 + 0.09 _d -3 *t
           pfactor = -(dv/(bigR*tk))*pressc
     &            +(0.5*dk/(bigR*tk))*pressc*pressc
           aks(i,j,bi,bj) = aks(i,j,bi,bj)*exp(pfactor)

           total2free = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))
C free2sw has an additional component from fluoride
           free2sw    = 1. _d 0
     &                 + st(i,j,bi,bj)/ aks(i,j,bi,bj)

           aks(i,j,bi,bj) = aks(i,j,bi,bj)/total2free

C------------------------------------------------------------------------
C Pressure correct akf on the free scale before converting back to total
           akf(i,j,bi,bj) = akf(i,j,bi,bj) * total2free_surf
           dv  =  -9.78 _d 0 -0.0090 _d 0 *t -0.942 _d -3 *t*t
           dk  =  -3.91 _d -3 + 0.054 _d -3 *t
           pfactor = -(dv/(bigR*tk))*pressc
     &               +(0.5*dk/(bigR*tk))*pressc*pressc
           akf(i,j,bi,bj) = akf(i,j,bi,bj) * exp(pfactor)
           akf(i,j,bi,bj) = akf(i,j,bi,bj)/total2free

           free2sw = free2sw
     &               + ft(i,j,bi,bj)/(akf(i,j,bi,bj)*total2free)
           total2sw = total2free * free2sw

C------------------------------------------------------------------------
C Convert pressure corrected ak1 and ak2 from the seawater pH scale to the total scale
           ak1(i,j,bi,bj)  = ak1(i,j,bi,bj)/total2sw
           ak2(i,j,bi,bj)  = ak1(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct akb on the seawater scale before converting back to total scale
            dv = -29.48 _d 0 + 0.1622 _d 0*t + 2.608 _d -3*t*t
            dk = -2.84 _d -3
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            akb(i,j,bi,bj) = total2sw_surf*akb(i,j,bi,bj)*exp(pfactor)
            akb(i,j,bi,bj) = akb(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct ak1p on the seawater scale before converting back to total scale
            dv = -14.51 _d 0 + 0.1211 _d 0*t -0.321 _d -3*t*t
            dk = -2.67 _d -3 + 0.0427 _d -3*t
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            ak1p(i,j,bi,bj) = total2sw_surf*ak1p(i,j,bi,bj)*exp(pfactor)
            ak1p(i,j,bi,bj) = ak1p(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct ak2p on the seawater scale before converting back to total scale
            dv = -23.12 _d 0 + 0.1758 _d 0*t -2.647 _d -3*t*t
            dk = -5.15 _d -3 + 0.09 _d -3*t
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            ak2p(i,j,bi,bj) = total2sw_surf*ak2p(i,j,bi,bj)*exp(pfactor)
            ak2p(i,j,bi,bj) = ak2p(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct ak3p on the seawater scale before converting back to total scale
            dv = -26.57 _d 0 + 0.2020 _d 0*t -3.042 _d -3*t*t
            dk = -4.08 _d -3 + 0.0714 _d -3*t
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            ak3p(i,j,bi,bj) = total2sw_surf*ak3p(i,j,bi,bj)*exp(pfactor)
            ak3p(i,j,bi,bj) = ak3p(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct akw on the seawater scale before converting back to total scale
            dv = -20.02 _d 0 + 0.1119 _d 0*t -1.409 _d -3*t*t
            dk = -5.13 _d -3 + 0.0794 _d -3 *t
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            akw(i,j,bi,bj) = total2sw_surf*akw(i,j,bi,bj)*exp(pfactor)
            akw(i,j,bi,bj) = akw(i,j,bi,bj)/total2sw

C------------------------------------------------------------------------
C Pressure correct aksi on the seawater scale before converting back to total scale
C Estimated from the effect on akb
            dv = -29.48 _d 0 + 0.1622 _d 0*t + 2.608 _d -3*t*t
            dk = -2.84 _d -3
            pfactor = - (dv/(bigR*tk))*pressc
     &                + (0.5*dk/(bigR*tk))*pressc*pressc
            aksi(i,j,bi,bj) = total2sw_surf*aksi(i,j,bi,bj)*exp(pfactor)
            aksi(i,j,bi,bj) = aksi(i,j,bi,bj)/total2sw
#endif

C------------------------------------------------------------------------
C solubility product for calcite
cav                  and aragonite
C
c Following Takahashi (1982) GEOSECS handbook
C NOT SURE THIS IS WORKING???
C Ingle et al. (1973)
c          Ksp_T_Calc = ( -34.452 - 39.866*(s**0.333333)
c    &                  + 110.21*log(s) - 7.5752d-6 * (tk**2.0)
c    &                  ) * 1.0d-7
c with pressure dependence Culberson and Pytkowicz (1968)
c          xvalue  =  (36-0.20*t)*(pressc-1.0)/(83.143*tk)
c          Ksp_TP_Calc(i,j,bi,bj) = Ksp_T_Calc*exp(xvalue)
c
c
C Following Mucci (1983) - from Zeebe/Wolf-Gladrow equic.m
         tmpa1 = - 171.9065 - (0.077993*tk) + (2839.319/tk)
     &            + (71.595*log10(tk))
         tmpa2 = +(-0.77712 + (0.0028426*tk) + (178.34/tk) )*sqrts
         tmpa3 = -(0.07711*s) + (0.0041249*s15)
         logKspc = tmpa1 + tmpa2 + tmpa3
         Ksp_T_Calc = 10.0**logKspc
C
         tmpa1 = - 171.945 - (0.077993*tk) + (2903.293/tk)
     &            + (71.595*log10(tk))
         tmpa2 = +(-0.068393 + (0.0017276*tk) + (88.135/tk) )*sqrts
         tmpa3 = -(0.10018*s) + (0.0059415*s15)
         logKspa = tmpa1 + tmpa2 + tmpa3
         Ksp_T_Arag = 10.0**logKspa

c with pressure dependence Culberson and Pytkowicz (1968)
c        xvalue  =  (36.0-0.20*t)*(pressc-1.0)/(83.143*tk)
c        Ksp_TP_Calc(i,j,bi,bj) = Ksp_T_Calc*exp(xvalue)

c alternative pressure depdendence
c following Millero (1995) but using info from Appendix A11 of
c Zeebe and Wolf-Gladrow (2001) book
c          dv = -48.6 - 0.5304*t
c          dk = -11.76d-3 - 0.3692*t
c          xvalue = - (dv/(bigR*tk))*pressc
c    &               + (0.5*dk/(bigR*tk))*pressc*pressc
c          Ksp_TP_Calc(i,j,bi,bj) = Ksp_T_Calc*exp(xvalue)

c alternative pressure dependence from Ingle (1975)

           zdum   = (pressc*10.0d0 - 10.0d0)/10.0d0
           xvalue = ( (48.8d0 - 0.53d0*t)*zdum
     &                 + (-0.00588d0 + 0.0001845d0*t)*zdum*zdum)
     &            / (188.93d0*(t + 273.15d0))

           Ksp_TP_Calc(i,j,bi,bj) = Ksp_T_Calc*10.**(xvalue)
           Ksp_TP_Arag(i,j,bi,bj) = Ksp_T_Arag*10.**(xvalue)

C------------------------------------------------------------------------
         else
            fugf(i,j,bi,bj) = 0.d0
            ff(i,j,bi,bj) = 0.d0
            ak0(i,j,bi,bj)= 0.d0
            ak1(i,j,bi,bj)= 0.d0
            ak2(i,j,bi,bj)= 0.d0
            akb(i,j,bi,bj)= 0.d0
            ak1p(i,j,bi,bj) = 0.d0
            ak2p(i,j,bi,bj) = 0.d0
            ak3p(i,j,bi,bj) = 0.d0
            aksi(i,j,bi,bj) = 0.d0
            akw(i,j,bi,bj) = 0.d0
            aks(i,j,bi,bj)= 0.d0
            akf(i,j,bi,bj)= 0.d0
            bt(i,j,bi,bj) = 0.d0
            st(i,j,bi,bj) = 0.d0
            ft(i,j,bi,bj) = 0.d0
            Ksp_TP_Calc(i,j,bi,bj) = 0.d0
            Ksp_TP_Arag(i,j,bi,bj) = 0.d0
         endif
         end do
        end do

        return
        end
