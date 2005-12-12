C $Header: /u/gcmpack/MITgcm/pkg/dic/carbon_chem.F,v 1.6 2005/12/12 19:07:36 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: CALC_PCO2

C !INTERFACE: ==========================================================
       SUBROUTINE CALC_PCO2(
     I                       donewt,inewtonmax,ibrackmax,
     I                       t,s,diclocal,pt,sit,ta,
     I                       k1local,k2local,
     I                       k1plocal,k2plocal,k3plocal,
     I                       kslocal,kblocal,kwlocal,
     I                       ksilocal,kflocal,
     I                       fflocal,btlocal,stlocal,ftlocal,
     U                       pHlocal,pCO2surfloc)

C !DESCRIPTION:
C  surface ocean inorganic carbon chemistry to OCMIP2       
C  regulations modified from OCMIP2 code;                  
C  Mick Follows, MIT, Oct 1999.                           


C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
C       diclocal = total inorganic carbon (mol/m^3) 
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3) 
C       pt  = inorganic phosphate (mol/^3) 
C       sit = inorganic silicate (mol/^3) 
C       t   = temperature (degrees C)
C       s   = salinity (PSU)
        INTEGER donewt
        INTEGER inewtonmax
        INTEGER ibrackmax
        _RL  t, s, pt, sit, ta
        _RL  pCO2surfloc, diclocal, pHlocal
        _RL  fflocal, btlocal, stlocal, ftlocal
        _RL  k1local, k2local
        _RL  k1plocal, k2plocal, k3plocal
        _RL  kslocal, kblocal, kwlocal, ksilocal, kflocal
CEndOfInterface

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
        _RL  tk
        _RL  tk100
        _RL  tk1002
        _RL  dlogtk
        _RL  sqrtis
        _RL  sqrts
        _RL  s15
        _RL  scl
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
        _RL  x1
        _RL  x2
        _RL  x3
        _RL  xmid
        _RL  ftest
        _RL  htotal
        _RL  htotal2
        _RL  s2
        _RL  xacc
        _RL  co2star 
        _RL  co2starair
        _RL  dco2star
        _RL  dpCO2
        _RL  phguess
        _RL  atmpres
        INTEGER inewton
        INTEGER ibrack
        INTEGER hstep
        _RL  fni(3)
        _RL  xlo
        _RL  xhi
        _RL  xguess
        _RL  invtk
        _RL  is
        _RL  is2
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
c where the ocean's mean surface density is 1024.5 kg/m^3
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
          do 100 inewton = 1, inewtonmax
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

c Evaluate f([H+]) and f'([H+])
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

 100      end do
c end of newton-raphson method
c....................................................
        else 
c....................................................
C BRACKET AND BISECTION METHOD
c....................................................
c (ii) If first step use Bracket and Bisection method
c      with fixed, large number of iterations
          do 200 ibrack = 1, ibrackmax
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
 200      end do
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
        pCO2surfloc = co2star / fflocal

C ----------------------------------------------------------------
C Reconvert units back to original values for input arguments
C no longer necessary????
C ----------------------------------------------------------------
c       Reconvert from mol/kg -> mol/m^3
        pt=pt/permil
        sit=sit/permil
        ta=ta/permil
        diclocal=diclocal/permil

        return
        end

c=================================================================
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  New efficient pCO2 solver, Mick Follows         CC
CC                             Taka Ito             CC
CC                             Stephanie Dutkiewicz CC
CC  20 April 2003                                   CC
CC  ADD CO3 ESTIMATION AND PASS OUT                 CC
CC  Karsten Friis, Mick Follows                     CC
CC  1 sep 04                                        CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#include "DIC_OPTIONS.h"
CStartOfInterFace
       SUBROUTINE CALC_PCO2_APPROX_CO3(
     I                       t,s,diclocal,pt,sit,ta,
     I                       k1local,k2local,
     I                       k1plocal,k2plocal,k3plocal,
     I                       kslocal,kblocal,kwlocal,
     I                       ksilocal,kflocal,
     I                       fflocal,btlocal,stlocal,ftlocal,
     U                       pHlocal,pCO2surfloc,
     U                       co3local)
C     /==========================================================\
C     | SUBROUTINE CALC_PCO2_APPROX_CO3                          |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (PSU)
        _RL  t, s, pt, sit, ta
        _RL  pCO2surfloc, diclocal, pHlocal
        _RL  fflocal, btlocal, stlocal, ftlocal
        _RL  k1local, k2local
        _RL  k1plocal, k2plocal, k3plocal
        _RL  kslocal, kblocal, kwlocal, ksilocal, kflocal
CEndOfInterface

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
c carbonate
        _RL  co3local


c ---------------------------------------------------------------------
C Change units from the input of mol/m^3 -> mol/kg:
c (1 mol/m^3)  x (1 m^3/1024.5 kg)
c where the ocean's mean surface density is 1024.5 kg/m^3
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

cmick - now estimate carbonate alkalinity
        cag = ta - bohg - (kwlocal/hguess) + hguess
     &           - hpo4g - 2.0*po4g + h3po4g
     &           - siooh3g

cmick - now evaluate better guess of hydrogen ion conc
cmick   htotal = [H+], hydrogen ion conc
        gamm  = diclocal/cag
        stuff = (1.0-gamm)*(1.0-gamm)*k1local*k1local
     &          - 4.0*k1local*k2local*(1.0-2.0*gamm)
        hnew  = 0.5*( (gamm-1.0)*k1local + sqrt(stuff) )
cmick - now determine [CO2*]
        co2s  = diclocal/
     &   (1.0 + (k1local/hnew) + (k1local*k2local/(hnew*hnew)))
cmick - return update pH to main routine
        phlocal = -log10(hnew)

c NOW EVALUATE CO32-, carbonate ion concentration
c used in determination of calcite compensation depth
c Karsten Friis & Mick - Sep 2004
        co3local = k1local*k2local*diclocal / 
     &         (hnew*hnew + k1local*hnew + k1local*k2local)

c ---------------------------------------------------------------
c surface pCO2 (following Dickson and Goyet, DOE...)
        pCO2surfloc = co2s/fflocal

C ----------------------------------------------------------------
c Reconvert from mol/kg -> mol/m^3
        pt=pt/permil
        sit=sit/permil
        ta=ta/permil
        diclocal=diclocal/permil
        return
        end

c=================================================================
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  New efficient pCO2 solver, Mick Follows         CC
CC                             Taka Ito             CC
CC                             Stephanie Dutkiewicz CC
CC  20 April 2003                                   CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
#include "DIC_OPTIONS.h"
CStartOfInterFace
       SUBROUTINE CALC_PCO2_APPROX(
     I                       t,s,diclocal,pt,sit,ta,
     I                       k1local,k2local,
     I                       k1plocal,k2plocal,k3plocal,
     I                       kslocal,kblocal,kwlocal,
     I                       ksilocal,kflocal,
     I                       fflocal,btlocal,stlocal,ftlocal,
     U                       pHlocal,pCO2surfloc)
C     /==========================================================\
C     | SUBROUTINE CALC_PCO2_APPROX                              |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"

C     == Routine arguments ==
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (PSU)
        _RL  t, s, pt, sit, ta
        _RL  pCO2surfloc, diclocal, pHlocal
        _RL  fflocal, btlocal, stlocal, ftlocal
        _RL  k1local, k2local
        _RL  k1plocal, k2plocal, k3plocal
        _RL  kslocal, kblocal, kwlocal, ksilocal, kflocal
CEndOfInterface

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
c carbonate
        _RL  co3local


c ---------------------------------------------------------------------
C Change units from the input of mol/m^3 -> mol/kg:
c (1 mol/m^3)  x (1 m^3/1024.5 kg)
c where the ocean's mean surface density is 1024.5 kg/m^3
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

cmick - now estimate carbonate alkalinity
        cag = ta - bohg - (kwlocal/hguess) + hguess
     &           - hpo4g - 2.0*po4g + h3po4g
     &           - siooh3g

cmick - now evaluate better guess of hydrogen ion conc
cmick   htotal = [H+], hydrogen ion conc
        gamm  = diclocal/cag
        stuff = (1.0-gamm)*(1.0-gamm)*k1local*k1local
     &          - 4.0*k1local*k2local*(1.0-2.0*gamm)
        hnew  = 0.5*( (gamm-1.0)*k1local + sqrt(stuff) )
cmick - now determine [CO2*]
        co2s  = diclocal/
     &   (1.0 + (k1local/hnew) + (k1local*k2local/(hnew*hnew)))
cmick - return update pH to main routine
        phlocal = -log10(hnew)

c NOW EVALUATE CO32-, carbonate ion concentration
c used in determination of calcite compensation depth
c Karsten Friis & Mick - Sep 2004
c       co3local = k1local*k2local*diclocal / 
c    &         (hnew*hnew + k1local*hnew + k1local*k2local)

c ---------------------------------------------------------------
c surface pCO2 (following Dickson and Goyet, DOE...)
        pCO2surfloc = co2s/fflocal

C ----------------------------------------------------------------
c Reconvert from mol/kg -> mol/m^3
        pt=pt/permil
        sit=sit/permil
        ta=ta/permil
        diclocal=diclocal/permil
        return
        end

c=================================================================
c *******************************************************************
c=================================================================
CStartOfInterFace
      SUBROUTINE CARBON_COEFFS(
     I                   ttemp,stemp,
     I                   bi,bj,iMin,iMax,jMin,jMax)
C
C     /==========================================================\
C     | SUBROUTINE CARBON_COEFFS                                 |
C     | determine coefficients for surface carbon chemistry      |
C     | adapted from OCMIP2:  SUBROUTINE CO2CALC                 |
C     | mick follows, oct 1999                                   |
c     | minor changes to tidy, swd aug 2002                      |
C     \==========================================================/
C INPUT
C       diclocal = total inorganic carbon (mol/m^3) 
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3) 
C       pt  = inorganic phosphate (mol/^3) 
C       sit = inorganic silicate (mol/^3) 
C       t   = temperature (degrees C)
C       s   = salinity (PSU)
C OUTPUT
C IMPORTANT: Some words about units - (JCO, 4/4/1999)
c     - Models carry tracers in mol/m^3 (on a per volume basis)
c     - Conversely, this routine, which was written by observationalists 
c       (C. Sabine and R. Key), passes input arguments in umol/kg  
c       (i.e., on a per mass basis)
c     - I have changed things slightly so that input arguments are in mol/m^3,
c     - Thus, all input concentrations (diclocal, ta, pt, and st) should be 
c       given in mol/m^3; output arguments "co2star" and "dco2star"  
c       are likewise be in mol/m^3.
C--------------------------------------------------------------------------
        IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"
C     == Routine arguments ==
C ttemp and stemp are local theta and salt arrays
C dont really need to pass T and S in, could use theta, salt in
C common block in DYNVARS.h, but this way keeps subroutine more
C general
        _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
        _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
        INTEGER bi,bj,iMin,iMax,jMin,jMax
CEndOfInterface


C LOCAL VARIABLES
        _RL  t
        _RL  s
        _RL  ta
        _RL  pt
        _RL  sit
        _RL  tk
        _RL  tk100
        _RL  tk1002
        _RL  dlogtk
        _RL  sqrtis
        _RL  sqrts
        _RL  s15
        _RL  scl
        _RL  x1
        _RL  x2
        _RL  s2
        _RL  xacc
        _RL  invtk
        _RL  is
        _RL  is2
        INTEGER i
        INTEGER j

C.....................................................................
C OCMIP note:
C Calculate all constants needed to convert between various measured
C carbon species. References for each equation are noted in the code.
C Once calculated, the constants are
C stored and passed in the common block "const". The original version 
C of this code was based on the code by dickson in Version 2 of 
C "Handbook of Methods C for the Analysis of the Various Parameters of 
C the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3, p25-26). 
C....................................................................

        do i=1-OLx,sNx+OLx
         do j=1-OLy,sNy+OLy
          if (hFacC(i,j,1,bi,bj).gt.0.d0) then
           t = ttemp(i,j,1,bi,bj)
           s = stemp(i,j,1,bi,bj)
C terms used more than once
           tk = 273.15 + t
           tk100 = tk/100.0
           tk1002=tk100*tk100
           invtk=1.0/tk
           dlogtk=log(tk)
           is=19.924*s/(1000.-1.005*s)
           is2=is*is
           sqrtis=sqrt(is)
           s2=s*s
           sqrts=sqrt(s)
           s15=s**1.5
           scl=s/1.80655
C------------------------------------------------------------------------
C f = k0(1-pH2O)*correction term for non-ideality
C Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)
           ff(i,j,bi,bj) = exp(-162.8301 + 218.2968/tk100  +
     &          90.9241*log(tk100) - 1.47696*tk1002 +
     &          s * (.025695 - .025225*tk100 + 
     &          0.0049867*tk1002))
C------------------------------------------------------------------------
C K0 from Weiss 1974
           ak0(i,j,bi,bj) = exp(93.4517/tk100 - 60.2409 + 
     &        23.3585 * log(tk100) +
     &        s * (0.023517 - 0.023656*tk100 + 
     &        0.0047036*tk1002))
C------------------------------------------------------------------------
C k1 = [H][HCO3]/[H2CO3]
C k2 = [H][CO3]/[HCO3]
C Millero p.664 (1995) using Mehrbach et al. data on seawater scale 
           ak1(i,j,bi,bj)=10**(-1*(3670.7*invtk - 
     &          62.008 + 9.7944*dlogtk -
     &          0.0118 * s + 0.000116*s2))
           ak2(i,j,bi,bj)=10**(-1*(1394.7*invtk + 4.777 - 
     &          0.0184*s + 0.000118*s2))
C------------------------------------------------------------------------
C kb = [H][BO2]/[HBO2]
C Millero p.669 (1995) using data from dickson (1990)
           akb(i,j,bi,bj)=exp((-8966.90 - 2890.53*sqrts - 77.942*s +
     &          1.728*s15 - 0.0996*s2)*invtk +
     &          (148.0248 + 137.1942*sqrts + 1.62142*s) +
     &          (-24.4344 - 25.085*sqrts - 0.2474*s) *
     &          dlogtk + 0.053105*sqrts*tk)
C------------------------------------------------------------------------
C k1p = [H][H2PO4]/[H3PO4]
C DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
           ak1p(i,j,bi,bj) = exp(-4576.752*invtk + 115.525 - 
     &          18.453*dlogtk +
     &          (-106.736*invtk + 0.69171)*sqrts +
     &          (-0.65643*invtk - 0.01844)*s)
C------------------------------------------------------------------------
C k2p = [H][HPO4]/[H2PO4]
C DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
           ak2p(i,j,bi,bj) = exp(-8814.715*invtk + 172.0883 - 
     &          27.927*dlogtk +
     &          (-160.340*invtk + 1.3566) * sqrts +
     &          (0.37335*invtk - 0.05778) * s)
C------------------------------------------------------------------------
C k3p = [H][PO4]/[HPO4]
C DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
           ak3p(i,j,bi,bj) = exp(-3070.75*invtk - 18.141 + 
     &          (17.27039*invtk + 2.81197) *
     &          sqrts + (-44.99486*invtk - 0.09984) * s)
C------------------------------------------------------------------------
C ksi = [H][SiO(OH)3]/[Si(OH)4]
C Millero p.671 (1995) using data from Yao and Millero (1995)
           aksi(i,j,bi,bj) = exp(-8904.2*invtk + 117.385 - 
     &          19.334*dlogtk +
     &          (-458.79*invtk + 3.5913) * sqrtis +
     &          (188.74*invtk - 1.5998) * is +
     &          (-12.1652*invtk + 0.07871) * is2 +
     &          log(1.0-0.001005*s))
C------------------------------------------------------------------------
C kw = [H][OH]
C Millero p.670 (1995) using composite data
           akw(i,j,bi,bj) = exp(-13847.26*invtk + 148.9652 - 
     &          23.6521*dlogtk + 
     &          (118.67*invtk - 5.977 + 1.0495 * dlogtk) *
     &          sqrts - 0.01615 * s)
C------------------------------------------------------------------------
C ks = [H][SO4]/[HSO4]
C dickson (1990, J. chem. Thermodynamics 22, 113)
           aks(i,j,bi,bj)=exp(-4276.1*invtk + 141.328 - 
     &          23.093*dlogtk +
     &          (-13856*invtk + 324.57 - 47.986*dlogtk)*sqrtis +
     &          (35474*invtk - 771.54 + 114.723*dlogtk)*is -
     &          2698*invtk*is**1.5 + 1776*invtk*is2 +
     &          log(1.0 - 0.001005*s))
C------------------------------------------------------------------------
C kf = [H][F]/[HF]
C dickson and Riley (1979) -- change pH scale to total
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
         else
            ff(i,j,bi,bj)=0.d0
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
         endif
         end do
        end do

        return
        end

c=================================================================
c *******************************************************************
c=================================================================
CStartOfInterFace
      SUBROUTINE CARBON_COEFFS_PRESSURE_DEP(
     I                   ttemp,stemp,
     I                   bi,bj,iMin,iMax,jMin,jMax,
     I                   Klevel)
C
C     /==========================================================\
C     | SUBROUTINE CARBON_COEFFS                                 |
C     | determine coefficients for surface carbon chemistry      |
C     | adapted from OCMIP2:  SUBROUTINE CO2CALC                 |
C     | mick follows, oct 1999                                   |
c     | minor changes to tidy, swd aug 2002                      |
c     | MODIFIED FOR PRESSURE DEPENDENCE                         |
c     | Karsten Friis and Mick Follows 2004                      |
C     \==========================================================/
C INPUT
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (PSU)
C OUTPUT
C IMPORTANT: Some words about units - (JCO, 4/4/1999)
c     - Models carry tracers in mol/m^3 (on a per volume basis)
c     - Conversely, this routine, which was written by observationalists
c       (C. Sabine and R. Key), passes input arguments in umol/kg
c       (i.e., on a per mass basis)
c     - I have changed things slightly so that input arguments are in mol/m^3,
c     - Thus, all input concentrations (diclocal, ta, pt, and st) should be
c       given in mol/m^3; output arguments "co2star" and "dco2star"
c       are likewise be in mol/m^3.
c
c
c NOW INCLUDES:
c PRESSURE DEPENDENCE of K1, K2, solubility product of calcite
c based on Takahashi, GEOSECS Atlantic Report, Vol. 1 (1981)
c
C--------------------------------------------------------------------------
        IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_ABIOTIC.h"
C     == Routine arguments ==
C ttemp and stemp are local theta and salt arrays
C dont really need to pass T and S in, could use theta, salt in
C common block in DYNVARS.h, but this way keeps subroutine more
C general
        _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
        _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
        INTEGER bi,bj,iMin,iMax,jMin,jMax
c K is depth index
        INTEGER Klevel
CEndOfInterface



C LOCAL VARIABLES
        _RL  t
        _RL  s
        _RL  ta
        _RL  pt
        _RL  sit
        _RL  tk
        _RL  tk100
        _RL  tk1002
        _RL  dlogtk
        _RL  sqrtis
        _RL  sqrts
        _RL  s15
        _RL  scl
        _RL  x1
        _RL  x2
        _RL  s2
        _RL  xacc
        _RL  invtk
        _RL  is
        _RL  is2
        INTEGER i
        INTEGER j
        INTEGER k
        _RL  bdepth
        _RL  cdepth
        _RL  pressc
        _RL  Ksp_T_Calc
        _RL  xvalue
        _RL  zdum
        _RL  tmpa1
        _RL  tmpa2
        _RL  tmpa3
        _RL  logKspc
        _RL  dv
        _RL  dk
        _RL  pfactor
        _RL  bigR

C.....................................................................
C OCMIP note:
C Calculate all constants needed to convert between various measured
C carbon species. References for each equation are noted in the code.
C Once calculated, the constants are
C stored and passed in the common block "const". The original version
C of this code was based on the code by dickson in Version 2 of
C "Handbook of Methods C for the Analysis of the Various Parameters of
C the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3, p25-26).
C....................................................................

c determine pressure (bar) from depth
c 1 BAR at z=0m (atmos pressure)
c use UPPER surface of cell so top layer pressure = 0 bar
c for surface exchange coeffs

cmick..............................
c        write(6,*)'Klevel ',klevel

        bdepth = 0.0d0
        cdepth = 0.0d0
        pressc = 1.0d0
        do k = 1,Klevel
            cdepth = bdepth + 0.5d0*drF(k)
            bdepth = bdepth + drF(k)
            pressc = 1.0d0 + 0.1d0*cdepth
        end do
cmick...................................................
c        write(6,*)'depth,pressc ',cdepth,pressc
cmick....................................................



        do i=1-OLx,sNx+OLx
         do j=1-OLy,sNy+OLy
          if (hFacC(i,j,Klevel,bi,bj).gt.0.d0) then
           t = ttemp(i,j,Klevel,bi,bj)
           s = stemp(i,j,Klevel,bi,bj)
C terms used more than once
           tk = 273.15 + t
           tk100 = tk/100.0
           tk1002=tk100*tk100
           invtk=1.0/tk
           dlogtk=log(tk)
           is=19.924*s/(1000.-1.005*s)
           is2=is*is
           sqrtis=sqrt(is)
           s2=s*s
           sqrts=sqrt(s)
           s15=s**1.5
           scl=s/1.80655

C------------------------------------------------------------------------
C f = k0(1-pH2O)*correction term for non-ideality
C Weiss & Price (1980, Mar. Chem., 8, 347-359; Eq 13 with table 6 values)
           ff(i,j,bi,bj) = exp(-162.8301 + 218.2968/tk100  +
     &          90.9241*log(tk100) - 1.47696*tk1002 +
     &          s * (.025695 - .025225*tk100 +
     &          0.0049867*tk1002))
C------------------------------------------------------------------------
C K0 from Weiss 1974
           ak0(i,j,bi,bj) = exp(93.4517/tk100 - 60.2409 + 
     &        23.3585 * log(tk100) +
     &        s * (0.023517 - 0.023656*tk100 + 
     &        0.0047036*tk1002))
C------------------------------------------------------------------------
C k1 = [H][HCO3]/[H2CO3]
C k2 = [H][CO3]/[HCO3]
C Millero p.664 (1995) using Mehrbach et al. data on seawater scale 
           ak1(i,j,bi,bj)=10**(-1*(3670.7*invtk - 
     &          62.008 + 9.7944*dlogtk -
     &          0.0118 * s + 0.000116*s2))
           ak2(i,j,bi,bj)=10**(-1*(1394.7*invtk + 4.777 - 
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
           akb(i,j,bi,bj)=exp((-8966.90 - 2890.53*sqrts - 77.942*s +
     &          1.728*s15 - 0.0996*s2)*invtk +
     &          (148.0248 + 137.1942*sqrts + 1.62142*s) +
     &          (-24.4344 - 25.085*sqrts - 0.2474*s) *
     &          dlogtk + 0.053105*sqrts*tk)
C Mick and Karsten - Dec 04
C ADDING pressure dependence based on Millero (1995), p675
C with additional info from CO2sys documentation (E. Lewis and 
C D. Wallace, 1998 - see endnotes for commentary on Millero, 95)
           bigR = 83.145 
           dv = -29.48 + 0.1622*t + 2.608d-3*t*t
           dk = -2.84d-3
           pfactor = - (dv/(bigR*tk))*pressc 
     &               + (0.5*dk/(bigR*tk))*pressc*pressc
           akb(i,j,bi,bj) = akb(i,j,bi,bj)*exp(pfactor)
C------------------------------------------------------------------------
C k1p = [H][H2PO4]/[H3PO4]
C DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
           ak1p(i,j,bi,bj) = exp(-4576.752*invtk + 115.525 - 
     &          18.453*dlogtk +
     &          (-106.736*invtk + 0.69171)*sqrts +
     &          (-0.65643*invtk - 0.01844)*s)
C------------------------------------------------------------------------
C k2p = [H][HPO4]/[H2PO4]
C DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
           ak2p(i,j,bi,bj) = exp(-8814.715*invtk + 172.0883 - 
     &          27.927*dlogtk +
     &          (-160.340*invtk + 1.3566) * sqrts +
     &          (0.37335*invtk - 0.05778) * s)
C------------------------------------------------------------------------
C k3p = [H][PO4]/[HPO4]
C DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
           ak3p(i,j,bi,bj) = exp(-3070.75*invtk - 18.141 + 
     &          (17.27039*invtk + 2.81197) *
     &          sqrts + (-44.99486*invtk - 0.09984) * s)
C------------------------------------------------------------------------
C ksi = [H][SiO(OH)3]/[Si(OH)4]
C Millero p.671 (1995) using data from Yao and Millero (1995)
           aksi(i,j,bi,bj) = exp(-8904.2*invtk + 117.385 - 
     &          19.334*dlogtk +
     &          (-458.79*invtk + 3.5913) * sqrtis +
     &          (188.74*invtk - 1.5998) * is +
     &          (-12.1652*invtk + 0.07871) * is2 +
     &          log(1.0-0.001005*s))
C------------------------------------------------------------------------
C kw = [H][OH]
C Millero p.670 (1995) using composite data
           akw(i,j,bi,bj) = exp(-13847.26*invtk + 148.9652 - 
     &          23.6521*dlogtk + 
     &          (118.67*invtk - 5.977 + 1.0495 * dlogtk) *
     &          sqrts - 0.01615 * s)
C------------------------------------------------------------------------
C ks = [H][SO4]/[HSO4]
C dickson (1990, J. chem. Thermodynamics 22, 113)
           aks(i,j,bi,bj)=exp(-4276.1*invtk + 141.328 - 
     &          23.093*dlogtk +
     &          (-13856*invtk + 324.57 - 47.986*dlogtk)*sqrtis +
     &          (35474*invtk - 771.54 + 114.723*dlogtk)*is -
     &          2698*invtk*is**1.5 + 1776*invtk*is2 +
     &          log(1.0 - 0.001005*s))
C------------------------------------------------------------------------
C kf = [H][F]/[HF]
C dickson and Riley (1979) -- change pH scale to total
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
C solubility product for calcite
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
c        write(6,*)i,j,k,tmpa1,tmpa2,tmpa3,logkspc,Ksp_T_Calc
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

           Ksp_TP_Calc(i,j,bi,bj) = Ksp_T_Calc*10**(xvalue)




C------------------------------------------------------------------------
         else
            ff(i,j,bi,bj)=0.d0
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
         endif
         end do
        end do

        return
        end


