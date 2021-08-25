#include "DIC_OPTIONS.h"

C--  File dic_solvesaphe.F:
C--   Contents:
C--   o AHINI_FOR_AT
C--   o ANW_INFSUP
C--   o EQUATION_AT
C--   o CALC_PCO2_SOLVESAPHE
C--   o DIC_COEFFS_SURF
C--   o DIC_COEFFS_DEEP
C--   o SOLVE_AT_FAST
C--   o SOLVE_AT_GENERAL
C--   o SOLVE_AT_GENERAL_SEC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    SolveSAPHE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    SolveSAPHE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with SolveSAPHE.  If not, see <http://www.gnu.org/licenses/>.
C
C    Copyright 2013 Guy Munhoven
C
C    From the paper: Munhoven, G (2013),
C        Mathematics of the total alkalinity–pH equation – pathway to robust
C        and universal solution algorithms: the SolveSAPHE package v1.0.1.
C      Geosci. Model Dev., 6, 1367–1388,  doi:10.5194/gmd-6-1367-2013
C
C    JMLauderdale Summer 2018:
C    - Modified for MITGCM style, just keeping the "general" equations from
C        MOD_PHSOLVERS SOLVE_AT_GENERAL SOLVE_AT_GENERAL_SEC and SOLVE_AT_FAST.
C        Use runtime flags in data.dic to use GENERAL (selectPHsolver=1,
C        default), SEC (selectPHsolver=2) or FAST (selectPHsolver=3).
C
C    - MOD_CHEMCONST is included here as DIC_COEFFS_SURF, with the
C        style brought in line with S/R CARBON_COEFF (i.e all ak values are
C        filled in one call, rather than all as separate functions).
C
C    - MOD_CHEMCONST pressure corrections have been split off into the new
C        S/R DIC_COEFFS_DEEP, call both to get pressure
C        adjusted dissociation constants
C
C    - Different coefficients can be accessed using runtime flags in data.dic:
C
C    Borate concentration from salinity:
C     selectBTconst=1 for the default formulation of Uppström (1974)
C                     i.e. the same as S/R CARBON_COEFFS
C     selectBTconst=2 for the new formulation from Lee et al (2010)
C
C    Fluoride concentration from salinity:
C     selectFTconst=1 for the default formulation of Riley (1965)
C                     i.e. the same as S/R CARBON_COEFFS
C     selectFTconst=2 for the new formulation from Culkin (1965)
C
C    First dissociation constant for hydrogen fluoride:
C     selectHFconst=1 for the default  Dickson and Riley (1979)
C                     i.e. the same as S/R CARBON_COEFFS
C     selectHFconst=2 for the formulation of Perez and Fraga (1987)
C
C    First and second dissociation constants of carbonic acid:
C     selectK1K2const=1 for the default formulation of Millero (1995) with data
C              from Mehrbach et al. (1973),  i.e. the same as S/R CARBON_COEFFS
C     selectK1K2const=2 for the formulation of Roy et al. (1993)
C     selectK1K2const=3 for the "combination" formulation of Millero (1995)
C     selectK1K2const=4 for the formulation of Luecker et al. (2000)
C     selectK1K2const=5 for the formulation of Millero
C                                              (2010, Mar. Fresh Wat. Res.)
C     selectK1K2const=6 for the formulation of Waters, Millero, Woosley
C                                              (2014, Mar. Chem.)
C      =========================================================================
      SUBROUTINE AHINI_FOR_AT(p_alkcb, p_dictot, p_bortot, p_hini,
     &                    i, j, k, bi, bj, myIter, myThid )

C      Subroutine returns the root for the 2nd order approximation of the
C      DIC -- B_T -- A_CB equation for [H+] (reformulated as a cubic polynomial)
C      around the local minimum, if it exists.

C      Returns * 1E-03 if p_alkcb <= 0
C              * 1E-10 if p_alkcb >= 2*p_dictot + p_bortot
C              * 1E-07 if 0 < p_alkcb < 2*p_dictot + p_bortot
C                       and the 2nd order approximation does not have a solution

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C -----------------------------------------------------------------------
C     Argument variables
C -----------------------------------------------------------------------

      _RL p_alkcb
      _RL p_dictot
      _RL p_bortot
      _RL p_hini
      INTEGER i,j,k,bi,bj,myIter,myThid

#ifdef CARBONCHEM_SOLVESAPHE
C -----------------------------------------------------------------------
C     Local variables
C -----------------------------------------------------------------------

      _RL zcar, zbor
      _RL zd, zsqrtd, zhmin
      _RL za2, za1, za0

      IF (p_alkcb .LE. 0. _d 0) THEN
       p_hini = 1. _d -3
      ELSEIF (p_alkcb .GE. (2. _d 0*p_dictot + p_bortot)) THEN
       p_hini = 1. _d -10
      ELSE
       zcar = p_dictot/p_alkcb
       zbor = p_bortot/p_alkcb

C      Coefficients of the cubic polynomial
       za2 = akb(i,j,bi,bj)*(1. _d 0 - zbor) + ak1(i,j,bi,bj)
     &       *(1. _d 0-zcar)
       za1 = ak1(i,j,bi,bj)*akb(i,j,bi,bj)*(1. _d 0 - zbor - zcar)
     &     + ak1(i,j,bi,bj)*ak2(i,j,bi,bj)*(1. _d 0 - (zcar+zcar))
       za0 = ak1(i,j,bi,bj)*ak2(i,j,bi,bj)*akb(i,j,bi,bj)
     &       *(1. _d 0 - zbor - (zcar+zcar))

C      Taylor expansion around the minimum
       zd = za2*za2 - 3. _d 0*za1
C            Discriminant of the quadratic equation
C                 for the minimum close to the root

       IF(zd .GT. 0. _d 0) THEN
C      If the discriminant is positive
         zsqrtd = SQRT(zd)
         IF(za2 .LT. 0) THEN
           zhmin = (-za2 + zsqrtd)/3. _d 0
         ELSE
           zhmin = -za1/(za2 + zsqrtd)
        ENDIF
        p_hini = zhmin + SQRT(-(za0 + zhmin*(za1 + zhmin*(za2 + zhmin)))
     &            /zsqrtd)
       ELSE
         p_hini = 1. _d -7
       ENDIF
      ENDIF

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C      END SUBROUTINE AHINI_FOR_AT
C      =========================================================================

C      =========================================================================
      SUBROUTINE ANW_INFSUP(p_dictot, p_bortot,
     &                      p_po4tot, p_siltot,
     &                      p_nh4tot, p_h2stot,
     &                      p_so4tot, p_flutot,
     &                      p_alknw_inf, p_alknw_sup,
     &                      i, j, k, bi, bj, myIter,
     &                      myThid )

C      Subroutine returns the lower and upper bounds of "non-water-selfionization"
C      Contributions to total alkalinity (the infimum and the supremum), i.e
C      inf(TA - [OH-] + [H+]) and sup(TA - [OH-] + [H+])

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C      --------------------
C      Argument variables
C      --------------------

      _RL p_dictot
      _RL p_bortot
      _RL p_po4tot
      _RL p_siltot
      _RL p_nh4tot
      _RL p_h2stot
      _RL p_so4tot
      _RL p_flutot
      _RL p_alknw_inf
      _RL p_alknw_sup
      INTEGER i,j,k,bi,bj,myIter,myThid

#ifdef CARBONCHEM_SOLVESAPHE
C      p_alknw_inf = -\Sum_i m_i Xtot_i
C
C      p_alknw_inf =-p_dictot*0. _d 0 & ! n = 2, m = 0
C                  -p_bortot*0. _d 0 &  ! n = 1, m = 0
C                  -p_po4tot*1. _d 0 &  ! n = 3, m = 1
C                  -p_siltot*0. _d 0 &  ! n = 1, m = 0
C                  -p_nh4tot*0. _d 0 &  ! n = 1, m = 0
C                  -p_h2stot*0. _d 0 &  ! n = 1, m = 0
C                  -p_so4tot*1. _d 0 &  ! n = 1, m = 1
C                  -p_flutot*1. _d 0    ! n = 1, m = 1

      p_alknw_inf =    -p_po4tot - p_so4tot - p_flutot

C      p_alknw_sup = \Sum_i (n_i - m_i) Xtot_i
C
C      p_alknw_sup = p_dictot*(2. _d 0-0. _d 0) &  ! n = 2, m = 0
C                  p_bortot*(1. _d 0-0. _d 0) &    ! n = 1, m = 0
C                  p_po4tot*(3. _d 0-1. _d 0) &    ! n = 3, m = 1
C                  p_siltot*(1. _d 0-0. _d 0) &    ! n = 1, m = 0
C                  p_nh4tot*(1. _d 0-0. _d 0) &    ! n = 1, m = 0
C                  p_h2stot*(1. _d 0-0. _d 0) &    ! n = 1, m = 0
C                  p_so4tot*(1. _d 0-1. _d 0) &    ! n = 1, m = 1
C                  p_flutot*(1. _d 0-1. _d 0)      ! n = 1, m = 1

      p_alknw_sup =  p_dictot + p_dictot + p_bortot
     &             + p_po4tot + p_po4tot + p_siltot
     &             + p_nh4tot + p_h2stot

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C      END SUBROUTINE ANW_INFSUP
C      =========================================================================

C      =========================================================================
      SUBROUTINE CALC_PCO2_SOLVESAPHE(
     I                    t,s,z_dictot,z_po4tot,z_siltot,z_alktot,
     U                    pHlocal,z_pco2,z_co3,
     I                    i,j,k,bi,bj,debugPrt,myIter,myThid )

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C -----------------------------------------------------------------------
C General parameters
C -----------------------------------------------------------------------
C       diclocal = total inorganic carbon (mol/m^3)
C             where 1 T = 1 metric ton = 1000 kg
C       ta  = total alkalinity (eq/m^3)
C       pt  = inorganic phosphate (mol/^3)
C       sit = inorganic silicate (mol/^3)
C       t   = temperature (degrees C)
C       s   = salinity (g/kg)
      _RL  t, s, z_po4tot, z_siltot, z_alktot
      _RL  z_pco2, z_dictot, pHlocal
      _RL  z_co3
      INTEGER i,j,k,bi,bj
      LOGICAL debugPrt
      INTEGER myIter,myThid

#ifdef CARBONCHEM_SOLVESAPHE

C     == Local variables ==
      _RL  z_h
      _RL  z_hini
      _RL  z_bortot
      _RL  z_nh4tot
      _RL  z_h2stot
      _RL  z_so4tot
      _RL  z_flutot
      _RL  z_val
      _RL  z_co2s
      _RL  z_fco2
      _RL  z_hco3
      _RL  z_co2aq
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C -----------------------------------------------------------------------
C Change units from the input of mol/m^3 -> mol/kg:
c (1 mol/m^3)  x (1 m^3/1024.5 kg)
c where the ocean mean surface density is 1024.5 kg/m^3
c Note: mol/kg are actually what the body of this routine uses
c for calculations.  Units are reconverted back to mol/m^3 at the
c end of this routine.
c To convert input in mol/m^3 -> mol/kg
      z_po4tot=z_po4tot*permil
      z_siltot=z_siltot*permil
      z_alktot=z_alktot*permil
      z_dictot=z_dictot*permil

C Load from the carbon_chem common block
      z_so4tot = st(i,j,bi,bj)
      z_flutot = ft(i,j,bi,bj)
      z_bortot = bt(i,j,bi,bj)

      z_nh4tot = 0. _d 0
      z_h2stot = 0. _d 0

      z_hini = 10. _d 0**(-1. _d 0 * pHlocal)

      at_maxniter  = 50

      IF ( selectPHsolver.EQ.1 ) THEN
#ifdef ALLOW_DEBUG
        IF (debugPrt) CALL DEBUG_CALL('SOLVE_AT_GENERAL',myThid)
#endif
        CALL SOLVE_AT_GENERAL(z_alktot, z_dictot, z_bortot,
     &                z_po4tot, z_siltot, z_nh4tot, z_h2stot,
     &                z_so4tot, z_flutot, z_hini,   z_val,
     &                z_h,
     &                i, j, k, bi, bj, debugPrt, myIter, myThid )
      ELSEIF ( selectPHsolver.EQ.2 ) THEN
#ifdef ALLOW_DEBUG
        IF (debugPrt) CALL DEBUG_CALL('SOLVE_AT_GENERAL_SEC',myThid)
#endif
        CALL SOLVE_AT_GENERAL_SEC(z_alktot, z_dictot, z_bortot,
     &                z_po4tot, z_siltot, z_nh4tot, z_h2stot,
     &                z_so4tot, z_flutot, z_hini,   z_val,
     &                z_h,
     &                i, j, k, bi, bj, debugPrt, myIter, myThid )
      ELSEIF ( selectPHsolver.EQ.3 ) THEN
#ifdef ALLOW_DEBUG
        IF (debugPrt) CALL DEBUG_CALL('SOLVE_AT_FAST',myThid)
#endif
        CALL SOLVE_AT_FAST(z_alktot, z_dictot, z_bortot,
     &                z_po4tot, z_siltot, z_nh4tot, z_h2stot,
     &                z_so4tot, z_flutot, z_hini,   z_val,
     &                z_h,
     &                i, j, k, bi, bj, debugPrt, myIter, myThid )
      ENDIF

C Unlikely, but it might happen...
      IF ( z_h .LT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &    'S/R CALC_PCO2_SOLVESAPHE:',
     &    ' H+ ion concentration less than 0',
     &    ' Divergence or too many iterations'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CALC_PCO2_SOLVESAPHE'
      ENDIF

C Return update pH to main routine
      phlocal = -log10(z_h)

C now determine [CO2*]
      z_co2s  = z_dictot/
     &   (1.0 _d 0 + (ak1(i,j,bi,bj)/z_h)
     & + (ak1(i,j,bi,bj)*ak2(i,j,bi,bj)/(z_h*z_h)))
C Evaluate HCO3- and CO32- , carbonate ion concentration
C used in determination of calcite compensation depth
      z_hco3 = ak1(i,j,bi,bj)*z_dictot /
     &         (z_h*z_h + ak1(i,j,bi,bj)*z_h
     &        + ak1(i,j,bi,bj)*ak2(i,j,bi,bj))
      z_co3 = ak1(i,j,bi,bj)*ak2(i,j,bi,bj)*z_dictot /
     &         (z_h*z_h + ak1(i,j,bi,bj)*z_h
     &        + ak1(i,j,bi,bj)*ak2(i,j,bi,bj))

c ---------------------------------------------------------------
c surface pCO2 (following Dickson and Goyet, DOE...)
#ifdef WATERVAP_BUG
      z_pco2 = z_co2s/ff(i,j,bi,bj)
#else
c bug fix by Bennington
      z_fco2 = z_co2s/ak0(i,j,bi,bj)
      z_pco2 = z_fco2/fugf(i,j,bi,bj)
#endif

C ----------------------------------------------------------------
C Reconvert from mol/kg -> mol/m^3
      z_po4tot = z_po4tot/permil
      z_siltot = z_siltot/permil
      z_alktot = z_alktot/permil
      z_dictot = z_dictot/permil
      z_hco3   = z_hco3/permil
      z_co3    = z_co3/permil
      z_co2aq  = z_co2s/permil

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C      END SUBROUTINE CALC_PCO2_SOLVESAPHE
C      =========================================================================

C      =========================================================================
      SUBROUTINE DIC_COEFFS_SURF(
     &                   ttemp,stemp,
     &                   bi,bj,iMin,iMax,jMin,jMax,myThid)

C     Determine coefficients for surface carbon chemistry,
C     loaded into common block

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

      _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax,myThid

#ifdef CARBONCHEM_SOLVESAPHE

C LOCAL VARIABLES
      INTEGER i, j
      _RL t
      _RL s
      _RL t_k
      _RL t_k_o_100
      _RL t_k_o_100_2
      _RL dlog_t_k
      _RL inv_t_k
      _RL ion_st, is_2, sqrtis
      _RL s_2, sqrts, s_15, scl, s35
      _RL log_fw2sw
      _RL B, B1
      _RL delta
      _RL P1atm
      _RL RT
      _RL Rgas
C conversions for different pH scales
      _RL total2free
      _RL free2total
      _RL free2sw
      _RL sw2free
      _RL total2sw
      _RL sw2total

        do i=imin,imax
         do j=jmin,jmax
          if (hFacC(i,j,1,bi,bj).gt.0. _d 0) then
           t = ttemp(i,j)
           s = stemp(i,j)
C terms used more than once for:
C temperature
           t_k         = 273.15 _d 0 + t
           t_k_o_100   = t_k/100. _d 0
           t_k_o_100_2 = t_k_o_100*t_k_o_100
           inv_t_k=1.0 _d 0/t_k
           dlog_t_k=LOG(t_k)
C ionic strength (converted to kgsw)
           ion_st=19.924 _d 0*s/(1000. _d 0-1.005 _d 0*s)
           is_2=ion_st*ion_st
           sqrtis=sqrt(ion_st)
C salinity
           s_2  = s*s
           sqrts=sqrt(s)
           s_15 = s*sqrts
           scl  = s/1.80655 _d 0
           s35  = s/35. _d 0

           log_fw2sw = LOG( 1. _d 0 - 0.001005 _d 0*s )

      IF ( selectBTconst.EQ.1 ) THEN
C -----------------------------------------------------------------------
C       Calculate the total borate concentration in mol/kg-SW
C       given the salinity of a sample
C       Ref: Uppström (1974), cited by  Dickson et al. (2007, chapter 5, p 10)
C            Millero (1982) cited in Millero (1995)
C       pH scale  : N/A
           bt(i,j,bi,bj) = 0.000232 _d 0* scl/10.811 _d 0
      ELSEIF ( selectBTconst.EQ.2 ) THEN
C -----------------------------------------------------------------------
C       Calculate the total borate concentration in mol/kg-SW
C       given the salinity of a sample
C       References: New formulation from Lee et al (2010)
C       pH scale  : N/A
           bt(i,j,bi,bj) = 0.0002414 _d 0* scl/10.811 _d 0
      ENDIF

      IF ( selectFTconst.EQ.1 ) THEN
C -----------------------------------------------------------------------
C       Calculate the total fluoride concentration in mol/kg-SW
C       given the salinity of a sample
C       References: Riley (1965)
C       pH scale  : N/A
           ft(i,j,bi,bj) = 0.000067 _d 0 * scl/18.9984 _d 0
      ELSEIF ( selectFTconst.EQ.2 ) THEN
C -----------------------------------------------------------------------
C       Calculate the total fluoride concentration in mol/kg-SW
C       given the salinity of a sample
C       References: Culkin (1965) (???)
C       pH scale  : N/A
           ft(i,j,bi,bj) = 0.000068 _d 0*(s35)
      ENDIF

C -----------------------------------------------------------------------
C       Calculate the total sulfate concentration in mol/kg-SW
C       given the salinity of a sample
C       References: Morris & Riley (1966) quoted in Handbook (2007)
C       pH scale  : N/A
           st(i,j,bi,bj) = 0.14 _d 0 * scl/96.062 _d 0

C -----------------------------------------------------------------------
C       Calculate the total calcium concentration in mol/kg-SW
C       given the salinity of a sample
C       References: Culkin (1965) (???)
C       pH scale  : N/A
           cat(i,j,bi,bj) = 0.010282 _d 0*(s35)

C -----------------------------------------------------------------------
C       Calculate K0 in (mol/kg-SW)/atmosphere
C       References: Weiss (1979) [(mol/kg-SW)/atm]
C       pH scale  : N/A
C       Note      : currently no pressure correction
           ak0(i,j,bi,bj)  = EXP( 93.4517 _d 0/t_k_o_100 - 60.2409 _d 0
     &                 + 23.3585 _d 0*LOG(t_k_o_100)
     &                 + s * (0.023517 _d 0 - 0.023656 _d 0*t_k_o_100
     &                 + 0.0047036 _d 0*t_k_o_100_2))

C------------------------------------------------------------------------
C       Calculate f = k0(1-pH2O)*correction term for non-ideality
C       References: Weiss & Price (1980, Mar. Chem., 8, 347-359
C                   Eq 13 with table 6 values)
C       pH scale  : N/A
           ff(i,j,bi,bj) = exp(-162.8301 _d 0 + 218.2968 _d 0/t_k_o_100
     &          + 90.9241 _d 0*log(t_k_o_100) - 1.47696 _d 0*t_k_o_100_2
     &          + s * (.025695 _d 0 - .025225 _d 0*t_k_o_100
     &          + 0.0049867 _d 0*t_k_o_100_2))

C------------------------------------------------------------------------
C       Calculate Fugacity Factor needed for non-ideality in ocean
C       References: Weiss (1974) Marine Chemistry
C       pH scale  : N/A
           P1atm = 1.01325 _d 0 ! bars
           Rgas = 83.1451 _d 0  ! bar*cm3/(mol*K)
           RT = Rgas*t_k
           delta = (57.7 _d 0 - 0.118 _d 0*t_k)
           B1 = -1636.75 _d 0 + 12.0408 _d 0*t_k
     &                  - 0.0327957 _d 0*t_k*t_k
           B  = B1 + 3.16528 _d 0*t_k*t_k*t_k*(0.00001 _d 0)

C   "x2" term often neglected (assumed=1) in applications of Weiss's (1974) eq.9
C    x2 = 1 - x1 = 1 - xCO2 (it is very close to 1, but not quite)
           fugf(i,j,bi,bj) = exp( (B+2. _d 0*delta) * P1atm / RT)

      IF ( selectK1K2const.EQ.1 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       References: Millero (1995, eq 35 -- pK1(MEHR));
C                   Mehrbach et al. (1973) data
C       pH scale:   Seawater
           ak1(i,j,bi,bj)=10.**(-1. _d 0*(3670.7 _d 0*inv_t_k
     &          - 62.008 _d 0 + 9.7944 _d 0*dlog_t_k
     &          - 0.0118 _d 0 * s + 0.000116 _d 0*s_2))

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       References: Millero (1995, eq 36 -- pK2(MEHR))
C                   Mehrbach et al. (1973) data
C       pH scale:   Seawater
           ak2(i,j,bi,bj)=10.**(-1. _d 0*(1394.7 _d 0*inv_t_k
     &           + 4.777 _d 0 - 0.0184 _d 0*s + 0.000118 _d 0*s_2))

      ELSEIF ( selectK1K2const.EQ.2 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Total pH-scale.
C       References: Roy et al. (1993) -- also Handbook (1994)
C       pH scale  : Total
C       Note      : converted here from mol/kg-H2O to mol/kg-SW
           ak1(i,j,bi,bj)  =  EXP(-2307.1255 _d 0*inv_t_k + 2.83655 _d 0
     &              - 1.5529413 _d 0*dlog_t_k
     &              + (-4.0484 _d 0*inv_t_k - 0.20760841)*sqrts
     &              + 0.08468345*s
     &              - 0.00654208*s_15
     &              + log_fw2sw )

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Total pH-scale.
C       References: Roy et al. (1993) -- also Handbook (1994)
C       pH scale  : Total
C       Note      : converted here from mol/kg-H2O to mol/kg-SW
           ak2(i,j,bi,bj) = EXP(-3351.6106 _d 0*inv_t_k - 9.226508 _d 0
     &              - 0.2005743 _d 0*dlog_t_k
     &              + ( -23.9722 _d 0*inv_t_k - 0.106901773 _d 0)*sqrts
     &              + 0.1130822*s - 0.00846934 _d 0*s_15
     &              + log_fw2sw )

      ELSEIF ( selectK1K2const.EQ.3 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       References: Millero (1995, eq 50 -- ln K1(COM))
C       pH scale:   Seawater
           ak1(i,j,bi,bj)  = EXP(2.18867 _d 0 - 2275.0360 _d 0*inv_t_k
     &              - 1.468591 _d 0*dlog_t_k
     &              + ( -0.138681 _d 0 - 9.33291 _d 0*inv_t_k)*sqrts
     &              + 0.0726483 _d 0*s - 0.00574938 _d 0*s_15)

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       References: Millero (1995, eq 51 -- ln K2(COM))
C       pH scale:   Seawater
           ak2(i,j,bi,bj)  = EXP(-0.84226 _d 0 - 3741.1288 _d 0*inv_t_k
     &              -  1.437139 _d 0*dlog_t_k
     &              + (-0.128417 _d 0 - 24.41239 _d 0*inv_t_k)*sqrts
     &              +  0.1195308 _d 0*s - 0.00912840 _d 0*s_15)

      ELSEIF ( selectK1K2const.EQ.4 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Total pH-scale.
C       Suitable when 2 < T < 35 and 19 < S < 43
C       References: Luecker et al. (2000) -- also Handbook (2007)
C       pH scale:   Total
           ak1(i,j,bi,bj)  = 10. _d 0**( 61.2172 _d 0
     &                 - 3633.86 _d 0*inv_t_k - 9.67770 _d 0*dlog_t_k
     &                 + s*(0.011555 - s*0.0001152 _d 0))

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Total pH-scale.
C       Suitable when 2 < T < 35 and 19 < S < 43
C       References: Luecker et al. (2000) -- also Handbook (2007)
C       pH scale:   Total
           ak2(i,j,bi,bj)  = 10. _d 0**(-25.9290 _d 0
     &                 - 471.78 _d 0*inv_t_k + 3.16967 _d 0*dlog_t_k
     &                 + s*(0.01781 _d 0 - s*0.0001122 _d 0))

      ELSEIF ( selectK1K2const.EQ.5 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       Suitable when 0 < T < 50 and 1 < S < 50
C       References: Millero (2010, Mar. Fresh Wat. Res.)
C                   Millero (1979) pressure correction
C       pH scale:   Seawater
           ak1(i,j,bi,bj) = 10.0 _d 0**(-1*(6320.813 _d 0*inv_t_k
     &      + 19.568224 _d 0*dlog_t_k -126.34048 _d 0
     &      + 13.4038 _d 0*sqrts + 0.03206 _d 0*s - (5.242 _d -5)*s_2
     &      + (-530.659 _d 0*sqrts - 5.8210 _d 0*s)*inv_t_k
     &      -2.0664 _d 0*sqrts*dlog_t_k))

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       Suitable when 0 < T < 50 and 1 < S < 50
C       References: Millero (2010, Mar. Fresh Wat. Res.)
C                   Millero (1979) pressure correction
C       pH scale:   Seawater
           ak2(i,j,bi,bj) = 10.0 _d 0**(-1*(5143.692 _d 0*inv_t_k
     &     + 14.613358 _d 0*dlog_t_k -90.18333 _d 0
     &     + 21.3728 _d 0*sqrts + 0.1218 _d 0*s - (3.688 _d -4)*s_2
     &     + (-788.289 _d 0*sqrts - 19.189 _d 0*s)*inv_t_k
     &     -3.374 _d 0*sqrts*dlog_t_k))

      ELSEIF ( selectK1K2const.EQ.6 ) THEN
C -----------------------------------------------------------------------
C       Calculate first dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       Suitable when 0 < T < 50 and 1 < S < 50
C       References: Waters, Millero, Woosley (Mar. Chem., 165, 66-67, 2014)
C                   Millero (1979) pressure correction
C       pH scale:   Seawater
           ak1(i,j,bi,bj) = 10.0 _d 0**(-1.*(6320.813 _d 0*inv_t_k
     &     + 19.568224 _d 0*dlog_t_k -126.34048 _d 0
     &     + 13.409160 _d 0*sqrts + 0.031646 _d 0*s - (5.1895 _d -5)*s_2
     &     + (-531.3642 _d 0*sqrts - 5.713 _d 0*s)*inv_t_k
     &     -2.0669166 _d 0*sqrts*dlog_t_k))

C       Calculate second dissociation constant of carbonic acid
C       in mol/kg-SW on the Seawater pH-scale.
C       Suitable when 0 < T < 50 and 1 < S < 50
C       References: Waters, Millero, Woosley (Mar. Chem., 165, 66-67, 2014)
C                   Millero (1979) pressure correction
C       pH scale:   Seawater
           ak2(i,j,bi,bj) = 10.0 _d 0**(-1.*
     &     ( 5143.692 _d 0*inv_t_k + 14.613358 _d 0*dlog_t_k
     &      - 90.18333 _d 0 + 21.225890 _d 0*sqrts + 0.12450870 _d 0*s
     &      - (3.7243 _d -4)*s_2
     &      + (-779.3444 _d 0*sqrts - 19.91739 _d 0*s)*inv_t_k
     &      - 3.3534679 _d 0*sqrts*dlog_t_k ) )

      ENDIF /* selectK1K2const */

C -----------------------------------------------------------------------
C       Calculate boric acid dissociation constant KB
C       in mol/kg-SW on the total pH-scale.
C       References: Dickson (1990, eq. 23) -- also Handbook (2007, eq. 37)
C       pH scale  : total
           akb(i,j,bi,bj)  = EXP(( -8966.90 _d 0 - 2890.53 _d 0*sqrts
     &      -77.942 _d 0*s + 1.728 _d 0*s_15 - 0.0996 _d 0*s_2 )*inv_t_k
     &      + (148.0248 _d 0 + 137.1942 _d 0*sqrts + 1.62142 _d 0*s)
     &      + (-24.4344 _d 0 - 25.085 _d 0*sqrts - 0.2474 _d 0*s)*
     &      dlog_t_k + 0.053105 _d 0*sqrts*t_k )

C -----------------------------------------------------------------------
C       Calculate the first dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Yao and Millero (1995)
C       pH scale  : Seawater
           ak1p(i,j,bi,bj) = EXP(115.54 _d 0
     &              - 4576.752 _d 0*inv_t_k - 18.453 _d 0*dlog_t_k
     &              + ( 0.69171 _d 0 -  106.736 _d 0*inv_t_k)*sqrts
     &              + (-0.01844 _d 0 -  0.65643 _d 0*inv_t_k)*s)

C -----------------------------------------------------------------------
C       Calculate the second dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Yao and Millero (1995)
C       pH scale  : Seawater
           ak2p(i,j,bi,bj) = EXP( 172.1033 _d 0
     &              - 8814.715 _d 0*inv_t_k
     &              -   27.927 _d 0*dlog_t_k
     &              + (  1.3566 _d 0 -  160.340 _d 0*inv_t_k)*sqrts
     &              + (-0.05778 _d 0 +  0.37335 _d 0*inv_t_k)*s)

C -----------------------------------------------------------------------
C       Calculate the third dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Yao and Millero (1995)
C       pH scale  : Seawater
           ak3p(i,j,bi,bj) = EXP(-18.126 _d 0  -  3070.75 _d 0*inv_t_k
     &                + ( 2.81197 _d 0 + 17.27039 _d 0*inv_t_k)*sqrts
     &                + (-0.09984 _d 0 - 44.99486 _d 0*inv_t_k)*s)

C -----------------------------------------------------------------------
C       Calculate the first dissociation constant
C       of silicic acid (H4SiO4) in seawater
C       References: Yao and Millero (1995) cited by Millero (1995)
C       pH scale  : Seawater (according to Dickson et al, 2007)
C       Note      : converted here from mol/kg-H2O to mol/kg-sw
           aksi(i,j,bi,bj) = EXP(
     &           117.40 _d 0 - 8904.2 _d 0*inv_t_k
     &         - 19.334 _d 0 * dlog_t_k
     &         + ( 3.5913 _d 0 -  458.79 _d 0*inv_t_k) * sqrtis
     &         + (-1.5998 _d 0 +  188.74 _d 0*inv_t_k) * ion_st
     &         + (0.07871 _d 0 - 12.1652 _d 0*inv_t_k) * ion_st*ion_st
     &         + log_fw2sw )

C -----------------------------------------------------------------------
C       Calculate the dissociation constant
C       of ammonium in sea-water [mol/kg-SW]
C       References: Yao and Millero (1995)
C       pH scale  : Seawater
           akn(i,j,bi,bj)  = EXP(-0.25444 _d 0 -  6285.33 _d 0*inv_t_k
     &                + 0.0001635 _d 0 * t_k
     &                + ( 0.46532 _d 0 - 123.7184 _d 0*inv_t_k)*sqrts
     &                + (-0.01992 _d 0 +  3.17556 _d 0*inv_t_k)*s)

C -----------------------------------------------------------------------
C       Calculate the dissociation constant of hydrogen sulfide in sea-water
C       References: Millero et al. (1988) (cited by Millero (1995)
C       pH scale  : - Seawater (according to Yao and Millero, 1995,
C                               p. 82: "refitted if necessary")
C                   - Total (according to Lewis and Wallace, 1998)
C       Note      : we stick to Seawater here for the time being
C       Note      : the fits from Millero (1995) and Yao and Millero (1995)
C                   derive from Millero et al. (1998), with all the coefficients
C                   multiplied by -ln(10)
           akhs(i,j,bi,bj) = EXP( 225.838 _d 0 - 13275.3 _d 0*inv_t_k
     &               - 34.6435 _d 0 * dlog_t_k
     &               +  0.3449 _d 0*sqrts -  0.0274 _d 0*s)

C -----------------------------------------------------------------------
C       Calculate the dissociation constant of hydrogen sulfate (bisulfate)
C       References: Dickson (1990) -- also Handbook (2007)
C       pH scale  : free
C       Note      : converted here from mol/kg-H2O to mol/kg-SW
           aks(i,j,bi,bj) = EXP(141.328 _d 0
     &                -   4276.1 _d 0*inv_t_k -  23.093 _d 0*dlog_t_k
     &                + ( 324.57 _d 0 - 13856. _d 0*inv_t_k
     &                -   47.986 _d 0*dlog_t_k) * sqrtis
     &                + (-771.54 _d 0 + 35474. _d 0*inv_t_k
     &                +  114.723 _d 0*dlog_t_k) * ion_st
     &                - 2698. _d 0*inv_t_k*ion_st**1.5 _d 0
     &                + 1776. _d 0*inv_t_k*ion_st*ion_st
     &                + log_fw2sw )

      IF ( selectHFconst.EQ.1 ) THEN
C -----------------------------------------------------------------------
C       Calculate the dissociation constant \beta_{HF} [(mol/kg-SW)^{-1}]
C       in (mol/kg-SW)^{-1}, where
C         \beta_{HF} = \frac{ [HF] }{ [H^{+}] [F^{-}] }
C       References: Dickson and Riley (1979)
C       pH scale  : free
C       Note      : converted here from mol/kg-H2O to mol/kg-SW
           akf(i,j,bi,bj) = EXP(1590.2 _d 0*inv_t_k - 12.641 _d 0
     &                 + 1.525 _d 0*sqrtis
     &                 + log_fw2sw )
      ELSEIF ( selectHFconst.EQ.2 ) THEN
C -----------------------------------------------------------------------
C       Calculate the dissociation constant for hydrogen fluoride
C       in mol/kg-SW
C       References: Perez and Fraga (1987)
C       pH scale  : Total (according to Handbook, 2007)
           akf(i,j,bi,bj) = EXP( 874. _d 0*inv_t_k - 9.68 _d 0
     &                           + 0.111 _d 0*sqrts )
      ENDIF

C -----------------------------------------------------------------------
C       Calculate the water dissociation constant Kw in (mol/kg-SW)^2
C       References: Millero (1995) for value at pressc = 0
C       pH scale  : Seawater
           akw(i,j,bi,bj) =  EXP(148.9802 _d 0 - 13847.26 _d 0*inv_t_k
     &              -   23.6521 _d 0*dlog_t_k
     &              + (-5.977 _d 0 + 118.67 _d 0*inv_t_k
     &              + 1.0495 _d 0*dlog_t_k)*sqrts
     &              -   0.01615 _d 0*s )

C -----------------------------------------------------------------------
C pH scale conversion factors and conversions. Everything on total pH scale
           total2free = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

           free2total = (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

           free2sw = 1. _d 0
     &                 + (st(i,j,bi,bj)/ aks(i,j,bi,bj))
     &                 + (ft(i,j,bi,bj)/(akf(i,j,bi,bj)*total2free))

           sw2free = 1. _d 0 / free2sw

           total2sw = total2free * free2sw

           sw2total = 1. _d 0 / total2sw

           aphscale(i,j,bi,bj) = 1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj)

      IF ( selectK1K2const.NE.2 .AND. selectK1K2const.NE.4 ) THEN
C Convert to the total pH scale
           ak1(i,j,bi,bj)  = ak1(i,j,bi,bj)*sw2total
           ak2(i,j,bi,bj)  = ak2(i,j,bi,bj)*sw2total
      ENDIF
           ak1p(i,j,bi,bj) = ak1p(i,j,bi,bj)*sw2total
           ak2p(i,j,bi,bj) = ak2p(i,j,bi,bj)*sw2total
           ak3p(i,j,bi,bj) = ak3p(i,j,bi,bj)*sw2total
           aksi(i,j,bi,bj) = aksi(i,j,bi,bj)*sw2total
           akn (i,j,bi,bj) = akn (i,j,bi,bj)*sw2total
           akhs(i,j,bi,bj) = akhs(i,j,bi,bj)*sw2total
           aks (i,j,bi,bj) = aks (i,j,bi,bj)*free2total
      IF ( selectHFconst.EQ.1 ) THEN
           akf (i,j,bi,bj) = akf (i,j,bi,bj)*free2total
      ENDIF
           akw (i,j,bi,bj) = akw (i,j,bi,bj)*sw2total

C -----------------------------------------------------------------------
C       Calculate the stoichiometric solubility product
C       of calcite in seawater
C       References: Mucci (1983)
C       pH scale  : N/A
C       Units     : (mol/kg-SW)^2

           Ksp_TP_Calc(i,j,bi,bj) = 10. _d 0**(-171.9065 _d 0
     &             - 0.077993 _d 0*t_k
     &             + 2839.319 _d 0*inv_t_k + 71.595 _d 0*LOG10(t_k)
     &             + ( -0.77712 _d 0 + 0.0028426 _d 0*t_k
     &             + 178.34 _d 0*inv_t_k)*sqrts
     &             - 0.07711 _d 0*s + 0.0041249 _d 0*s_15)

C -----------------------------------------------------------------------
C       Calculate the stoichiometric solubility product
C       of aragonite in seawater
C       References: Mucci (1983)
C       pH scale  : N/A
C       Units     : (mol/kg-SW)^2

           Ksp_TP_Arag(i,j,bi,bj) = 10. _d 0**(-171.945 _d 0
     &             - 0.077993 _d 0*t_k
     &             + 2903.293 _d 0*inv_t_k + 71.595 _d 0*LOG10(t_k)
     &             + ( -0.068393 _d 0 + 0.0017276 _d 0*t_k
     &             + 88.135 _d 0*inv_t_k)*sqrts
     &             - 0.10018 _d 0*s + 0.0059415 _d 0*s_15)
         else
           bt(i,j,bi,bj)  = 0. _d 0
           st(i,j,bi,bj)  = 0. _d 0
           ft(i,j,bi,bj)  = 0. _d 0
           cat(i,j,bi,bj) = 0. _d 0
           fugf(i,j,bi,bj)= 0. _d 0
           ff(i,j,bi,bj)  = 0. _d 0
           ak0(i,j,bi,bj) = 0. _d 0
           ak1(i,j,bi,bj) = 0. _d 0
           ak2(i,j,bi,bj) = 0. _d 0
           akb(i,j,bi,bj) = 0. _d 0
           ak1p(i,j,bi,bj)= 0. _d 0
           ak2p(i,j,bi,bj)= 0. _d 0
           ak3p(i,j,bi,bj)= 0. _d 0
           aksi(i,j,bi,bj)= 0. _d 0
           akw(i,j,bi,bj) = 0. _d 0
           aks(i,j,bi,bj) = 0. _d 0
           akf(i,j,bi,bj) = 0. _d 0
           akn(i,j,bi,bj) = 0. _d 0
           akhs(i,j,bi,bj)= 0. _d 0
           Ksp_TP_Calc(i,j,bi,bj) = 0. _d 0
           Ksp_TP_Arag(i,j,bi,bj) = 0. _d 0
           aphscale(i,j,bi,bj)    = 0. _d 0
         endif
         end do
        end do
#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C     END SUBROUTINE DIC_COEFFS_SURF
C -----------------------------------------------------------------------

C -----------------------------------------------------------------------
      SUBROUTINE DIC_COEFFS_DEEP(
     &                   ttemp,stemp,
     &                   bi,bj,iMin,iMax,jMin,jMax,
     &                   Klevel,myThid)

C     Add depth dependence to carbon chemistry coefficients loaded into
C     common block. Corrections occur on the Seawater pH scale and are
C     converted back to the total scale
      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

      _RL  ttemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  stemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax,Klevel,myThid

#ifdef CARBONCHEM_SOLVESAPHE
C LOCAL VARIABLES
      INTEGER i, j, k
      _RL  bdepth
      _RL  cdepth
      _RL  pressc
      _RL t
      _RL s
      _RL zds
      _RL t_k
      _RL t_k_o_100
      _RL t_k_o_100_2
      _RL dlog_t_k
      _RL sqrtis
      _RL sqrts
      _RL s_2
      _RL s_15
      _RL inv_t_k
      _RL ion_st
      _RL is_2
      _RL scl
      _RL zrt
      _RL B1
      _RL B
      _RL delta
      _RL Pzatm
      _RL zdvi
      _RL zdki
      _RL pfac
C pH scale converstions
      _RL total2free_surf
      _RL free2sw_surf
      _RL total2sw_surf
      _RL total2free
      _RL free2total
      _RL free2sw
      _RL sw2free
      _RL total2sw
      _RL sw2total

c determine pressure (bar) from depth
c 1 BAR at z=0m (atmos pressure)
c use UPPER surface of cell so top layer pressure = 0 bar
c for surface exchange coeffs

cmick..............................
c        write(6,*)'Klevel ',klevel

        bdepth = 0. _d 0
        cdepth = 0. _d 0
        pressc = 1. _d 0
        do k = 1,Klevel
            cdepth = bdepth + 0.5 _d 0*drF(k)
            bdepth = bdepth + drF(k)
            pressc = 1. _d 0 + 0.1 _d 0*cdepth
        end do
cmick...................................................
c        write(6,*)'depth,pressc ',cdepth,pressc
cmick....................................................

        do i=imin,imax
         do j=jmin,jmax
          if (hFacC(i,j,Klevel,bi,bj).gt.0. _d 0) then
           t = ttemp(i,j)
           s = stemp(i,j)
C terms used more than once for:
C temperature
           t_k = 273.15 _d 0 + t
           zrt= 83.14472 _d 0 * t_k
           t_k_o_100 = t_k/100. _d 0
           t_k_o_100_2=t_k_o_100*t_k_o_100
           inv_t_k=1.0 _d 0/t_k
           dlog_t_k=log(t_k)
C ionic strength
           ion_st=19.924 _d 0*s/(1000. _d 0-1.005 _d 0*s)
           is_2=ion_st*ion_st
           sqrtis=sqrt(ion_st)
C salinity
           s_2=s*s
           sqrts=sqrt(s)
           s_15=s**1.5 _d 0
           scl=s/1.80655 _d 0
           zds = s - 34.8 _d 0

           total2free_surf = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

           free2sw_surf = 1. _d 0
     &                 + st(i,j,bi,bj)/ aks(i,j,bi,bj)
     &                 + ft(i,j,bi,bj)/(akf(i,j,bi,bj)*total2free_surf)

           total2sw_surf = total2free_surf * free2sw_surf

C------------------------------------------------------------------------
C       Recalculate Fugacity Factor needed for non-ideality in ocean
C       with pressure dependence.
C       Reference : Weiss (1974) Marine Chemistry
C       pH scale  : N/A
           Pzatm = 1.01325 _d 0+pressc ! bars
           delta = (57.7 _d 0 - 0.118 _d 0*t_k)
           B1 = -1636.75 _d 0 + 12.0408 _d 0*t_k -0.0327957 _d 0*t_k*t_k
           B  = B1 + 3.16528 _d 0*t_k*t_k*t_k*(0.00001 _d 0)
C   "x2" term often neglected (assumed=1) in applications of Weiss's (1974) eq.9
C    x2 = 1 - x1 = 1 - xCO2 (it is very close to 1, but not quite)
           fugf(i,j,bi,bj) = exp( (B+2. _d 0*delta) * Pzatm / zrt)

C -----------------------------------------------------------------------
C       Apply pressure dependence to the dissociation constant of hydrogen
C       sulfate (bisulfate).  Ref: Millero (1995) for pressure correction
           zdvi         =  -18.03 _d 0 + t*(0.0466 _d 0 + t*0.316 _d -3)
           zdki         = ( -4.53 _d 0 + t*0.0900 _d 0)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           aks(i,j,bi,bj) = total2free_surf*aks(i,j,bi,bj) * exp(pfac)

           total2free = 1. _d 0/
     &                 (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

           free2total = (1. _d 0 + st(i,j,bi,bj)/aks(i,j,bi,bj))

C free2sw has an additional component from fluoride
           free2sw    = 1. _d 0
     &                 + st(i,j,bi,bj)/ aks(i,j,bi,bj)

           aks(i,j,bi,bj) = aks(i,j,bi,bj)*free2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to dissociation constant for hydrogen fluoride
C       References: Millero (1995) for pressure correction
           zdvi   =   -9.78 _d 0 + t*(-0.0090 _d 0 - t*0.942 _d -3)
           zdki   = ( -3.91 _d 0 + t*0.054 _d 0)*1. _d -3
           pfac   = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           akf(i,j,bi,bj) = total2free_surf*akf(i,j,bi,bj) * exp(pfac)

C free2sw has an additional component from fluoride add it here
           free2sw = free2sw
     &               + ft(i,j,bi,bj)/akf(i,j,bi,bj)

           sw2free = 1. _d 0 / free2sw

           total2sw = total2free * free2sw

           sw2total = 1. _d 0 / total2sw

           akf(i,j,bi,bj) = akf(i,j,bi,bj)*free2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to 1rst dissociation constant of carbonic acid
C       References: Millero (1982) pressure correction
           zdvi =  -25.50 _d 0 -0.151 _d 0*zds + 0.1271 _d 0*t
           zdki = ( -3.08 _d 0 -0.578 _d 0*zds + 0.0877 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           ak1(i,j,bi,bj) = (total2sw_surf*ak1(i,j,bi,bj)
     &                      * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to 2nd dissociation constant of carbonic acid
C       References: Millero (1979) pressure correction
           zdvi = -15.82 _d 0 + 0.321 _d 0*zds - 0.0219 _d 0*t
           zdki = ( 1.13 _d 0 - 0.314 _d 0*zds - 0.1475 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           ak2(i,j,bi,bj) = (total2sw_surf*ak2(i,j,bi,bj)
     &                      * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the boric acid dissociation constant KB
C       References: Millero (1979) pressure correction
           zdvi      = -29.48 _d 0 + 0.295 _d 0*zds + 0.1622 _d 0*t
     &                 - 0.002608 _d 0*t*t
           zdki      = (-2.84 _d 0 + 0.354 _d 0*zds)*1. _d -3
           pfac =  (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           akb(i,j,bi,bj) = (total2sw_surf*akb(i,j,bi,bj)
     &                      * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to water dissociation constant Kw in
C       (mol/kg-SW)^2. Ref.: Millero (pers. comm. 1996) for pressure correction
           zdvi    =  -20.02 _d 0 + 0.1119 _d 0*t - 0.1409 _d -2*t*t
           zdki    = ( -5.13 _d 0 + 0.0794 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           akw(i,j,bi,bj) = (total2sw_surf*akw(i,j,bi,bj)
     &                      * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the first dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Millero (1995) for pressure correction
           zdvi      =  -14.51 _d 0 + 0.1211 _d 0*t - 0.321 _d -3*t*t
           zdki      = ( -2.67 _d 0 + 0.0427 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           ak1p(i,j,bi,bj) = (total2sw_surf*ak1p(i,j,bi,bj)
     &                       * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the second dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Millero (1995) for pressure correction
           zdvi       =  -23.12 _d 0 + 0.1758 _d 0*t -2.647 _d -3*t*t
           zdki       = ( -5.15 _d 0 +   0.09 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           ak2p(i,j,bi,bj) = (total2sw_surf*ak2p(i,j,bi,bj)
     &                       * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the third dissociation constant
C       of phosphoric acid (H3PO4) in seawater
C       References: Millero (1995) for pressure correction
           zdvi      =  -26.57 _d 0 + 0.2020 _d 0*t -3.042 _d -3*t*t
           zdki      = ( -4.08 _d 0 + 0.0714 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           ak3p(i,j,bi,bj) = (total2sw_surf*ak3p(i,j,bi,bj)
     &                       * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the first dissociation constant
C       of silicic acid (H4SiO4) in seawater
C       References: Millero (1979) pressure correction. Note: Pressure
C        correction estimated to be the same as borate (Millero, 1995)
           zdvi      = -29.48 _d 0 + 0.295 _d 0*zds + 0.1622 _d 0*t
     &                 - 0.002608 _d 0*t*t
           zdki      = (-2.84 _d 0 + 0.354 _d 0*zds)*1. _d -3
           pfac =  (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           aksi(i,j,bi,bj) = (total2sw_surf*aksi(i,j,bi,bj)
     &                       * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the dissociation constant of hydrogen
C       sulfide in sea-water
C       References: Millero (1995) for pressure correction
           zdvi         =  -14.80 _d 0 + t*(0.0020 _d 0 - t*0.400 _d -3)
           zdki         = (  2.89 _d 0 + t*0.054 _d 0)*1. _d -3
           pfac  = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           akhs(i,j,bi,bj) = (total2sw_surf*akhs(i,j,bi,bj)
     &                       * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the dissociation constant
C       of ammonium in sea-water [mol/kg-SW]
C       References: Millero (1995) for pressure correction
           zdvi         =  -26.43 _d 0 + t*(0.0889 _d 0 - t*0.905 _d -3)
           zdki         = ( -5.03 _d 0 + t*0.0814 _d 0)*1. _d -3
           pfac  = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           akn(i,j,bi,bj) = (total2sw_surf*akn(i,j,bi,bj)
     &                      * exp(pfac))*sw2total

C -----------------------------------------------------------------------
C       Apply pressure dependence to the stoichiometric solubility product
C       of calcite in seawater
C       References: Millero (1995) for pressure correction
           zdvi      =  -48.76 _d 0 + 0.5304 _d 0*t
           zdki      = (-11.76 _d 0 + 0.3692 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           Ksp_TP_Calc(i,j,bi,bj) = Ksp_TP_Calc(i,j,bi,bj) * exp(pfac)

C -----------------------------------------------------------------------
C       Apply pressure dependence to the stoichiometric solubility product
C       of aragonite in seawater
C       References: Millero (1979) for pressure correction
           zdvi      =  -48.76 _d 0 + 0.5304 _d 0*t  + 2.8 _d 0
           zdki      = (-11.76 _d 0 + 0.3692 _d 0*t)*1. _d -3
           pfac = (-zdvi + zdki*pressc/2. _d 0)*pressc/zrt
           Ksp_TP_Arag(i,j,bi,bj) = Ksp_TP_Arag(i,j,bi,bj) * exp(pfac)
         else
           bt(i,j,bi,bj)  = 0. _d 0
           st(i,j,bi,bj)  = 0. _d 0
           ft(i,j,bi,bj)  = 0. _d 0
           cat(i,j,bi,bj) = 0. _d 0
           fugf(i,j,bi,bj)= 0. _d 0
           ff(i,j,bi,bj)  = 0. _d 0
           ak0(i,j,bi,bj) = 0. _d 0
           ak1(i,j,bi,bj) = 0. _d 0
           ak2(i,j,bi,bj) = 0. _d 0
           akb(i,j,bi,bj) = 0. _d 0
           ak1p(i,j,bi,bj)= 0. _d 0
           ak2p(i,j,bi,bj)= 0. _d 0
           ak3p(i,j,bi,bj)= 0. _d 0
           aksi(i,j,bi,bj)= 0. _d 0
           akw(i,j,bi,bj) = 0. _d 0
           aks(i,j,bi,bj) = 0. _d 0
           akf(i,j,bi,bj) = 0. _d 0
           akn(i,j,bi,bj) = 0. _d 0
           akhs(i,j,bi,bj)= 0. _d 0
           Ksp_TP_Calc(i,j,bi,bj) = 0. _d 0
           Ksp_TP_Arag(i,j,bi,bj) = 0. _d 0
           aphscale(i,j,bi,bj)    = 0. _d 0
         endif
       enddo
      enddo
#endif /* CARBONCHEM_SOLVESAPHE */

      RETURN
      END
C     END SUBROUTINE DIC_COEFFS_DEEP
C      =========================================================================

C      =========================================================================
      SUBROUTINE EQUATION_AT(p_alktot, p_h,      p_dictot,
     &                       p_bortot, p_po4tot, p_siltot,
     &                       p_nh4tot, p_h2stot, p_so4tot,
     &                       p_flutot, p_eqn   , p_deriveqn,
     &                       i, j, k, bi, bj, myIter,
     &                       myThid )

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C      --------------------
C      Argument variables
C      --------------------

      _RL p_alktot
      _RL p_h
      _RL p_dictot
      _RL p_bortot
      _RL p_po4tot
      _RL p_siltot
      _RL p_nh4tot
      _RL p_h2stot
      _RL p_so4tot
      _RL p_flutot
      _RL p_deriveqn
      _RL p_eqn
      INTEGER i,j,k,bi,bj,myIter,myThid

#ifdef CARBONCHEM_SOLVESAPHE

C      -----------------
C      Local variables
C      -----------------

      _RL znumer_dic, zdnumer_dic, zdenom_dic, zalk_dic, zdalk_dic
      _RL znumer_bor, zdnumer_bor, zdenom_bor, zalk_bor, zdalk_bor
      _RL znumer_po4, zdnumer_po4, zdenom_po4, zalk_po4, zdalk_po4
      _RL znumer_sil, zdnumer_sil, zdenom_sil, zalk_sil, zdalk_sil
      _RL znumer_nh4, zdnumer_nh4, zdenom_nh4, zalk_nh4, zdalk_nh4
      _RL znumer_h2s, zdnumer_h2s, zdenom_h2s, zalk_h2s, zdalk_h2s
      _RL znumer_so4, zdnumer_so4, zdenom_so4, zalk_so4, zdalk_so4
      _RL znumer_flu, zdnumer_flu, zdenom_flu, zalk_flu, zdalk_flu
      _RL                                      zalk_wat

C      H2CO3 - HCO3 - CO3 : n=2, m=0
      znumer_dic = 2. _d 0*ak1(i,j,bi,bj)*ak2(i,j,bi,bj)
     &             + p_h*  ak1(i,j,bi,bj)
      zdenom_dic =      ak1(i,j,bi,bj)*ak2(i,j,bi,bj)
     &             + p_h*(ak1(i,j,bi,bj) + p_h)
      zalk_dic   = p_dictot * (znumer_dic/zdenom_dic)

C      B(OH)3 - B(OH)4 : n=1, m=0
      znumer_bor =      akb(i,j,bi,bj)
      zdenom_bor =      akb(i,j,bi,bj) + p_h
      zalk_bor   = p_bortot * (znumer_bor/zdenom_bor)

C      H3PO4 - H2PO4 - HPO4 - PO4 : n=3, m=1
      znumer_po4 =
     &        3. _d 0*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)*ak3p(i,j,bi,bj)
     &      + p_h*(2. _d 0*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     &               + p_h*ak1p(i,j,bi,bj))
      zdenom_po4 =    ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)*ak3p(i,j,bi,bj)
     &           + p_h*(ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     &           + p_h*(ak1p(i,j,bi,bj) + p_h))
      zalk_po4   = p_po4tot * (znumer_po4/zdenom_po4 - 1. _d 0)
C       Zero level of H3PO4 = 1

C      H4SiO4 - H3SiO4 : n=1, m=0
      znumer_sil =      aksi(i,j,bi,bj)
      zdenom_sil =      aksi(i,j,bi,bj) + p_h
      zalk_sil   = p_siltot * (znumer_sil/zdenom_sil)

C      NH4 - NH3 : n=1, m=0
      znumer_nh4 =      akn(i,j,bi,bj)
      zdenom_nh4 =      akn(i,j,bi,bj) + p_h
      zalk_nh4   = p_nh4tot * (znumer_nh4/zdenom_nh4)

C      H2S - HS : n=1, m=0
      znumer_h2s =      akhs(i,j,bi,bj)
      zdenom_h2s =      akhs(i,j,bi,bj) + p_h
      zalk_h2s   = p_h2stot * (znumer_h2s/zdenom_h2s)

C      HSO4 - SO4 : n=1, m=1
      znumer_so4 =      aks(i,j,bi,bj)
      zdenom_so4 =      aks(i,j,bi,bj) + p_h
      zalk_so4   = p_so4tot * (znumer_so4/zdenom_so4 - 1. _d 0)

C      HF - F : n=1, m=1
      znumer_flu =      akf(i,j,bi,bj)
      zdenom_flu =      akf(i,j,bi,bj) + p_h
      zalk_flu   = p_flutot * (znumer_flu/zdenom_flu - 1. _d 0)

C      H2O - OH
      zalk_wat   = akw(i,j,bi,bj)/p_h - p_h/aphscale(i,j,bi,bj)

C      EQUATION_AT =    zalk_dic + zalk_bor + zalk_po4 + zalk_sil &
C              + zalk_nh4 + zalk_h2s + zalk_so4 + zalk_flu &
C                  + zalk_wat - p_alktot
      p_eqn =   zalk_dic + zalk_bor + zalk_po4 + zalk_sil
     &         + zalk_nh4 + zalk_h2s + zalk_so4 + zalk_flu
     &         + zalk_wat - p_alktot

C      IF(PRESENT(p_deriveqn)) THEN

C      H2CO3 - HCO3 - CO3 : n=2
        zdnumer_dic = ak1(i,j,bi,bj)*ak1(i,j,bi,bj)*ak2(i,j,bi,bj)
     &                + p_h*(4. _d 0*ak1(i,j,bi,bj)*ak2(i,j,bi,bj)
     &                               + p_h*      ak1(i,j,bi,bj))
        zdalk_dic   = -p_dictot*(zdnumer_dic/zdenom_dic**2)

C      B(OH)3 - B(OH)4 : n=1
        zdnumer_bor = akb(i,j,bi,bj)
        zdalk_bor   = -p_bortot*(zdnumer_bor/zdenom_bor**2)

C      H3PO4 - H2PO4 - HPO4 - PO4 : n=3
        zdnumer_po4 = ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     &               *ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     &               *ak3p(i,j,bi,bj)
     & + p_h*(4. _d 0*ak1p(i,j,bi,bj)*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     &               *ak3p(i,j,bi,bj)
     & + p_h*(9. _d 0*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)*ak3p(i,j,bi,bj)
     &              + ak1p(i,j,bi,bj)*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     & + p_h*(4. _d 0*ak1p(i,j,bi,bj)*ak2p(i,j,bi,bj)
     & + p_h*         ak1p(i,j,bi,bj))))
        zdalk_po4   = -p_po4tot * (zdnumer_po4/zdenom_po4**2)

C      H4SiO4 - H3SiO4 : n=1
        zdnumer_sil = aksi(i,j,bi,bj)
        zdalk_sil   = -p_siltot * (zdnumer_sil/zdenom_sil**2)

C      NH4 - NH3 : n=1
        zdnumer_nh4 = akn(i,j,bi,bj)
        zdalk_nh4   = -p_nh4tot * (zdnumer_nh4/zdenom_nh4**2)

C      H2S - HS : n=1
        zdnumer_h2s = akhs(i,j,bi,bj)
        zdalk_h2s   = -p_h2stot * (zdnumer_h2s/zdenom_h2s**2)

C      HSO4 - SO4 : n=1
        zdnumer_so4 = aks(i,j,bi,bj)
        zdalk_so4   = -p_so4tot * (zdnumer_so4/zdenom_so4**2)

C      HF - F : n=1
        zdnumer_flu = akf(i,j,bi,bj)
        zdalk_flu   = -p_flutot * (zdnumer_flu/zdenom_flu**2)

        p_deriveqn = zdalk_dic + zdalk_bor + zdalk_po4 + zdalk_sil
     &             + zdalk_nh4 + zdalk_h2s + zdalk_so4 + zdalk_flu
     &             - akw(i,j,bi,bj)/p_h**2 - 1. _d 0/aphscale(i,j,bi,bj)
C      ENDIF

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C     END SUBROUTINE EQUATION_AT
C      =========================================================================

C      =========================================================================
      SUBROUTINE SOLVE_AT_FAST(p_alktot, p_dictot, p_bortot,
     &                         p_po4tot, p_siltot, p_nh4tot,
     &                         p_h2stot, p_so4tot, p_flutot,
     &                         p_hini,   p_val,    p_hnew,
     &                         i, j, k, bi, bj,
     &                         debugPrt, myIter, myThid )
C      =========================================================================
C      Fast version of SOLVE_AT_GENERAL, without any bounds checking.

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C     _RL SOLVE_AT_FAST

C     --------------------
C     Argument variables
C     --------------------

      _RL p_alktot
      _RL p_dictot
      _RL p_bortot
      _RL p_po4tot
      _RL p_siltot
      _RL p_nh4tot
      _RL p_h2stot
      _RL p_so4tot
      _RL p_flutot
      _RL p_hini
      _RL p_val
      _RL p_hnew
      INTEGER i, j, k, bi, bj
      LOGICAL debugPrt
      INTEGER myIter, myThid

#ifdef CARBONCHEM_SOLVESAPHE

C     --------------------
C     Local variables
C     --------------------

      _RL zh_ini, zh, zh_prev, zh_lnfactor
      _RL zhdelta
      _RL zeqn, zdeqndh

C     LOGICAL l_exitnow
      LOGICAL iterate4pH
      _RL pz_exp_threshold
      _RL pp_rdel_ah_target
      INTEGER niter_atfast

      pp_rdel_ah_target = 1. _d -8
      pz_exp_threshold = 1.0 _d 0
C      =========================================================================

C     IF(PRESENT(p_hini)) THEN
      zh_ini = p_hini
C     ELSE
C     #if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_FAST] Calling AHINI_FOR_AT for h_ini'
C     #endif
C
C        CALL AHINI_FOR_AT(p_alktot, p_dictot, p_bortot, zh_ini)
C
C     #if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_FAST] h_ini :', zh_ini
C     #endif
C     ENDIF

      zh = zh_ini
C Reset counters of iterations
      niter_atfast    = 0

      iterate4pH = .TRUE.
      DO WHILE ( iterate4pH )

       niter_atfast = niter_atfast + 1
       IF ( niter_atfast .GT. at_maxniter ) THEN
         zh = -1. _d 0
         iterate4pH = .FALSE.
       ELSE

         zh_prev = zh

C        zeqn = EQUATION_AT(p_alktot, zh,       p_dictot, p_bortot,
C     &                      p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                      p_so4tot, p_flutot, P_DERIVEQN = zdeqndh)

        CALL EQUATION_AT( p_alktot, zh      , p_dictot, p_bortot,
     &                    p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                    p_so4tot, p_flutot,
     &                    zeqn    , zdeqndh,
     &                    i, j, k, bi, bj, myIter, myThid )
C zh is the root
        iterate4pH = ( zeqn .NE. 0. _d 0 )
       ENDIF

       IF ( iterate4pH ) THEN
        zh_lnfactor = -zeqn/(zdeqndh*zh_prev)
        IF(ABS(zh_lnfactor) .GT. pz_exp_threshold) THEN
           zh          = zh_prev*EXP(zh_lnfactor)
        ELSE
           zhdelta     = zh_lnfactor*zh_prev
           zh          = zh_prev + zhdelta
        ENDIF

C     #if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_FAST] testing zh :', zh, zeqn, zh_lnfactor
C     #endif
C
C      ! Stop iterations once |\delta{[H]}/[H]| < rdel
C      ! <=> |(zh - zh_prev)/zh_prev| = |EXP(-zeqn/(zdeqndh*zh_prev)) -1| < rdel
C      ! |EXP(-zeqn/(zdeqndh*zh_prev)) -1| ~ |zeqn/(zdeqndh*zh_prev)|
C
C      ! Alternatively:
C      ! |\Delta pH| = |zeqn/(zdeqndh*zh_prev*LOG(10))|
C      !             ~ 1/LOG(10) * |\Delta [H]|/[H]
C      !             < 1/LOG(10) * rdel
C
C      ! Hence |zeqn/(zdeqndh*zh)| < rdel
C
C      ! rdel <- pp_rdel_ah_target

C        l_exitnow = (ABS(zh_lnfactor) < pp_rdel_ah_target)
C        IF(l_exitnow) EXIT
        iterate4pH = ( ABS(zh_lnfactor) .GE. pp_rdel_ah_target )

       ENDIF
      ENDDO

C     SOLVE_AT_FAST = zh
      p_hnew = zh

C     IF(PRESENT(p_val)) THEN
        IF(zh .GT. 0. _d 0) THEN
C           p_val = EQUATION_AT(p_alktot, zh,       p_dictot, p_bortot,
C     &                          p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                          p_so4tot, p_flutot)
           CALL EQUATION_AT( p_alktot, zh,       p_dictot, p_bortot,
     &                       p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                       p_so4tot, p_flutot,
     &                       p_val   , zdeqndh,
     &                       i, j, k, bi, bj, myIter, myThid )
        ELSE
c          p_val = HUGE(1. _d 0)
           p_val = 1. _d +65
        ENDIF
C     ENDIF

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C END FUNCTION SOLVE_AT_FAST
C      =========================================================================

C      =========================================================================
      SUBROUTINE SOLVE_AT_GENERAL(p_alktot, p_dictot, p_bortot,
     &                         p_po4tot, p_siltot, p_nh4tot,
     &                         p_h2stot, p_so4tot, p_flutot,
     &                         p_hini,   p_val,    p_hnew,
     &                         i, j, k, bi, bj,
     &                         debugPrt, myIter, myThid )
C      Universal pH solver that converges from any given initial value,
C      determines upper an lower bounds for the solution if required

      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C      _RL SOLVE_AT_GENERAL

C     --------------------
C     Argument variables
C     --------------------

       _RL p_alktot
       _RL p_dictot
       _RL p_bortot
       _RL p_po4tot
       _RL p_siltot
       _RL p_nh4tot
       _RL p_h2stot
       _RL p_so4tot
       _RL p_flutot
       _RL p_hini
       _RL p_hnew
       _RL p_val
      INTEGER i, j, k, bi, bj
      LOGICAL debugPrt
      INTEGER myIter, myThid

#ifdef CARBONCHEM_SOLVESAPHE

C     -----------------
C     Local variables
C     -----------------

      _RL zh_ini, zh, zh_prev, zh_lnfactor
      _RL p_alknw_inf, p_alknw_sup
      _RL zh_min, zh_max
      _RL zdelta, zh_delta
      _RL zeqn, zdeqndh, zeqn_absmin
      INTEGER niter_atgen

C      LOGICAL       :: l_exitnow
      LOGICAL iterate4pH
      _RL pz_exp_threshold
      _RL pp_rdel_ah_target

      pp_rdel_ah_target = 1. _d -8
      pz_exp_threshold = 1. _d 0

C      IF(PRESENT(p_hini)) THEN
        zh_ini = p_hini
C      ELSE
C#if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_GENERAL] Calling AHINI_FOR_AT for h_ini'
C#endif
C        CALL AHINI_FOR_AT(p_alktot, p_dictot, p_bortot, zh_ini)
C#if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_GENERAL] h_ini :', zh_ini
C#endif
C      ENDIF
#ifdef ALLOW_DEBUG
      IF (debugPrt) CALL DEBUG_CALL('ANW_INFSUP',myThid)
#endif
      CALL ANW_INFSUP(p_dictot, p_bortot,
     &                p_po4tot, p_siltot,  p_nh4tot, p_h2stot,
     &                p_so4tot, p_flutot,
     &                p_alknw_inf, p_alknw_sup,
     &                i, j, k, bi, bj, myIter, myThid )

      zdelta = (p_alktot-p_alknw_inf)**2 + 4. _d 0*akw(i,j,bi,bj)
     &         /aphscale(i,j,bi,bj)

      IF(p_alktot .GE. p_alknw_inf) THEN
        zh_min = 2. _d 0*akw(i,j,bi,bj) /( p_alktot-p_alknw_inf
     &           + SQRT(zdelta) )
      ELSE
        zh_min = aphscale(i,j,bi,bj)*(-(p_alktot-p_alknw_inf)
     &           + SQRT(zdelta) ) / 2. _d 0
      ENDIF

      zdelta = (p_alktot-p_alknw_sup)**2 + 4. _d 0*akw(i,j,bi,bj)
     &         /aphscale(i,j,bi,bj)

      IF(p_alktot .LE. p_alknw_sup) THEN
        zh_max = aphscale(i,j,bi,bj)*(-(p_alktot-p_alknw_sup)
     &           + SQRT(zdelta) ) / 2. _d 0
      ELSE
        zh_max = 2. _d 0*akw(i,j,bi,bj) /( p_alktot-p_alknw_sup
     &           + SQRT(zdelta) )
      ENDIF

C#if defined(DEBUG_PHSOLVERS)
C           PRINT*, '[SOLVE_AT_GENERAL] h_min :', zh_min
C           PRINT*, '[SOLVE_AT_GENERAL] h_max :', zh_max
C#endif

      zh = MAX(MIN(zh_max, zh_ini), zh_min)
C     Uncomment this line for the "safe" initialisation test
C      zh = SQRT(zh_max*zh_min)

C     Reset counters of iterations
      niter_atgen       = 0
c     zeqn_absmin       = HUGE(1. _d 0)
      zeqn_absmin       = 1. _d +65

      iterate4pH = .TRUE.
      DO WHILE ( iterate4pH )

       IF ( niter_atgen .GE. at_maxniter ) THEN
           zh = -1. _d 0
           iterate4pH = .FALSE.
       ELSE

        zh_prev = zh
#ifdef ALLOW_DEBUG
        IF (debugPrt) CALL DEBUG_CALL('EQUATION_AT',myThid)
#endif
C        zeqn = EQUATION_AT(p_alktot, zh,      p_dictot, p_bortot,
C     &                   p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                   p_so4tot, p_flutot, P_DERIVEQN = zdeqndh)
        CALL EQUATION_AT( p_alktot, zh,       p_dictot, p_bortot,
     &                    p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                    p_so4tot, p_flutot,
     &                    zeqn   , zdeqndh,
     &                    i, j, k, bi, bj, myIter, myThid )

C         Adapt bracketing interval
        IF(zeqn .GT. 0. _d 0) THEN
           zh_min = zh_prev
        ELSEIF(zeqn .LT. 0. _d 0) THEN
           zh_max = zh_prev
        ELSE
C          zh is the root; unlikely but, one never knows
           iterate4pH = .FALSE.
        ENDIF
       ENDIF

       IF ( iterate4pH ) THEN
C         Now determine the next iterate zh
        niter_atgen = niter_atgen + 1

        IF(ABS(zeqn) .GE. 0.5 _d 0*zeqn_absmin) THEN
C          if the function evaluation at the current point is
C          not decreasing faster than with a bisection step (at least linearly)
C          in absolute value take one bisection step on [ph_min, ph_max]
C          ph_new = (ph_min + ph_max)/2 _d 0
C          In terms of [H]_new:
C            [H]_new = 10**(-ph_new)
C                   = 10**(-(ph_min + ph_max)/2 _d 0)
C                   = SQRT(10**(-(ph_min + phmax)))
C                   = SQRT(zh_max * zh_min)
           zh          = SQRT(zh_max * zh_min)
C Required to test convergence below
           zh_lnfactor = (zh - zh_prev)/zh_prev
        ELSE
C          dzeqn/dpH = dzeqn/d[H] * d[H]/dpH
C                   = -zdeqndh * LOG(10) * [H]
C          \Delta pH = -zeqn/(zdeqndh*d[H]/dpH) = zeqn/(zdeqndh*[H]*LOG(10))

C          pH_new = pH_old + \deltapH

C          [H]_new = 10**(-pH_new)
C                 = 10**(-pH_old - \Delta pH)
C                 = [H]_old * 10**(-zeqn/(zdeqndh*[H]_old*LOG(10)))
C                 = [H]_old * EXP(-LOG(10)*zeqn/(zdeqndh*[H]_old*LOG(10)))
C                 = [H]_old * EXP(-zeqn/(zdeqndh*[H]_old))
           zh_lnfactor = -zeqn/(zdeqndh*zh_prev)

           IF(ABS(zh_lnfactor) .GT. pz_exp_threshold) THEN
             zh          = zh_prev*EXP(zh_lnfactor)
           ELSE
             zh_delta    = zh_lnfactor*zh_prev
             zh          = zh_prev + zh_delta
           ENDIF

C#if defined(DEBUG_PHSOLVERS)
C           PRINT*, '[SOLVE_AT_GENERAL] testing zh :', zh, zeqn, zh_lnfactor
C#endif

           IF( zh .LT. zh_min ) THEN
C              if [H]_new < [H]_min
C              i.e., if ph_new > ph_max then
C              take one bisection step on [ph_prev, ph_max]
C              ph_new = (ph_prev + ph_max)/2 _d 0
C              In terms of [H]_new:
C              [H]_new = 10**(-ph_new)
C                     = 10**(-(ph_prev + ph_max)/2 _d 0)
C                     = SQRT(10**(-(ph_prev + phmax)))
C                     = SQRT([H]_old*10**(-ph_max))
C                     = SQRT([H]_old * zh_min)
             zh               = SQRT(zh_prev * zh_min)
C Required to test convergence below
             zh_lnfactor      = (zh - zh_prev)/zh_prev
           ENDIF

           IF( zh .GT. zh_max ) THEN
C              if [H]_new > [H]_max
C              i.e., if ph_new < ph_min, then
C              take one bisection step on [ph_min, ph_prev]
C              ph_new = (ph_prev + ph_min)/2 _d 0
C              In terms of [H]_new:
C              [H]_new = 10**(-ph_new)
C                     = 10**(-(ph_prev + ph_min)/2 _d 0)
C                     = SQRT(10**(-(ph_prev + ph_min)))
C                     = SQRT([H]_old*10**(-ph_min))
C                     = SQRT([H]_old * zhmax)
             zh               = SQRT(zh_prev * zh_max)
C Required to test convergence below
             zh_lnfactor      = (zh - zh_prev)/zh_prev

           ENDIF
        ENDIF

        zeqn_absmin = MIN( ABS(zeqn), zeqn_absmin)
C        Stop iterations once |\delta{[H]}/[H]| < rdel
C        <=> |(zh - zh_prev)/zh_prev| = |EXP(-zeqn/(zdeqndh*zh_prev)) -1| < rdel
C        |EXP(-zeqn/(zdeqndh*zh_prev)) -1| ~ |zeqn/(zdeqndh*zh_prev)|
C
C        Alternatively:
C        |\Delta pH| = |zeqn/(zdeqndh*zh_prev*LOG(10))|
C                    ~ 1/LOG(10) * |\Delta [H]|/[H]
C                    < 1/LOG(10) * rdel
C
C        Hence |zeqn/(zdeqndh*zh)| < rdel
C
C        rdel <-- pp_rdel_ah_target
C        l_exitnow = (ABS(zh_lnfactor) < pp_rdel_ah_target)
C        IF(l_exitnow) EXIT
         iterate4pH = ( ABS(zh_lnfactor) .GE. pp_rdel_ah_target )

       ENDIF
      ENDDO

C      SOLVE_AT_GENERAL = zh
      p_hnew = zh

C      IF(PRESENT(p_val)) THEN
C           p_val = EQUATION_AT(p_alktot, zh,      p_dictot, p_bortot,
C     &                       p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                       p_so4tot, p_flutot)
C        ELSE
C           p_val = HUGE(1. _d 0)
C       ENDIF
C      ENDIF
#ifdef ALLOW_DEBUG
      IF (debugPrt) CALL DEBUG_LEAVE('SOLVE_AT_GENERAL',myThid)
#endif
#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C      END FUNCTION SOLVE_AT_GENERAL
C      =========================================================================

C      =========================================================================
      SUBROUTINE SOLVE_AT_GENERAL_SEC(p_alktot, p_dictot,
     &                           p_bortot, p_po4tot,
     &                           p_siltot, p_nh4tot,
     &                           p_h2stot, p_so4tot,
     &                           p_flutot, p_hini,
     &                           p_val,    p_hnew,
     &                           i, j, k, bi, bj,
     &                           debugPrt, myIter, myThid )

C      Universal pH solver that converges from any given initial value,
C      determines upper an lower bounds for the solution if required
      IMPLICIT NONE

C     MITgcm GLobal variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIC_VARS.h"

C     --------------------
C     Argument variables
C     --------------------

      _RL p_alktot
      _RL p_dictot
      _RL p_bortot
      _RL p_po4tot
      _RL p_siltot
      _RL p_nh4tot
      _RL p_h2stot
      _RL p_so4tot
      _RL p_flutot
      _RL p_hini
      _RL p_hnew
      _RL p_val
      INTEGER i, j, k, bi, bj
      LOGICAL debugPrt
      INTEGER myIter, myThid

#ifdef CARBONCHEM_SOLVESAPHE

C     -----------------
C     Local variables
C     -----------------

      _RL  zh_ini, zh, zh_1, zh_2, zh_delta
      _RL  p_alknw_inf, p_alknw_sup
      _RL  zh_min, zh_max
      _RL  zeqn, zeqn_1, zeqn_2, zeqn_absmin
      _RL  zdelta, zdeqndh
      _RL pp_rdel_ah_target
      INTEGER niter_atsec
C      LOGICAL       ::  l_exitnow
      LOGICAL iterate4pH

      pp_rdel_ah_target = 1. _d -8

C      IF(PRESENT(p_hini)) THEN
        zh_ini = p_hini
C      ELSE
C#if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_GENERAL_SEC] Calling AHINI_FOR_AT for h_ini'
C#endif
C       CALL AHINI_FOR_AT(p_alktot, p_dictot, p_bortot, zh_ini)
C#if defined(DEBUG_PHSOLVERS)
C        PRINT*, '[SOLVE_AT_GENERAL_SEC] h_ini :', zh_ini
C#endif
C      ENDIF

      CALL ANW_INFSUP(p_dictot, p_bortot,
     &               p_po4tot, p_siltot,  p_nh4tot, p_h2stot,
     &               p_so4tot, p_flutot,
     &               p_alknw_inf, p_alknw_sup,
     &               i, j, k, bi, bj, myIter, myThid )

      zdelta = (p_alktot-p_alknw_inf)**2 + 4. _d 0*akw(i,j,bi,bj)
     &         /aphscale(i,j,bi,bj)

      IF(p_alktot .GE. p_alknw_inf) THEN
        zh_min = 2. _d 0*akw(i,j,bi,bj) /( p_alktot-p_alknw_inf
     &           + SQRT(zdelta) )
      ELSE
        zh_min = aphscale(i,j,bi,bj)*(-(p_alktot-p_alknw_inf)
     &           + SQRT(zdelta) ) / 2. _d 0
      ENDIF

      zdelta = (p_alktot-p_alknw_sup)**2 + 4. _d 0*akw(i,j,bi,bj)
     &         /aphscale(i,j,bi,bj)

      IF(p_alktot .LE. p_alknw_sup) THEN
        zh_max = aphscale(i,j,bi,bj)*(-(p_alktot-p_alknw_sup)
     &           + SQRT(zdelta) ) / 2. _d 0
      ELSE
        zh_max = 2. _d 0*akw(i,j,bi,bj) /( p_alktot-p_alknw_sup
     &           + SQRT(zdelta) )
      ENDIF

C#if defined(DEBUG_PHSOLVERS)
C           PRINT*, '[SOLVE_AT_GENERAL_SEC] h_min :', zh_min
C           PRINT*, '[SOLVE_AT_GENERAL_SEC] h_max :', zh_max
C#endif

C     Uncomment this line for the "safe" initialisation test
      zh = MAX(MIN(zh_max, zh_ini), zh_min)
C      zh = SQRT(zh_max*zh_min)

C      Reset counters of iterations
      niter_atsec       = 0

C      Prepare the secant iterations: two initial (zh, zeqn) pairs are required
C      We have the starting value, that needs to be completed by the evaluation
C      of the equation value it produces.
C      Complete the initial value with its equation evaluation
C      (will take the role of the $n-2$ iterate at the first secant evaluation)

C     zh_2 is the initial value
      zh_2   = zh
C      zeqn_2 = EQUATION_AT(p_alktot, zh_2,     p_dictot, p_bortot,
C     &                   p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                   p_so4tot, p_flutot)
      CALL EQUATION_AT( p_alktot, zh_2,    p_dictot, p_bortot,
     &                  p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                  p_so4tot, p_flutot,
     &                  zeqn_2  , zdeqndh,
     &                  i, j, k, bi, bj, myIter, myThid )

      zeqn_absmin       = ABS(zeqn_2)

C      Adapt bracketing interval and heuristically set zh_1

      IF(zeqn_2 .LT. 0. _d 0) THEN
C                 If zeqn_2 < 0, then we adjust zh_max:
C                 we can be sure that zh_min < zh_2 < zh_max.
        zh_max = zh_2
C                 for zh_1, try 25% (0.1 pH units) below the current zh_max,
C                 but stay above SQRT(zh_min*zh_max), which would be equivalent
C                 to a bisection step on [pH@zh_min, pH@zh_max]
        zh_1   = MAX(zh_max/1.25 _d 0, SQRT(zh_min*zh_max))

      ELSEIF(zeqn_2 .GT. 0. _d 0) THEN
C                 If zeqn_2 < 0, then we adjust zh_min:
C                 we can be sure that zh_min < zh_2 < zh_max.
        zh_min = zh_2
C                 for zh_1, try 25% (0.1 pH units) above the current zh_min,
C                 but stay below SQRT(zh_min*zh_max) which would be equivalent
C                 to a bisection step on [pH@zh_min, pH@zh_max]
        zh_1   = MIN(zh_min*1.25 _d 0, SQRT(zh_min*zh_max))

      ELSE
C       we have got the root; unlikely, but one never knows
C        SOLVE_AT_GENERAL_SEC = zh_2
         p_hnew = zh_2
C        IF(PRESENT(p_val)) p_val = zeqn_2
         p_val = zeqn_2
        RETURN
      ENDIF

C      We now have the first pair completed (zh_2, zeqn_2).
C      Define the second one (zh_1, zeqn_1), which is also the first iterate.
C      zh_1 has already been set above

C     Update counter of iterations
      niter_atsec = 1

C      zeqn_1 = EQUATION_AT(p_alktot, zh_1,      p_dictot, p_bortot,
C     &                   p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                   p_so4tot, p_flutot)
      CALL EQUATION_AT( p_alktot, zh_1,     p_dictot, p_bortot,
     &                  p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                  p_so4tot, p_flutot,
     &                  zeqn_1  , zdeqndh,
     &                  i, j, k, bi, bj, myIter, myThid )

C      Adapt bracketing interval: we know that zh_1 <= zh <= zh_max (if zeqn_1>0)
C      or zh_min <= zh <= zh_1 (if zeqn_1 < 0), so this can always be done

      IF(zeqn_1 .GT. 0. _d 0) THEN
        zh_min = zh_1
      ELSEIF(zeqn_1 .LT. 0. _d 0) THEN
        zh_max = zh_1
      ELSE
C      zh_1 is the root
C        SOLVE_AT_GENERAL_SEC = zh_1
        p_hnew = zh_1
C        IF(PRESENT(p_val)) p_val = zeqn_1
        p_val = zeqn_1
      ENDIF

      IF(ABS(zeqn_1) .GT. zeqn_absmin) THEN
C     Swap zh_2 and zh_1 if ABS(zeqn_2) < ABS(zeqn_1)
C                 so that zh_2 and zh_1 lead to decreasing equation
C                 values (in absolute value)
        zh     = zh_1
        zeqn   = zeqn_1
        zh_1   = zh_2
        zeqn_1 = zeqn_2
        zh_2   = zh
        zeqn_2 = zeqn
      ELSE
        zeqn_absmin = ABS(zeqn_1)
      ENDIF

C     Pre-calculate the first secant iterate (this is the second iterate)
      niter_atsec = 2

      zh_delta = -zeqn_1/((zeqn_2-zeqn_1)/(zh_2 - zh_1))
      zh = zh_1 + zh_delta

C      Make sure that zh_min < zh < zh_max (if not,
C      bisect around zh_1 which is the best estimate)

      IF (zh .GT. zh_max) THEN
C       This can only happen if zh_2 < zh_1
C                 and zeqn_2 > zeqn_1 > 0
        zh = SQRT(zh_1*zh_max)
      ENDIF

      IF (zh .LT. zh_min) THEN
C       This can only happen if zh_2 > zh_1
C                and zeqn_2 < zeqn_1 < 0
        zh = SQRT(zh_1*zh_min)
      ENDIF

      iterate4pH = .TRUE.
      DO WHILE ( iterate4pH )

       IF ( niter_atsec .GE. at_maxniter ) THEN
           zh = -1. _d 0
           iterate4pH = .FALSE.
       ELSE

C        zeqn = EQUATION_AT(p_alktot, zh, p_dictot, p_bortot,
C      &                  p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C      &                  p_so4tot, p_flutot)
        CALL EQUATION_AT( p_alktot, zh,       p_dictot, p_bortot,
     &                    p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                    p_so4tot, p_flutot,
     &                    zeqn    , zdeqndh,
     &                    i, j, k, bi, bj, myIter, myThid )

C       Adapt bracketing interval: since initially, zh_min <= zh <= zh_max
C       we are sure that zh will improve either bracket, depending on the sign
C       of zeqn
        IF(zeqn .GT. 0. _d 0) THEN
          zh_min = zh
        ELSEIF(zeqn .LT. 0. _d 0) THEN
          zh_max = zh
        ELSE
C         zh is the root
          iterate4pH = .FALSE.
        ENDIF
       ENDIF

       IF ( iterate4pH ) THEN
C      Start calculation of next iterate
        niter_atsec = niter_atsec + 1

        zh_2   = zh_1
        zeqn_2 = zeqn_1
        zh_1   = zh
        zeqn_1 = zeqn

        IF(ABS(zeqn) .GE. 0.5 _d 0*zeqn_absmin) THEN
C            if the function evaluation at the current point
C            is not decreasing faster in absolute value than
C            we may expect for a bisection step, then take
C            one bisection step on [ph_min, ph_max]
C            ph_new = (ph_min + ph_max)/2 _d 0
C            In terms of [H]_new:
C            [H]_new = 10**(-ph_new)
C                   = 10**(-(ph_min + ph_max)/2 _d 0)
C                   = SQRT(10**(-(ph_min + phmax)))
C                   = SQRT(zh_max * zh_min)
           zh              = SQRT(zh_max * zh_min)
           zh_delta          = zh - zh_1
        ELSE
C            \Delta H = -zeqn_1*(h_2 - h_1)/(zeqn_2 - zeqn_1)
C            H_new = H_1 + \Delta H
           zh_delta = -zeqn_1/((zeqn_2-zeqn_1)/(zh_2 - zh_1))
           zh      = zh_1 + zh_delta

C#if defined(DEBUG_PHSOLVERS)
C           PRINT*, '[SOLVE_AT_GENERAL_SEC] testing zh :', zh, zeqn, zh_delta
C#endif
           IF( zh .LT. zh_min ) THEN
C              if [H]_new < [H]_min
C              i.e., if ph_new > ph_max then
C              take one bisection step on [ph_prev, ph_max]
C              ph_new = (ph_prev + ph_max)/2 _d 0
C              In terms of [H]_new:
C              [H]_new = 10**(-ph_new)
C                     = 10**(-(ph_prev + ph_max)/2 _d 0)
C                     = SQRT(10**(-(ph_prev + phmax)))
C                     = SQRT([H]_old*10**(-ph_max))
C                     = SQRT([H]_old * zh_min)
             zh              = SQRT(zh_1 * zh_min)
             zh_delta         = zh - zh_1
           ENDIF

           IF( zh .GT. zh_max ) THEN
C              if [H]_new > [H]_max
C              i.e., if ph_new < ph_min, then
C              take one bisection step on [ph_min, ph_prev]
C              ph_new = (ph_prev + ph_min)/2 _d 0
C              In terms of [H]_new:
C              [H]_new = 10**(-ph_new)
C                     = 10**(-(ph_prev + ph_min)/2 _d 0)
C                     = SQRT(10**(-(ph_prev + ph_min)))
C                     = SQRT([H]_old*10**(-ph_min))
C                     = SQRT([H]_old * zhmax)
             zh               = SQRT(zh_1 * zh_max)
             zh_delta          = zh - zh_1
           ENDIF
        ENDIF

        zeqn_absmin = MIN(ABS(zeqn), zeqn_absmin)

C       Stop iterations once |([H]-[H_1])/[H_1]| < rdel
C        l_exitnow = (ABS(zh_delta) < pp_rdel_ah_target*zh_1)
C        IF(l_exitnow) EXIT
        iterate4pH = ( ABS(zh_delta) .GE. pp_rdel_ah_target*zh_1 )

       ENDIF
      ENDDO

C      SOLVE_AT_GENERAL_SEC = zh
      p_hnew = zh

C      IF(PRESENT(p_val)) THEN
       IF(zh .GT. 0. _d 0) THEN
C           p_val = EQUATION_AT(p_alktot, zh,      p_dictot, p_bortot,
C     &                       p_po4tot, p_siltot, p_nh4tot, p_h2stot,
C     &                       p_so4tot, p_flutot)
          CALL EQUATION_AT( p_alktot, zh,       p_dictot, p_bortot,
     &                      p_po4tot, p_siltot, p_nh4tot, p_h2stot,
     &                      p_so4tot, p_flutot,
     &                      p_val   , zdeqndh,
     &                      i, j, k, bi, bj, myIter, myThid )
       ELSE
c         p_val = HUGE(1. _d 0)
          p_val = 1. _d +65
       ENDIF
C      ENDIF

#endif /* CARBONCHEM_SOLVESAPHE */
      RETURN
      END
C      END FUNCTION SOLVE_AT_GENERAL_SEC
C      =========================================================================
