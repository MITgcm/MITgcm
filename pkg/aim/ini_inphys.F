C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/ini_inphys.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE INPHYS (FSG,HSG,RLAT)

      IMPLICIT rEAL*8 ( A-H,O-Z)

C--
C--   SUBROUTINE INPHYS (FSG,HSG,RLAT)
C--
C--   Purpose: Initialize common blocks for physical parametrization routines 
C--   Input :  FSG  : sigma at full levels
C--            HSG  : sigma at half levels
C--            RLAT : gaussian-grid latitudes
C--   Initialized common blocks: PHYCON, FSIGMU, VDICON,
C--                              FORCON, SFLCON, CNVCON, LSCCON, RADCON
C--
C     Resolution parameters
C
C
#include "atparam.h"
#include "atparam1.h"
C
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C
#include "Lev_def.h"

#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C
C     Physical constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Constants for sub-grid-scale physics
C
#include "com_forcon.h"
#include "com_sflcon.h"
#include "com_cnvcon.h"
#include "com_lsccon.h"
#include "com_radcon.h"  
#include "com_vdicon.h"  
C
C     == Routine Arguments ==
      REAL FSG(NLEV), HSG(0:NLEV), RLAT(NLAT)  

C     == Local Variables ==
      INTEGER I2
      INTEGER bi, bj
      DATA Pground /1. _d +5/

C     == Set for a single tile per proc. for now
      bi = 1
      bj = 1
C
C---  1. Time independent parameters and arrays
C
C     1.1 Physical constants
C
      P0 = 1. _d +5
      GG = 9.81 _d 0
      RD = 287. _d 0
      CP = 1004. _d 0
C     Latent heat is in J/g for consistency with spec.hum. in g/Kg
      ALHC = 2501. _d 0
      SBC = 5.67 _d -8
C
C     1.2 Functions of sigma and latitude
C
      SIGH(0)=HSG(0)
C
      DO K=1,NLEV
       SIG(K)  = FSG(K)
Cg77   SIGL(K) = DLOG(FSG(K))
       SIGL(K) = LOG(FSG(K))
       SIGH(K) = HSG(K)
       DSIG(K) = HSG(K)-HSG(K-1)
C      POUT(K) = PRLEV(FSG(K))
       GRDSIG(K) = GG/(DSIG(K)*P0)
       GRDSCP(K) = GRDSIG(K)/CP
      ENDDO
C
C     Weights for vertical interpolation at half-levels(1,nlev) and surface
C     Note that for phys.par. half-lev(k) is between full-lev k and k+1 
C     Fhalf(k) = Ffull(k)+WVI(K,2)*(Ffull(k+1)-Ffull(k))
C     Fsurf = Ffull(nlev)+WVI(nlev,2)*(Ffull(nlev)-Ffull(nlev-1))
C
      DO K=1,NLEV-1
       WVI(K,1)=1./(SIGL(K+1)-SIGL(K))
       WVI(K,2)=(DLOG(SIGH(K))-SIGL(K))*WVI(K,1)
      ENDDO
C
      WVI(NLEV,1)=0.
      WVI(NLEV,2)=-SIGL(NLEV)*WVI(NLEV-1,2)
cchdbg
      WVI(NLEV,2)=-SIGL(NLEV)*WVI(NLEV-1,1)
cchdbg
C
      DO J=1,NLAT
       FMU(J,1)=SIN(RLAT(J))
       FMU(J,2)=1.5 _d 0*FMU(J,1)**2-0.5 _d 0
      ENDDO
C
C---  2. Constants for boundary forcing (common FORCON):
C
      SOLC   = 342. _d 0

      ALBSEA = 0.07 _d 0
      ALBICE = 0.60 _d 0
      ALBSN  = 0.60 _d 0
      SDALB  = 60. _d 0
      SWCAP = 75. _d 0
      SWWIL =  0. _d 0
C
C---  3. Constants for surface fluxes (common SFLCON):
C
      FWIND0 = 0.6 _d 0
cchdbg *****************************************************
      FWIND0 = 0.6 _d 0*0.7 _d 0
cchdbg ******************************************************
cchdbg      FTEMP0 = 1.
cchdbg      FHUM0  = 1.
      FTEMP0 = 0. _d 0
      FHUM0  = 0.5 _d 0
      FHUM0  = 1. _d 0

c      CDL = 3.0E-3
c      CDS = 1.2E-3
c      CHL = 1.2E-3
c      CHS = 1.2E-3
      CDL = 1.8 _d -3
      CDS = 0.8 _d -3
      CHL = 2.0 _d -3
      CHS = 0.9 _d -3
C
      VGUST  = 4. _d 0
      DTHETAF = 3. _d 0
      FSTAB  = 0.67 _d 0
C
C---  4. Constants for convection (common CNVCON):
C
      RHBL   = 0.8 _d 0
      TRCNV  = 6. _d 0
      ENTMAX = 0.5 _d 0
C
C---  5. Constants for large-scale condensation (common LSCCON):
C
      RHLSC  = 0.9 _d 0
      TRLSC  = 4. _d 0
C
C---  6. Constants for radiation (common RADCON):
C
      ABSSW = 0.06 _d 0
cchdbg ********************
      ABSSW = 0.04 _d 0
      ABSSW = 0.06 _d 0
cchdbg *********************
      ABSLW = 1.40 _d 0
cchdbg ****************************************************
cch      ABSLW = 1.00
cchdbg ****************************************************

      ABWSW = 0.06 _d 0
      ABWLW = 0.55 _d 0
cchdbg ****************************************************
      ABWLW = 0.60 _d 0
cchdbg      ABWLW = 0.65
cchdbg      ABWLW = 0.90
cchdbg ****************************************************

      ABCSW = 0.0 _d 0
      ABCLW = 0.15 _d 0
      ABCLW = 0.15 _d 0
cchdbg *******************************************
c      ABCLW = 3.0
c      ABCLW = 2.0
cch      ABCLW = 3.5
cch      ABCLW = 2.5
cch      ABCLW = 2.8
cchdbg ********************************************

      EPSSW = 0.03 _d 0
      EPSLW = 0.05 _d 0

      ALBCL = 0.50 _d 0
cchdbg *******************************************
      ALBCL = 0.49 _d 0
      ALBCL = 0.47 _d 0
      ALBCL = 0.40 _d 0
      ALBCL = 0.35 _d 0
      ALBCL = 0.33 _d 0
      ALBCL = 0.36 _d 0
      ALBCL = 0.34 _d 0
cchdbg ********************************************
cchdbg      RHCL1 = 0.45
cchdbg      RHCL2 = 0.85
cchdbg      QACL  = 1.00
cchdbg *******************************************
      RHCL1 = 0.50 _d 0
      RHCL2 = 0.90 _d 0
      QACL  = 0.50 _d 0
cchdbg *******************************************
C
C     2.6 Constants for vertical diffusion and sh. conv. (common VDICON):
C
      TRVDI = 48. _d 0
      TRSHC = 24. _d 0
cchdbg *******************************************
      TRSHC = 12. _d 0
cchdbg *******************************************
C
C Computation of the last air-level
C ---------------------------------
      I2=0
      DO J=1,NLAT
        DO I=1,NLON
          I2=I2+1
          NLEVxy(I2) =0
          NLEVxyU(I2)=0
          NLEVxyV(I2)=0
          DO k=1,NLEV
            NLEVxy(I2) = NLEVxy(I2)  +int( hFacC(I,J,K,bi,bj) )
            NLEVxyU(I2)= NLEVxyU(I2) +int( hFacW(I,J,K,bi,bj) )
            NLEVxyV(I2)= NLEVxyV(I2) +int( hFacS(I,J,K,bi,bj) )
          ENDDO
        ENDDO
      ENDDO
CcnhDebugStarts
      UT_PBL = 0.
      VT_PBL = 0.
CcnhDebugEnds
C     DO I=1,NLON
C       NLEVxyV(I)=1
C     ENDDO

CcnhDebugStarts
C     write(0,*) 'MAXVAL(NLEVxy)=',MAXVAL(NLEVxy)
C     write(0,*) 'MINVAL(NLEVxy)=',MINVAL(NLEVxy)
CcnhDebugEnds
C
      RETURN
      END
