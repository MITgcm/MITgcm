C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_inphys.F,v 1.1 2002/10/09 01:01:24 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE INPHYS (FSG,HSG,RLAT,myThid)

      IMPLICIT NONE

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

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "AIM_GRID.h"
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
      INTEGER myThid
      _RL FSG(NLEV), HSG(0:NLEV), RLAT(NLAT)  

#ifdef ALLOW_AIM

C     == Local Variables ==
      INTEGER K
c     DATA Pground /1. _d +5/

C---  1. Time independent parameters and arrays
C
C     1.1 Physical constants
C
      IF ( myThid .EQ. 1 ) THEN
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
       WVI(K,2)=(LOG(SIGH(K))-SIGL(K))*WVI(K,1)
      ENDDO
C
      WVI(NLEV,1)=0.
      WVI(NLEV,2)=-SIGL(NLEV)*WVI(NLEV-1,2)
cchdbg
      WVI(NLEV,2)=-SIGL(NLEV)*WVI(NLEV-1,1)
cchdbg
 
C---  2. Constants for boundary forcing (common FORCON):
 
      SOLC   = 342. _d 0

      ALBSEA = 0.07 _d 0
      ALBICE = 0.60 _d 0
      ALBSN  = 0.60 _d 0
      SDALB  = 60. _d 0
      SWCAP = 75. _d 0
      SWWIL =  0. _d 0

C---  3. Constants for surface fluxes (common SFLCON):

C- jmc: return to F.Molteni values (Clim Dim, 02):

      FWIND0 = 0.6 _d 0
      FTEMP0 = 1. _d 0
      FHUM0  = 1. _d 0

      CDL = 1.8 _d -3
      CDS = 0.8 _d -3
      CHL = 1.2 _d -3
      CHS = 0.8 _d -3
      VGUST  = 5. _d 0

C---  4. Constants for convection (common CNVCON):

      RHBL   = 0.8 _d 0
      TRCNV  = 6. _d 0
      ENTMAX = 0.5 _d 0

C---  5. Constants for large-scale condensation (common LSCCON):

      RHLSC  = 0.9 _d 0
      TRLSC  = 4. _d 0

C---  6. Constants for radiation (common RADCON):

      ABSSW = 0.06 _d 0
      ABSLW = 1.40 _d 0

      ABWSW = 0.06 _d 0
      ABWLW = 0.55 _d 0

      ABCSW = 0.0 _d 0
      ABCLW = 0.15 _d 0

      EPSSW = 0.03 _d 0
      EPSLW = 0.05 _d 0

C- jmc: return to F.Molteni values (Clim Dim, 02):
      ALBCL = 0.40 _d 0
      RHCL1 = 0.45 _d 0
      RHCL2 = 0.85 _d 0
      QACL  = 1.00 _d 0

C--   7. Constants for vertical diffusion and sh. conv. (common VDICON):
 
C- jmc: return to F.Molteni values (Clim Dim, 02):
      TRVDI = 40. _d 0
      TRSHC = 24. _d 0

      ENDIF

#endif /* ALLOW_AIM */ 

      RETURN
      END
