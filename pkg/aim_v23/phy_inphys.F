C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_inphys.F,v 1.4 2004/06/24 23:43:11 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE INPHYS (HSG, myThid)

      IMPLICIT NONE

C--
C--   SUBROUTINE INPHYS (HSG,PPL,RLAT)
C--
C--   Purpose: Initialize common blocks for physical parametrization routines 
C--   Input :  HSG  : sigma at half levels
C--            PPL  : pressure levels for post-processing
C--            RLAT : gaussian-grid latitudes
C--   Initialized common blocks: PHYCON, FSIGLT, FORCON,
C--                              CNVCON, LSCCON, RADCON, SFLCON, VDICON
C--
C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c #include "AIM_GRID.h"

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Constants for sub-grid-scale physics

#include "com_forcon.h"
#include "com_cnvcon.h"
#include "com_lsccon.h"
#include "com_radcon.h"  
#include "com_sflcon.h"
#include "com_vdicon.h"  

C     == Routine Arguments ==
      INTEGER myThid
c     REAL HSG(0:NLEV), PPL(NLEV), RLAT(NLAT)  
      _RL  HSG(0:NLEV)

#ifdef ALLOW_AIM

C     == Local Variables ==
      INTEGER K

      _BEGIN_MASTER(myThid)  

C---  1. Time independent parameters and arrays
C
C     1.1 Physical constants

c     P0 = 1. _d +5
c     GG = 9.81 _d 0
c     RD = 287. _d 0
c     CP = 1004. _d 0
      P0 = atm_Po
      GG = gravity
      RD = atm_Rd
      CP = atm_Cp
C     Latent heat is in J/g for consistency with spec.hum. in g/Kg
      ALHC = 2501. _d 0
      ALHF =  334. _d 0
      SBC = 5.67 _d -8
C     Heat capacity of rain is also in J/g/K for the same reasons
c     rainCP = HeatCapacity_Cp / 1000. _d 0 
      rainCP = 4200. _d 0 / 1000. _d 0 
      tFreeze= celsius2K
C
C     1.2 Functions of sigma and latitude
C
      SIGH(0)=HSG(0)
C
      DO K=1,NLEV
       SIG(K)  = 0.5*(HSG(K)+HSG(K-1))
       SIGL(K) = LOG(SIG(K))
       SIGH(K) = HSG(K)
       DSIG(K) = HSG(K)-HSG(K-1)
c      POUT(K) = PPL(K)
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
 
c--- jmc: write WVI to check:
      WRITE(standardMessageUnit,'(A)')
     &     '- INPHYS: k,SIG, SIGH,   SIGL,   WVI(1),  WVI(2):'
      DO K=1,NLEV
       WRITE(standardMessageUnit,'(I3,6F9.4)') 
     &      k,SIG(k),SIGH(k),SIGL(k),WVI(K,1),WVI(K,2)
      ENDDO
      WRITE(standardMessageUnit,'(A)') '- INPHYS: end setup WVI.'
c--- jmc.

c- jmc: initialize SLAT & CLAT in aim_dyn2aim.F
c     DO J=1,NLAT
c      SLAT(J)=SIN(RLAT(J))
c      CLAT(J)=COS(RLAT(J))
c     ENDDO

C--   2. Constants for physical parametrization routines:
 
c_FM  include "cls_inphys.h"
#include "phy_const.h"
 
C-     pot. temp. increment for computing stability function derivative
C      note: use the discrete form: F(Ts+dTstab)-F(Ts-dTstab)/2.dTstab 
       dTstab = 1. _d 0

      _END_MASTER(myThid)

#endif /* ALLOW_AIM */ 

      RETURN
      END
