C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_SOM_VARS.h,v 1.2 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: GAD_SOM_VARS.h
C     !INTERFACE:
C     include "GAD_SOM_VARS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | GAD_SOM_VARS.h
C     | o state variables for 2nd Order-Moment Advection
C     *==========================================================*
C     | Storage needed for Temperature and Salinity when using
C     | 2nd Order-Moment (SOM) Advection
C     *==========================================================*
C     \ev
CEOP

#ifdef GAD_ALLOW_TS_SOM_ADV
C--   COMMON /GAD_SOM_VARS_R/ REAL, state variables for 2nd Order-Moment Advection
C     moments are stored in this order (3-D case): 1rst Order moments (3:x,y,z)
C                                   and 2nd Order moments (6:xx,yy,zz,xy,xz,yz).
C     som_T  :: Pot.Temp 1rst & 2nd Order Moments
C     som_S  :: Salinity 1rst & 2nd Order Moments

      COMMON /GAD_SOM_VARS_R/
     &                som_T,
     &                som_S
      _RL  som_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,nSOM)
      _RL  som_S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,nSOM)

#endif /* GAD_ALLOW_TS_SOM_ADV */
