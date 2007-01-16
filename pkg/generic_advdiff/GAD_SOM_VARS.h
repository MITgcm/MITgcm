C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_SOM_VARS.h,v 1.1 2007/01/16 04:38:34 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: GAD_SOM_VARS.h
C     !INTERFACE:
C     include "GAD_SOM_VARS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | GAD_SOM_VARS.h
C     | o Additional state variables for non-hydrostatic model
C     *==========================================================*
C     | In N-H mode, wVel becomes a prognostic variable: need
C     | to hold two levels of time tendency for w (for AB-2)
C     *==========================================================*
C     \ev
CEOP

#ifdef GAD_ALLOW_SOM_ADVECT
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

#endif /* GAD_ALLOW_SOM_ADVECT */
