C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_SIZE.h,v 1.1 2002/11/22 17:16:06 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | AIM_SIZE.h Declare size of arrays for AIM Physics        |
C     *==========================================================*

C     MITgcm declaration of grid size. 
C     Latitudinal extent is one less than MITgcm ( i.e. NY-1)
C     because MITgcm has dummy layer of land at northern most
C     edge.  <-- no longer the case now
#include "SIZE.h"

C- dimension for AIM Physics package   
      INTEGER NLON, NLAT, NLEV, NGP
      PARAMETER ( NLON=sNx, NLAT=sNy, NLEV=Nr, NGP=NLON*NLAT ) 

C- dimension for LW radiative scheme
C      NBAND  = Number of LW radiation bands with tau < 1
C      lwTemp1= minimum temperature for LW radiation scheme
C      lwTemp2= maximum temperature for LW radiation scheme
      INTEGER NBAND, lwTemp1, lwTemp2
      PARAMETER ( NBAND=4, lwTemp1=100, lwTemp2=400 )

