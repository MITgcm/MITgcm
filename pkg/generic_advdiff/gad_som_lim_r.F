C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_lim_r.F,v 1.4 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_SOM_LIM_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_SOM_LIM_R(
     I           bi,bj, limiter,
     U           sm_v,  sm_o,  sm_x,  sm_y,  sm_z,
     U           sm_xx, sm_yy, sm_zz, sm_xy, sm_xz, sm_yz,
     I           myThid )

C !DESCRIPTION:
C  Apply limiter before calculating vertical advection
C        Second-Order Moments Advection of tracer in Z-direction
C        ref: M.J.Prather, 1986, JGR, 91, D6, pp 6671-6681.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
c #include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj        :: tile indices
C  limiter      :: 0: no limiter ; 1: Prather, 1986 limiter
C  myThid       :: my Thread Id. number
      INTEGER bi,bj
      INTEGER limiter
c     _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  sm_v         :: volume of grid cell
C  sm_o         :: tracer content of grid cell (zero order moment)
C  sm_x,y,z     :: 1rst order moment of tracer distribution, in x,y,z direction
C  sm_xx,yy,zz  ::  2nd order moment of tracer distribution, in x,y,z direction
C  sm_xy,xz,yz  ::  2nd order moment of tracer distr., in cross direction xy,xz,yz
      _RL sm_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_o  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_y  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_z  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_xx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_yy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_zz (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_xy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_xz (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sm_yz (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C !LOCAL VARIABLES: ====================================================
C  i,j,k        :: loop indices
      _RL  three
      PARAMETER( three = 3. _d 0 )
      INTEGER i,j,k
      _RL  slpmax, s1max, s1new, s2new
CEOP

      IF ( limiter.EQ.1 ) THEN
       DO k=1,Nr
        DO j=jMinAdvR,jMaxAdvR
         DO i=iMinAdvR,iMaxAdvR
C     If flux-limiting transport is to be applied, place limits on
C     appropriate moments before transport.
          slpmax = 0.
          IF ( sm_o(i,j,k).GT.0. ) slpmax = sm_o(i,j,k)
          s1max = slpmax*1.5 _d 0
          s1new = MIN(  s1max, MAX(-s1max,sm_z(i,j,k)) )
          s2new = MIN( (slpmax+slpmax-ABS(s1new)/three),
     &                 MAX(ABS(s1new)-slpmax,sm_zz(i,j,k))  )
          sm_xz(i,j,k) = MIN( slpmax, MAX(-slpmax,sm_xz(i,j,k)) )
          sm_yz(i,j,k) = MIN( slpmax, MAX(-slpmax,sm_yz(i,j,k)) )
          sm_z (i,j,k) = s1new
          sm_zz(i,j,k) = s2new
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
