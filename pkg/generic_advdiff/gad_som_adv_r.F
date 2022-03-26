#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GAD_SOM_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_SOM_ADV_R(
     I           bi,bj,k, kUp, kDw,
     I           deltaTloc, rTrans, maskUp, maskIn,
     U           sm_v,  sm_o,  sm_x,  sm_y,  sm_z,
     U           sm_xx, sm_yy, sm_zz, sm_xy, sm_xz, sm_yz,
     U           alp,   aln,   fp_v,  fn_v,  fp_o,  fn_o,
     U           fp_x,  fn_x,  fp_y,  fn_y,  fp_z,  fn_z,
     U           fp_xx, fn_xx, fp_yy, fn_yy, fp_zz, fn_zz,
     U           fp_xy, fn_xy, fp_xz, fn_xz, fp_yz, fn_yz,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated vertical flux due to advection
C  of a tracer using
C--
C        Second-Order Moments Advection of tracer in Z-direction
C        ref: M.J.Prather, 1986, JGR, 91, D6, pp 6671-6681.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C      The 3-D grid has dimension  (Nx,Ny,Nz) with corresponding
C      velocity field (U,V,W).  Parallel subroutine calculate
C      advection in the X- and Y- directions.
C      The moment [Si] are as defined in the text, Sm refers to
C      the total mass in each grid box
C      the moments [Fi] are similarly defined and used as temporary
C      storage for portions of the grid boxes in transit.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj        :: tile indices
C  k            :: vertical level
C  kUp          :: index into 2 1/2D array, toggles between 1 and 2
C  kDw          :: index into 2 1/2D array, toggles between 2 and 1
C  rTrans       :: vertical volume transport
C  maskUp       :: 2-D array mask for W points
C  maskIn       :: 2-D array Interior mask
C  myThid       :: my Thread Id. number
      INTEGER bi,bj,k, kUp, kDw
      _RL deltaTloc
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskIn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  sm_v         :: volume of grid cell
C  sm_o         :: tracer content of grid cell (zero order moment)
C  sm_x,y,z     :: 1rst order moment of tracer distribution, in x,y,z direction
C  sm_xx,yy,zz  ::  2nd order moment of tracer distribution, in x,y,z direction
C  sm_xy,xz,yz  ::  2nd order moment of tracer distr., in cross direction xy,xz,yz
C  wT           :: vertical advective flux
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
      _RL  alp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  aln  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_o (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_o (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_x (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_x (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_y (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_y (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_z (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_z (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_yy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_yy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_zz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_zz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_xz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_xz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fp_yz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL  fn_yz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j          :: loop indices
C  wLoc         :: volume transported (per time step)
      _RL three
      PARAMETER( three = 3. _d 0 )
      INTEGER i,j
      INTEGER km1
      LOGICAL noFlowAcrossSurf
      _RL  recip_dT
      _RL  wLoc, alf1, alf1q, alpmn
      _RL  alfp, alpq, alp1, locTp
      _RL  alfn, alnq, aln1, locTn
CEOP

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT somtape_r = COMMON, 1
# endif
      alp(1,1,kDw) = alp(1,1,kDw)
      aln(1,1,kDw) = aln(1,1,kDw)
      fp_v(1,1,kDw) = fp_v(1,1,kDw)
      fn_v(1,1,kDw) = fn_v(1,1,kDw)
      fp_o(1,1,kDw) = fp_o(1,1,kDw)
      fn_o(1,1,kDw) = fn_o(1,1,kDw)
      fp_x(1,1,kDw) = fp_x(1,1,kDw)
      fn_x(1,1,kDw) = fn_x(1,1,kDw)
      fp_y(1,1,kDw) = fp_y(1,1,kDw)
      fn_y(1,1,kDw) = fn_y(1,1,kDw)
      fp_z(1,1,kDw) = fp_z(1,1,kDw)
      fn_z(1,1,kDw) = fn_z(1,1,kDw)
      fp_xx(1,1,kDw) = fp_xx(1,1,kDw)
      fn_xx(1,1,kDw) = fn_xx(1,1,kDw)
      fp_yy(1,1,kDw) = fp_yy(1,1,kDw)
      fn_yy(1,1,kDw) = fn_yy(1,1,kDw)
      fp_zz(1,1,kDw) = fp_zz(1,1,kDw)
      fn_zz(1,1,kDw) = fn_zz(1,1,kDw)
      fp_xy(1,1,kDw) = fp_xy(1,1,kDw)
      fn_xy(1,1,kDw) = fn_xy(1,1,kDw)
      fp_xz(1,1,kDw) = fp_xz(1,1,kDw)
      fn_xz(1,1,kDw) = fn_xz(1,1,kDw)
      fp_yz(1,1,kDw) = fp_yz(1,1,kDw)
      fn_yz(1,1,kDw) = fn_yz(1,1,kDw)
#endif

      recip_dT = zeroRL
      IF ( deltaTloc.GT.zeroRL ) recip_dT = 1.0 _d 0 / deltaTloc
      noFlowAcrossSurf = rigidLid .OR. nonlinFreeSurf.GE.1
     &                            .OR. select_rStar.NE.0

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sm_o,sm_v,sm_x,sm_xz,sm_y,sm_yz,sm_z,sm_zz
CADJ &     = somtape_r, key = 1, kind = isbyte
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  part.1 : calculate flux for all moments
      DO j=jMinAdvR,jMaxAdvR
        DO i=iMinAdvR,iMaxAdvR
          wLoc = rTrans(i,j)*deltaTloc
C--    Flux from (k) to (k-1) when W>0 (i.e., take upper side of box k)
C- note: Linear free surface case: this takes care of w_surf advection out
C       of the domain since for this particular case, rTrans is not masked
          fp_v (i,j,kUp) = MAX( zeroRL,  wLoc )
          alp  (i,j,kUp) = fp_v(i,j,kUp)/sm_v(i,j,k)
          alpq           = alp(i,j,kUp)*alp(i,j,kUp)
          alp1           = 1. _d 0 - alp(i,j,kUp)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "p" for positive W
          fp_o (i,j,kUp) = alp(i,j,kUp)*
     &                   ( sm_o(i,j, k ) + alp1*sm_z(i,j, k )
     &                   + alp1*(alp1-alp(i,j,kUp))*sm_zz(i,j, k )
     &                   )
          fp_z (i,j,kUp) = alpq*
     &                   ( sm_z(i,j, k ) + three*alp1*sm_zz(i,j, k ) )
          fp_zz(i,j,kUp) = alp(i,j,kUp)*alpq*sm_zz(i,j, k )
          fp_x (i,j,kUp) = alp(i,j,kUp)*
     &                   ( sm_x(i,j, k ) + alp1*sm_xz(i,j, k ) )
          fp_y (i,j,kUp) = alp(i,j,kUp)*
     &                   ( sm_y(i,j, k ) + alp1*sm_yz(i,j, k ) )
          fp_xz(i,j,kUp) = alpq        *sm_xz(i,j, k )
          fp_yz(i,j,kUp) = alpq        *sm_yz(i,j, k )
          fp_xx(i,j,kUp) = alp(i,j,kUp)*sm_xx(i,j, k )
          fp_yy(i,j,kUp) = alp(i,j,kUp)*sm_yy(i,j, k )
          fp_xy(i,j,kUp) = alp(i,j,kUp)*sm_xy(i,j, k )
        ENDDO
      ENDDO
      IF ( k.EQ.1 ) THEN
C--   Linear free surface, calculate w_surf (<0) advection term
       km1 = 1
       DO j=jMinAdvR,jMaxAdvR
        DO i=iMinAdvR,iMaxAdvR
          wLoc = rTrans(i,j)*deltaTloc
C-     Flux from above to (k) when W<0 , surface case:
C      take box k=1, assuming zero 1rst & 2nd moment in Z dir.
          fn_v (i,j,kUp) = MAX( zeroRL, -wLoc )
          aln  (i,j,kUp) = fn_v(i,j,kUp)/sm_v(i,j,km1)
          alnq           = aln(i,j,kUp)*aln(i,j,kUp)
          aln1           = 1. _d 0 - aln(i,j,kUp)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "n" for negative W
          fn_o (i,j,kUp) = aln(i,j,kUp)*sm_o(i,j,km1)
          fn_z (i,j,kUp) = zeroRL
          fn_zz(i,j,kUp) = zeroRL
          fn_x (i,j,kUp) = aln(i,j,kUp)*sm_x(i,j,km1)
          fn_y (i,j,kUp) = aln(i,j,kUp)*sm_y(i,j,km1)
          fn_xz(i,j,kUp) = zeroRL
          fn_yz(i,j,kUp) = zeroRL
          fn_xx(i,j,kUp) = aln(i,j,kUp)*sm_xx(i,j,km1)
          fn_yy(i,j,kUp) = aln(i,j,kUp)*sm_yy(i,j,km1)
          fn_xy(i,j,kUp) = aln(i,j,kUp)*sm_xy(i,j,km1)
C--    Save zero-order flux:
          wT(i,j) = ( fp_o(i,j,kUp) - fn_o(i,j,kUp) )*recip_dT
        ENDDO
       ENDDO
      ELSE
C--   Interior only: mask rTrans (if not already done)
       km1 = k-1
       DO j=jMinAdvR,jMaxAdvR
        DO i=iMinAdvR,iMaxAdvR
          wLoc = maskUp(i,j)*rTrans(i,j)*deltaTloc
C-     Flux from (k-1) to (k) when W<0 (i.e., take lower side of box k-1)
          fn_v (i,j,kUp) = MAX( zeroRL, -wLoc )
          aln  (i,j,kUp) = fn_v(i,j,kUp)/sm_v(i,j,km1)
          alnq           = aln(i,j,kUp)*aln(i,j,kUp)
          aln1           = 1. _d 0 - aln(i,j,kUp)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "n" for negative W
          fn_o (i,j,kUp) = aln(i,j,kUp)*
     &                   ( sm_o(i,j,km1) - aln1*sm_z(i,j,km1)
     &                   + aln1*(aln1-aln(i,j,kUp))*sm_zz(i,j,km1)
     &                   )
          fn_z (i,j,kUp) = alnq*
     &                   ( sm_z(i,j,km1) - three*aln1*sm_zz(i,j,km1) )
          fn_zz(i,j,kUp) = aln(i,j,kUp)*alnq*sm_zz(i,j,km1)
          fn_x (i,j,kUp) = aln(i,j,kUp)*
     &                   ( sm_x(i,j,km1) - aln1*sm_xz(i,j,km1) )
          fn_y (i,j,kUp) = aln(i,j,kUp)*
     &                   ( sm_y(i,j,km1) - aln1*sm_yz(i,j,km1) )
          fn_xz(i,j,kUp) = alnq        *sm_xz(i,j,km1)
          fn_yz(i,j,kUp) = alnq        *sm_yz(i,j,km1)
          fn_xx(i,j,kUp) = aln(i,j,kUp)*sm_xx(i,j,km1)
          fn_yy(i,j,kUp) = aln(i,j,kUp)*sm_yy(i,j,km1)
          fn_xy(i,j,kUp) = aln(i,j,kUp)*sm_xy(i,j,km1)
C--    Save zero-order flux:
          wT(i,j) = ( fp_o(i,j,kUp) - fn_o(i,j,kUp) )*recip_dT
        ENDDO
       ENDDO
C--   end surface/interior cases for W<0 advective fluxes
      ENDIF
      IF ( .NOT.uniformFreeSurfLev .AND. k.NE.1 .AND.
     &     .NOT.noFlowAcrossSurf ) THEN
C--   Linear free surface, but surface not @ k=1 :
C     calculate w_surf (<0) advection term from current level
C     moments assuming zero 1rst & 2nd moment in Z dir. ;
C     and add to previous fluxes; note: identical to resetting fluxes
C     since previous fluxes are zero in this case (=> let TAF decide)
       km1 = k
       DO j=jMinAdvR,jMaxAdvR
        DO i=iMinAdvR,iMaxAdvR
         wLoc = rTrans(i,j)*deltaTloc
         IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
C-     Flux from (k-1) to (k) when W<0 (special surface case, take box k)
          fn_v (i,j,kUp) = MAX( zeroRL, -wLoc )
          aln  (i,j,kUp) = fn_v(i,j,kUp)/sm_v(i,j,km1)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "n" for negative W
          fn_o (i,j,kUp) = aln(i,j,kUp)*sm_o(i,j,km1)
          fn_x (i,j,kUp) = aln(i,j,kUp)*sm_x(i,j,km1)
          fn_y (i,j,kUp) = aln(i,j,kUp)*sm_y(i,j,km1)
          fn_xx(i,j,kUp) = aln(i,j,kUp)*sm_xx(i,j,km1)
          fn_yy(i,j,kUp) = aln(i,j,kUp)*sm_yy(i,j,km1)
          fn_xy(i,j,kUp) = aln(i,j,kUp)*sm_xy(i,j,km1)
C--    Save zero-order flux:
          wT(i,j) = ( fp_o(i,j,kUp) - fn_o(i,j,kUp) )*recip_dT
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  part.2 : re-adjust moments remaining in the box
C      take off from grid box (k): negative W(kDw) and positive W(kUp)
      DO j=jMinAdvR,jMaxAdvR
       DO i=iMinAdvR,iMaxAdvR
#ifdef ALLOW_OBCS
        IF ( maskIn(i,j).NE.zeroRS ) THEN
#endif /* ALLOW_OBCS */
        alf1  = 1. _d 0 - aln(i,j,kDw) - alp(i,j,kUp)
        alf1q = alf1*alf1
        alpmn = alp(i,j,kUp) - aln(i,j,kDw)
        sm_v (i,j,k) = sm_v (i,j,k) - fn_v (i,j,kDw) - fp_v (i,j,kUp)
        sm_o (i,j,k) = sm_o (i,j,k) - fn_o (i,j,kDw) - fp_o (i,j,kUp)
        sm_z (i,j,k) = alf1q*( sm_z(i,j,k) - three*alpmn*sm_zz(i,j,k) )
        sm_zz(i,j,k) = alf1*alf1q*sm_zz(i,j,k)
        sm_xz(i,j,k) = alf1q*sm_xz(i,j,k)
        sm_yz(i,j,k) = alf1q*sm_yz(i,j,k)
        sm_x (i,j,k) = sm_x (i,j,k) - fn_x (i,j,kDw) - fp_x (i,j,kUp)
        sm_xx(i,j,k) = sm_xx(i,j,k) - fn_xx(i,j,kDw) - fp_xx(i,j,kUp)
        sm_y (i,j,k) = sm_y (i,j,k) - fn_y (i,j,kDw) - fp_y (i,j,kUp)
        sm_yy(i,j,k) = sm_yy(i,j,k) - fn_yy(i,j,kDw) - fp_yy(i,j,kUp)
        sm_xy(i,j,k) = sm_xy(i,j,k) - fn_xy(i,j,kDw) - fp_xy(i,j,kUp)
#ifdef ALLOW_OBCS
        ENDIF
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  part.3 : Put the temporary moments into appropriate neighboring boxes
C      add into grid box (k): positive W(kDw) and negative W(kUp)
      DO j=jMinAdvR,jMaxAdvR
       DO i=iMinAdvR,iMaxAdvR
#ifdef ALLOW_OBCS
        IF ( maskIn(i,j).NE.zeroRS ) THEN
#endif /* ALLOW_OBCS */
        sm_v (i,j,k) = sm_v (i,j,k) + fp_v (i,j,kDw) + fn_v (i,j,kUp)
        alfp = fp_v(i,j,kDw)/sm_v(i,j,k)
        alfn = fn_v(i,j,kUp)/sm_v(i,j,k)
        alf1 = 1. _d 0 - alfp - alfn
        alp1 = 1. _d 0 - alfp
        aln1 = 1. _d 0 - alfn
        alpmn = alfp - alfn
        locTp = alfp*sm_o(i,j,k) - alp1*fp_o(i,j,kDw)
        locTn = alfn*sm_o(i,j,k) - aln1*fn_o(i,j,kUp)
        sm_zz(i,j,k) = alf1*alf1*sm_zz(i,j,k) + alfp*alfp*fp_zz(i,j,kDw)
     &                                        + alfn*alfn*fn_zz(i,j,kUp)
     &     - 5. _d 0*(-alpmn*alf1*sm_z(i,j,k) + alfp*alp1*fp_z(i,j,kDw)
     &                                        - alfn*aln1*fn_z(i,j,kUp)
     &               + twoRL*alfp*alfn*sm_o(i,j,k) + (alp1-alfp)*locTp
     &                                             + (aln1-alfn)*locTn
     &               )
        sm_xz(i,j,k) = alf1*sm_xz(i,j,k) + alfp*fp_xz(i,j,kDw)
     &                                   + alfn*fn_xz(i,j,kUp)
     &       + three*( alpmn*sm_x(i,j,k) - alp1*fp_x(i,j,kDw)
     &                                   + aln1*fn_x(i,j,kUp)
     &               )
        sm_yz(i,j,k) = alf1*sm_yz(i,j,k) + alfp*fp_yz(i,j,kDw)
     &                                   + alfn*fn_yz(i,j,kUp)
     &       + three*( alpmn*sm_y(i,j,k) - alp1*fp_y(i,j,kDw)
     &                                   + aln1*fn_y(i,j,kUp)
     &               )
        sm_z (i,j,k) = alf1*sm_z(i,j,k) + alfp*fp_z(i,j,kDw)
     &                                  + alfn*fn_z(i,j,kUp)
     &               + three*( locTp - locTn )
        sm_o (i,j,k) = sm_o (i,j,k) + fp_o (i,j,kDw) + fn_o (i,j,kUp)
        sm_x (i,j,k) = sm_x (i,j,k) + fp_x (i,j,kDw) + fn_x (i,j,kUp)
        sm_xx(i,j,k) = sm_xx(i,j,k) + fp_xx(i,j,kDw) + fn_xx(i,j,kUp)
        sm_y (i,j,k) = sm_y (i,j,k) + fp_y (i,j,kDw) + fn_y (i,j,kUp)
        sm_yy(i,j,k) = sm_yy(i,j,k) + fp_yy(i,j,kDw) + fn_yy(i,j,kUp)
        sm_xy(i,j,k) = sm_xy(i,j,k) + fp_xy(i,j,kDw) + fn_xy(i,j,kUp)
#ifdef ALLOW_OBCS
        ENDIF
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO

      RETURN
      END
