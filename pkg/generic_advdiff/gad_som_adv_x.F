#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GAD_SOM_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_SOM_ADV_X(
     I           bi,bj,k, limiter,
     I           overlapOnly, interiorOnly,
     I           N_edge, S_edge, E_edge, W_edge,
     I           deltaTloc, uTrans, maskIn,
     U           sm_v, sm_o, sm_x, sm_y, sm_z,
     U           sm_xx, sm_yy, sm_zz, sm_xy, sm_xz, sm_yz,
     O           uT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated zonal flux due to advection
C  of a tracer using
C--
C        Second-Order Moments Advection of tracer in X-direction
C        ref: M.J.Prather, 1986, JGR, 91, D6, pp 6671-6681.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C      The 3-D grid has dimension  (Nx,Ny,Nz) with corresponding
C      velocity field (U,V,W).  Parallel subroutine calculate
C      advection in the Y- and Z- directions.
C      The moment [Si] are as defined in the text, Sm refers to
C      the total mass in each grid box
C      the moments [Fi] are similarly defined and used as temporary
C      storage for portions of the grid boxes in transit.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj         :: tile indices
C  k             :: vertical level
C  limiter       :: 0: no limiter ; 1: Prather, 1986 limiter
C  overlapOnly   :: only update the edges of myTile, but not the interior
C  interiorOnly  :: only update the interior of myTile, but not the edges
C [N,S,E,W]_edge :: true if N,S,E,W edge of myTile is an Edge of the cube
C  uTrans        :: zonal volume transport
C  maskIn        :: 2-D array Interior mask
C  myThid        :: my Thread Id. number
      INTEGER bi,bj,k
      INTEGER limiter
      LOGICAL overlapOnly, interiorOnly
      LOGICAL N_edge, S_edge, E_edge, W_edge
      _RL deltaTloc
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskIn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  sm_v         :: volume of grid cell
C  sm_o         :: tracer content of grid cell (zero order moment)
C  sm_x,y,z     :: 1rst order moment of tracer distribution, in x,y,z direction
C  sm_xx,yy,zz  ::  2nd order moment of tracer distribution, in x,y,z direction
C  sm_xy,xz,yz  ::  2nd order moment of tracer distr., in cross direction xy,xz,yz
C  uT           :: zonal advective flux
      _RL sm_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_o  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_y  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_z  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xx (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_yy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_zz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_yz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j           :: loop indices
C  uLoc          :: volume transported (per time step)
C [iMin,iMax]Upd :: loop range to update tracer field
C [jMin,jMax]Upd :: loop range to update tracer field
C  nbStrips      :: number of strips (if region to update is splitted)
      _RL three
      PARAMETER( three = 3. _d 0 )
      INTEGER i,j
      INTEGER ns, nbStrips
      INTEGER iMinUpd(2), iMaxUpd(2), jMinUpd(2), jMaxUpd(2)
      _RL  recip_dT
      _RL  slpmax, s1max, s1new, s2new
      _RL  uLoc, alf1, alf1q, alpmn
      _RL  alfp, alpq, alp1, locTp
      _RL  alfn, alnq, aln1, locTn
      _RL  alp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  aln  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_v (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_o (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_o (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_x (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_x (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_y (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_y (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_z (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_z (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_xx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_xx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_yy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_yy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_zz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_zz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_xy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_xy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_xz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_xz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fp_yz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fn_yz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT somtape_x = COMMON, 2
#endif
      recip_dT = 0.
      IF ( deltaTloc.GT.zeroRL ) recip_dT = 1.0 _d 0 / deltaTloc

C-    Set loop ranges for updating tracer field (splitted in 2 strips)
      nbStrips   = 1
      iMinUpd(1) = 1-OLx+1
      iMaxUpd(1) = sNx+OLx-1
      jMinUpd(1) = 1-OLy
      jMaxUpd(1) = sNy+OLy
      IF ( overlapOnly ) THEN
C     update in overlap-Only
        IF ( W_edge ) iMinUpd(1) = 1
        IF ( E_edge ) iMaxUpd(1) = sNx
        IF ( S_edge ) THEN
          jMinUpd(1) = 1-OLy
          jMaxUpd(1) = 0
        ENDIF
        IF ( N_edge ) THEN
          IF ( S_edge ) nbStrips = 2
          jMinUpd(nbStrips) = sNy+1
          jMaxUpd(nbStrips) = sNy+OLy
        ENDIF
      ELSE
C     do not only update the overlap
        IF ( interiorOnly .AND. S_edge ) jMinUpd(1) = 1
        IF ( interiorOnly .AND. N_edge ) jMaxUpd(1) = sNy
      ENDIF

C--   start 1rst loop on strip number "ns"
      DO ns=1,nbStrips

      IF ( limiter.EQ.1 ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sm_o,sm_x,sm_xx,sm_xy,sm_xz,sm_y,sm_yy,sm_yz,sm_z,sm_zz
CADJ &     = somtape_x, key = ns, kind = isbyte
#endif
       DO j=jMinUpd(ns),jMaxUpd(ns)
        DO i=iMinUpd(1)-1,iMaxUpd(1)+1
C     If flux-limiting transport is to be applied, place limits on
C     appropriate moments before transport.
         slpmax = 0.
         IF ( sm_o(i,j).GT.zeroRL ) slpmax = sm_o(i,j)
         s1max = slpmax*1.5 _d 0
         s1new = MIN(  s1max, MAX(-s1max,sm_x(i,j)) )
         s2new = MIN( (slpmax+slpmax-ABS(s1new)/three),
     &                MAX(ABS(s1new)-slpmax,sm_xx(i,j))  )
         sm_xy(i,j) = MIN( slpmax, MAX(-slpmax,sm_xy(i,j)) )
         sm_xz(i,j) = MIN( slpmax, MAX(-slpmax,sm_xz(i,j)) )
         sm_x (i,j) = s1new
         sm_xx(i,j) = s2new
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sm_o,sm_v,sm_x,sm_xx,sm_xy,sm_xz,sm_y,sm_yy,sm_yz,sm_z,sm_zz
CADJ &     = somtape_x, key = ns, kind = isbyte
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  part.1 : calculate flux for all moments
      DO j=jMinUpd(ns),jMaxUpd(ns)
       DO i=iMinUpd(1),iMaxUpd(1)+1
        uLoc = uTrans(i,j)*deltaTloc
C--    Flux from (i-1) to (i) when U>0 (i.e., take right side of box i-1)
        fp_v (i,j) = MAX( zeroRL,  uLoc )
        alp  (i,j) = fp_v(i,j)/sm_v(i-1,j)
        alpq       = alp(i,j)*alp(i,j)
        alp1       = 1. _d 0 - alp(i,j)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "p" for positive U
        fp_o (i,j) = alp(i,j)*( sm_o(i-1,j) + alp1*sm_x(i-1,j)
     &                        + alp1*(alp1-alp(i,j))*sm_xx(i-1,j)
     &                        )
        fp_x (i,j) = alpq    *( sm_x(i-1,j) + three*alp1*sm_xx(i-1,j) )
        fp_xx(i,j) = alp(i,j)*alpq*sm_xx(i-1,j)
        fp_y (i,j) = alp(i,j)*( sm_y(i-1,j) + alp1*sm_xy(i-1,j) )
        fp_z (i,j) = alp(i,j)*( sm_z(i-1,j) + alp1*sm_xz(i-1,j) )
        fp_xy(i,j) = alpq    *sm_xy(i-1,j)
        fp_xz(i,j) = alpq    *sm_xz(i-1,j)
        fp_yy(i,j) = alp(i,j)*sm_yy(i-1,j)
        fp_zz(i,j) = alp(i,j)*sm_zz(i-1,j)
        fp_yz(i,j) = alp(i,j)*sm_yz(i-1,j)
C--    Flux from (i) to (i-1) when U<0 (i.e., take left side of box i)
        fn_v (i,j) = MAX( zeroRL, -uLoc )
        aln  (i,j) = fn_v(i,j)/sm_v( i ,j)
        alnq       = aln(i,j)*aln(i,j)
        aln1       = 1. _d 0 - aln(i,j)
C-     Create temporary moments/masses for partial boxes in transit
C       use same indexing as velocity, "n" for negative U
        fn_o (i,j) = aln(i,j)*( sm_o( i ,j) - aln1*sm_x( i ,j)
     &                        + aln1*(aln1-aln(i,j))*sm_xx( i ,j)
     &                        )
        fn_x (i,j) = alnq    *( sm_x( i ,j) - three*aln1*sm_xx( i ,j) )
        fn_xx(i,j) = aln(i,j)*alnq*sm_xx( i ,j)
        fn_y (i,j) = aln(i,j)*( sm_y( i ,j) - aln1*sm_xy( i ,j) )
        fn_z (i,j) = aln(i,j)*( sm_z( i ,j) - aln1*sm_xz( i ,j) )
        fn_xy(i,j) = alnq    *sm_xy( i ,j)
        fn_xz(i,j) = alnq    *sm_xz( i ,j)
        fn_yy(i,j) = aln(i,j)*sm_yy( i ,j)
        fn_zz(i,j) = aln(i,j)*sm_zz( i ,j)
        fn_yz(i,j) = aln(i,j)*sm_yz( i ,j)
C--    Save zero-order flux:
        uT(i,j) = ( fp_o(i,j) - fn_o(i,j) )*recip_dT
       ENDDO
      ENDDO

C--   end 1rst loop on strip number "ns"
c     ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sm_o,sm_v,sm_x,sm_xx,sm_xy,sm_xz,sm_y,sm_yy,sm_yz,sm_z,sm_zz
CADJ &     = somtape_x, key = ns, kind = isbyte
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   start 2nd loop on strip number "ns"
c     DO ns=1,nbStrips

C---  part.2 : re-adjust moments remaining in the box
C      take off from grid box (i): negative U(i) and positive U(i+1)
      DO j=jMinUpd(ns),jMaxUpd(ns)
       DO i=iMinUpd(1),iMaxUpd(1)
#ifdef ALLOW_OBCS
        IF ( maskIn(i,j).NE.zeroRS ) THEN
#endif /* ALLOW_OBCS */
        alf1  = 1. _d 0 - aln(i,j) - alp(i+1,j)
        alf1q = alf1*alf1
        alpmn = alp(i+1,j) - aln(i,j)
        sm_v (i,j) = sm_v (i,j) - fn_v (i,j) - fp_v (i+1,j)
        sm_o (i,j) = sm_o (i,j) - fn_o (i,j) - fp_o (i+1,j)
        sm_x (i,j) = alf1q*( sm_x(i,j) - three*alpmn*sm_xx(i,j) )
        sm_xx(i,j) = alf1*alf1q*sm_xx(i,j)
        sm_xy(i,j) = alf1q*sm_xy(i,j)
        sm_xz(i,j) = alf1q*sm_xz(i,j)
        sm_y (i,j) = sm_y (i,j) - fn_y (i,j) - fp_y (i+1,j)
        sm_yy(i,j) = sm_yy(i,j) - fn_yy(i,j) - fp_yy(i+1,j)
        sm_z (i,j) = sm_z (i,j) - fn_z (i,j) - fp_z (i+1,j)
        sm_zz(i,j) = sm_zz(i,j) - fn_zz(i,j) - fp_zz(i+1,j)
        sm_yz(i,j) = sm_yz(i,j) - fn_yz(i,j) - fp_yz(i+1,j)
#ifdef ALLOW_OBCS
        ENDIF
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  part.3 : Put the temporary moments into appropriate neighboring boxes
C      add into grid box (i): positive U(i) and negative U(i+1)
      DO j=jMinUpd(ns),jMaxUpd(ns)
       DO i=iMinUpd(1),iMaxUpd(1)
#ifdef ALLOW_OBCS
        IF ( maskIn(i,j).NE.zeroRS ) THEN
#endif /* ALLOW_OBCS */
        sm_v (i,j) = sm_v (i,j) + fp_v (i,j) + fn_v (i+1,j)
        alfp = fp_v( i ,j)/sm_v(i,j)
        alfn = fn_v(i+1,j)/sm_v(i,j)
        alf1 = 1. _d 0 - alfp - alfn
        alp1 = 1. _d 0 - alfp
        aln1 = 1. _d 0 - alfn
        alpmn = alfp - alfn
        locTp = alfp*sm_o(i,j) - alp1*fp_o(i,j)
        locTn = alfn*sm_o(i,j) - aln1*fn_o(i+1,j)
        sm_xx(i,j) = alf1*alf1*sm_xx(i,j) + alfp*alfp*fp_xx(i,j)
     &                                    + alfn*alfn*fn_xx(i+1,j)
     &   - 5. _d 0*(-alpmn*alf1*sm_x(i,j) + alfp*alp1*fp_x(i,j)
     &                                    - alfn*aln1*fn_x(i+1,j)
     &             + twoRL*alfp*alfn*sm_o(i,j) + (alp1-alfp)*locTp
     &                                         + (aln1-alfn)*locTn
     &             )
        sm_xy(i,j) = alf1*sm_xy(i,j) + alfp*fp_xy(i,j)
     &                               + alfn*fn_xy(i+1,j)
     &     + three*( alpmn*sm_y(i,j) - alp1*fp_y(i,j)
     &                               + aln1*fn_y(i+1,j)
     &             )
        sm_xz(i,j) = alf1*sm_xz(i,j) + alfp*fp_xz(i,j)
     &                               + alfn*fn_xz(i+1,j)
     &     + three*( alpmn*sm_z(i,j) - alp1*fp_z(i,j)
     &                               + aln1*fn_z(i+1,j)
     &             )
        sm_x (i,j) = alf1*sm_x(i,j) + alfp*fp_x(i,j) + alfn*fn_x(i+1,j)
     &             + three*( locTp - locTn )
        sm_o (i,j) = sm_o (i,j) + fp_o (i,j) + fn_o (i+1,j)
        sm_y (i,j) = sm_y (i,j) + fp_y (i,j) + fn_y (i+1,j)
        sm_yy(i,j) = sm_yy(i,j) + fp_yy(i,j) + fn_yy(i+1,j)
        sm_z (i,j) = sm_z (i,j) + fp_z (i,j) + fn_z (i+1,j)
        sm_zz(i,j) = sm_zz(i,j) + fp_zz(i,j) + fn_zz(i+1,j)
        sm_yz(i,j) = sm_yz(i,j) + fp_yz(i,j) + fn_yz(i+1,j)
#ifdef ALLOW_OBCS
        ENDIF
#endif /* ALLOW_OBCS */
       ENDDO
      ENDDO

C--   end 2nd loop on strip number "ns"
      ENDDO

      RETURN
      END
