
c     ==================================================================
c     HEADER GRADIENT_CHECK
c     ==================================================================
c
c     o Header for doing gradient checks with the ECCO ocean state
c       estimation tool.
c
c     started: Christian Eckert eckert@mit.edu  01-Mar-2000
c
c     changed: Christian Eckert eckert@mit.edu
c              heimbach@mit.edu 24-Feb-2003
c
c
c     ==================================================================
c     HEADER GRADIENT_CHECK
c     ==================================================================

c     maxgrdchecks - maximum number of gradient checks done per tile.

      integer     maxgrdchecks
      parameter ( maxgrdchecks = 4000 )

      common /grdchkparms_r/
     &                       grdchk_eps
      _RL     grdchk_eps

      common /grdchkparms_l/
     &                       useCentralDiff
      logical useCentralDiff

      common /grdchkparms_i/
     &                       nbeg,
     &                       nend,
     &                       nstep,
     &                       grdchkvarindex,
     &                       grdchkwhichproc,
     &                       iGloPos,
     &                       jGloPos,
     &                       kGloPos,
     &                       iGloTile,
     &                       jGloTile,
     &                       obcsglo,
     &                       recglo,
     &                       iwetsum

      integer nbeg
      integer nend
      integer nstep
      integer grdchkvarindex
      integer grdchkwhichproc
      integer iGloPos
      integer jGloPos
      integer kGloPos
      integer iGloTile
      integer jGloTile
      integer obcsglo
      integer recglo
      integer iwetsum(nsx,nsy,0:nr)

      common /grdchk_r/
     &                  fcrmem, fcppmem, fcpmmem,
     &                  xxmemref, xxmempert,
     &                  gfdmem, adxxmem, ftlxxmem,
     &                  ratioadmem, ratioftlmem
      _RL fcrmem      ( maxgrdchecks )
      _RL fcppmem     ( maxgrdchecks )
      _RL fcpmmem     ( maxgrdchecks )
      _RL xxmemref    ( maxgrdchecks )
      _RL xxmempert   ( maxgrdchecks )
      _RL gfdmem      ( maxgrdchecks )
      _RL adxxmem     ( maxgrdchecks )
      _RL ftlxxmem    ( maxgrdchecks )
      _RL ratioadmem  ( maxgrdchecks )
      _RL ratioftlmem ( maxgrdchecks )

      common /grdchk_i/
     &                  ncvarcomp, maxncvarcomps,
     &                  nwettile,
     &                  irecmem,
     &                  bimem, bjmem,
     &                  ilocmem,jlocmem,klocmem,iobcsmem,
     &                  ichkmem, icompmem, itestmem, ierrmem, icglomem
      integer ncvarcomp
      integer maxncvarcomps
      integer nwettile( nsx,nsy,nr,nobcs )
      integer irecmem ( maxgrdchecks )
      integer bjmem   ( maxgrdchecks )
      integer bimem   ( maxgrdchecks )
      integer klocmem ( maxgrdchecks )
      integer iobcsmem( maxgrdchecks )
      integer jlocmem ( maxgrdchecks )
      integer ilocmem ( maxgrdchecks )
      integer ichkmem ( maxgrdchecks )
      integer icompmem( maxgrdchecks )
      integer itestmem( maxgrdchecks )
      integer ierrmem ( maxgrdchecks )
      integer icglomem( maxgrdchecks )

#ifdef ALLOW_OBCS_CONTROL
      common /grdchk_obcs_mask_r/
     &                            grdchk_maskxz, grdchk_maskyz
      _RL     grdchk_maskxz(1-olx:snx+olx,nr,nsx,nsy,nobcs)
      _RL     grdchk_maskyz(1-oly:sny+oly,nr,nsx,nsy,nobcs)
#endif



c     ==================================================================
c     END OF HEADER GRADIENT_CHECK
c     ==================================================================


