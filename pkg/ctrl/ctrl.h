
c     ==================================================================
c     HEADER CONTROLVARS
c     ==================================================================
c
c     o Control variables of the ECCO state estimation tool.
c
c     Depending on the specific problem to be studied users will have to
c     modify this header file.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER CONTROLVARS
c     ==================================================================
c
c     nwet[c/s/w]tile - Number of wet points in a tile for center (c),
c                       south (s), and western (w) mask, resp. .

      integer     maxcvars
      parameter ( maxcvars = 20 )

      common /controlvars_i/
     &                       nvartype,
     &                       nvarlength,
     &                       ncvarindex,
     &                       ncvarrecs,
     &                       ncvarrecstart,
     &                       ncvarrecsend,
     &                       ncvarxmax,
     &                       ncvarymax,
     &                       ncvarnrmax,
     &                       nwetctile,
     &                       nwetstile,
     &                       nwetwtile
      integer nvartype
      integer nvarlength
      integer ncvarindex    ( maxcvars )
      integer ncvarrecs     ( maxcvars )
      integer ncvarrecstart ( maxcvars )
      integer ncvarrecsend  ( maxcvars )
      integer ncvarxmax     ( maxcvars )
      integer ncvarymax     ( maxcvars )
      integer ncvarnrmax    ( maxcvars )
      integer nwetctile     ( nsx, nsy, nr )
      integer nwetstile     ( nsx, nsy, nr )
      integer nwetwtile     ( nsx, nsy, nr )

      common /controlvars_c/
     &                       ncvargrd
      character*(1) ncvargrd(maxcvars)

c     Control variables:
c     ==================
c
c     xx_theta - control vector temperature part.
c     xx_salt  - control vector salt part.
cph(
c     xx_... are to be replaced by tmpfld2d/3d throughout the code; 
c     control variables are written to / read from active files
c     TAMC sees xx_..._dummy

      common /controlvars_r/
     &                       tmpfld2d,
     &                       tmpfld3d
      _RL tmpfld2d (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL tmpfld3d (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)


c     Files where the control variables are stored:
c     =============================================
c
c     xx_theta_file - control vector temperature part.
c     xx_salt_file  - control vector salinity part.

      common /controlfiles_c/
     &                      xx_theta_file
     &                    , xx_salt_file
     &                    , xx_tr1_file
     &                    , xx_tauu_file
     &                    , xx_tauv_file
     &                    , xx_sflux_file
     &                    , xx_hflux_file
     &                    , xx_sss_file
     &                    , xx_sst_file
     &                    , xx_diffkr_file
     &                    , xx_kapgm_file
      character*(MAX_LEN_FNAM) xx_theta_file
      character*(MAX_LEN_FNAM) xx_salt_file
      character*(MAX_LEN_FNAM) xx_tr1_file
      character*(MAX_LEN_FNAM) xx_tauu_file
      character*(MAX_LEN_FNAM) xx_tauv_file
      character*(MAX_LEN_FNAM) xx_sflux_file
      character*(MAX_LEN_FNAM) xx_hflux_file
      character*(MAX_LEN_FNAM) xx_sss_file
      character*(MAX_LEN_FNAM) xx_sst_file
      character*(MAX_LEN_FNAM) xx_diffkr_file
      character*(MAX_LEN_FNAM) xx_kapgm_file

      common /packnames_c/
     &                      yadmark
     &                    , expId
     &                    , ctrlname
     &                    , costname
     &                    , scalname
     &                    , maskname
     &                    , metaname
      character*2 yadmark
      character*10 expId
      character*9 ctrlname
      character*9 costname
      character*9 scalname
      character*9 maskname
      character*9 metaname

c     ==================================================================
c     END OF HEADER CONTROLVARS
c     ==================================================================


