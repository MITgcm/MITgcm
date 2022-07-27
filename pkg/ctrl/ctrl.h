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
c     ==================================================================
c     HEADER CONTROLVARS ctrl.h
c     ==================================================================

      integer     maxcvars
#if (defined (CTRL_SET_OLD_MAXCVARS_30))
      parameter ( maxcvars = 30 )
#elif (defined (CTRL_SET_OLD_MAXCVARS_40))
      parameter ( maxcvars = 40 )
#elif (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      parameter ( maxcvars = 400 )
#else
      parameter ( maxcvars = 60 )
#endif

cph ctrlprec will be set to 32 for ECCO to reduce I/O
cph but jeopardizes some gradient checks, so should be
cph set to 64 by default.
      integer     ctrlprec
      common /controlparams_i/ ctrlprec

#ifdef ALLOW_ADMTLM
      integer admtlmrec
      parameter( admtlmrec = Nx*Ny*(4*Nr+1) )
      common / controlvars_admtlm_r /
     &                       cbuffGlobal
      _RL cbuffGlobal( admtlmrec )
#endif

      common /controlparams_r/
     &                       delZexp,
     &                       forcingPrecond

      _RL delZexp
      _RL forcingPrecond

C     doInitXX            :: at iter 0 only, set ctrls to 0 and write
C                            to xx*000.data
C     doMainPack          :: pack adxx*data files into ecco_cost_* file
C                            (usually for optim.x)
C     doMainUnpack        :: unpack ecco_ctrl_* file (usually from optim.x)
C                            into xx_*data files
C     doPackDiag          :: output diag_pack*/diag_unpack* files during
C                            ctrl_pack/ctrl_unpack
C     doSinglePrecTapelev :: reduce precision of ad tape files to float32
C                            (only used in pkg/autodiff ...)

      common /controlvars_l /
     &                       doInitXX,
     &                       doAdmTlm,
     &                       doPackDiag,
     &                       doZscaleUnpack,
     &                       doZscalePack,
     &                       doMainUnpack,
     &                       doMainPack,
     &                       doSinglePrecTapelev,
     &                       doAdmtlmBypassAD

      logical doInitXX
      logical doAdmTlm
      logical doPackDiag
      logical doZscaleUnpack
      logical doZscalePack
      logical doMainUnpack
      logical doMainPack
      logical doSinglePrecTapelev
      logical doAdmtlmBypassAD

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
     &                       nwetwtile,
     &                       nwetvtile,
     &                       nwetcglobal,
     &                       nwetsglobal,
     &                       nwetwglobal,
     &                       nwetvglobal,
     &                       nbuffglobal
      integer nvartype
      integer nvarlength
      integer ncvarindex    ( maxcvars )
      integer ncvarrecs     ( maxcvars )
      integer ncvarrecstart ( maxcvars )
      integer ncvarrecsend  ( maxcvars )
      integer ncvarxmax     ( maxcvars )
      integer ncvarymax     ( maxcvars )
      integer ncvarnrmax    ( maxcvars )
      integer nwetctile     ( nsx,nsy,nr )
      integer nwetstile     ( nsx,nsy,nr )
      integer nwetwtile     ( nsx,nsy,nr )
      integer nwetvtile     ( nsx,nsy,nr )
      integer nwetcglobal     ( nr )
      integer nwetsglobal     ( nr )
      integer nwetwglobal     ( nr )
      integer nwetvglobal     ( nr )
      integer nbuffglobal

#ifdef ALLOW_SHELFICE
      common /controlvars_i_shifwflx/
     &     nwetitile, nwetiglobal, filenWetiGlobal
      integer nwetitile     ( nsx,nsy,nr )
      integer nwetiglobal     ( nr )
      integer filenWetiGlobal(nr)
#endif /* ALLOW_SHELFICE */

      common /controlvars_c/
     &                       ncvargrd
     &                     , yadprefix
      character*(1) ncvargrd(maxcvars)
      character*(2) yadprefix

      common /controlvec_header_i/
     &        filenvartype,
     &        filenvarlength,
     &        fileOptimCycle,
     &        filencbuffindex,
     &        fileIg,
     &        fileJg,
     &        fileI,
     &        fileJ,
     &        filensx,
     &        filensy,
     &        filek,
     &        filenWetcGlobal,
     &        filenWetsGlobal,
     &        filenWetwGlobal,
     &        filenWetvGlobal,
     &        filencvarindex,
     &        filencvarrecs,
     &        filencvarxmax,
     &        filencvarymax,
     &        filencvarnrmax
      integer        filenvartype
      integer        filenvarlength
      integer        fileOptimCycle
      integer        filencbuffindex
      integer        fileIg
      integer        fileJg
      integer        fileI
      integer        fileJ
      integer        filensx
      integer        filensy
      integer        filek
      integer        filenWetcGlobal(nr)
      integer        filenWetsGlobal(nr)
      integer        filenWetwGlobal(nr)
      integer        filenWetvGlobal(nr)
      integer        filencvarindex(maxcvars)
      integer        filencvarrecs(maxcvars)
      integer        filencvarxmax(maxcvars)
      integer        filencvarymax(maxcvars)
      integer        filencvarnrmax(maxcvars)

      common /controlvec_header_r/
     &               filefc
      _RL            filefc

      common /controlvec_header_c/
     &        fileYctrlid,
     &        filencvargrd
      character*(10) fileYctrlid
      character*( 1) filencvargrd(maxcvars)

c     Define unit weight as a placeholder
      common /ctrl_weights_unit_r/
     &                        wunit
      _RL wunit     (nr,nsx,nsy)

      common /packnames_c/
     &                      yadmark,
     &                      ctrlname,
     &                      costname,
     &                      scalname,
     &                      maskname,
     &                      metaname,
     &                      yctrlid,
     &                      yctrlposunpack,
     &                      yctrlpospack
      character*2 yadmark
      character*9 ctrlname
      character*9 costname
      character*9 scalname
      character*9 maskname
      character*9 metaname
      character*10 yctrlid
      character*4 yctrlposunpack
      character*4 yctrlpospack

#ifdef ALLOW_ADMTLM
      integer          maxm, maxn
      parameter       ( maxm = Nx*Ny*(4*Nr+1), maxn=Nx*Ny*(4*Nr+1) )

      common /admtlm_i/ nveccount
      integer nveccount

      common /admtlm_r/ phtmpadmtlm
      double precision phtmpadmtlm(maxn)
#endif

c     Control variables:
c     ==================
c
#ifdef ALLOW_OPENAD
C
      common /controlvars_r_openad/
     &        xx_place_holder
# ifdef ALLOW_GENARR2D_CONTROL
     &      , xx_genarr2d
# endif
# ifdef ALLOW_GENARR3D_CONTROL
     &      , xx_genarr3d
# endif
C
      _RL xx_place_holder

# ifdef ALLOW_GENARR2D_CONTROL
      _RL xx_genarr2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,
     &                maxCtrlArr2D)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      _RL xx_genarr3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,
     &                maxCtrlArr3D)
# endif

#endif
c     ==================================================================
c     END OF HEADER CONTROLVARS ctrl.h
c     ==================================================================
