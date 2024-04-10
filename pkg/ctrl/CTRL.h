CBOP
C     !ROUTINE: CTRL.h
C     !INTERFACE:
C     #include "CTRL.h"

C     !DESCRIPTION:
C     *================================================================*
C     | CTRL.h
C     | o Header file defining control variables of the ECCO state
C     |   estimation tool.
C     | o Depending on the specific problem to be studied users may
C     |   have to modify this header file.
C     | o started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     *================================================================*
CEOP

C--   set maximum number of control variables:
      INTEGER     maxcvars
#ifdef ALLOW_OBCS_CONTROL
      PARAMETER( maxcvars = 4
#else
      PARAMETER( maxcvars = 0
#endif
     &         + maxCtrlArr2D + maxCtrlArr3D + maxCtrlTim2D )

#ifdef READ_OLD_CTRL_PACK_FILE
C--   Just to enable to read-in old packed-ctrl file (specially the header):
C     set "old_maxcvars" to previous maxcvars value that was used to write
C     this specific old (prior to PR #796) packed-ctrl file
      INTEGER old_maxcvars
      PARAMETER( old_maxcvars = 400 )
#endif

C-  ctrlprec will be set to 32 for ECCO to reduce I/O but this jeopardizes
C   gradient checks accuracy, so should be set to 64 by default.
      INTEGER     ctrlprec
      COMMON /controlparams_i/ ctrlprec

#ifdef ALLOW_ADMTLM
      INTEGER admtlmrec
      PARAMETER( admtlmrec = Nx*Ny*(4*Nr+1) )
      COMMON /controlvars_admtlm_r/
     &                       cbuffGlobal
      _RL cbuffGlobal( admtlmrec )
#endif

      COMMON /controlparams_r/
     &                       delZexp,
     &                       forcingPrecond

      _RL delZexp
      _RL forcingPrecond

      COMMON /controlparams_c/
     &                       ctrlDir

      CHARACTER*(MAX_LEN_FNAM) ctrlDir

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

      COMMON /controlparams_l/
     &                       doInitXX,
     &                       doAdmTlm,
     &                       doPackDiag,
     &                       doZscaleUnpack,
     &                       doZscalePack,
     &                       doMainUnpack,
     &                       doMainPack,
     &                       doSinglePrecTapelev,
     &                       doAdmtlmBypassAD

      LOGICAL doInitXX
      LOGICAL doAdmTlm
      LOGICAL doPackDiag
      LOGICAL doZscaleUnpack
      LOGICAL doZscalePack
      LOGICAL doMainUnpack
      LOGICAL doMainPack
      LOGICAL doSinglePrecTapelev
      LOGICAL doAdmtlmBypassAD

C--   parameters vectors, set in S/R CTRL_INIT_CTRLVAR, that describe
C     the contorl variables, also used for identification across
C     different parts of the code:
C     ncvarfname    :: unique (and predefined) name of control variable
C     ncvarindex    :: number to identify variable, depends specified ctrl-vars
C     ncvarrecs     :: number of records for time dependent ctrl-variables;
C     ncvarrecstart :: first and last record of time dependent ctrl-variables;
C     ncvarrecsend  :: for constant-in-time variables all three are 1
C     ncvargrd      :: type or/and position on the grid, possible values:
C                      'c' (cell Center), 'w' (West face) ,'s' (South face),
C                      'i' (shelfice), 'm' (obcs)
C     ncvartype     :: shape of the grid: Arr3D, Arr2D, Tim2D, SecXZ, SecYZ
C     ncvarx/y/nrmax:: i,j,k-dimensions of ctrl-variable (on tile)

C--   holds control-variable setting and params as maxcvars long vector
C     in following "controlvar_*" common blocks:
      COMMON /controlvars_i/
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
      INTEGER nvartype
      INTEGER nvarlength
      INTEGER ncvarindex    ( maxcvars )
      INTEGER ncvarrecs     ( maxcvars )
      INTEGER ncvarrecstart ( maxcvars )
      INTEGER ncvarrecsend  ( maxcvars )
      INTEGER ncvarxmax     ( maxcvars )
      INTEGER ncvarymax     ( maxcvars )
      INTEGER ncvarnrmax    ( maxcvars )
      INTEGER nwetctile     ( nSx,nSy,Nr )
      INTEGER nwetstile     ( nSx,nSy,Nr )
      INTEGER nwetwtile     ( nSx,nSy,Nr )
      INTEGER nwetvtile     ( nSx,nSy,Nr )
      INTEGER nwetcglobal     ( Nr )
      INTEGER nwetsglobal     ( Nr )
      INTEGER nwetwglobal     ( Nr )
      INTEGER nwetvglobal     ( Nr )
      INTEGER nbuffglobal

#ifdef ALLOW_SHELFICE
      COMMON /controlvars_i_shifwflx/
     &     nwetitile, nwetiglobal
      INTEGER nwetitile     ( nSx,nSy,Nr )
      INTEGER nwetiglobal     ( Nr )
#endif /* ALLOW_SHELFICE */

      COMMON /controlvars_c/
     &                       ncvargrd,
     &                       ncvartype,
     &                       ncvarfname,
     &                       yadprefix
      CHARACTER*(1)            ncvargrd  ( maxcvars )
      CHARACTER*(5)            ncvartype ( maxcvars )
      CHARACTER*(MAX_LEN_FNAM) ncvarfname( maxcvars )
      CHARACTER*(2)            yadprefix

C     Define unit weight as a placeholder
      COMMON /ctrl_weights_unit_r/
     &                        wunit
      _RL wunit     (Nr,nSx,nSy)

      COMMON /packnames_c/
     &                      yadmark,
     &                      ctrlname,
     &                      costname,
     &                      scalname,
     &                      maskname,
     &                      metaname,
     &                      yctrlid,
     &                      yctrlposunpack,
     &                      yctrlpospack
      CHARACTER*2 yadmark
      CHARACTER*9 ctrlname
      CHARACTER*9 costname
      CHARACTER*9 scalname
      CHARACTER*9 maskname
      CHARACTER*9 metaname
      CHARACTER*10 yctrlid
      CHARACTER*4 yctrlposunpack
      CHARACTER*4 yctrlpospack

#ifdef ALLOW_ADMTLM
      INTEGER          maxm, maxn
      PARAMETER       ( maxm = Nx*Ny*(4*Nr+1), maxn=Nx*Ny*(4*Nr+1) )

      COMMON /admtlm_i/ nveccount
      INTEGER nveccount

      COMMON /admtlm_r/ phtmpadmtlm
      DOUBLE PRECISION phtmpadmtlm(maxn)
#endif

C     Control variables:
C     ==================

#ifdef ALLOW_OPENAD

      COMMON /controlvars_r_openad/
     &        xx_place_holder
# ifdef ALLOW_GENARR2D_CONTROL
     &      , xx_genarr2d
# endif
# ifdef ALLOW_GENARR3D_CONTROL
     &      , xx_genarr3d
# endif

      _RL xx_place_holder
# ifdef ALLOW_GENARR2D_CONTROL
      _RL xx_genarr2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,
     &                maxCtrlArr2D)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      _RL xx_genarr3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                maxCtrlArr3D)
# endif

#endif /* ALLOW_OPENAD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
