CBOP
C     !ROUTINE: CTRL.h
C     !INTERFACE:
C     #include "CTRL.h"

C     !DESCRIPTION:
C     *================================================================*
C     | CTRL.h
C     | o Header file defining control variables of the ECCO state
C     |   estimation tool.
C     | o Depending on the specific problem to be studied users will
C     |   have to modify this header file.
C     | o started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     *================================================================*
CEOP

      INTEGER     maxcvars
#if (defined (CTRL_SET_OLD_MAXCVARS_30))
      PARAMETER ( maxcvars = 30 )
#elif (defined (CTRL_SET_OLD_MAXCVARS_40))
      PARAMETER ( maxcvars = 40 )
#elif (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      PARAMETER ( maxcvars = 400 )
#else
      PARAMETER ( maxcvars = 60 )
#endif

cph ctrlprec will be set to 32 for ECCO to reduce I/O
cph but jeopardizes some gradient checks, so should be
cph set to 64 by default.
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

      COMMON /controlvars_l /
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
     &     nwetitile, nwetiglobal, filenWetiGlobal
      INTEGER nwetitile     ( nSx,nSy,Nr )
      INTEGER nwetiglobal     ( Nr )
      INTEGER filenWetiGlobal(Nr)
#endif /* ALLOW_SHELFICE */

      COMMON /controlvars_c/
     &                       ncvargrd
     &                     , yadprefix
      CHARACTER*(1) ncvargrd(maxcvars)
      CHARACTER*(2) yadprefix

      COMMON /controlvec_header_i/
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
      INTEGER filenvartype
      INTEGER filenvarlength
      INTEGER fileOptimCycle
      INTEGER filencbuffindex
      INTEGER fileIg
      INTEGER fileJg
      INTEGER fileI
      INTEGER fileJ
      INTEGER filensx
      INTEGER filensy
      INTEGER filek
      INTEGER filenWetcGlobal(Nr)
      INTEGER filenWetsGlobal(Nr)
      INTEGER filenWetwGlobal(Nr)
      INTEGER filenWetvGlobal(Nr)
      INTEGER filencvarindex(maxcvars)
      INTEGER filencvarrecs(maxcvars)
      INTEGER filencvarxmax(maxcvars)
      INTEGER filencvarymax(maxcvars)
      INTEGER filencvarnrmax(maxcvars)

      COMMON /controlvec_header_r/
     &               filefc
      _RL            filefc

      COMMON /controlvec_header_c/
     &        fileYctrlid,
     &        filencvargrd
      CHARACTER*(10) fileYctrlid
      CHARACTER*( 1) filencvargrd(maxcvars)

c     Define unit weight as a placeholder
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

c     Control variables:
c     ==================
c
#ifdef ALLOW_OPENAD
C
      COMMON /controlvars_r_openad/
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
      _RL xx_genarr2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,
     &                maxCtrlArr2D)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      _RL xx_genarr3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                maxCtrlArr3D)
# endif

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
