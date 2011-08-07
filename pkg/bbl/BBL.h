C $Header: /u/gcmpack/MITgcm/pkg/bbl/BBL.h,v 1.2 2011/08/07 07:08:15 dimitri Exp $
C $Name:  $

#ifdef ALLOW_BBL

C     bbl_RelaxH  :: horizontal relaxation coefficient for
C                    transferring properties downslope (s)
C     bbl_RelaxR  :: vertical relaxation coefficient for exchanging
C                    properties with containing grid cell (s)
C     bbl_InitEta :: default bbl thickness
      _RL                 bbl_RelaxH, bbl_RelaxR, bbl_InitEta
      COMMON /BBL_PARM_R/ bbl_RelaxH, bbl_RelaxR, bbl_InitEta

C     file names for initial conditions:
      CHARACTER*(MAX_LEN_FNAM) bbl_thetaFile
      CHARACTER*(MAX_LEN_FNAM) bbl_saltFile
      CHARACTER*(MAX_LEN_FNAM) bbl_etaFile
      COMMON /BBL_PARM_C/ bbl_thetaFile, bbl_saltFile, bbl_etaFile

C     BBL 2-dim. fields
      _RL bbl_theta (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bbl_salt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bbl_eta   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bbl_rho_nr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /BBL_STATE_2D/ bbl_theta, bbl_salt, bbl_eta, bbl_rho_nr

C     BBL tendency terms
      _RL bbl_TendTheta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bbl_TendSalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /BBL_TENDENCY/ bbl_TendTheta, bbl_TendSalt

#endif /* ALLOW_BBL */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
