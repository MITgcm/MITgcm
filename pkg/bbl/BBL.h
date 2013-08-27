C $Header: /u/gcmpack/MITgcm/pkg/bbl/BBL.h,v 1.3 2013/08/27 21:34:21 dimitri Exp $
C $Name:  $

#ifdef ALLOW_BBL

C     bbl_wvel    :: default vertical entrainment velocity (m/s)
C     bbl_hvvel   :: default horizontal velocity of BBL (m/s)
C     bbl_initEta :: default initial thickness of BBL (m)
      _RL                 bbl_wvel, bbl_hvel, bbl_initEta
      COMMON /BBL_PARM_R/ bbl_wvel, bbl_hvel, bbl_initEta

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
