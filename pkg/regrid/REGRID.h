C $Header: /u/gcmpack/MITgcm/pkg/regrid/REGRID.h,v 1.1 2006/08/15 04:05:48 edhill Exp $
C $Name:  $


C     Package flags
      LOGICAL regridisON
      LOGICAL regrid_MNC
      LOGICAL regrid_MDSIO
      COMMON /REGRID_PACKAGE/
     &     regridisON, regrid_MNC, regrid_MDSIO


C     =====  Parameters  =====
C     Integer
      INTEGER regrid_ngrids
      COMMON /REGRID_PARAM_I/
     &     regrid_ngrids

C     Character
      CHARACTER*(MAX_LEN_FNAM) regrid_fbname_in(REGRID_NGRID_MAX)
      CHARACTER*(MAX_LEN_FNAM) regrid_fbname_out(REGRID_NGRID_MAX)
      COMMON /REGRID_CHAR/
     &     regrid_fbname_in, regrid_fbname_out


C     =====  DATA  =====
C     Real
      _RL regrid_amat(REGRID_NELEM_MAX,nSx,nSy)
      COMMON /REGRID_RL/
     &     regrid_amat

C     Integer
      INTEGER regrid_ibeg(REGRID_NGRID_MAX,nSx,nSy)
      INTEGER regrid_iend(REGRID_NGRID_MAX,nSx,nSy)
      INTEGER regrid_nout(REGRID_NGRID_MAX)
      INTEGER regrid_i_loc(REGRID_NELEM_MAX,nSx,nSy)
      INTEGER regrid_j_loc(REGRID_NELEM_MAX,nSx,nSy)
      INTEGER regrid_i_out(REGRID_NELEM_MAX,nSx,nSy)
      COMMON /REGRID_RL/
     &     regrid_ibeg, regrid_iend, regrid_nout,
     &     regrid_i_loc, regrid_j_loc,
     &     regrid_i_out


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
