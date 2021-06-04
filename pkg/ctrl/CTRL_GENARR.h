C     ==================================================================
C     CTRL_GENARR.h
C     ==================================================================

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))

      COMMON /CONTROLFILES_CARR/
     &     xx_genarr2d_file,
     &     xx_genarr3d_file,
     &     xx_genarr2d_weight,
     &     xx_genarr3d_weight,
     &     xx_genarr2d_preproc, xx_genarr2d_preproc_c,
     &     xx_genarr3d_preproc, xx_genarr3d_preproc_c
      CHARACTER*(MAX_LEN_FNAM) xx_genarr2d_file(maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr3d_file(maxCtrlArr3D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr2d_weight(maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM) xx_genarr3d_weight(maxCtrlArr3D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_genarr2d_preproc(maxCtrlProc,maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_genarr3d_preproc(maxCtrlProc,maxCtrlArr3D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_genarr2d_preproc_c(maxCtrlProc,maxCtrlArr2D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_genarr3d_preproc_c(maxCtrlProc,maxCtrlArr3D)

      COMMON /CONTROLFILES_RARR/
     &     genarr2dPrecond, genarr3dPrecond,
     &     xx_genarr2d_bounds,xx_genarr3d_bounds,
     &     xx_genarr2d_preproc_r,xx_genarr3d_preproc_r
      _RL genarr2dPrecond(maxCtrlArr2D)
      _RL genarr3dPrecond(maxCtrlArr3D)
      _RL xx_genarr2d_bounds(5,maxCtrlArr2D)
      _RL xx_genarr3d_bounds(5,maxCtrlArr3D)
      _RL xx_genarr2d_preproc_r(maxCtrlProc,maxCtrlArr2D)
      _RL xx_genarr3d_preproc_r(maxCtrlProc,maxCtrlArr3D)

      COMMON /CONTROLFILES_IARR/
     &     xx_genarr2d_preproc_i,xx_genarr3d_preproc_i
      integer xx_genarr2d_preproc_i(maxCtrlProc,maxCtrlArr2D)
      integer xx_genarr3d_preproc_i(maxCtrlProc,maxCtrlArr3D)

      COMMON /CONTROLFILES_CTIM/
     &     xx_gentim2d_file, xx_gentim2d_weight,
     &     xx_gentim2d_preproc, xx_gentim2d_preproc_c
      CHARACTER*(MAX_LEN_FNAM) xx_gentim2d_file(maxCtrlTim2D)
      CHARACTER*(MAX_LEN_FNAM) xx_gentim2d_weight(maxCtrlTim2D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_gentim2d_preproc(maxCtrlProc,maxCtrlTim2D)
      CHARACTER*(MAX_LEN_FNAM)
     &         xx_gentim2d_preproc_c(maxCtrlProc,maxCtrlTim2D)

      COMMON /CONTROLFILES_ITIM/
     &     xx_gentim2d_startdate1,
     &     xx_gentim2d_startdate2,
     &     xx_gentim2d_startdate,
     &     xx_gentim2d_preproc_i
      INTEGER xx_gentim2d_startdate1(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate2(maxCtrlTim2D)
      INTEGER xx_gentim2d_startdate(4,maxCtrlTim2D)
      INTEGER xx_gentim2d_preproc_i(maxCtrlProc,maxCtrlTim2D)

      COMMON /CONTROLFILES_RTIM/
     &     xx_gentim2d_period, gentim2dPrecond,
     &     xx_gentim2d_preproc_r, xx_gentim2d_bounds
      _RL xx_gentim2d_period(maxCtrlTim2D)
      _RL gentim2dPrecond(maxCtrlTim2D)
      _RL xx_gentim2d_preproc_r(maxCtrlProc,maxCtrlTim2D)
      _RL xx_gentim2d_bounds(5,maxCtrlTim2D)

      COMMON /CONTROLFILES_LTIM/
     &     xx_gentim2d_cumsum, xx_gentim2d_glosum
      LOGICAL xx_gentim2d_cumsum(maxCtrlTim2D)
      LOGICAL xx_gentim2d_glosum(maxCtrlTim2D)

      common /controlaux_gencost_r/
     &     objf_gentim2d,  num_gentim2d, mult_gentim2d,
#ifdef ECCO_CTRL_DEPRECATED
     &     objf_gentim2dm,  objf_gentim2dsmoo, num_gentim2dm,
#endif /* ECCO_CTRL_DEPRECATED */
     &     objf_genarr2d,  num_genarr2d, mult_genarr2d,
     &     objf_genarr3d,  num_genarr3d, mult_genarr3d

      _RL  objf_gentim2d(nsx,nsy,maxCtrlTim2D)
      _RL  num_gentim2d(nsx,nsy,maxCtrlTim2D)
      _RL  mult_gentim2d(maxCtrlTim2D)
#ifdef ECCO_CTRL_DEPRECATED
      _RL  objf_gentim2dm(nsx,nsy,maxCtrlTim2D)
      _RL  objf_gentim2dsmoo(nsx,nsy,maxCtrlTim2D)
      _RL  num_gentim2dm(nsx,nsy,maxCtrlTim2D)
#endif /* ECCO_CTRL_DEPRECATED */
      _RL  objf_genarr2d(nsx,nsy,maxCtrlArr2D)
      _RL  num_genarr2d(nsx,nsy,maxCtrlArr2D)
      _RL  mult_genarr2d(maxCtrlArr2D)
      _RL  objf_genarr3d(nsx,nsy,maxCtrlArr3D)
      _RL  num_genarr3d(nsx,nsy,maxCtrlArr3D)
      _RL  mult_genarr3d(maxCtrlArr3D)

#endif

#ifdef ALLOW_GENARR2D_CONTROL
      common /controlaux_genarr2d_r/
     &                      wgenarr2d
      _RL wgenarr2d(1-olx:snx+olx,1-oly:sny+oly,
     &              nsx,nsy,maxCtrlArr2D)
#endif

#ifdef ALLOW_GENARR3D_CONTROL
      common /controlaux_genarr3d_r/
     &                      wgenarr3d
      _RL wgenarr3d(1-olx:snx+olx,1-oly:sny+oly,
     &              nr,nsx,nsy,maxCtrlArr3D)
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      common /controlaux_gentim2d_r/
     &                      xx_gentim2d0,
     &                      xx_gentim2d1,
     &                      xx_gentim2d,
     &                      wgentim2d
      _RL
     & xx_gentim2d0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL
     & xx_gentim2d1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL
     & xx_gentim2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
      _RL
     & wgentim2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy,maxCtrlTim2D)
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
