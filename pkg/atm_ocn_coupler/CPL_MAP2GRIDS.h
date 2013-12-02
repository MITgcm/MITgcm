C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/CPL_MAP2GRIDS.h,v 1.4 2013/12/02 22:03:09 jmc Exp $
C $Name:  $

C     *==========================================================*
C     | CPL_MAP2GRIDS.h
C     |   Declare arrays used for mapping coupling fields from
C     |     one grid (atmos., ocean) to the other grid
C     *==========================================================*

      INTEGER ROsize
      PARAMETER (ROsize=Nx_atm*Ny_atm)

C--   COMMON / RUNOFF_MAP/: to map runoff from atmos. grid to ocean grid
C     nROmap  :: Nunber of connected grid points.
C     ijROatm :: index of land grid point that runoff to the ocean
C     ijROocn :: index of ocean grid point where the runoff ends
C     arROmap :: fraction of the land runoff ijROatm that go to ijROocn
      COMMON / RUNOFF_MAP_I / nROmap, ijROocn, ijROatm
      INTEGER nROmap
      INTEGER ijROocn(ROsize), ijROatm(ROsize)
      COMMON / RUNOFF_MAP_R / arROmap
      _RL arROmap(ROsize)

