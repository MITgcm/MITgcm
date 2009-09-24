C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_FFIELDS.h,v 1.6 2009/09/24 20:08:06 dfer Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     *==========================================================*
C     | AIM_FFIELDS.h
C     | o AIM (surface) forcing fields.
C     *==========================================================*


C---  COMMON /AIM_FFIELDS_R/
C     truncSurfP : use to correct Surf.Temp for truncation of Surf.Ref.Press
C     aim_landFr :: Holds Land Fraction       ( 0-1 )
C     aim_veget  :: Holds vegetation fraction ( 0-1 )
C     aim_albedo :: Holds surface albedo      ( 0-1 )
C     aim_...0   :: Holds data centered in time before the current time
C     aim_...1   :: Holds data centered in time after  the current time
C     aim_sst0,1 :: Holds Sea  surface temperature ( K )
C     aim_lst0,1 :: Holds Land surface temperature ( K )
C     aim_oic0,1 :: Holds Sea Ice fraction ( 0-1 )
C     aim_snw0,1 :: Holds snow depth ( 0-1 )
C     aim_sw10,1 :: Holds soil water content, level 1 ( 0-1 )
C     aim_sw20,1 :: Holds soil water content, level 2 ( 0-1 )
C     aim_qfx0,1 :: Holds q-flux (W/m^2, >0 = OHT convergence = T increases)

      COMMON /AIM_FFIELDS_R/
     &   truncSurfP, aim_landFr, aim_veget, aim_albedo,
     &   aim_sst0, aim_lst0, aim_oic0, aim_snw0, aim_sw10, aim_sw20,
     &   aim_sst1, aim_lst1, aim_oic1, aim_snw1, aim_sw11, aim_sw21,
     &   aim_qfx0, aim_qfx1

      _RS  truncSurfP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_landFr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_veget (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_albedo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RS  aim_sst0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_lst0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_oic0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_snw0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_sw10  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_sw20  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RS  aim_sst1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_lst1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_oic1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_snw1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_sw11  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_sw21  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RS  aim_qfx0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_qfx1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
