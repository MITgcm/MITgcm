C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_FFIELDS.h,v 1.3 2005/06/23 20:02:50 edhill Exp $
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
C     aim_sWght0,1 :: weight for time interpolation of surface BC
C                       0/1 = time period before/after the current time
C     aim_sst0,1 :: Holds Sea  surface temperature ( K ) 
C     aim_lst0,1 :: Holds Land surface temperature ( K )
C     aim_oic0,1 :: Holds Sea Ice fraction ( 0-1 )
C     aim_snw0,1 :: Holds snow depth ( 0-1 )
C     aim_sw10,1 :: Holds soil water content, level 1 ( 0-1 )
C     aim_sw20,1 :: Holds soil water content, level 2 ( 0-1 )
      COMMON /AIM_FFIELDS_R/
     &   truncSurfP, aim_landFr, aim_veget, aim_albedo, 
     &   aim_sst0, aim_lst0, aim_oic0, aim_snw0, aim_sw10, aim_sw20,
     &   aim_sst1, aim_lst1, aim_oic1, aim_snw1, aim_sw11, aim_sw21,
     &   aim_sWght0, aim_sWght1
 
      _RL  truncSurfP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy) 
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
      _RL aim_sWght0, aim_sWght1

C--  used only if aim_useMMsurfFc=T : Forcing fields are loaded 
C     and used directly with no time-interpolation.
      _RS  aim_surfTemp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aim_soilWater(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      EQUIVALENCE (aim_surfTemp,  aim_sst0) 
      EQUIVALENCE (aim_soilWater, aim_sw10) 

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
