C--   COMMON /DIC_LOAD/
C     DIC_ldRec     :: time-record currently loaded (in temp arrays *[1])
C     chlinput      :: chlorophyll climatology input field [mg/m3]

      COMMON /DIC_LOAD_I/ DIC_ldRec
      COMMON /DIC_LOAD_RS/
     &    dicwind0, dicwind1, ice0, ice1, atmosp0, atmosp1,
     &    silicaSurf0, silicaSurf1
#ifdef DIC_CALCITE_SAT
     &  , silicaDeep0, silicaDeep1
#endif
#ifdef READ_PAR
     &  , par0, par1
#endif
#ifdef ALLOW_FE
     &  , feinput0, feinput1
#endif
#ifdef LIGHT_CHL
     &  , chlinput
#endif

      INTEGER DIC_ldRec(nSx,nSy)
      _RS dicwind0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dicwind1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice0       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice1       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS silicaSurf0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS silicaSurf1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef DIC_CALCITE_SAT
      _RS silicaDeep0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS silicaDeep1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef READ_PAR
      _RS par0       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS par1       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_FE
      _RS feinput0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS feinput1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef LIGHT_CHL
      _RS chlinput   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
