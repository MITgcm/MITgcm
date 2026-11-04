C--   COMMON /BLING_LOAD/
C     bling_ldRec     :: time-record currently loaded (in temp arrays *[1])

      COMMON /BLING_LOAD_I/ bling_ldRec
      COMMON /BLING_LOAD_RL/
     &    dicwind0, dicwind1,
     &    apress0, apress1,
     &    silica0, silica1,
     &    ice0, ice1,
     &    InputFe0, InputFe1,
     &    bling_apco20, bling_apco21

      INTEGER bling_ldRec(nSx,nSy)
      _RL dicwind0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dicwind1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL apress0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL apress1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL silica0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL silica1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice0      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice1      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL InputFe0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL InputFe1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bling_apco20(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bling_apco21(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
