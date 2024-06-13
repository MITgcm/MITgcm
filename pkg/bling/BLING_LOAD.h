C--   COMMON /BLING_LOAD/
C     BLING_ldRec     :: time-record currently loaded (in temp arrays *[1])

      COMMON /BLING_LOAD_I/ BLING_ldRec
      COMMON /BLING_LOAD_RS/
     &    dicwind0, dicwind1,
     &    atmosp0, atmosp1,
     &    silica0, silica1,
     &    ice0, ice1,
     &    feinput0, feinput1

      INTEGER BLING_ldRec(nSx,nSy)
      _RS dicwind0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dicwind1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS silica0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS silica1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice0      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice1      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS feinput0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS feinput1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
