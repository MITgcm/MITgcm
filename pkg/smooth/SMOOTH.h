C pkg/smooth constants

      INTEGER     smoothprec
      PARAMETER ( smoothprec = 32 )

      LOGICAL smooth3DdoImpldiff
      PARAMETER ( smooth3DdoImpldiff = .TRUE. )

      INTEGER smoothOpNbMax
      PARAMETER ( smoothOpNbMax = 10 )

      _RL smooth2DdelTime, smooth3DdelTime
      PARAMETER ( smooth2DdelTime = 1. _d 0 )
      PARAMETER ( smooth3DdelTime = 1. _d 0 )

C parameters:

      COMMON /smooth_operators_i/
     & smooth3Dnbt, smooth2Dnbt,
     & smooth2Dtype, smooth2Dsize,
     & smooth3DtypeZ, smooth3DsizeZ,
     & smooth3DtypeH, smooth3DsizeH,
     & smooth2Dfilter, smooth3Dfilter
      INTEGER smooth2Dnbt(smoothOpNbMax),
     & smooth2Dtype(smoothOpNbMax), smooth2Dsize(smoothOpNbMax),
     & smooth2Dfilter(smoothOpNbMax)
      INTEGER smooth3Dnbt(smoothOpNbMax),
     & smooth3DtypeZ(smoothOpNbMax), smooth3DsizeZ(smoothOpNbMax),
     & smooth3DtypeH(smoothOpNbMax), smooth3DsizeH(smoothOpNbMax),
     & smooth3Dfilter(smoothOpNbMax)

      COMMON /smooth_param_r/
     & smooth3DtotTime,
     & smooth3D_Lx0, smooth3D_Ly0, smooth3D_Lz0,
     & smooth2DtotTime, smooth2D_Lx0, smooth2D_Ly0
      _RL smooth3DtotTime,
     & smooth3D_Lx0(smoothOpNbMax),
     & smooth3D_Ly0(smoothOpNbMax), smooth3D_Lz0(smoothOpNbMax)
      _RL smooth2DtotTime,
     & smooth2D_Lx0(smoothOpNbMax), smooth2D_Ly0(smoothOpNbMax)

      COMMON /smooth_flds_c/
     & smooth3DmaskName, smooth2DmaskName
      CHARACTER*(5) smooth3DmaskName(smoothOpNbMax)
      CHARACTER*(5) smooth2DmaskName(smoothOpNbMax)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C fields:
      COMMON /smooth_flds_rs/
     & smooth_recip_hFacC, smooth_hFacW, smooth_hFacS
      _RS
     & smooth_recip_hFacC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth_hFacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth_hFacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      COMMON /smooth_flds_rl/
     & smooth3D_Lx, smooth3D_Ly, smooth3D_Lz,
     & smooth3Dnorm,
     & smooth2D_Lx, smooth2D_Ly,
     & smooth2Dnorm
      _RL
     & smooth3D_Lx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Ly(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Lz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3Dnorm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL
     & smooth2D_Lx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy),
     & smooth2D_Ly(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy),
     & smooth2Dnorm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /smooth_operators_r/
     & smooth3D_kappaR, smooth3D_Kwx, smooth3D_Kwy, smooth3D_Kwz,
     & smooth3D_Kux, smooth3D_Kvy, smooth3D_Kuz, smooth3D_Kvz,
     & smooth3D_Kuy, smooth3D_Kvx,
     & smooth2D_Kux, smooth2D_Kvy
      _RL
     & smooth3D_kappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kuz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kvz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kuy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy),
     & smooth3D_Kvx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL
     & smooth2D_Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy),
     & smooth2D_Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
