CBOP
C    !ROUTINE: ECCO_SIZE.h
C    !INTERFACE:
C #include ECCO_SIZE.h

C    !DESCRIPTION: \bv
C     ==================================================================
C     ECCO_SIZE.h
C     ==================================================================
C     Contains ECCO cost-term array dimension
C     \ev
CEOP

C     Number of User Cost terms:
C     =============================
      INTEGER NUSERCOST
      PARAMETER ( NUSERCOST=10 )

C     Number of Generic Cost terms:
C     =============================
      INTEGER NGENCOST
      PARAMETER ( NGENCOST=30 )

      INTEGER NGENCOST3D
#ifdef ALLOW_GENCOST3D
      PARAMETER ( NGENCOST3D=6 )
#else
      PARAMETER ( NGENCOST3D=0 )
#endif

      INTEGER NGENPPROC
      PARAMETER ( NGENPPROC=10 )

      INTEGER N1DDATA
      PARAMETER ( N1DDATA=300 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
