c     ==================================================================
c     ECCO_SIZE.h
c     ==================================================================

C     ecco cost term dimension

c     Number of User Cost terms:
c     =============================
      INTEGER NUSERCOST
      PARAMETER ( NUSERCOST=10 )

c     Number of Generic Cost terms:
c     =============================
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
