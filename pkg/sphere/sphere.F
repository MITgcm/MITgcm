
#include "CPP_OPTIONS.h"

c     ==================================================================
c
c     shpere.F: Routines that handle the projection onto sherical
c               harmonics.
c
c     Routines:
c
c     o adfsc4dat  - Adjoint routine of fsc4dat.
c     o shc2grid   - Evaluate a spherical harmonics model on a
c                    regular grid.
c     o shc4grid   - Evaluate a spherical harmonics model on a
c                    regular grid.
c
c     o shcrotate  - s/r used for the sph analysis ...
c     o shc2zone   - s/r used for the sph analysis ...
c     o shc4zone   - s/r used for the sph analysis ...
c     o fsc2dat    - s/r used for the sph analysis ...
c     o frsbase    - s/r used for the sph analysis ...
c     o helmholtz  - s/r used for the sph analysis ...
c     o recur_z    - s/r used for the sph analysis ...
c
c     o shcError   - Print error message and stop program (see below).
c
c     IMSL Routines:
c
c     o fftrb      - Compute the real periodic sequence from its Fourier
c                    coefficients.
c                    --> replace by NAGLIB routine. C06...
c
c     Platform-specific:
c
c     M abort      - FORTRAN intrinsic on SUN and, presumably, on CRAY to
c                    exit the program if an error condition is met.
c
c     -->  Replaced ABORT by shcError ( Christian Eckert  MIT  22-Mar-2000 )
c
c     Note:
c     =====
c
c     Where code is written in lower case letters, changes were
c     introduced. The changes are mainly related to F90 syntax.
c     Additionally, I replaced the call to the intrinsic *AMOD*
c     by its generic name *MOD*. ( Ch.E. 25-May-1999 )
c
c
c     Documentation:
c
c
c     ==================================================================

      SUBROUTINE FSC4DAT(N,FSC)

      IMPLICIT NONE

      INTEGER N
      REAL FSC(N)
      REAL SCALE
      integer i

#ifdef USE_SPH_PROJECTION
      CALL FFTRF(N,FSC,FSC)
#else
      do i=1,n
        fsc(i) = 0.0
      enddo
#endif

      SCALE        = 2.0/FLOAT(N)
      FSC(1)       = FSC(1)/FLOAT(N)

CE      FSC(2:N-1:2) = FSC(2:N-1:2)*SCALE
CE      FSC(3:N  :2) =-FSC(3:N:  2)*SCALE ! change sign of SINE coeffs.

      do i = 2,n-1,2
        fsc( i ) =  fsc( i )*scale
      enddo
      do i = 3,n,2
        fsc( i ) = -fsc( i )*scale ! change sign of sine coeffs.
      enddo

      IF (MOD(N,2).EQ.0) THEN
         FSC(N) = FSC(N)/FLOAT(N)
      ENDIF

      RETURN
      END

c     ==================================================================

      subroutine adfsc4dat(n,adfsc)

      implicit none

      integer n
      real adfsc(n)

      integer i

#ifdef USE_SPH_PROJECTION
      call fftrb(n,adfsc,adfsc)
#else
      do i=1,n
        adfsc(i) = 0.0
      enddo
#endif

      do i=1,n
        adfsc(i) = adfsc(i)/float(n)
      enddo

      end

c     ==================================================================

      SUBROUTINE SHC2GRID( LMAX, SHC, NLON, NLAT, GRID, P )
C
C --- Evaluate a spherical harmonics model on a regular grid.
C
C      SHC  ! spherical harmonic coefficients in the order of
C           !
C           !         C00         :               SHC(1)
C           !     S11 C10 C11     :        SHC(2) SHC(3) SHC(4)
C           ! S22 S21 C20 C21 C22 : SHC(5) SHC(6) SHC(7) SHC(8) SHC(9)
C           !
C           ! i.e.,  C(L,M) = SHC(L*L+L+1+M)
C           !        S(L,M) = SHC(L*L+L+1-M)
C     NLAT  ! number of latitude (zonal) lines.
C     NLON  ! number of longitude (meridional) lines.
C           !
C     GRID  ! grid values in the order of
C           ! ((west to east),south to north)
C           ! ((LON=1,NLON),LAT=1,NLAT)
C           ! grid values are defined on the "centers"
C           ! of geographically regular equi-angular blocks.
C
C --- Coded by Dr. Myung-Chan Kim, Center for Space Research, 1997
C              University of Texas at Austin, Austin, Texas, 78712
C
      IMPLICIT NONE

      INTEGER LMAX                       ! INPUT  max. degree
      REAL    SHC((1+LMAX)*(1+LMAX))     ! INPUT  spherical harmonics
      INTEGER NLAT                       ! INPUT  number of latitude  lines.
      INTEGER NLON                       ! INPUT  number of longitude lines.
      REAL    GRID(NLON,NLAT)            ! OUTPUT grid values
      REAL    P((LMAX+1)*(LMAX+2)/2)     ! work space

      INTEGER NLONLMT
      PARAMETER (NLONLMT=10000)
      REAL HS (NLONLMT)
      REAL HN (NLONLMT)

      REAL    DLAT, ANGLE, XLAT1, XLAT2
      INTEGER LATS, LATN

ce    integer js, jn

      integer i

ce      IF(NLON.GT.NLONLMT) CALL ABORT('NLON.GT.NLONLMT')
ce      IF(NLON.LT.LMAX*2 ) CALL ABORT('NLON.LT.LMAX*2 ')
      IF(NLON.GT.NLONLMT) CALL shcError('NLON.GT.NLONLMT',1)
      IF(NLON.LT.LMAX*2 ) CALL shcError('NLON.LT.LMAX*2 ',1)

      DLAT  = 180.0/FLOAT(NLAT)
      ANGLE = 180.0/FLOAT(NLON)

      CALL SHCROTATE(LMAX,SHC,ANGLE)
      DO LATS = 1,(NLAT+1)/2
         LATN = NLAT-(LATS-1)
         XLAT2 =-90.0 + FLOAT(LATS)*DLAT
         XLAT1 = XLAT2-DLAT
         do i=1,nlon
           hs(i) = 0.0
           hn(i) = 0.0
         enddo
         CALL SHC2ZONE( LMAX, SHC, XLAT1, XLAT2, HS, HN, P )
         CALL FSC2DAT( NLON, HS )
         CALL FSC2DAT( NLON, HN )
         do i=1,nlon
           grid(i,lats) = hs(i)
           grid(i,latn) = hn(i)
         enddo
      ENDDO
      CALL SHCROTATE(LMAX,SHC,-ANGLE)
      RETURN
      END

c     ==================================================================

      SUBROUTINE SHC4GRID( LMAX, SHC, NLON, NLAT, GRID, P )

      IMPLICIT NONE

      INTEGER LMAX                       ! INPUT  max. degree
      REAL    SHC((1+LMAX)*(1+LMAX))     ! OUTPUT spherical harmonics
      INTEGER NLAT                       ! INPUT  number of latitude  lines.
      INTEGER NLON                       ! INPUT  number of longitude lines.
      REAL    GRID(NLON,NLAT)            ! INPUT  grid values
      REAL    P((LMAX+1)*(LMAX+2)/2)     ! work space

      INTEGER NLONLMT
      PARAMETER (NLONLMT=10000)
      REAL HS (NLONLMT)
      REAL HN (NLONLMT)

      REAL    DLAT, ANGLE, XLAT1, XLAT2
      INTEGER LATS, LATN

ce    integer js, jn

      integer i

      IF(NLON.GT.NLONLMT) THEN
         PRINT *, 'NLON = ', NLON
         PRINT *, 'NLONLMT = ', NLONLMT
ce         CALL ABORT('NLON.GT.NLONLMT')
         CALL shcError('NLON.GT.NLONLMT',1)
      END IF

      IF(NLON.LT.LMAX*2 ) THEN
         PRINT *, 'NLON = ', NLON
         PRINT *, 'LMAX = ', LMAX
ce         CALL ABORT('NLON.LT.LMAX*2')
         CALL shcError('NLON.LT.LMAX*2',1)
      END IF

      DLAT  = 180.0/FLOAT(NLAT)
      ANGLE = 180.0/FLOAT(NLON)
      DO LATS = 1,(NLAT+1)/2
         LATN = NLAT-(LATS-1)
         do i = 1,nlon
           hs(i) = grid(i,lats)
           hn(i) = grid(i,latn)
         enddo
         CALL FSC4DAT(NLON,HS)
         CALL FSC4DAT(NLON,HN)
         XLAT2 =-90.0 + FLOAT(LATS)*DLAT
         XLAT1 = XLAT2-DLAT
         CALL SHC4ZONE(LMAX,SHC,XLAT1,XLAT2,HS,HN,P)
      ENDDO
      CALL SHCROTATE(LMAX,SHC,-ANGLE)

      RETURN
      END

c     ==================================================================

      SUBROUTINE SHCROTATE(LMAX,SHC,ANGLE)

      IMPLICIT NONE

      INTEGER  LMAX                   ! max. degree of spherical harmonics.
      REAL     SHC((1+LMAX)*(1+LMAX)) ! spherical harmonic coeffs.
      REAL     ANGLE                  ! in degree.

      INTEGER LMAXLMT
      PARAMETER (LMAXLMT=10000)

      REAL    H(LMAXLMT*2+1)
      INTEGER K, L, M
      REAL    SINA, COSA, C, S

      if (mod(angle,360.0) .ne. 0.0) then

         CALL FRSBASE(ANGLE,H,1,1+LMAX+LMAX)

         DO M = 0,LMAX
            IF(M.EQ.0) THEN
               SINA = 0.0
               COSA = 1.0
            ELSE
               COSA = H(M+M)
               SINA = H(M+M+1)
            ENDIF
            DO L = M,LMAX
               K = L*L+L+1
               C = SHC(K+M)
               S = SHC(K-M)
               SHC(K+M) = COSA*C + SINA*S
               SHC(K-M) =-SINA*C + COSA*S
            ENDDO
         ENDDO

      ENDIF

      RETURN
      END

c     ==================================================================

      SUBROUTINE SHC2ZONE(LMAX,SHC,XLAT1,XLAT2,HS,HN,P)

      IMPLICIT NONE

      INTEGER LMAX                   ! INPUT  max. degree of spher. harmonics.
      REAL    SHC((1+LMAX)*(1+LMAX)) ! INPUT  spherical harmonic coeffs.
      REAL    XLAT1                  ! INPUT  latitude.
      REAL    XLAT2                  ! INPUT  latitude.
      REAL    HS(1+LMAX+LMAX)        ! OUTPUT
      REAL    HN(1+LMAX+LMAX)        ! OUTPUT
      REAL    P((LMAX+1)*(LMAX+2)/2) ! INPUT

      INTEGER LMAXLMT
      PARAMETER (LMAXLMT=5000)

      REAL    FACT(0:LMAXLMT) !
      INTEGER LMAX1, J, K, L, M
      REAL    DEG2RAD, SLAT
      REAL    A, B, E, F, Q, DA, DB, XLAT

ce      IF (LMAX.GT.LMAXLMT) CALL ABORT('LMAX.GT.LMAXLMT')
cph      IF (LMAX.GT.LMAXLMT) CALL shcError('LMAX.GT.LMAXLMT',1)

      DEG2RAD = ACOS(-1.0)/180.0
      XLAT = 0.5*(XLAT1+XLAT2)
      do k = 1,lmax
        fact(k) = 1.0
      enddo
      SLAT = SIN(XLAT*DEG2RAD)

      CALL HELMHOLTZ(LMAX,SLAT,P)
      LMAX1 = LMAX+1
      K    = 0
      DO M = 0,LMAX
         A    = 0.0
         B    = 0.0
         E    = 0.0
         F    = 0.0
         DO L = M,LMAX-1,2
            K    = K+1
            J    = L*L+L+1
            Q    = FACT(L)*P(K)
            DA   = Q*SHC(J+M)
            DB   = Q*SHC(J-M)
            A    = A+DA
            B    = B+DB
            E    = E+DA
            F    = F+DB
            K    = K+1
            J    = J+L+L+2
            Q    = FACT(L+1)*P(K)
            DA   = Q*SHC(J+M)
            DB   = Q*SHC(J-M)
            A    = A+DA
            B    = B+DB
            E    = E-DA
            F    = F-DB
         ENDDO
         IF(MOD(LMAX-L,2).EQ.0) THEN
            K    = K+1
            J    = LMAX*LMAX+LMAX+1
            Q    = FACT(LMAX)*P(K)
            DA   = Q*SHC(J+M)
            DB   = Q*SHC(J-M)
            A    = A+DA
            B    = B+DB
            E    = E+DA
            F    = F+DB
         ENDIF
         IF(M.EQ.0) THEN
            HS(1) = A
            HN(1) = E
         ELSE
            HS(M+M  ) = A
            HS(M+M+1) = B
            HN(M+M  ) = E
            HN(M+M+1) = F
         ENDIF
      ENDDO

      RETURN
      END

c     ==================================================================

      SUBROUTINE SHC4ZONE(LMAX,SHC,XLAT1,XLAT2,HS,HN,P)

      IMPLICIT NONE

      INTEGER LMAX
      REAL    SHC((1+LMAX)*(1+LMAX))    ! spherical harmonic coeffs.
      REAL    XLAT1                     ! latitude.
      REAL    XLAT2                     ! latitude.
      REAL    P((LMAX+1)*(LMAX+2)/2)
C
C   - output -
C
      REAL HS(1+LMAX+LMAX)
      REAL HN(1+LMAX+LMAX)
C
C   - work space -
C
      INTEGER LMAXLMT
      PARAMETER (LMAXLMT=5000)

      REAL    XLAT, SIN0, SIN1, SIN2, SCALE
      REAL    DEG2RAD, CE, CO, SE, SO
      INTEGER I, J, K, L, M
      INTEGER JP, I1, I2

ce      IF(LMAX.GT.LMAXLMT) CALL ABORT('LMAX.GT.LMAXLMT')
cph      IF(LMAX.GT.LMAXLMT) CALL shcError('LMAX.GT.LMAXLMT',1)

      IF(XLAT1 .LT. -90.0+1.E-10) THEN
        do i = 1,(1+lmax)*(1+lmax)
          shc(i) = 0.0
        enddo
      ENDIF

      XLAT  = 0.5*(XLAT1+XLAT2)
      DEG2RAD  = ACOS(-1.0)/180.0
         SIN0  = SIN(XLAT *DEG2RAD)
         SIN1  = SIN(AMAX1(-90.0,XLAT1)*DEG2RAD)
         SIN2  = SIN(AMIN1(  0.0,XLAT2)*DEG2RAD)
         SCALE = (SIN2 - SIN1)*0.25
      do i = 1,lmax+lmax+1
        hs(i) = hs(i)*scale
        hn(i) = hn(i)*scale
      enddo

      CALL HELMHOLTZ(LMAX,SIN0,P)
      I    = 0
cadj loop = parallel
      DO M = 0,LMAX
         IF (M .EQ. 0) THEN
            J  = 1
            JP = 1
         ELSE
            J  = 2*M
            JP = J+1
         ENDIF
         CE = HS(J)+HN(J)
         CO = HS(J)-HN(J)
         SE = HS(J)+HN(JP)
         SO = HS(J)-HN(JP)
         I1 = I+1
         I2 = I+2
cadj loop = parallel
         DO L = M,LMAX-1,2
            K = L*L+L+1
            SHC(K+M) = SHC(K+M) + P(I1) * CE
            SHC(K-M) = SHC(K-M) + P(I1) * SE
            K = K+L+L+2
            SHC(K+M) = SHC(K+M) + P(I2) * CO                    
            SHC(K-M) = SHC(K-M) + P(I2) * SO
         ENDDO
         I = I + 2
         IF(MOD(LMAX-M,2).NE.1) THEN
            I = I+1
            K = LMAX*LMAX+LMAX+1
            SHC(K+M) = SHC(K+M) + P(I) * CE
            SHC(K-M) = SHC(K-M) + P(I) * SE
         ENDIF
      ENDDO
      RETURN
      END

c     ==================================================================

      subroutine fsc2dat(n,fsc)
c
c --- this routine are coded to clarify the imsl's fftrf/fftrb.
c
      implicit none

      integer n
      real fsc(n)

      integer i

      do i = 2,n-1,2
        fsc(i  ) =  fsc(i  )*0.5
        fsc(i+1) = -fsc(i+1)*0.5   ! change sign of sine coeffs.
      enddo

#ifdef USE_SPH_PROJECTION
      call fftrb(n,fsc,fsc)
#else
      do i = 1,n
        fsc( i ) = 0.0
      enddo
#endif

      return
      end

c     ==================================================================

      SUBROUTINE FRSBASE(A,H,I,J)
C
C --- Given A (in degree), return H(I:J) = 1,COS(A),SIN(A).....
C
      IMPLICIT NONE

      REAL A
      REAL H(1)
      INTEGER I, J

      REAL DEG2RAD, T, ARG
      INTEGER N, L

      DEG2RAD = ACOS(-1.0)/180.0

      N      = J-I+1
      IF(N.LE.0) RETURN
      H(I)   = 1.0
      IF(N.EQ.1) RETURN
      ARG    = A * DEG2RAD
      H(I+1) = COS(ARG)
      IF(N.EQ.2) RETURN
      H(I+2) = SIN(ARG)
      IF(N.EQ.3) RETURN
      T      = H(I+1)+H(I+1)
      H(I+3) = T*H(I+1) - 1.0
      IF(N.EQ.4) RETURN
      H(I+4) = T*H(I+2)
      IF(N.EQ.5) RETURN
      DO L = I+5,J
      H(L)   = T*H(L-2)-H(L-4)
      END DO

      RETURN
      END 

c     ==================================================================

      SUBROUTINE HELMHOLTZ(LMAX,S,P)
C
C --- compute the fully normalized associated legendre polynomials.
C
      IMPLICIT NONE

      INTEGER LMAX        ! INPUT  max. degree.
      REAL    S           ! INPUT  sin(latitude).
      REAL    P(1)        ! OUTPUT assoc. legendre polynomials.

      INTEGER LMAXLMT
      PARAMETER(LMAXLMT=10800)

      REAL A(0:LMAXLMT)
      REAL ISECT(0:LMAXLMT)
      REAL X(LMAXLMT)
      REAL Y(LMAXLMT+LMAXLMT)
      REAL Z(LMAXLMT)

      INTEGER LMAXOLD
      DATA    LMAXOLD/-1/
      SAVE    LMAXOLD, ISECT, X, Y, Z

      REAL    C, CM, AK
      INTEGER K, L, N, M

      IF(LMAX.NE.LMAXOLD) THEN
         ISECT(0) = 1
         DO L = 1,LMAX
            X(L) = SNGL(1.0D0/DSQRT(DBLE(FLOAT(4*L*L-1))))
            Y(L) = SNGL(DSQRT(DBLE(FLOAT(L))))
            Y(L+LMAX) = SNGL(DSQRT(DBLE(FLOAT(L+LMAX))))
            ISECT(L) = L*LMAX-L*(L-3)/2+1
         ENDDO
         CALL RECUR_Z(Z,LMAX)
         LMAXOLD = LMAX
      ENDIF

      C  = SNGL(DSQRT(1.0D0-DBLE(S)*DBLE(S)))
      CM = 1.0
         P(1) = 1.0
         DO M = 1,LMAX
            K = ISECT(M)
            CM   =    C * CM
            P(K) = Z(M) * CM
         ENDDO
         N    = 1
         DO M = 0,LMAX-N
            K = ISECT(M)+N
            L = N+M
            AK   = X(L)*Y(L+M)*Y(N)
            P(K) = S*P(K-1)/AK
            A(M) = AK
         ENDDO
         DO N = 2,LMAX
            DO M = 0,LMAX-N
               K = ISECT(M)+N
               L = N+M
               AK   = X(L)*Y(L+M)*Y(N)
               P(K) =(S*P(K-1)-P(K-2)*A(M))/AK
               A(M) = AK
            ENDDO
         ENDDO
      RETURN
      END

c     ==================================================================

      SUBROUTINE RECUR_Z(Z,LMAX)
C
C --- Coefficients for fully normalized sectorial Legendre polynomials.
C
      IMPLICIT NONE

      INTEGER LMAX
      REAL    Z(LMAX)

      DOUBLE PRECISION ZZ
      INTEGER L

      ZZ   = 2.0D0
      DO L = 1,LMAX
         ZZ   = ZZ * DBLE(FLOAT(L+L+1))/DBLE(FLOAT(L+L))
         Z(L) = SNGL(DSQRT(ZZ))
      ENDDO

      RETURN
      END

c     ==================================================================

      subroutine shcError( errstring, ierr )
c
c --- Print an error message and exit. Written in order to replace
c     the machine specific routine ABORT.
c     ( Christian Eckert  MIT  22-Mar-2000 )
c
      implicit none

      integer       ierr
      character*(*) errstring

      if ( ierr .ne. 0 ) then
        print*
        print*,' sphere: ',errstring
        print*
        stop   ' ... program stopped.'
      endif

      return
      end

c     ==================================================================
