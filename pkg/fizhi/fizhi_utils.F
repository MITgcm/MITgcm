C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_utils.F,v 1.16 2009/03/30 02:14:48 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      function minval (q,im)
      implicit none
      integer  im, i
      _RL q(im), minval
      minval = 1.e15
      do i=1,im
      if( q(i).lt.minval ) minval = q(i)
      enddo
      return
      end
      FUNCTION ERRF (ARG)
C***********************************************************************
C  FUNCTION ERRF
C  PURPOSE
C     COMPUTES ERROR FUNCTION OF ARGUMENT
C  USAGE
C     CALLED BY TRBFLX
C  DESCRIPTION OF PARAMETERS
C     ARG   -  INPUTED ARGUMENT
C  REMARKS:
C        USED TO COMPUTE FRACTIONAL CLOUD COVER AND LIQUID WATER CONTENT
C          FROM TURBULENCE STATISTICS
C **********************************************************************
      implicit none
      _RL arg,errf

      _RL aa1,aa2,aa3,aa4,aa5,pp,x2,x3,x4,x5
      PARAMETER ( AA1  =   0.254829592 )
      PARAMETER ( AA2  =  -0.284496736 )
      PARAMETER ( AA3  =   1.421413741 )
      PARAMETER ( AA4  =  -1.453152027 )
      PARAMETER ( AA5  =   1.061405429 )
      PARAMETER ( PP   =   0.3275911   )
      PARAMETER ( X2   =   AA2 / AA1   )
      PARAMETER ( X3   =   AA3 / AA2   )
      PARAMETER ( X4   =   AA5 / AA3   )
      PARAMETER ( X5   =   AA5 / AA4   )

      _RL aarg,tt

      ERRF = 1.
      AARG=ABS(ARG)

      IF ( AARG .LT. 4.0 )  THEN
       TT = 1./(1.+PP*AARG)
       ERRF = 1. -
     1  (AA1*TT*(1.+X2*TT*(1.+X3*TT*(1.+X4*TT*(1.+X5*TT)))))
     2   * EXP(-AARG*AARG)
      ENDIF

      IF ( ARG .LT. 0.0 )  ERRF = -ERRF

      RETURN
      END

      SUBROUTINE STRIP(A,B,IA,IB,L,K)
      implicit none
      integer ia,ib,L,K
      _RL A(IA,L), B(IB,L)

      INTEGER OFFSET,Lena,i,j

      OFFSET = IB*(K-1)
      Lena    = MIN(IB,IA-OFFSET)
      OFFSET = OFFSET+1

      IF(Lena.EQ.IB) THEN
        DO 100 J=1,L
        DO 100 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
100     CONTINUE
      ELSE
        DO 200 J=1,L
        DO 300 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
300     CONTINUE
        DO 400 I=1,IB-Lena
        B(Lena+I,J) = A(Lena+OFFSET-1,J)
400     CONTINUE
200     CONTINUE
      ENDIF

      RETURN
      END
      SUBROUTINE STRIPINT(A,B,IA,IB,L,K)
      implicit none
      integer ia,ib,L,K
      INTEGER A(IA,L), B(IB,L)

      INTEGER OFFSET,Lena,i,j

      OFFSET = IB*(K-1)
      Lena    = MIN(IB,IA-OFFSET)
      OFFSET = OFFSET+1

      IF(Lena.EQ.IB) THEN
        DO 100 J=1,L
        DO 100 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
100     CONTINUE
      ELSE
        DO 200 J=1,L
        DO 300 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
300     CONTINUE
        DO 400 I=1,IB-Lena
        B(Lena+I,J) = A(Lena+OFFSET-1,J)
400     CONTINUE
200     CONTINUE
      ENDIF

      RETURN
      END
      SUBROUTINE PASTE(B,A,IB,IA,L,K)
      implicit none
      integer ia,ib,L,K
      _RL A(IA,L), B(IB,L)

      INTEGER OFFSET,Lena,i,j

      OFFSET = IB*(K-1)
      Lena    = MIN(IB,IA-OFFSET)
      OFFSET = OFFSET+1

      DO 100 J=1,L
      DO 100 I=1,Lena
      A(I+OFFSET-1,J) = B(I,J)
100   CONTINUE

      RETURN
      END
      SUBROUTINE PSTBMP(B,A,IB,IA,L,K)
      implicit none
      integer ia,ib,L,K
      _RL A(IA,L), B(IB,L)

      INTEGER OFFSET,Lena,i,j

      OFFSET = IB*(K-1)
      Lena    = MIN(IB,IA-OFFSET)
      OFFSET = OFFSET+1

      DO 100 J=1,L
      DO 100 I=1,Lena
      A(I+OFFSET-1,J) = A(I+OFFSET-1,J) + B(I,J)
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE STRINT(A,B,IA,IB,L,K)
      implicit none
      integer ia,ib,L,K
      INTEGER A(IA,L), B(IB,L)

      INTEGER OFFSET,Lena,i,j

      OFFSET = IB*(K-1)
      Lena = MIN(IB,IA-OFFSET)
      OFFSET = OFFSET+1

      IF(Lena.EQ.IB) THEN
        DO 100 J=1,L
        DO 100 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
100     CONTINUE
      ELSE
        DO 200 J=1,L
        DO 300 I=1,Lena
        B(I,J) = A(I+OFFSET-1,J)
300     CONTINUE
        DO 400 I=1,IB-Lena
        B(Lena+I,J) = A(Lena+OFFSET-1,J)
400     CONTINUE
200     CONTINUE
      ENDIF

      RETURN
      END
      SUBROUTINE QSAT (TT,P,Q,DQDT,LDQDT)
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute Saturation Specific Humidity
C
C  INPUT:
C  ======
C    TT ......... Temperature (Kelvin)
C    P .......... Pressure (mb)
C    LDQDT ...... Logical Flag to compute QSAT Derivative
C
C  OUTPUT:
C  =======
C    Q .......... Saturation Specific Humidity
C    DQDT ....... Saturation Specific Humidity Derivative wrt Temperature
C
C
C***********************************************************************

      IMPLICIT NONE
      _RL TT, P, Q, DQDT
      LOGICAL LDQDT

      _RL AIRMW, H2OMW

      PARAMETER ( AIRMW  = 28.97      )
      PARAMETER ( H2OMW  = 18.01      )

      _RL ESFAC, ERFAC
      PARAMETER ( ESFAC = H2OMW/AIRMW       )
      PARAMETER ( ERFAC = (1.0-ESFAC)/ESFAC )

      _RL aw0, aw1, aw2, aw3, aw4, aw5, aw6
      _RL bw0, bw1, bw2, bw3, bw4, bw5, bw6
      _RL ai0, ai1, ai2, ai3, ai4, ai5, ai6
      _RL bi0, bi1, bi2, bi3, bi4, bi5, bi6

      _RL d0, d1, d2, d3, d4, d5, d6
      _RL e0, e1, e2, e3, e4, e5, e6
      _RL f0, f1, f2, f3, f4, f5, f6
      _RL g0, g1, g2, g3, g4, g5, g6

c ********************************************************
c ***  Polynomial Coefficients WRT Water (Lowe, 1977) ****
c ***              (Valid +50 C to -50 C)             ****
c ********************************************************

      parameter ( aw0 =  6.107799961e+00 * esfac )
      parameter ( aw1 =  4.436518521e-01 * esfac )
      parameter ( aw2 =  1.428945805e-02 * esfac )
      parameter ( aw3 =  2.650648471e-04 * esfac )
      parameter ( aw4 =  3.031240396e-06 * esfac )
      parameter ( aw5 =  2.034080948e-08 * esfac )
      parameter ( aw6 =  6.136820929e-11 * esfac )

      parameter ( bw0 = +4.438099984e-01 * esfac )
      parameter ( bw1 = +2.857002636e-02 * esfac )
      parameter ( bw2 = +7.938054040e-04 * esfac )
      parameter ( bw3 = +1.215215065e-05 * esfac )
      parameter ( bw4 = +1.036561403e-07 * esfac )
      parameter ( bw5 = +3.532421810e-10 * esfac )
      parameter ( bw6 = -7.090244804e-13 * esfac )


c ********************************************************
c ***   Polynomial Coefficients WRT Ice  (Lowe, 1977) ****
c ***              (Valid  +0 C to -50 C)             ****
c ********************************************************

      parameter ( ai0 = +6.109177956e+00 * esfac )
      parameter ( ai1 = +5.034698970e-01 * esfac )
      parameter ( ai2 = +1.886013408e-02 * esfac )
      parameter ( ai3 = +4.176223716e-04 * esfac )
      parameter ( ai4 = +5.824720280e-06 * esfac )
      parameter ( ai5 = +4.838803174e-08 * esfac )
      parameter ( ai6 = +1.838826904e-10 * esfac )

      parameter ( bi0 = +5.030305237e-01 * esfac )
      parameter ( bi1 = +3.773255020e-02 * esfac )
      parameter ( bi2 = +1.267995369e-03 * esfac )
      parameter ( bi3 = +2.477563108e-05 * esfac )
      parameter ( bi4 = +3.005693132e-07 * esfac )
      parameter ( bi5 = +2.158542548e-09 * esfac )
      parameter ( bi6 = +7.131097725e-12 * esfac )


c ********************************************************
c ***         Polynomial Coefficients WRT Ice         ****
c ***   Starr and Cox (1985) (Valid -40 C to -70 C)   ****
c ********************************************************


      parameter ( d0 = 0.535098336e+01 * esfac )
      parameter ( d1 = 0.401390832e+00 * esfac )
      parameter ( d2 = 0.129690326e-01 * esfac )
      parameter ( d3 = 0.230325039e-03 * esfac )
      parameter ( d4 = 0.236279781e-05 * esfac )
      parameter ( d5 = 0.132243858e-07 * esfac )
      parameter ( d6 = 0.314296723e-10 * esfac )

      parameter ( e0 = 0.469290530e+00 * esfac )
      parameter ( e1 = 0.333092511e-01 * esfac )
      parameter ( e2 = 0.102164528e-02 * esfac )
      parameter ( e3 = 0.172979242e-04 * esfac )
      parameter ( e4 = 0.170017544e-06 * esfac )
      parameter ( e5 = 0.916466531e-09 * esfac )
      parameter ( e6 = 0.210844486e-11 * esfac )


c ********************************************************
c ***         Polynomial Coefficients WRT Ice         ****
c ***   Starr and Cox (1985) (Valid -65 C to -95 C)   ****
c ********************************************************

      parameter ( f0 = 0.298152339e+01 * esfac )
      parameter ( f1 = 0.191372282e+00 * esfac )
      parameter ( f2 = 0.517609116e-02 * esfac )
      parameter ( f3 = 0.754129933e-04 * esfac )
      parameter ( f4 = 0.623439266e-06 * esfac )
      parameter ( f5 = 0.276961083e-08 * esfac )
      parameter ( f6 = 0.516000335e-11 * esfac )

      parameter ( g0 = 0.312654072e+00 * esfac )
      parameter ( g1 = 0.195789002e-01 * esfac )
      parameter ( g2 = 0.517837908e-03 * esfac )
      parameter ( g3 = 0.739410547e-05 * esfac )
      parameter ( g4 = 0.600331350e-07 * esfac )
      parameter ( g5 = 0.262430726e-09 * esfac )
      parameter ( g6 = 0.481960676e-12 * esfac )

      _RL TMAX, TICE
      PARAMETER ( TMAX=323.15, TICE=273.16)

      _RL T, D, W, QX, DQX
      T = MIN(TT,TMAX) - TICE
      DQX = 0.
      QX  = 0.

c Fitting for temperatures above 0 degrees centigrade
c ---------------------------------------------------
      if(t.gt.0.) then
       qx = aw0+T*(aw1+T*(aw2+T*(aw3+T*(aw4+T*(aw5+T*aw6)))))
      if (ldqdt)  then
      dqx = bw0+T*(bw1+T*(bw2+T*(bw3+T*(bw4+T*(bw5+T*bw6)))))
      endif
      endif

c Fitting for temperatures between 0 and -40
c ------------------------------------------
      if( t.le.0. .and. t.gt.-40.0 ) then
        w = (40.0 + t)/40.0
       qx =     w *(aw0+T*(aw1+T*(aw2+T*(aw3+T*(aw4+T*(aw5+T*aw6))))))
     .    + (1.-w)*(ai0+T*(ai1+T*(ai2+T*(ai3+T*(ai4+T*(ai5+T*ai6))))))
      if (ldqdt)  then
      dqx =     w *(bw0+T*(bw1+T*(bw2+T*(bw3+T*(bw4+T*(bw5+T*bw6))))))
     .    + (1.-w)*(bi0+T*(bi1+T*(bi2+T*(bi3+T*(bi4+T*(bi5+T*bi6))))))
      endif
      endif

c Fitting for temperatures between -40 and -70
c --------------------------------------------
      if( t.le.-40.0 .and. t.ge.-70.0 ) then
       qx = d0+T*(d1+T*(d2+T*(d3+T*(d4+T*(d5+T*d6)))))
      if (ldqdt) then
      dqx = e0+T*(e1+T*(e2+T*(e3+T*(e4+T*(e5+T*e6)))))
      endif
      endif

c Fitting for temperatures less than -70
c --------------------------------------
      if(t.lt.-70.0) then
       qx = f0+t*(f1+t*(f2+t*(f3+t*(f4+t*(f5+t*f6)))))
      if (ldqdt) then
      dqx = g0+t*(g1+t*(g2+t*(g3+t*(g4+t*(g5+t*g6)))))
      endif
      endif

c Compute Saturation Specific Humidity
c ------------------------------------
      D = (P-ERFAC*QX)
      IF(D.LT.0.) THEN
       Q = 1.0
       IF (LDQDT)  DQDT = 0.
      ELSE
       D = 1.0 / D
       Q = MIN(QX * D,1.0 _d 0)
       IF (LDQDT)  DQDT = (1.0 + ERFAC*Q) * D * DQX
      ENDIF
      RETURN
      END
      subroutine vqsat (tt,p,q,dqdt,ldqdt,n)
      implicit none
      integer  i,n
      logical  ldqdt
      _RL tt(n), p(n), q(n), dqdt(n)
#ifdef CRAY
#ifdef f77
cfpp$ expand (qsat)
#endif
#endif
      do i=1,n
      call qsat ( tt(i),p(i),q(i),dqdt(i),ldqdt )
      enddo
      return
      end

      subroutine stripit(a,b,irun,ia,ib,l,k)
      implicit none
      integer ia,ib,irun,l,k
      _RL a(ia,l), b(ib,l)
      integer   i,j,Lena,offset

      offset = ib*(k-1)
      Lena    = min(ib,irun-offset)
      offset = offset+1

      if(Lena.eq.ib) then
        do 100 j=1,l
        do 100 i=1,Lena
        b(i,j) = a(i+offset-1,j)
100     continue
      else
        do 200 j=1,l
        do 300 i=1,Lena
        b(i,j) = a(i+offset-1,j)
300     continue
        do 400 i=1,ib-Lena
        b(Lena+i,j) = a(Lena+offset-1,j)
400     continue
200     continue
      endif
      return
      end

      subroutine stripitint(a,b,irun,ia,ib,l,k)
      implicit none
      integer ia,ib,irun,l,k,a(ia,l),b(ib,l)
      integer i,j,Lena,offset

      offset = ib*(k-1)
      Lena = min(ib,irun-offset)
      offset = offset+1

      if(Lena.eq.ib) then
        do 100 j=1,l
        do 100 i=1,Lena
        b(i,j) = a(i+offset-1,j)
100     continue
      else
        do 200 j=1,l
        do 300 i=1,Lena
        b(i,j) = a(i+offset-1,j)
300     continue
        do 400 i=1,ib-Lena
        b(Lena+i,j) = a(Lena+offset-1,j)
400     continue
200     continue
      endif
      return
      end

      subroutine pastit(b,a,ib,ia,irun,L,k)
      implicit none
      integer ib,ia,L,k,irun,Lena,offset
      integer i,j
      _RL a(ia,l), b(ib,l)

      offset = ib*(k-1)
      Lena    = min(ib,irun-offset)
      offset = offset+1

      do 100 j=1,L
      do 100 i=1,Lena
      a(i+offset-1,j) = b(i,j)
100   continue
      return
      end

      subroutine pstbitint(b,a,ib,ia,irun,l,k)
      implicit none
      integer ib,ia,L,k,irun,Lena,offset
      _RL a(ia,l)
      integer b(ib,l)
      integer i,j

      offset = ib*(k-1)
      Lena    = min(ib,irun-offset)
      offset = offset+1

      do 100 j=1,L
      do 100 i=1,Lena
      a(i+offset-1,j) = a(i+offset-1,j) + float(b(i,j))
100   continue
      return
      end


      subroutine pstbmpit(b,a,ib,ia,irun,l,k)
      implicit none
      integer ib,ia,L,k,irun,Lena,offset
      _RL a(ia,l), b(ib,l)
      integer i,j

      offset = ib*(k-1)
      Lena    = min(ib,irun-offset)
      offset = offset+1

      do 100 j=1,L
      do 100 i=1,Lena
      a(i+offset-1,j) = a(i+offset-1,j) + b(i,j)
100   continue
      return
      end

      subroutine strip2tile(a,indx,b,irun,ia,ib,levs,npeice)
c-----------------------------------------------------------------------
c subroutine strip2tile - extract one processors worth of grid points
c                         from a grid space array to a stripped tile
c                         space array
c
c  input:
c      a      - array to be stripped FROM [ia,levs]
c      indx   - array of horizontal indeces of grid points to convert to
c               tile space
c      irun   - number of points in array a that need to be stripped
c      ia     - inner of dimension of source array
c      ib     - inner dimension of target array AND the number of points
c               in the target array to be filled
c      levs   - number of vertical levels AND outer dimension of arrays
c      npeice - the current strip number to be filled
c output:
c      b      - array to be filled, ie, one processors field [ib,levs]
c-----------------------------------------------------------------------
      implicit none
      integer ia,ib,irun,levs,npeice
      _RL a(ia,levs), b(ib,levs)
      integer indx(irun)
      integer i,k,Lena,offset

      offset = ib*(npeice-1)
      Lena    = min(ib,irun-offset)
      offset = offset+1

      if(Lena.eq.ib) then
        do 100 k=1,levs
        do 100 i=1,Lena
        b(i,k) = a(indx(i+offset-1),k)
100     continue
      else
        do 200 k=1,levs
        do 300 i=1,Lena
        b(i,k) = a(indx(i+offset-1),k)
300     continue
        do 400 i=1,ib-Lena
        b(Lena+i,k) = a(indx(Lena+offset-1),k)
400     continue
200     continue
      endif
      return
      end

      subroutine paste2grd_old(b,indx,chfr,ib,numpts,a,ia,levs,npeice)
c-----------------------------------------------------------------------
c subroutine paste2grd - paste one processors worth of grid points
c                        from a stripped tile array to a grid
c                        space array
c
c  input:
c      b      - array to be pasted back into grid space [ib,levs]
c      indx   - array of horizontal indeces of grid points to convert to
c               tile space[numpts]
c      chfr   - fractional area covered by the tile [ib]
c      ib     - inner dimension of source array AND number of points in
c               array a that need to be pasted
c      numpts - total number of points which were stripped
c      ia     - inner of dimension of target array
c      levs   - number of vertical levels AND outer dimension of arrays
c      npeice - the current strip number to be filled
c output:
c      a      - grid space array to be filled [ia,levs]
c
c IMPORTANT NOTE:
c
c This routine will result in roundoff differences if called from
c within a parallel region.
c-----------------------------------------------------------------------

      implicit none
      integer ia,ib,levs,numpts,npeice
      integer indx(numpts)
      _RL a(ia,levs), b(ib,levs), chfr(ib)

      integer i,L,offset,Lena

      offset = ib*(npeice-1)
      Lena    = min(ib,numpts-offset)
      offset = offset+1

      do L = 1,levs
      do i=1,Lena
       a(indx(i+offset-1),L) = a(indx(i+offset-1),L) + b(i,L)*chfr(i)
      enddo
      enddo
      return
      end
      subroutine paste2grd (b,indx,chfr,ib,numpts,a,ia,levs,npeice,
     .                                                            check)
c-----------------------------------------------------------------------
c subroutine paste2grd - paste one processors worth of grid points
c                        from a stripped tile array to a grid
c                        space array
c
c  input:
c      b      - array to be pasted back into grid space [ib,levs]
c      indx   - array of horizontal indeces of grid points to convert to
c               tile space[numpts]
c      chfr   - fractional area covered by the tile [ib]
c      ib     - inner dimension of source array AND number of points in
c               array a that need to be pasted
c      numpts - total number of points which were stripped
c      ia     - inner of dimension of target array
c      levs   - number of vertical levels AND outer dimension of arrays
c      npeice - the current strip number to be filled
c      check  - logical to check for undefined values
c output:
c      a      - grid space array to be filled [ia,levs]
c
c IMPORTANT NOTE:
c
c This routine will result in roundoff differences if called from
c within a parallel region.
c-----------------------------------------------------------------------

      implicit none
      integer ia,ib,levs,numpts,npeice
      integer indx(numpts)
      _RL a(ia,levs), b(ib,levs), chfr(ib)
      logical check

      integer i,L,offset,Lena
      _RL    undef,getcon

      offset = ib*(npeice-1)
      Lena    = min(ib,numpts-offset)
      offset = offset+1

      if( check ) then
      undef = getcon('UNDEF')
       do L= 1,levs
       do i= 1,Lena
        if( a(indx(i+offset-1),L).eq.undef .or. b(i,L).eq.undef ) then
         a(indx(i+offset-1),L) = undef
        else
         a(indx(i+offset-1),L)=a(indx(i+offset-1),L) + b(i,L)*chfr(i)
        endif
       enddo
       enddo
      else
       do L= 1,levs
       do i= 1,Lena
        a(indx(i+offset-1),L)=a(indx(i+offset-1),L) + b(i,L)*chfr(i)
       enddo
       enddo
      endif

      return
      end
      SUBROUTINE GRD2MSC(A,IM,JM,IGRD,B,MXCHPS,NCHP)

      implicit none
      integer im,jm,mxchps,nchp
      integer igrd(mxchps)
c     _RL A(IM,JM), B(MXCHPS)
      _RL A(IM*JM), B(MXCHPS)

      integer i

      IF(NCHP.GE.0) THEN
        DO I=1,NCHP
c        B(I) = A(IGRD(I),1)
         B(I) = A(IGRD(I))
        ENDDO
      ELSE
        PRINT *, 'ERROR IN GRD2MSC'
      ENDIF

      RETURN
      END

      SUBROUTINE MSC2GRD(IGRD,CHFR,B,MXCHPS,NCHP,FRACG,A,IM,JM)

      implicit none
      _RL zero,one
      parameter ( one = 1.)
      parameter (zero = 0.)
      integer im,jm,mxchps,nchp
      integer igrd(mxchps)
c     _RL A(IM,JM), B(MXCHPS), CHFR(MXCHPS), FRACG(IM,JM)
      _RL A(IM*JM), B(MXCHPS), CHFR(MXCHPS), FRACG(IM*JM)

c     _RL VT1(IM,JM)
      _RL VT1(IM*JM)
      integer i

      IF(NCHP.GE.0) THEN
        DO I=1,IM*JM
c        VT1(I,1) = ZERO
         VT1(I) = ZERO
        ENDDO

        DO I=1,NCHP
c        VT1(IGRD(I),1) = VT1(IGRD(I),1) + B(I)*CHFR(I)
         VT1(IGRD(I)) = VT1(IGRD(I)) + B(I)*CHFR(I)
        ENDDO

        DO I=1,IM*JM
c        A(I,1) = A(I,1)*(ONE-FRACG(I,1)) + VT1(I,1)
         A(I) = A(I)*(ONE-FRACG(I)) + VT1(I)
        ENDDO
      ELSE
        PRINT *, 'ERROR IN MSC2GRD'
      ENDIF

      RETURN
      END

      subroutine chpprm(nymd,nhms,mxchps,nchp,chlt,ityp,alai,
     1       agrn,zoch,z2ch,cdrc,cdsc,sqsc,ufac,rsl1,rsl2,rdcs)

       implicit none

       integer nymd,nhms,nchp,mxchps,ityp(mxchps)
       _RL chlt(mxchps)
       _RL alai(mxchps),agrn(mxchps)
       _RL zoch(mxchps), z2ch(mxchps), cdrc(mxchps), cdsc(mxchps)
       _RL sqsc(mxchps), ufac(mxchps), rsl1(mxchps), rsl2(mxchps)
       _RL rdcs(mxchps)

C*********************************************************************
C********************* SUBROUTINE CHPPRM  ****************************
C**********************  14 JUNE 1991   ******************************
C*********************************************************************

      integer ntyps
      parameter (ntyps=10)

      _RL pblzet
      parameter (pblzet = 50.)
      integer k1,k2,nymd1,nhms1,nymd2,nhms2,i
      _RL getcon,vkrm,rootl,vroot,dum1,dum2,alphaf
      _RL facm,facp
      _RL scat,d

      _RL
     &     vgdd(12, ntyps),    vgz0(12, ntyps),
     &     vgrd(12, ntyps),    vgrt(12, ntyps),

     &     vgrf11(ntyps),         vgrf12(ntyps),
     &     vgtr11(ntyps),         vgtr12(ntyps),
     &     vgroca(ntyps),         vgrotd(ntyps),
     &     vgrdrs(ntyps),         vgz2  (ntyps)


      data vgz0  /
     1   2.6530, 2.6530, 2.6530, 2.6530, 2.6530, 2.6530, 2.6530,
     1   2.6530, 2.6530, 2.6530, 2.6530, 2.6530,
     2   0.5200, 0.5200, 0.6660, 0.9100, 1.0310, 1.0440, 1.0420,
     2   1.0370, 1.0360, 0.9170, 0.6660, 0.5200,
     3   1.1120, 1.1030, 1.0880, 1.0820, 1.0760, 1.0680, 1.0730,
     3   1.0790, 1.0820, 1.0880, 1.1030, 1.1120,
     4   0.0777, 0.0778, 0.0778, 0.0779, 0.0778, 0.0771, 0.0759,
     4   0.0766, 0.0778, 0.0779, 0.0778, 0.0778,
     5   0.2450, 0.2450, 0.2270, 0.2000, 0.2000, 0.2000, 0.2000,
     5   0.267,  0.292,  0.280,  0.258,  0.2450,
     6   0.0752, 0.0752, 0.0752, 0.0752, 0.0752, 0.0757, 0.0777,
     6   0.0778, 0.0774, 0.0752, 0.0752, 0.0752,
     7   0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     7   0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     8   0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     8   0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     9   0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     9   0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     1   0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112, 0.0112,
     1   0.0112, 0.0112, 0.0112, 0.0112, 0.0112
     &  /


      data vgrd    /
     1   285.87, 285.87, 285.87, 285.87, 285.87, 285.87, 285.87,
     1   285.87, 285.87, 285.87, 285.87, 285.87,
     2   211.32, 211.32, 218.78, 243.40, 294.87, 345.90, 355.18,
     2   341.84, 307.22, 244.84, 218.78, 211.32,
     3   565.41, 587.05, 623.46, 638.13, 652.86, 675.04, 660.24,
     3   645.49, 638.13, 623.46, 587.05, 565.41,
     4    24.43,  24.63,  24.80,  24.96,  25.72,  27.74,  30.06,
     4    28.86,  25.90,  25.11,  24.80,  24.63,
     5   103.60, 103.60, 102.35, 100.72, 100.72, 100.72, 100.72,
     5   105.30, 107.94, 106.59, 104.49, 103.60,
     6    22.86,  22.86,  22.86,  22.86,  22.86,  23.01,  24.36,
     6    24.69,  24.04,  22.86,  22.86,  22.86,
     7    23.76,  23.76,  23.76,  23.76,  23.76,  23.76,  23.76,
     7    23.76,  23.76,  23.76,  23.76,  23.76,
     8    23.76,  23.76,  23.76,  23.76,  23.76,  23.76,  23.76,
     8    23.76,  23.76,  23.76,  23.76,  23.76,
     9    23.76,  23.76,  23.76,  23.76,  23.76,  23.76,  23.76,
     9    23.76,  23.76,  23.76,  23.76,  23.76,
     1    23.76,  23.76,  23.76,  23.76,  23.76,  23.76,  23.76,
     1    23.76,  23.76,  23.76,  23.76,  23.76
     &  /

      data vgrt  /
     1   19737.8, 19737.8, 19737.8, 19737.8, 19737.8, 19737.8, 19737.8,
     1   19737.8, 19737.8, 19737.8, 19737.8, 19737.8,
     2    5010.0,  5010.0,  5270.0,  6200.0,  8000.0,  9700.0,  9500.0,
     2    8400.0,  6250.0,  5270.0,  5010.0,  5010.0,
     3    9000.0,  9200.0,  9533.3,  9666.7,  9800.0,  9866.7,  9733.3,
     3    9666.7,  9533.3,  9200.0,  9000.0,  9000.0,
     4    5500.0,  5625.0,  5750.0,  5875.0,  6625.0,  8750.0,  9375.0,
     4    6875.0,  6000.0,  5750.0,  5625.0,  5500.0,
     5    6500.0,  6000.0,  5500.0,  5500.0,  5500.0,  5500.0,  5500.0,
     5    7500.0,  8500.0,  7000.0,  6500.0,  6500.0,
     6   10625.0, 10625.0, 10625.0, 10625.0, 10625.0, 11250.0, 18750.0,
     6   17500.0, 10625.0, 10625.0, 10625.0, 10625.0,
     7       1.0,     1.0,     1.0,     1.0,     1.0,     1.0,     1.0,
     7       1.0,     1.0,     1.0,     1.0,     1.0,
     8       1.0,     1.0,     1.0,     1.0,     1.0,     1.0,     1.0,
     8       1.0,     1.0,     1.0,     1.0,     1.0,
     9       1.0,     1.0,     1.0,     1.0,     1.0,     1.0,     1.0,
     9       1.0,     1.0,     1.0,     1.0,     1.0,
     1       1.0,     1.0,     1.0,     1.0,     1.0,     1.0,     1.0,
     1       1.0,     1.0,     1.0,     1.0,     1.0
     &  /


      data vgdd  /
     1   27.37,   27.37,   27.37,   27.37,   27.37,   27.37,  27.37,
     1   27.37,   27.37,   27.37,   27.37,   27.37,
     2   13.66,   13.66,   14.62,   15.70,   16.33,   16.62,   16.66,
     2   16.60,   16.41,   15.73,   14.62,   13.66,
     3   13.76,   13.80,   13.86,   13.88,   13.90,   13.93,   13.91,
     3   13.89,   13.88,   13.86,   13.80,   13.76,
     4    0.218,   0.227,   0.233,   0.239,   0.260,   0.299,   0.325,
     4    0.313,   0.265,   0.244,   0.233,   0.227,
     5    2.813,   2.813,   2.662,   2.391,   2.391,   2.391,   2.391,
     5    2.975,   3.138,   3.062,   2.907,   2.813,
     6    0.10629, 0.10629, 0.10629, 0.10629, 0.10629, 0.12299, 0.21521,
     6    0.22897, 0.19961, 0.10629, 0.10629, 0.10629,
     7    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     7    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     8    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     8    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     9    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     9    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     1    0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,  0.0001,
     1    0.0001,  0.0001,  0.0001,  0.0001,  0.0001
     &  /


      data vgrf11 /0.10,0.10,0.07,0.105,0.10,0.10,.001,.001,.001,.001/

      data vgrf12 /0.16,0.16,0.16,0.360,0.16,0.16,.001,.001,.001,.001/

      data vgtr11 /0.05,0.05,0.05,0.070,0.05,0.05,.001,.001,.001,.001/

      data vgtr12 /.001,.001,.001, .220,.001,.001,.001,.001,.001,.001/

      data vgroca /
     &  0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6,
     &   .1E-6, .1E-6, .1E-6, .1E-6  /

      data vgrotd /1.00,1.00,0.50,0.50,0.50,0.20,0.10,0.10,0.10,0.10/

      data vgrdrs  /
     &   0.75E13, 0.75E13, 0.75E13, 0.40E13, 0.75E13, 0.75E13,
     &   0.10E13, 0.10E13, 0.10E13, 0.10E13  /

      data vgz2 /35.0, 20.0, 17.0, 0.6, 5.0, 0.6, 0.1, 0.1, 0.1, 0.1/

      vkrm = GETCON('VON KARMAN')

      call time_bound  ( nymd,nhms, nymd1,nhms1, nymd2,nhms2, k1,k2 )
      call interp_time ( nymd,nhms, nymd1,nhms1, nymd2,nhms2, facm,facp)

      do i=1,nchp

      zoch(i) = vgz0(k2,ityp(i))*facp + vgz0(k1,ityp(i))*facm
      rdcs(i) = vgrd(k2,ityp(i))*facp + vgrd(k1,ityp(i))*facm

      rootl   = vgrt(k2,ityp(i))*facp + vgrt(k1,ityp(i))*facm

      vroot  = rootl * vgroca(ityp (i))
      dum1   = log (vroot / (1. - vroot))
      dum2   = 1. / (8. * 3.14159 * rootl)
      alphaf = dum2 * (vroot - 3. -2. * dum1)

      rsl1(i) = vgrdrs (ityp (i)) / (rootl * vgrotd (ityp (i)))
      rsl2(i) = alphaf / vgrotd (ityp (i))

      scat =       agrn(i) *(vgtr11(ityp(i)) + vgrf11(ityp(i)))
     &     + (1. - agrn(i))*(vgtr12(ityp(i)) + vgrf12(ityp(i)))
      sqsc(i) = sqrt (1. - scat)

      d       = vgdd(k2,ityp(i))*facp + vgdd(k1,ityp(i))*facm
      ufac(i) = log( (vgz2(ityp(i)) - d) / zoch(i) )
     *        / log(  pblzet             / zoch(i) )

      z2ch(i) = vgz2(ityp (i))

      cdsc(i) = pblzet/zoch(i)+1.
      cdrc(i) = vkrm/log(cdsc(i))
      cdrc(i) = cdrc(i)*cdrc(i)
      cdsc(i) = sqrt(cdsc(i))
      cdsc(i) = cdrc(i)*cdsc(i)

      enddo

      return
      end

      subroutine pkappa (im,jm,lm,ple,pkle,pkz)
C***********************************************************************
C  Purpose
C     Calculate Phillips P**Kappa
C
C  Arguments
C     PLE .... edge-level pressure
C     PKLE ... edge-level pressure**kappa
C     IM ..... longitude  dimension
C     JM ..... latitude   dimension
C     LM ..... vertical   dimension
C     PKZ .... mid-level pressure**kappa
C***********************************************************************
      implicit  none

      integer   im,jm,lm
      _RL ple(im,jm,lm+1)
      _RL pkle(im,jm,lm+1)
      _RL pkz(im,jm,lm)

      _RL akap1,getcon
      integer i,j,L

      akap1 = 1.0 + getcon('KAPPA')

      do L = 1,lm
      do j = 1,jm
      do i = 1,im
       pkz(i,j,L) = ( ple (i,j,l+1)*pkle(i,j,l+1)
     .             - ple (i,j,l)*pkle(i,j,l) )
     .           /  ( akap1* (ple (i,j,l+1)-ple (i,j,l)) )
      enddo
      enddo
      enddo

      return
      end
