C $Header: /u/gcmpack/MITgcm/utils/knudsen2/unesco.f,v 1.1 2002/08/07 16:55:52 mlosch Exp $

      PROGRAM KNUDSEN2
      implicit none
C
C     COEFFICIENTS FOR DENSITY COMPUTATION
C
C     THIS PROGRAM CALCULATES THE COEFFICIENTS OF THE POLYNOMIAL
C     APPROXIMATION TO THE KNUDSEN FORMULA.  THE PROGRAM IS SET UP
C     TO YIELD COEFFICIENTS THAT WILL COMPUTE DENSITY AS A FUNCTION
C     OF POTENTIAL TEMPERATURE, SALINITY, AND DEPTH.
C
C     Number of levels
      integer NumLevels
      PARAMETER (NumLevels=15)

C     Number of Temperature and Salinity callocation points
      integer NumTempPt,NumSalPt,NumCallocPt
      PARAMETER (NumTempPt = 10, NumSalPt = 5 )
      PARAMETER (NumCallocPt =NumSalPt*NumTempPt )

C     Number of terms in fit polynomial
      integer NTerms
      PARAMETER (NTerms = 9 )

C     Functions (Density and Potential temperature)
      DOUBLE PRECISION DN
      DOUBLE PRECISION DUNESCO
      real POTEM
C
C     Arrays for least squares routine
      real A(NumCallocPt,NumCallocPt),
     &    B(NumCallocPt),
     &    X(NTerms),
     &    AB(13,NumLevels),
     &    C(NumCallocPt,NTerms),
     &    H(NTerms,NTerms),
     &    R(NumCallocPt*2),SB(NumCallocPt*4)

      real 
     &    Z(NumLevels),
     &    DZ(NumLevels)
      integer
     &    IDX(NumLevels)

      real 
     &    Theta(NumCallocPt),
     &    SPick(NumCallocPt),TPick(NumCallocPt),DenPick(NumCallocPt)

C     ENTER BOUNDS FOR POLYNOMIAL FIT:
C           TMin(K) = LOWER BND OF T AT Level K  (Insitu Temperature)
C           TMax(K) = UPPER BND OF T          "  (Insitu Temperature)
C           SMin(K) = LOWER BND OF S          "
C           SMax(K) = UPPER BND OF S          "

      real TMin(25), TMax(25)
     &    ,SMin(25), SMax(25)


      DATA TMin / 4*-2., 15*-1., 6*0.     /
      DATA TMax / 29., 19., 14., 11., 9., 7., 5., 3*4., 5*3., 10*2. /
      DATA SMin / 28.5, 33.7, 34., 34.1, 34.2, 34.4, 2*34.5,
     &    15*34.6, 2*34.7 /
      DATA SMax / 36.7, 36.6, 35.8, 35.7, 35.3, 2*35.1, 7*35.,
     &    9*34.9, 2*34.8 /

! Local
      integer I,J,K,IT,IEQ,IRANK,NDIM,NHDIM,N,M,IN,ITMAX,MP,L,NN
      real T,S,D,EPS,DeltaT,DeltaS,ENORM,DeltaDen,DensityRef
      real Tbar,Sbar,Tsum,SSum,TempInc,SalnInc
      real DensitySum,ThetaSum
      real DensityBar,thetabar,TempRef,SAlRef


C  ENTER LEVEL THICKNESSES IN CENTIMETERS  
C     DATA dz/ 4*50.E2, 2*100.E2, 200.E2, 400.E2, 7*500.E2, 6*10.E2 /
       DATA dz/   5.00e+03,7.00e+03,1.00e+04,1.40e+04,
     &1.90e+04,2.40e+04,2.90e+04,3.40e+04,3.90e+04,
     &4.40e+04,4.90e+04,5.40e+04,5.90e+04,6.40e+04,
     &6.90e+04/ ! ,6.90e+04/

C     CALC DEPTHS OF LEVELS FROM DZ (IN METERS)
C     THE MAXIMUM ALLOWABLE DEPTH IS 8000 METERS
      Z(1) = 0.             ! for level at box edges
      Z(1)=.5*DZ(1)/100.    ! for level at box center
      DO K=2,NumLevels
        Z(K)=Z(K-1)+.5*(DZ(K)+DZ(K-1))/100.
      ENDDO


C     Break the levels up into 250m bands
      DO  I=1,NumLevels
        IDX( I ) = I 
C       Comment out the next line to use input bands as polynomial levels
        IDX( I ) = IFIX(Z(I)/250.)+1  
      ENDDO


C     Write some diagnostics 
      PRINT 419
      PRINT 422,(Z(I),
     &    Tmin( IDX(I)),Tmax( IDX(I)),
     &    SMin(IDX(I)),Smax( IDX(I)),
CCC     &    TMin(I),TMax(I),SMin(I),SMax(I),
     &    (   IDX(I) ),I=1,NumLevels )


C     Loop over each level and calculate the polynomial coefficients

      DO MP=1,NumLevels

C     Choose callocation points
        TempInc =(Tmax( IDX(MP))-TMin(IDX(MP)))/(2.*FLOAT(NumSalPt)-1.0)
        SalnInc =(Smax( IDX(MP))-SMin(IDX(MP)))/(FLOAT(NumSalPt)-1.0)
        DO I=1,NumTempPt
          DO J=1,NumSalPt
            K=NumSalPt*I+J-NumSalPt
            TPick(K)=TMin(IDX(MP))+(FLOAT(I)-1.0)*TempInc
            SPick(K)=SMin(IDX(MP))+(FLOAT(J)-1.0)*SalnInc
          ENDDO
        ENDDO


C  For each callocation point, convert insitu temperature to
C  potential temperature and calculate the corresponding density.
        Tsum=0.0
        Ssum=0.0
        DensitySum=0.0
        ThetaSum=0.0
        DO K=1,NumCallocPt
          D=Z(MP)
          S=SPick(K)
          T=TPick(K)
C          DenPick(K)=DN(T,S,D)
          DenPick(K)=DUNESCO(T,S,D)
          Theta(K)=POTEM(T,S,D)
          Tsum=Tsum+TPick(K)
          Ssum=Ssum+SPick(K)
          DensitySum = DensitySum+DenPick(K)
          ThetaSum   = ThetaSum+Theta(K)
        ENDDO

C     Let (Tbar,Sbar) = the average of (T,S) used in the set of calloc pts
C     NOTE:  Tbar is still an insitu temperature
C     Also, calculate the average density of the set of callocation points

        Tbar=Tsum / FLOAT( NumCallocPt )
        Sbar=Ssum / FLOAT( NumCallocPt )
        DensityBar=DensitySum / FLOAT( NumCallocPt )

C     Calculate the average potential temperature of the callocation points
        ThetaBar=ThetaSum/ FLOAT( NumCallocPt )

C     Set the reference temperature, salinity and density
C        DensityRef=DN(Tbar,Sbar,D)
        DensityRef=DUNESCO(Tbar,Sbar,D)

        SalRef = Sbar

        TempRef=TBar
        TempRef=ThetaBar   ! DELETE THIS LINE IF USING IN SITU TEMPERATURES

C$$$        TempRef=POTEM(TBar,Sbar,D)


        AB(1,MP)=Z(MP)
        AB(2,MP)=DensityRef
        AB(3,MP)=TempRef
        AB(4,MP)=SalRef
        DO K=1,NumCallocPt
          TPick(K)=Theta(K)   ! DELETE THIS LINE IF USING IN SITU TEMPERATURES
          DeltaT   = TPick(K)   - TempRef
          DeltaS   = SPick(K)   - SalRef
          DeltaDen = DenPick(K) - DensityRef
          B(K)= DeltaDen
          A(K,1)=DeltaT
          A(K,2)=DeltaS
          A(K,3)=DeltaT*DeltaT
          A(K,4)=DeltaT*DeltaS
          A(K,5)=DeltaS*DeltaS
          A(K,6)=A(K,3)*DeltaT
          A(K,7)=A(K,4)*DeltaT
          A(K,8)=A(K,4)*DeltaS
          A(K,9)=A(K,5)*DeltaS
        ENDDO
C     SET THE ARGUMENTS IN CALL TO LSQSL2
C     FIRST DIMENSION OF ARRAY A
        NDIM=NumCallocPt
C     
C     NUMBER OF ROWS OF A
        M=NumCallocPt
C     
C     NUMBER OF COLUMNS OF A
        N=NTerms
C     OPTION NUMBER OF LSQSL2
        IN=1
C     
C     ITMAX=NUMBER OF ITERATIONS
        ITMAX=100
C     
        IT=0
        IEQ=2
        IRANK=0
        EPS=1.0E-7
        EPS=1.0E-11
        NHDIM=NTerms
        CALL LSQSL2(NDIM,A,M,N,B,X,IRANK,IN,ITMAX,IT,IEQ,ENORM,EPS,
     &      NHDIM,H,C,R,SB)

CCC   PRINT 411
        DO I=1,N
          AB(I+4,MP)=X(I)
        ENDDO
      ENDDO
      PRINT 430
  430 FORMAT(1X,'  Z    SIG0    T    S       X1          X2
     1    X3          X4          X5          X6          X7          X8
     2    X9     ',/)
      NN=N+4
ccc C************************************************************************
ccc C     WRITE TO UNIT 50
ccc C************************************************************************
ccc       OPEN(UNIT=50,FILE='KNUDSEN_COEFS.mit.h',STATUS='UNKNOWN')
ccc 
ccc C***      WRITE(50,613)
ccc 613   FORMAT('      REAL SIGREF(KM)')
ccc       ISEQ=114399990
ccc       DO  J=1,NumLevels
ccc         ISEQ=ISEQ+10
ccc C$$$        WRITE(50,615)J,.01*DZ(J)
ccc       ENDDO
ccc   615 FORMAT('      DZ(',I3,')=',F7.2,'E2')
ccc C$$$      WRITE(50,601)
ccc 601   FORMAT('C  REFERENCE DENSITIES AT T-POINTS'                )
ccc       ISEQ=114500030
ccc       DO J=1,NumLevels
ccc         ISEQ=ISEQ+10
ccc C$$$        WRITE(50,501)J,AB(2,J)
ccc       ENDDO
ccc 501   FORMAT('      SIGREF(',I3,')=',F8.4)
ccc 
ccc 
ccc       WRITE(50,'("      DATA SIGREF/")')
ccc 
ccc       N0 = 1
ccc c      DO ILINE = 1,NumLevels/5 -1
ccc 222   CONTINUE      
ccc         N1 = N0 + 4
ccc         N1 = MIN(N1, NumLevels)
ccc         WRITE(50,1280) (AB(2,I),I=N0,N1)
ccc         N0 = N1 + 1
ccc       IF ( N1 .LT. NumLevels ) GOTO 222
ccc C     N1 = N0 + 4
ccc C     IF( N1 .GT. NumLevels ) N1 = NumLevels
ccc       WRITE(50,1286) 
ccc 

C**********************************************************************

C MITgcmUV

      open(99,file='POLY3.COEFFS',form='formatted',status='unknown')
      write(99,*) NumLevels
      write(99,'(3(G20.13))') (ab(3,J),ab(4,J),ab(2,J),J=1 , NumLevels)
      do J=1,NumLevels
         write(99,'(3(G20.13))')(AB(I,J),I=5,13)
      enddo    
      close(99)

C**********************************************************************

c     PRINT 412,((AB(I,J),I=1,NN),J=1,NumLevels)

C     WRITE DATA STATEMENTS TO UNIT 50
caja  DO  L=1,NumLevels
caja    AB(2,L)=1.E-3*AB(2,L)
caja    AB(4,L)=1.E-3*AB(4,L)-.035
caja    AB(5,L)=1.E-3*AB(5,L)
caja    AB(7,L)=1.E-3*AB(7,L)
caja    AB(10,L)=1.E-3*AB(10,L)
caja    AB( 9,L)=1.E+3*AB( 9,L)
caja    AB(11,L)=1.E+3*AB(11,L)
caja    AB(13,L)=1.E+6*AB(13,L)
caja  ENDDO

C**********************************************************************

C MITgcm (compare01)

      open(99,file='polyeos.coeffs',form='formatted',status='unknown')
      do J=1,NumLevels
       write(99,*) ab(3,J)  ! ref temperature
      enddo    
      do J=1,NumLevels
       write(99,*) ab(4,J)  ! ref sal
      enddo    
      do J=1,NumLevels
       do I=5,13
        write(99,*) AB(I,J)
       enddo    
      enddo    
      do J=1,NumLevels
       write(99,*) ab(2,J)  ! ref sig0
      enddo    
CEK      write(99,200)(ab(3,J),J=11,15)  ! ref temperature
CEK      write(99,200)(ab(4,J),J= 1, 5)  ! ref sal
CEK      write(99,200)(ab(4,J),J= 6,10)  ! ref sal
CEK      write(99,200)(ab(4,J),J= 11,15) ! ref sal
CEK      do I=5,NN
CEK         write(99,200)(AB(I,J),J= 1, 5)
CEK         write(99,200)(AB(I,J),J= 6,10)
CEK         write(99,200)(AB(I,J),J=11,15)
CEK      enddo    
CEK      write(99,200)(AB(2,J),J= 1, 5)  ! ref sig0
CEK      write(99,200)(AB(2,J),J= 6,10)  ! ref sig0
CEK      write(99,200)(AB(2,J),J=11,15)  ! ref sig0
CEK  200 format(5(E14.7,1X))

C**********************************************************************



ccc       NSEQ=603800000
ccc       WRITE(50,1298)
ccc       N=0
ccc  1260 IS=N+1
ccc       IE=N+5
ccc       NSEQ=NSEQ+10
ccc       IF(IE.LT.NumLevels) THEN
ccc         WRITE(50,1280) (AB(3,I),I=IS,IE)
ccc         N=IE
ccc         GO TO 1260
ccc       ELSE
ccc         IE=NumLevels
ccc         N=IE-IS+1
ccc         GO TO (1261,1262,1263,1264,1265),N
ccc  1261     WRITE(50,1281) (AB(3,I),I=IS,IE)
ccc           GO TO 1268
ccc  1262     WRITE(50,1282) (AB(3,I),I=IS,IE)
ccc           GO TO 1268
ccc  1263     WRITE(50,1283) (AB(3,I),I=IS,IE)
ccc           GO TO 1268
ccc  1264     WRITE(50,1284) (AB(3,I),I=IS,IE)
ccc           GO TO 1268
ccc  1265     WRITE(50,1285) (AB(3,I),I=IS,IE)
ccc       ENDIF
ccc  1268 CONTINUE
ccc       NSEQ=603900000
ccc       WRITE(50,1297)
ccc       N=0
ccc  1270 IS=N+1
ccc       IE=N+5
ccc       NSEQ=NSEQ+10
ccc       IF(IE.LT.NumLevels) THEN
ccc         WRITE(50,1280) (AB(4,I),I=IS,IE)
ccc         N=IE
ccc         GO TO 1270
ccc       ELSE
ccc         IE=NumLevels
ccc         N=IE-IS+1
ccc         GO TO (1271,1272,1273,1274,1275),N
ccc  1271     WRITE(50,1281) (AB(4,I),I=IS,IE)
ccc           GO TO 1278
ccc  1272     WRITE(50,1282) (AB(4,I),I=IS,IE)
ccc           GO TO 1278
ccc  1273     WRITE(50,1283) (AB(4,I),I=IS,IE)
ccc           GO TO 1278
ccc  1274     WRITE(50,1284) (AB(4,I),I=IS,IE)
ccc           GO TO 1278
ccc  1275     WRITE(50,1285) (AB(4,I),I=IS,IE)
ccc       ENDIF
ccc  1278 CONTINUE
ccc       DO 1200 L=1,NumLevels
ccc       IF(L.EQ.1) NSEQ=604000000
ccc       WRITE(50,1296) L
ccc       NSEQ=NSEQ+10
ccc       WRITE(50,1295) (AB(I,L),I=5,8)
ccc       NSEQ=NSEQ+10
ccc       WRITE(50,1295) (AB(I,L),I=9,12)
ccc       NSEQ=NSEQ+10
ccc       WRITE(50,1294) AB(13,L)
ccc       NSEQ=NSEQ+10
ccc  1200 CONTINUE
ccc  1288 CONTINUE
 1298 FORMAT(18H      DATA TRef  /,67X,I9)
 1297 FORMAT(18H      DATA SRef  /,67X,I9)
C 419  FORMAT(5X,'LEVEL   TMIN      TMAX      SMIN      SMAX   ',
C     &          ' TMIN()  Tmax()   Smin()    Smax()  D/250')
C  422 FORMAT (5X,F6.1,4F10.3,4F10.3,I3)
  419 FORMAT(5X,'LEVEL   TMIN      TMAX      SMIN      SMAX  D/250')
  422 FORMAT (5X,F6.1,4F10.3,I3)
  412 FORMAT(1X,F6.1,F8.4,F7.3,F6.2,9E12.5)
 1296 FORMAT(6X,'DATA (C(',I2,',N),N=1,9)/',54X,I9)
 1295 FORMAT(5X,1H*,9X,4(E13.7,1H,),10X,I9)
 1294 FORMAT(5X,1H*,9X,E13.7,1H/,52X,I9)
 1280 FORMAT(5X,1H*,8X,5(F10.7,1H,),12X,I9)
 1281 FORMAT(5X,1H*,8X,F10.7,1H/,56X,I9)
 1282 FORMAT(5X,1H*,8X,F10.7,1H,,F10.7,1H/,45X,I9)
 1283 FORMAT(5X,1H*,8X,2(F10.7,1H,),F10.7,1H/,34X,I9)
 1284 FORMAT(5X,1H*,8X,3(F10.7,1H,),F10.7,1H/,23X,I9)
 1285 FORMAT(5X,1H*,8X,4(F10.7,1H,),F10.7,1H/,12X,I9)
 1286 FORMAT(5X,1H*,1H/)
  350 FORMAT(1X,E14.7)
  351 FORMAT(1H+,T86,E14.7/)
  400 FORMAT(1X,9E14.7)
  410 FORMAT(1X,5E14.7)
  411 FORMAT(///)
      STOP
      END
C****************************************************************************
*DECK LSQSL2
      SUBROUTINE LSQSL2 (NDIM,A,D,W,B,X,IRANK,IN,ITMAX,IT,IEQ,ENORM,EPS1
     1,NHDIM,H,AA,R,S)
      implicit none
C     THIS ROUTINE IS A MODIFICATION OF LSQSOL. MARCH,1968. R. HANSON.
C     LINEAR LEAST SQUARES SOLUTION
C
C     THIS ROUTINE FINDS X SUCH THAT THE EUCLIDEAN LENGTH OF
C     (*) AX-B IS A MINIMUM.
C
C     HERE A HAS K ROWS AND N COLUMNS, WHILE B IS A COLUMN VECTOR WITH
C     K COMPONENTS.
C
C     AN ORTHOGONAL MATRIX Q IS FOUND SO THAT QA IS ZERO BELOW
C     THE MAIN DIAGONAL.
C     SUPPOSE THAT RANK (A)=R
C     AN ORTHOGONAL MATRIX S IS FOUND SUCH THAT
C     QAS=T IS AN R X N UPPER TRIANGULAR MATRIX WHOSE LAST N-R COLUMNS
C     ARE ZERO.
C     THE SYSTEM TZ=C (C THE FIRST R COMPONENTS OF QB) IS THEN
C     SOLVED. WITH W=SZ, THE SOLUTION MAY BE EXPRESSED
C     AS X = W + SY, WHERE W IS THE SOLUTION OF (*) OF MINIMUM EUCLID-
C     EAN LENGTH AND Y IS ANY SOLUTION TO (QAS)Y=TY=0.
C
C     ITERATIVE IMPROVEMENTS ARE CALCULATED USING RESIDUALS AND
C     THE ABOVE PROCEDURES WITH B REPLACED BY B-AX, WHERE X IS AN
C     APPROXIMATE SOLUTION.
C
      integer ndim,nhdim
      DOUBLE PRECISION SJ,DP,DP1,UP,BP,AJ
      LOGICAL ERM
      INTEGER D,W
C
C     IN=1 FOR FIRST ENTRY.
C                   A IS DECOMPOSED AND SAVED. AX-B IS SOLVED.
C     IN = 2 FOR SUBSEQUENT ENTRIES WITH A NEW VECTOR B.
C     IN=3 TO RESTORE A FROM THE PREVIOUS ENTRY.
C     IN=4 TO CONTINUE THE ITERATIVE IMPROVEMENT FOR THIS SYSTEM.
C     IN = 5 TO CALCULATE SOLUTIONS TO AX=0, THEN STORE IN THE ARRAY H.
C     IN  =  6   DO NOT STORE A  IN AA.  OBTAIN  T = QAS, WHERE T IS
C     MIN(K,N) X MIN(K,N) AND UPPER TRIANGULAR. NOW RETURN.DO NOT OBTAIN
C     A SOLUTION.
C     NO SCALING OR COLUMN INTERCHANGES ARE PERFORMED.
C     IN  =  7   SAME AS WITH  IN = 6  EXCEPT THAT SOLN. OF MIN. LENGTH
C                IS PLACED INTO X. NO ITERATIVE REFINEMENT.  NOW RETURN.
C     COLUMN INTERCHANGES ARE PERFORMED. NO SCALING IS PERFORMED.
C     IN  = 8    SET ADDRESSES. NOW RETURN.
C
C     OPTIONS FOR COMPUTING  A MATRIX PRODUCT   Y*H  OR  H*Y ARE
C     AVAILABLE WITH THE USE OF THE ENTRY POINTS  MYH AND MHY.
C     USE OF THESE OPTIONS IN THESE ENTRY POINTS ALLOW A GREAT SAVING IN
C     STORAGE REQUIRED.
C
C
      real A(NDIM,NDIM),B(1),AA(D,W),S(1), X(1),H(NHDIM,NHDIM),R(1)
C     D = DEPTH OF MATRIX.
C     W = WIDTH OF MATRIX.
C----
      integer K,N,IT,ISW,L,M,IRANK,IEQ,IN,K1
      integer J1,J2,J3,J4,J5,J6,J7,J8,J9
      integer N1,N2,N3,N4,N5,N6,N7,N8,NS
      integer I,ITMAX,IPM1,II,LM,J,IP,KM,IPP1,IRP1,IRM1
      real SP,ENORM,TOP,TOP1,ENM1,TOP2,EPS1,EPS2,A1,A2,AM
C----
      K=D
      N=W
      ERM=.TRUE.
C
C     IF IT=0 ON ENTRY, THE POSSIBLE ERROR MESSAGE WILL BE SUPPRESSED.
C
      IF (IT.EQ.0) ERM=.FALSE.
C
C     IEQ = 2      IF COLUMN SCALING BY LEAST MAX. COLUMN LENGTH IS
C     TO BE PERFORMED.
C
C     IEQ = 1       IF SCALING OF ALL COMPONENTS IS TO BE DONE WITH
C     THE SCALAR MAX(ABS(AIJ))/K*N.
C
C     IEQ = 3 IF COLUMN SCALING AS WITH IN =2 WILL BE RETAINED IN
C     RANK DEFICIENT CASES.
C
C     THE ARRAY S MUST CONTAIN AT LEAST MAX(K,N) + 4N + 4MIN(K,N) CELLS
C        THE   ARRAY R MUST CONTAIN K+4N S.P. CELLS.
C
      DATA EPS2/1.E-16/
C     THE LAST CARD CONTROLS DESIRED RELATIVE ACCURACY.
C     EPS1  CONTROLS  (EPS) RANK.
C
      ISW=1
      L=MIN0(K,N)
      M=MAX0(K,N)
      J1=M
      J2=N+J1
      J3=J2+N
      J4=J3+L
      J5=J4+L
      J6=J5+L
      J7=J6+L
      J8=J7+N
      J9=J8+N
      LM=L
      IF (IRANK.GE.1.AND.IRANK.LE.L) LM=IRANK
      IF (IN.EQ.6) LM=L
      IF (IN.EQ.8) RETURN
C
C     RETURN AFTER SETTING ADDRESSES WHEN IN=8.
C
      GO TO (10,360,810,390,830,10,10), IN
C
C     EQUILIBRATE COLUMNS OF A (1)-(2).
C
C     (1)
C
   10 CONTINUE
C
C     SAVE DATA WHEN IN = 1.
C
      IF (IN.GT.5) GO TO 30
      DO 20 J=1,N
      DO 20 I=1,K
   20 AA(I,J)=A(I,J)
   30 CONTINUE
      IF (IEQ.EQ.1) GO TO 60
      DO 50 J=1,N
      AM=0.E0
      DO 40 I=1,K
   40 AM=AMAX1(AM,ABS(A(I,J)))
C
C      S(M+N+1)-S(M+2N) CONTAINS SCALING FOR OUTPUT VARIABLES.
C
      N2=J2+J
      IF (IN.EQ.6) AM=1.E0
      S(N2)=1.E0/AM
      DO 50 I=1,K
   50 A(I,J)=A(I,J)*S(N2)
      GO TO 100
   60 AM=0.E0
      DO 70 J=1,N
      DO 70 I=1,K
   70 AM=AMAX1(AM,ABS(A(I,J)))
      AM=AM/FLOAT(K*N)
      IF (IN.EQ.6) AM=1.E0
      DO 80 J=1,N
      N2=J2+J
   80 S(N2)=1.E0/AM
      DO 90 J=1,N
      N2=J2+J
      DO 90 I=1,K
   90 A(I,J)=A(I,J)*S(N2)
C     COMPUTE COLUMN LENGTHS WITH D.P. SUMS FINALLY ROUNDED TO S.P.
C
C     (2)
C
  100 DO 110 J=1,N
      N7=J7+J
      N2=J2+J
  110 S(N7)=S(N2)
C
C      S(M+1)-S(M+ N) CONTAINS VARIABLE PERMUTATIONS.
C
C     SET PERMUTATION TO IDENTITY.
C
      DO 120 J=1,N
      N1=J1+J
  120 S(N1)=J
C
C     BEGIN ELIMINATION ON THE MATRIX A WITH ORTHOGONAL MATRICES .
C
C     IP=PIVOT ROW
C
      DO 250 IP=1,LM
C
C
      DP=0.D0
      KM=IP
      DO 140 J=IP,N
      SJ=0.D0
      DO 130 I=IP,K
      SJ=SJ+A(I,J)**2
  130 CONTINUE
      IF (DP.GT.SJ) GO TO 140
      DP=SJ
      KM=J
      IF (IN.EQ.6) GO TO 160
  140 CONTINUE
C
C     MAXIMIZE (SIGMA)**2 BY COLUMN INTERCHANGE.
C
C      SUPRESS COLUMN INTERCHANGES WHEN IN=6.
C
C
C     EXCHANGE COLUMNS IF NECESSARY.
C
      IF (KM.EQ.IP) GO TO 160
      DO 150 I=1,K
      A1=A(I,IP)
      A(I,IP)=A(I,KM)
  150 A(I,KM)=A1
C
C     RECORD PERMUTATION AND EXCHANGE SQUARES OF COLUMN LENGTHS.
C
      N1=J1+KM
      A1=S(N1)
      N2=J1+IP
      S(N1)=S(N2)
      S(N2)=A1
      N7=J7+KM
      N8=J7+IP
      A1=S(N7)
      S(N7)=S(N8)
      S(N8)=A1
  160 IF (IP.EQ.1) GO TO 180
      A1=0.E0
      IPM1=IP-1
      DO 170 I=1,IPM1
      A1=A1+A(I,IP)**2
  170 CONTINUE
      IF (A1.GT.0.E0) GO TO 190
  180 IF (DP.GT.0.D0) GO TO 200
C
C     TEST FOR RANK DEFICIENCY.
C
  190 IF (DSQRT(DP/A1).GT.EPS1) GO TO 200
      IF (IN.EQ.6) GO TO 200
      II=IP-1
      IF (ERM) WRITE (6,1140) IRANK,EPS1,II,II
      IRANK=IP-1
      ERM=.FALSE.
      GO TO 260
C
C     (EPS1) RANK IS DEFICIENT.
C
  200 SP=DSQRT(DP)
C
C     BEGIN FRONT ELIMINATION ON COLUMN IP.
C
C     SP=SQROOT(SIGMA**2).
C
      BP=1.D0/(DP+SP*ABS(A(IP,IP)))
C
C     STORE BETA IN S(3N+1)-S(3N+L).
C
      IF (IP.EQ.K) BP=0.D0
      N3=K+2*N+IP
      R(N3)=BP
      UP=DSIGN(DBLE(SP)+ABS(A(IP,IP)),DBLE(A(IP,IP)))
      IF (IP.GE.K) GO TO 250
      IPP1=IP+1
      IF (IP.GE.N) GO TO 240
      DO 230 J=IPP1,N
      SJ=0.D0
      DO 210 I=IPP1,K
  210 SJ=SJ+A(I,J)*A(I,IP)
      SJ=SJ+UP*A(IP,J)
      SJ=BP*SJ
C
C     SJ=YJ NOW
C
      DO 220 I=IPP1,K
  220 A(I,J)=A(I,J)-A(I,IP)*SJ
  230 A(IP,J)=A(IP,J)-SJ*UP
  240 A(IP,IP)=-SIGN(SP,A(IP,IP))
C
      N4=K+3*N+IP
      R(N4)=UP
  250 CONTINUE
      IRANK=LM
  260 IRP1=IRANK+1
      IRM1=IRANK-1
      IF (IRANK.EQ.0.OR.IRANK.EQ.N) GO TO 360
      IF (IEQ.EQ.3) GO TO 290
C
C     BEGIN BACK PROCESSING FOR RANK DEFICIENCY CASE
C      IF IRANK IS LESS THAN N.
C
      DO 280 J=1,N
      N2=J2+J
      N7=J7+J
      L=MIN0(J,IRANK)
C
C     UNSCALE COLUMNS FOR RANK DEFICIENT MATRICES WHEN IEQ.NE.3.
C
      DO 270 I=1,L
  270 A(I,J)=A(I,J)/S(N7)
      S(N7)=1.E0
  280 S(N2)=1.E0
  290 IP=IRANK
  300 SJ=0.D0
      DO 310 J=IRP1,N
      SJ=SJ+A(IP,J)**2
  310 CONTINUE
      SJ=SJ+A(IP,IP)**2
      AJ=DSQRT(SJ)
      UP=DSIGN(AJ+ABS(A(IP,IP)),DBLE(A(IP,IP)))
C
C     IP TH ELEMENT OF U VECTOR CALCULATED.
C
      BP=1.D0/(SJ+ABS(A(IP,IP))*AJ)
C
C     BP = 2/LENGTH OF U SQUARED.
C
      IPM1=IP-1
      IF (IPM1.LE.0) GO TO 340
      DO 330 I=1,IPM1
      DP=A(I,IP)*UP
      DO 320 J=IRP1,N
      DP=DP+A(I,J)*A(IP,J)
  320 CONTINUE
      DP=DP/(SJ+ABS(A(IP,IP))*AJ)
C
C     CALC. (AJ,U), WHERE AJ=JTH ROW OF A
C
      A(I,IP)=A(I,IP)-UP*DP
C
C     MODIFY ARRAY A.
C
      DO 330 J=IRP1,N
  330 A(I,J)=A(I,J)-A(IP,J)*DP
  340 A(IP,IP)=-DSIGN(AJ,DBLE(A(IP,IP)))
C
C     CALC. MODIFIED PIVOT.
C
C
C     SAVE BETA AND IP TH ELEMENT OF U VECTOR IN R ARRAY.
C
      N6=K+IP
      N7=K+N+IP
      R(N6)=BP
      R(N7)=UP
C
C     TEST FOR END OF BACK PROCESSING.
C
      IF (IP-1) 360,360,350
  350 IP=IP-1
      GO TO 300
  360 IF (IN.EQ.6) RETURN
      DO 370 J=1,K
  370 R(J)=B(J)
      IT=0
C
C     SET INITIAL X VECTOR TO ZERO.
C
      DO 380 J=1,N
  380 X(J)=0.D0
      IF (IRANK.EQ.0) GO TO 690
C
C     APPLY Q TO RT. HAND SIDE.
C
  390 DO 430 IP=1,IRANK
      N4=K+3*N+IP
      SJ=R(N4)*R(IP)
      IPP1=IP+1
      IF (IPP1.GT.K) GO TO 410
      DO 400 I=IPP1,K
  400 SJ=SJ+A(I,IP)*R(I)
  410 N3=K+2*N+IP
      BP=R(N3)
      IF (IPP1.GT.K) GO TO 430
      DO 420 I=IPP1,K
  420 R(I)=R(I)-BP*A(I,IP)*SJ
  430 R(IP)=R(IP)-BP*R(N4)*SJ
      DO 440 J=1,IRANK
  440 S(J)=R(J)
      ENORM=0.E0
      IF (IRP1.GT.K) GO TO 510
      DO 450 J=IRP1,K
  450 ENORM=ENORM+R(J)**2
      ENORM=SQRT(ENORM)
      GO TO 510
  460 DO 480 J=1,N
      SJ=0.D0
      N1=J1+J
      IP=S(N1)
      DO 470 I=1,K
  470 SJ=SJ+R(I)*AA(I,IP)
C
C     APPLY AT TO RT. HAND SIDE.
C     APPLY SCALING.
C
      N7=J2+IP
      N1=K+N+J
  480 R(N1)=SJ*S(N7)
      N1=K+N
      S(1)=R(N1+1)/A(1,1)
      IF (N.EQ.1) GO TO 510
      DO 500 J=2,N
      N1=J-1
      SJ=0.D0
      DO 490 I=1,N1
  490 SJ=SJ+A(I,J)*S(I)
      N2=K+J+N
  500 S(J)=(R(N2)-SJ)/A(J,J)
C
C     ENTRY TO CONTINUE ITERATING.  SOLVES TZ = C = 1ST IRANK
C     COMPONENTS OF QB .
C
  510 S(IRANK)=S(IRANK)/A(IRANK,IRANK)
      IF (IRM1.EQ.0) GO TO 540
      DO 530 J=1,IRM1
      N1=IRANK-J
      N2=N1+1
      SJ=0.
      DO 520 I=N2,IRANK
  520 SJ=SJ+A(N1,I)*S(I)
  530 S(N1)=(S(N1)-SJ)/A(N1,N1)
C
C     Z CALCULATED.  COMPUTE X = SZ.
C
  540 IF (IRANK.EQ.N) GO TO 590
      DO 550 J=IRP1,N
  550 S(J)=0.E0
      DO 580 I=1,IRANK
      N7=K+N+I
      SJ=R(N7)*S(I)
      DO 560 J=IRP1,N
      SJ=SJ+A(I,J)*S(J)
  560 CONTINUE
      N6=K+I
      DO 570 J=IRP1,N
  570 S(J)=S(J)-A(I,J)*R(N6)*SJ
  580 S(I)=S(I)-R(N6)*R(N7)*SJ
C
C     INCREMENT FOR X OF MINIMAL LENGTH CALCULATED.
C
  590 DO 600 I=1,N
  600 X(I)=X(I)+S(I)
      IF (IN.EQ.7) GO TO 750
C
C     CALC. SUP NORM OF INCREMENT AND RESIDUALS
C
      TOP1=0.E0
      DO 610 J=1,N
      N2=J7+J
  610 TOP1=AMAX1(TOP1,ABS(S(J))*S(N2))
      DO 630 I=1,K
      SJ=0.D0
      DO 620 J=1,N
      N1=J1+J
      IP=S(N1)
      N7=J2+IP
  620 SJ=SJ+AA(I,IP)*X(J)*S(N7)
  630 R(I)=B(I)-SJ
      IF (ITMAX.LE.0) GO TO 750
C
C     CALC. SUP NORM OF X.
C
      TOP=0.E0
      DO 640 J=1,N
      N2=J7+J
  640 TOP=AMAX1(TOP,ABS(X(J))*S(N2))
C
C     COMPARE RELATIVE CHANGE IN X WITH TOLERANCE EPS .
C
      IF (TOP1-TOP*EPS2) 690,650,650
  650 IF (IT-ITMAX) 660,680,680
  660 IT=IT+1
      IF (IT.EQ.1) GO TO 670
      IF (TOP1.GT..25*TOP2) GO TO 690
  670 TOP2=TOP1
      GO TO (390,460), ISW
  680 IT=0
  690 SJ=0.D0
      DO 700 J=1,K
      SJ=SJ+R(J)**2
  700 CONTINUE
      ENORM=DSQRT(SJ)
      IF (IRANK.EQ.N.AND.ISW.EQ.1) GO TO 710
      GO TO 730
  710 ENM1=ENORM
C
C     SAVE X ARRAY.
C
      DO 720 J=1,N
      N1=K+J
  720 R(N1)=X(J)
      ISW=2
      IT=0
      GO TO 460
C
C     CHOOSE BEST SOLUTION
C
  730 IF (IRANK.LT.N) GO TO 750
      IF (ENORM.LE.ENM1) GO TO 750
      DO 740 J=1,N
      N1=K+J
  740 X(J)=R(N1)
      ENORM=ENM1
C
C     NORM OF AX - B LOCATED IN THE CELL ENORM .
C
C
C     REARRANGE VARIABLES.
C
  750 DO 760 J=1,N
      N1=J1+J
  760 S(J)=S(N1)
      DO 790 J=1,N
      DO 770 I=J,N
      IP=S(I)
      IF (J.EQ.IP) GO TO 780
  770 CONTINUE
  780 S(I)=S(J)
      S(J)=J
      SJ=X(J)
      X(J)=X(I)
  790 X(I)=SJ
C
C     SCALE VARIABLES.
C
      DO 800 J=1,N
      N2=J2+J
  800 X(J)=X(J)*S(N2)
      RETURN
C
C     RESTORE A.
C
  810 DO 820 J=1,N
      N2=J2+J
      DO 820 I=1,K
  820 A(I,J)=AA(I,J)
      RETURN
C
C     GENERATE SOLUTIONS TO THE HOMOGENEOUS EQUATION AX = 0.
C
  830 IF (IRANK.EQ.N) RETURN
      NS=N-IRANK
      DO 840 I=1,N
      DO 840 J=1,NS
  840 H(I,J)=0.E0
      DO 850 J=1,NS
      N2=IRANK+J
  850 H(N2,J)=1.E0
      IF (IRANK.EQ.0) RETURN
      DO 870 J=1,IRANK
      DO 870 I=1,NS
      N7=K+N+J
      SJ=R(N7)*H(J,I)
      DO 860 K1=IRP1,N
  860 SJ=SJ+H(K1,I)*A(J,K1)
      N6=K+J
      BP=R(N6)
      DP=BP*R(N7)*SJ
      A1=DP
      A2=DP-A1
      H(J,I)=H(J,I)-(A1+2.*A2)
      DO 870 K1=IRP1,N
      DP=BP*A(J,K1)*SJ
      A1=DP
      A2=DP-A1
  870 H(K1,I)=H(K1,I)-(A1+2.*A2)
C
C     REARRANGE ROWS OF SOLUTION MATRIX.
C
      DO 880 J=1,N
      N1=J1+J
  880 S(J)=S(N1)
      DO 910 J=1,N
      DO 890 I=J,N
      IP=S(I)
      IF (J.EQ.IP) GO TO 900
  890 CONTINUE
  900 S(I)=S(J)
      S(J)=J
      DO 910 K1=1,NS
      A1=H(J,K1)
      H(J,K1)=H(I,K1)
  910 H(I,K1)=A1
      RETURN
C
 1140 FORMAT (31H0WARNING. IRANK HAS BEEN SET TO,I4,6H  BUT(,1PE10.3,9H)
     1 RANK IS,I4,25H.  IRANK IS NOW TAKEN AS ,I4,1H.)
      END
      FUNCTION POTEM(T,S,P)
      implicit none
C     POTENTIAL TEMPERATURE FUNCTION
C     BASED ON FOFONOFF AND FROESE (1958) AS SHOWN IN "THE SEA" VOL. 1,
C     PAGE 17, TABLE IV
C     INPUT IS TEMPERATURE, SALINITY, PRESSURE (OR DEPTH)
C     UNITS ARE DEG.C., PPT, DBARS (OR METERS)
      real POTEM,T,S,P
      real B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11
      real T2,T3,S2,P2
      B1=-1.60E-5*P
      B2=1.014E-5*P*T
      T2=T*T
      T3=T2*T
      B3=-1.27E-7*P*T2
      B4=2.7E-9*P*T3
      B5=1.322E-6*P*S
      B6=-2.62E-8*P*S*T
      S2=S*S
      P2=P*P
      B7=4.1E-9*P*S2
      B8=9.14E-9*P2
      B9=-2.77E-10*P2*T
      B10=9.5E-13*P2*T2
      B11=-1.557E-13*P2*P
      POTEM=B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11
      POTEM=T-POTEM
      RETURN
      END
      FUNCTION DN(T,S,D)
      implicit none
      real T,S,D
      DOUBLE PRECISION DN,T3,S2,T2,S3,F1,F2,F3,FS,SIGMA,A,B1,B2,B,CO,
     1ALPHA,ALPSTD
      T2 = T*T
      T3= T2* T
      S2 = S*S
      S3 = S2 * S
      F1 = -(T-3.98)**2 * (T+283.)/(503.57*(T+67.26))
      F2 = T3*1.0843E-6 - T2*9.8185E-5 + T*4.786E-3
      F3 = T3*1.667E-8 - T2*8.164E-7 + T*1.803E-5
      FS= S3*6.76786136D-6 - S2*4.8249614D-4 + S*8.14876577D-1
      SIGMA= F1 + (FS+3.895414D-2)*(1.-F2+F3*(FS-.22584586D0))
      A= D*1.0E-4*(105.5+ T*9.50 - T2*0.158 - D*T*1.5E-4)  -
     1(227. + T*28.33 - T2*0.551 + T3* 0.004)
      B1 = (FS-28.1324)/10.0
      B2 = B1 * B1
      B= -B1* (147.3-T*2.72 + T2*0.04 - D*1.0E-4*(32.4- 0.87*T+.02*T2))
      B= B+ B2*(4.5-0.1*T - D*1.0E-4*(1.8-0.06*T))
      CO = 4886./(1. + 1.83E-5*D)
      ALPHA=     D*1.0E-6* (CO+A+B)
      DN=(SIGMA+ALPHA)/(1.-1.E-3*ALPHA)
      RETURN
      END

      double precision function dunesco(t,s,d)

      implicit none
      real t,s,d
      double precision t2,t3,t4,s2,s3o2,p,p2,r,comp

c     convert from meters to bars
      p = d*0.1D0
      
      t2 = t*t
      t3 = t2*t
      t4 = t3*t
      
      s2 = s*s
      if (s .lt. 0.) then
         write(*,*) '****************************'
         write(*,*) '* subroutine density_field *'
         write(*,*) '****************************'
         write(*,*) '             WARNING: salinity takes on'
         write(*,*)
     &        '             negative values '
         write(*,*) '             S = ', s
         write(*,*) '             This is not supposed to ',
     &        'happen. Have a look!'
         stop
      end if
      s3o2 = sqrt(s2*s)
      
      
***   density at one standard atmosphere pressure (p=0) in si-units
      
      r =    999.842594                 + 6.793952e-02 * t 
     &     -          9.095290e-3  * t2 + 1.001685e-04 * t3  
     &     -          1.120083e-06 * t4 + 6.536332e-9  * t4*t
     &     + s    * ( 8.24493e-01       - 4.0899e-3    * t 
     &              + 7.6438e-05   * t2 - 8.2467e-07   * t3 
     &              + 5.3875e-09   * t4 ) 
     &     + s3o2 * (                   - 5.72466e-03 
     &              + 1.0227e-04   * t  - 1.6546e-06   * t2 ) 
     &     +          4.8314e-04   * s2 

***   density at depth d[m] = p[bar] :
***   include depth effect of secant bulk modulus k(s,t,p) if desired
              
      p2   = p*p
      
      comp = 19652.21 
     &     +        148.4206       * t  -   2.327105     * t2 
     &     +          1.360477e-02 * t3 -   5.155288e-05 * t4  
     &     + p    * ( 3.239908          +   1.43713e-03  * t 
     &              + 1.16092e-04  * t2 -   5.77905e-07  * t3 )  
     &     + p2   * ( 8.50935e-05       -   6.12293e-06  * t 
     &              + 5.2787e-08   * t2 ) 
     &     + s    * (54.6746            -   0.603459     * t 
     &              + 1.09987e-02  * t2 -   6.1670e-5    * t3 ) 
     &     + s3o2 * ( 7.944e-02         +   1.6483e-02   * t 
     &              - 5.3009e-04*t2)        
     &     + p*s  * ( 2.2838e-03 
     &              - 1.0981e-05   * t  -   1.6078e-06   * t2 ) 
     &              + 1.91075e-04  * p*s3o2  
     &     + p2*s * (                   -  9.9348e-07 
     &              + 2.0816e-08   * t  +   9.1697e-10   * t2 )

      dunesco = r/(1.D0-(p/comp))-1000.D0

      return
      end
