C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_test_addfluid.h,v 1.1 2008/08/24 21:50:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_ADDFLUID
C   this is a small piece of code to add to S/R thsice_main.F
C   to check AddFluid implementation : 
C   uses addMass instead of EmPmR but solution should not change.
C   (tested in global_ocean.cs32x15.thsice set-up)

      IF ( selectAddFluid.NE.0 .AND. hsMax.GT.hiMax ) THEN
       IF ( myIter.EQ.nIter0 ) THEN
        WRITE(0,*) 'THSICE_MAIN: mv EmPmR to addMass(k=1)'
       ENDIF
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy-1
          DO i = 1-OLx, sNx+OLx-1
           addMass(i,j,1,bi,bj) = -EmPmR(i,j,bi,bj)
     &                            *rA(i,j,bi,bj)
     &                            *maskC(i,j,1,bi,bj)
           EmPmR(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_ADDFLUID */
