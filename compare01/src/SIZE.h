C $Id: SIZE.h,v 1.3 1998/06/12 19:33:31 cnh Exp $
C     IM - Model X domain extent i.e. no. of boxes in X direction.
C     JM - Model Y domain extent i.e. no. of boxes in Y direction.
C     KM - Model Z domain extent i.e. no. of boxes in Z direction.
      INTEGER IM
      INTEGER JM
      INTEGER KM
      PARAMETER (
     &           IM=   90,
     &           JM=   41,
     &           KM=   20
     &          )
C     Variables used in model to declare array sizes. 
C     Note: One day these will be in common block and the program will not 
C           need to be recompiled every time the comutational domai size is 
C           altered.
      INTEGER nX
      INTEGER nY
      INTEGER nZ
      PARAMETER (
     &           nX=IM,
     &           nY=JM,
     &           nZ=KM
     &          )
