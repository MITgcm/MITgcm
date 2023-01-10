C Pure tangent mode:

      SUBROUTINE PUSHREAL8_D(V,VD)
      REAL*8 V,VD
      call PUSHREAL8(V)
      call PUSHREAL8(VD)
      END

      SUBROUTINE POPREAL8_D(V,VD)
      REAL*8 V,VD
      call POPREAL8(VD)
      call POPREAL8(V)
      END

      SUBROUTINE PUSHREAL8ARRAY_D(V,VD,size)
      INTEGER size
      REAL*8 V(size),VD(size)
      call PUSHREAL8ARRAY(V,size)
      call PUSHREAL8ARRAY(VD,size)
      END

      SUBROUTINE POPREAL8ARRAY_D(V,VD,size)
      INTEGER size
      REAL*8 V(size),VD(size)
      call POPREAL8ARRAY(VD,size)
      call POPREAL8ARRAY(V,size)
      END

C Multi-directional mode:

      SUBROUTINE PUSHREAL8_DV(V,VD,nbdirs)
      INTEGER nbdirs
      REAL*8 V,VD(nbdirs)
      call PUSHREAL8(V)
      call PUSHREAL8ARRAY(VD,nbdirs)
      END

      SUBROUTINE POPREAL8_DV(V,VD,nbdirs)
      INTEGER nbdirs
      REAL*8 V,VD(nbdirs)
      call POPREAL8ARRAY(VD,nbdirs)
      call POPREAL8(V)
      END

      SUBROUTINE PUSHREAL8ARRAY_DV(V,VD,size,nbdirs)
      INTEGER size,nbdirs
      REAL*8 V(size),VD(size,nbdirs)
      call PUSHREAL8ARRAY(V,size)
      call PUSHREAL8ARRAY(VD,size*nbdirs)
      END

      SUBROUTINE POPREAL8ARRAY_DV(V,VD,size,nbdirs)
      INTEGER size,nbdirs
      REAL*8 V(size),VD(size,nbdirs)
      call POPREAL8ARRAY(VD,size*nbdirs)
      call POPREAL8ARRAY(V,size)
      END
