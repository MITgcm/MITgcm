!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
        module OAD_active
        use w2f__types
        implicit none
        private :: runTimeErrorStop, shapeChange
        public :: active
#ifndef TRACE
        public :: saxpy, sax, zero_deriv, setderiv
        public :: set_neg_deriv, inc_deriv, dec_deriv
#endif
        public :: oad_convert
        public :: oad_allocateMatching, oad_allocateShape, oad_shapeTest
#ifndef TRACE
        integer :: count_mult = 0
        integer :: count_add = 0
#endif
        integer, parameter :: shapeChange=0
#ifdef VECTOR
        integer :: max_deriv_vec_len
        parameter ( max_deriv_vec_len = 100 )
        integer :: oad_ctmp_i1, oad_ctmp_i2, oad_ctmp_i3, & 
        oad_ctmp_i4, oad_ctmp_i5, oad_ctmp_i6, oad_ctmp_i7
# define VECTOR_DIM , dimension(max_deriv_vec_len)
# define VECTOR_LOOP_VAR integer :: i
# define VECTOR_LOOP_BEGIN do i=1,max_deriv_vec_len
# define VECTOR_LOOP_END end do
# define DELEM d(i)
#else 
# define VECTOR_DIM 
# define VECTOR_LOOP_VAR
# define VECTOR_LOOP_BEGIN
# define VECTOR_LOOP_END
# define DELEM d
#endif
#ifdef SCALARNDI
# define DINIT
#else
# define DINIT =0.0d0
#endif

        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          real(w2f__8) :: v 
#ifndef TRACE
          ! initialization does not work for active variables
          ! inside of common block, such as in boxmodel
          ! initialization is required for correct adjoint
          real(w2f__8) VECTOR_DIM :: d DINIT
#endif
        end type
#ifndef TRACE
        interface saxpy
          module procedure saxpy_d0_a0_a0
          module procedure saxpy_l0_a0_a0
          module procedure saxpy_i0_a0_a0
          module procedure saxpy_d0_a0_a1
          module procedure saxpy_d0_a1_a1
          module procedure saxpy_d0_a2_a2
          module procedure saxpy_d1_a0_a1
          module procedure saxpy_d1_a1_a1 
          module procedure saxpy_l1_a1_a1 
          module procedure saxpy_i1_a1_a1
          module procedure saxpy_a1_a1_a1
          module procedure saxpy_d2_a0_a2
          module procedure saxpy_d2_a2_a2
# ifndef DEFAULT_R8
          module procedure saxpy_r0_a0_a0
          module procedure saxpy_r0_a1_a1
          module procedure saxpy_r1_a0_a1
          module procedure saxpy_r1_a1_a1
# endif
        end interface
        
        interface setderiv
          module procedure setderiv_a0_a0
          module procedure setderiv_a1_a0
          module procedure setderiv_a1_a1
          module procedure setderiv_a2_a0
          module procedure setderiv_a2_a2
          module procedure setderiv_a3_a3
        end interface

        interface set_neg_deriv
          module procedure set_neg_deriv_a0_a0
          module procedure set_neg_deriv_a1_a1
        end interface set_neg_deriv

        interface inc_deriv
          module procedure inc_deriv_a0_a0
          module procedure inc_deriv_a1_a1
          module procedure inc_deriv_a2_a2
        end interface inc_deriv

        interface dec_deriv
          module procedure dec_deriv_a0_a0
          module procedure dec_deriv_a1_a1
          module procedure dec_deriv_a2_a2
        end interface dec_deriv

        interface zero_deriv
          module procedure zero_deriv_a0
          module procedure zero_deriv_a1
          module procedure zero_deriv_a2
          module procedure zero_deriv_a3
          module procedure zero_deriv_a4
        end interface

        interface sax
          module procedure sax_d0_a0_a0 
          module procedure sax_l0_a0_a0
          module procedure sax_i0_a0_a0
          module procedure sax_d0_a0_a1
          module procedure sax_d0_a0_a2
          module procedure sax_d0_a1_a1
          module procedure sax_d0_a2_a2
          module procedure sax_d0_a3_a3
          module procedure sax_d1_a0_a1
          module procedure sax_d1_a1_a1 
          module procedure sax_l1_a1_a1 
          module procedure sax_i1_a1_a1
          module procedure sax_d2_a0_a2
          module procedure sax_d2_a2_a2
# ifndef DEFAULT_R8
          module procedure sax_r0_a0_a0
# endif
        end interface

#endif
        interface oad_convert
          module procedure convert_d0_a0
          module procedure convert_d1_a1
          module procedure convert_d2_a2
          module procedure convert_d3_a3
          module procedure convert_d4_a4
          module procedure convert_d5_a5
          module procedure convert_d6_a6
          module procedure convert_d7_a7
          module procedure convert_a0_d0
          module procedure convert_a1_d1
          module procedure convert_a2_d2
          module procedure convert_a3_d3
          module procedure convert_a4_d4
          module procedure convert_a5_d5
          module procedure convert_a6_d6
          module procedure convert_a7_d7
#ifndef DEFAULT_R8
          module procedure convert_r0_a0
          module procedure convert_r1_a1
          module procedure convert_r2_a2
          module procedure convert_r3_a3
          module procedure convert_r4_a4
          module procedure convert_r5_a5
          module procedure convert_r6_a6
          module procedure convert_r7_a7
          module procedure convert_a0_r0
          module procedure convert_a1_r1
          module procedure convert_a2_r2
          module procedure convert_a3_r3
          module procedure convert_a4_r4
          module procedure convert_a5_r5
          module procedure convert_a6_r6
          module procedure convert_a7_r7
#endif
        end interface

        interface oad_allocateMatching
          module procedure allocateMatching_d1_d1
          module procedure allocateMatching_a1_d1
          module procedure allocateMatching_d1_a1
          module procedure allocateMatching_a1_a1
          module procedure allocateMatching_d2_d2
          module procedure allocateMatching_a2_d2
          module procedure allocateMatching_d2_a2
          module procedure allocateMatching_a2_a2
          module procedure allocateMatching_d3_d3
          module procedure allocateMatching_a3_d3
          module procedure allocateMatching_d3_a3
          module procedure allocateMatching_a3_a3
          module procedure allocateMatching_a4_a4
          module procedure allocateMatching_d4_a4
          module procedure allocateMatching_d5_d5
          module procedure allocateMatching_a5_d5
          module procedure allocateMatching_d5_a5
          module procedure allocateMatching_a5_a5
          module procedure allocateMatching_d6_d6
          module procedure allocateMatching_a6_d6
          module procedure allocateMatching_d6_a6
          module procedure allocateMatching_a6_a6
#ifndef DEFAULT_R8
          module procedure allocateMatching_r1_r1
          module procedure allocateMatching_d1_r1
          module procedure allocateMatching_r1_d1
          module procedure allocateMatching_a1_r1
          module procedure allocateMatching_r1_a1
          module procedure allocateMatching_a2_r2
          module procedure allocateMatching_r2_a2
#endif
        end interface 

        interface oad_allocateShape
          module procedure allocateShape_d1
          module procedure allocateShape_d2
        end interface 

        interface oad_shapeTest
          module procedure shapeTest_a1_d1
          module procedure shapeTest_a1_a1
          module procedure shapeTest_d1_a1
          module procedure shapeTest_a2_d2
          module procedure shapeTest_a2_a2
          module procedure shapeTest_d2_a2
          module procedure shapeTest_a3_a3
          module procedure shapeTest_d3_a3
          module procedure shapeTest_a4_a4
          module procedure shapeTest_d4_a4
          module procedure shapeTest_a5_a5
          module procedure shapeTest_d5_a5
          module procedure shapeTest_a5_d5
          module procedure shapeTest_a6_a6
          module procedure shapeTest_d6_a6
          module procedure shapeTest_a6_d6
#ifndef DEFAULT_R8
          module procedure shapeTest_r1_a1
          module procedure shapeTest_r2_a2
          module procedure shapeTest_a1_r1
          module procedure shapeTest_a2_r2
#endif
        end interface 

        interface runTimeErrorStop
          module procedure runTimeErrorStopI
        end interface 

        contains
#ifndef TRACE
        !
        ! chain rule saxpy to be used in forward and reverse modes
        !
        subroutine saxpy_d0_a0_a0(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_l0_a0_a0(a,x,y)
          integer(kind=w2f__i8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_i0_a0_a0(a,x,y)
          integer(kind=w2f__i4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d0_a0_a1(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d0_a1_a1(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d0_a2_a2(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), dimension(:,:), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d1_a0_a1(a,x,y)
          real(w2f__8), dimension(:), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d1_a1_a1(a,x,y)
          real(w2f__8), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_l1_a1_a1(a,x,y)
          integer(kind=w2f__i8), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_i1_a1_a1(a,x,y)
          integer(kind=w2f__i4), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_a1_a1_a1(a,x,y)
          type(active), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a%v
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d2_a0_a2(a,x,y)
          real(w2f__8), dimension(:,:), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_d2_a2_a2(a,x,y)
          real(w2f__8), dimension(:,:), intent(in) :: a
          type(active), dimension(:,:), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
# ifndef DEFAULT_R8
        subroutine saxpy_r0_a0_a0(a,x,y)
          real(w2f__4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_r0_a1_a1(a,x,y)
          real(w2f__4), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_r1_a0_a1(a,x,y)
          real(w2f__4), dimension(:), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine saxpy_r1_a1_a1(a,x,y)
          real(w2f__4), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM+x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
# endif
        !
        ! chain rule saxpy to be used in forward and reverse modes
        ! derivative component of y is equal to zero initially
        ! note: y needs to be inout as otherwise value component gets
        ! zeroed out
        !
        subroutine sax_d0_a0_a0(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_l0_a0_a0(a,x,y)
          integer(kind=w2f__i8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_i0_a0_a0(a,x,y)
          integer(kind=w2f__i4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d0_a0_a1(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d0_a0_a2(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d0_a1_a1(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d0_a2_a2(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), dimension(:,:), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d0_a3_a3(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), dimension(:,:,:), intent(in) :: x
          type(active), dimension(:,:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d1_a0_a1(a,x,y)
          real(w2f__8), dimension(:), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d1_a1_a1(a,x,y)
          real(w2f__8), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_l1_a1_a1(a,x,y)
          integer(kind=w2f__i8), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_i1_a1_a1(a,x,y)
          integer(kind=w2f__i4), dimension(:), intent(in) :: a
          type(active), dimension(:), intent(in) :: x
          type(active), dimension(:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d2_a0_a2(a,x,y)
          real(w2f__8), dimension(:,:), intent(in) :: a
          type(active), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
        subroutine sax_d2_a2_a2(a,x,y)
          real(w2f__8), dimension(:,:), intent(in) :: a
          type(active), dimension(:,:), intent(in) :: x
          type(active), dimension(:,:), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
# ifndef DEFAULT_R8
        subroutine sax_r0_a0_a0(a,x,y)
          real(w2f__4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=x%DELEM*a
          VECTOR_LOOP_END
        end subroutine
# endif
        !
        ! set derivative of y to be equal to derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        subroutine setderiv_a0_a0(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        subroutine setderiv_a1_a0(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        subroutine setderiv_a1_a1(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        subroutine setderiv_a2_a0(y,x)
          type(active), intent(inout), dimension(:,:) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        subroutine setderiv_a2_a2(y,x)
          type(active), intent(inout), dimension(:,:) :: y
          type(active), intent(in), dimension(:,:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        subroutine setderiv_a3_a3(y,x)
          type(active), intent(inout), dimension(:,:,:) :: y
          type(active), intent(in), dimension(:,:,:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = x%DELEM
          VECTOR_LOOP_END
        end subroutine
        !
        ! set the derivative of y to be the negated derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine set_neg_deriv_a0_a0(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = -x%DELEM
          VECTOR_LOOP_END
        end subroutine

        subroutine set_neg_deriv_a1_a1(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM = -x%DELEM
          VECTOR_LOOP_END
        end subroutine

        !
        ! increment the derivative of y by the derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine inc_deriv_a0_a0(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM
          VECTOR_LOOP_END
        end subroutine

        subroutine inc_deriv_a1_a1(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM
          VECTOR_LOOP_END
        end subroutine

        subroutine inc_deriv_a2_a2(y,x)
          type(active), intent(inout), dimension(:,:) :: y
          type(active), intent(in), dimension(:,:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM + x%DELEM
          VECTOR_LOOP_END
        end subroutine

        !
        ! decrement the derivative of y by the derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine dec_deriv_a0_a0(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM - x%DELEM
          VECTOR_LOOP_END
        end subroutine

        subroutine dec_deriv_a1_a1(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM - x%DELEM
          VECTOR_LOOP_END
        end subroutine
        
        subroutine dec_deriv_a2_a2(y,x)
          type(active), intent(inout), dimension(:,:) :: y
          type(active), intent(in), dimension(:,:) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
            y%DELEM=y%DELEM - x%DELEM
          VECTOR_LOOP_END
        end subroutine
        
        !
        ! set derivative components to 0.0
        !
        subroutine zero_deriv_a0(x)
          type(active), intent(inout) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
             x%DELEM=0.0d0
          VECTOR_LOOP_END 
        end subroutine

        subroutine zero_deriv_a1(x)
          type(active), dimension(:), intent(inout) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
             x%DELEM=0.0d0
          VECTOR_LOOP_END 
        end subroutine

        subroutine zero_deriv_a2(x)
          type(active), dimension(:,:), intent(inout) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
             x%DELEM=0.0d0
          VECTOR_LOOP_END 
        end subroutine

        subroutine zero_deriv_a3(x)
          type(active), dimension(:,:,:), intent(inout) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
             x%DELEM=0.0d0
          VECTOR_LOOP_END 
        end subroutine

        subroutine zero_deriv_a4(x)
          type(active), dimension(:,:,:,:), intent(inout) :: x
          VECTOR_LOOP_VAR
          VECTOR_LOOP_BEGIN
             x%DELEM=0.0d0
          VECTOR_LOOP_END 
        end subroutine

#endif
        !
        ! conversions
        !
        subroutine convert_d0_a0(convertTo, convertFrom)
          real(w2f__8), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d1_a1(convertTo, convertFrom)
          real(w2f__8), dimension(:), intent(out) :: convertTo
          type(active), dimension(:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d2_a2(convertTo, convertFrom)
          real(w2f__8), dimension(:,:), intent(out) :: convertTo
          type(active), dimension(:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d3_a3(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d4_a4(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d5_a5(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d6_a6(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d7_a7(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a0_d0(convertTo, convertFrom)
          type(active), intent(inout) :: convertTo
          real(w2f__8), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a1_d1(convertTo, convertFrom)
          type(active), dimension(:), intent(inout) :: convertTo
          real(w2f__8), dimension(:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a2_d2(convertTo, convertFrom)
          type(active), dimension(:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a3_d3(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a4_d4(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a5_d5(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a6_d6(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a7_d7(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
#ifndef DEFAULT_R8
        subroutine convert_r0_a0(convertTo, convertFrom)
          real(w2f__4), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r1_a1(convertTo, convertFrom)
          real(w2f__4), dimension(:), intent(out) :: convertTo
          type(active), dimension(:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r2_a2(convertTo, convertFrom)
          real(w2f__4), dimension(:,:), intent(out) :: convertTo
          type(active), dimension(:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r3_a3(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r4_a4(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r5_a5(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r6_a6(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r7_a7(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a0_r0(convertTo, convertFrom)
          type(active), intent(inout) :: convertTo
          real(w2f__4), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a1_r1(convertTo, convertFrom)
          type(active), dimension(:), intent(inout) :: convertTo
          real(w2f__4), dimension(:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a2_r2(convertTo, convertFrom)
          type(active), dimension(:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a3_r3(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a4_r4(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a5_r5(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a6_r6(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a7_r7(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
#endif
        !
        ! allocations
        !
        subroutine allocateMatching_d1_d1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_d1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d1_a1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_a1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d2_d2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_a2_d2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_d2_a2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_a2_a2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_d3_d3(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a3_d3(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_d3_a3(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a3_a3(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a4_a4(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4)))
        end subroutine
        subroutine allocateMatching_d4_a4(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4)))
        end subroutine
        subroutine allocateMatching_d5_d5(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_a5_d5(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_d5_a5(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_a5_a5(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_d6_d6(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_a6_d6(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_d6_a6(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_a6_a6(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
#ifndef DEFAULT_R8
        subroutine allocateMatching_r1_r1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d1_r1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_r1_d1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_r1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_r1_a1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a2_r2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_r2_a2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
#endif
        !
        ! allocate shape
        !
        subroutine allocateShape_d1(toBeAllocated,s1)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          integer(w2f__i8) :: s1
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(s1))
        end subroutine
        subroutine allocateShape_d2(toBeAllocated,s1,s2)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          integer(w2f__i8) :: s1,s2
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(s1,s2))
        end subroutine
        !
        ! shape tests
        !
        subroutine shapeTest_a1_d1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          real(w2f__8), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a1_a1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d1_a1(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_d2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_a2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d2_a2(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a3_a3(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d3_a3(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a4_a4(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d4_a4(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a5_a5(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d5_a5(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a5_d5(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a6_a6(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d6_a6(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a6_d6(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
#ifndef DEFAULT_R8
        subroutine shapeTest_r1_a1(allocatedVar,origVar)
          implicit none
          real(w2f__4), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_r2_a2(allocatedVar,origVar)
          implicit none
          real(w2f__4), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a1_r1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          real(w2f__4), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_r2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          real(w2f__4), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
#endif
        subroutine runTimeErrorStopI(mesgId)
          implicit none
          integer mesgId
          select case (mesgId) 
          case (shapeChange)
             stop "ERROR: OAD run time library: detected shape change"
             end select 
        end subroutine

        end module
