! *
! **********************************************************************************
! *                                                                                *
! * INTEL CORPORATION                                                              *
! * Copyright 2006-2007 Intel Corporation All Rights Reserved.                     *
! *                                                                                *
! * The source code contained or described herein and all documents related to     *
! * the source code ("Material") are owned by Intel Corporation or its suppliers   *
! * or licensors. Title to the Material remains with Intel Corporation or its      *
! * suppliers and licensors. The Material contains trade secrets and proprietary   *
! * and confidential information of Intel or its suppliers and licensors. The      *
! * Material is protected by worldwide copyright and trade secret laws and         *
! * treaty provisions. No part of the Material may be used, copied, reproduced,    *
! * modified, published, uploaded, posted, transmitted, distributed, or            *
! * disclosed in any way without Intel's prior express written permission.         *
! *                                                                                *
! * No license under any patent, copyright, trade secret or other intellectual     *
! * property right is granted to or conferred upon you by disclosure or delivery   *
! * of the Materials, either expressly, by implication, inducement, estoppel or    *
! * otherwise. Any license under such intellectual property rights must be         *
! * express and approved by Intel in writing.                                      *
! *                                                                                *
! **********************************************************************************
! *

!
! **************************** Edit History *******************************
!
! Sep-07-2006  sl   Initial creation based on the Fortran 2003 standard
!                   (J3/04-007) 15.1
! Feb-02-2007  sl   Fix C_INT_FAST16_T, C_INT_FAST32_T, and C_LONG_DOUBLE for MAC 
! Feb-06-2007  sl   tr74712. Use attribute directive C_PTR for C_PTR and C_FUNPTR
! Oct-22-2007  sl   tr80874. Add BIND(C) for C_PTR and C_FUNPTR
!
! Notes:
!
! 1) This module is created with explicit integer and real types and is
!    independent of default integer and default real.
!
! 2) Preset Defines that are used in this module:
!      Windows: _WIN32
!      Linux: __x86_64__  is defined for Intel(R) 64 architecture
!             __ia64__ is defined for Intel(R) IA-64 architecture
!
! 3) On Windows, the type "_Bool", and the complex types "float _Complex",
!    "double _Complex", and "long double _Complex" are only supported in C
!    with the option "/Qc99".  We assume that this option is specified for C.
!    (See C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX, C_BOOL)


MODULE ISO_C_BINDING
    IMPLICIT NONE

    ! Pointer length is 4/8 for 32/64 bit architecture
    INTEGER, PARAMETER :: POINTER_LEN = 4

! -------------------------------------------------------------------------
! integer types
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_INT = 4
    INTEGER (KIND=4), PARAMETER :: C_SHORT = 2

    INTEGER (KIND=4), PARAMETER :: C_LONG = 4


    INTEGER (KIND=4), PARAMETER :: C_LONG_LONG = 8
    INTEGER (KIND=4), PARAMETER :: C_SIGNED_CHAR = 1
! On linux the C type size_t is defined in stddef.h
    INTEGER (KIND=4), PARAMETER :: C_SIZE_T = POINTER_LEN

! On linux the following C types (int8_t .. intmax_t) are defined in stdint.h
! On Windows there is no stdint.h and these C types are not supported
!
    INTEGER (KIND=4), PARAMETER :: C_INT8_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT16_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT32_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT64_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST8_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST16_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST32_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST64_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST8_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST16_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST32_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST64_T = -2
    INTEGER (KIND=4), PARAMETER :: C_INTMAX_T = -2

    ! On linux the C type intptr_t is defined in stdint.h
    ! On Windows intptr_t is defined in stddef.h
    INTEGER (KIND=4), PARAMETER :: C_INTPTR_T = POINTER_LEN

! -------------------------------------------------------------------------
! real types
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_FLOAT = 4
    INTEGER (KIND=4), PARAMETER :: C_DOUBLE = 8

      INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE = -1




! -------------------------------------------------------------------------
! complex types 
! -------------------------------------------------------------------------
    ! On Windows, these complex types are only supported in C with /Qc99
    INTEGER (KIND=4), PARAMETER :: C_FLOAT_COMPLEX = C_FLOAT
    INTEGER (KIND=4), PARAMETER :: C_DOUBLE_COMPLEX = C_DOUBLE
    INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE_COMPLEX = C_LONG_DOUBLE

! -------------------------------------------------------------------------
! logical type
! -------------------------------------------------------------------------
    ! On Windows, _Bool is only supported in C with /Qc99
    INTEGER (KIND=4), PARAMETER :: C_BOOL = 1

! -------------------------------------------------------------------------
! character type
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_CHAR = 1
      
! -------------------------------------------------------------------------
! special characters
! -------------------------------------------------------------------------
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_NULL_CHAR = ACHAR(0)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_ALERT = ACHAR(7)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_BACKSPACE = ACHAR(8)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_FORM_FEED = ACHAR(12)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_NEW_LINE = ACHAR(10)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_CARRIAGE_RETURN = ACHAR(13)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_HORIZONTAL_TAB = ACHAR(9)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_VERTICAL_TAB = ACHAR(11)
 
! -------------------------------------------------------------------------
! C_PTR, C_FUNPTR, C_NULL_PTR, C_NULL_FUNPTR
! -------------------------------------------------------------------------
    TYPE :: C_PTR
        PRIVATE
        INTEGER(C_INTPTR_T) :: ptr
    END TYPE C_PTR

    TYPE :: C_FUNPTR
        PRIVATE
        INTEGER(C_INTPTR_T) :: ptr
    END TYPE C_FUNPTR


    TYPE(C_PTR), PARAMETER :: C_NULL_PTR = C_PTR(0_4)
    TYPE(C_FUNPTR), PARAMETER :: C_NULL_FUNPTR = C_FUNPTR(0_4)

! -------------------------------------------------------------------------
! private types for fortran descriptors
! -------------------------------------------------------------------------
    INTEGER(4), PARAMETER, PRIVATE :: for_desc_max_rank = 7    
    INTEGER(C_INTPTR_T), PARAMETER, PRIVATE :: for_desc_array_defined= 1
    INTEGER(C_INTPTR_T), PARAMETER, PRIVATE :: for_desc_array_nodealloc = 2
    INTEGER(C_INTPTR_T), PARAMETER, PRIVATE :: for_desc_array_contiguous = 4
    INTEGER(C_INTPTR_T), PARAMETER, PRIVATE :: &
            for_desc_flags = for_desc_array_defined + &
                             for_desc_array_nodealloc + &
                             for_desc_array_contiguous

    TYPE, PRIVATE :: for_desc_triplet
        INTEGER(C_INTPTR_T) :: extent
        INTEGER(C_INTPTR_T) :: mult  ! multiplier for this dimension
        INTEGER(C_INTPTR_T) :: lowerbound
    END TYPE for_desc_triplet

    TYPE, PRIVATE :: for_array_descriptor
        INTEGER(C_INTPTR_T) :: base
        INTEGER(C_INTPTR_T) :: len  ! len of data type
        INTEGER(C_INTPTR_T) :: offset
        INTEGER(C_INTPTR_T) :: flags
        INTEGER(C_INTPTR_T) :: rank
        INTEGER(C_INTPTR_T) :: reserved1
        TYPE(for_desc_triplet) :: diminfo(for_desc_max_rank)
    END TYPE for_array_descriptor

! -------------------------------------------------------------------------
! procedure interfaces
! -------------------------------------------------------------------------

    ! ---------------------------------------------------------------------
    ! Generic interface for indicating association status of pointers
    ! ---------------------------------------------------------------------
    INTERFACE c_associated
        MODULE PROCEDURE c_associated_ptr, c_associated_funptr
    END INTERFACE

    ! ---------------------------------------------------------------------
    ! Generic interface for associating a data pointer with the target of
    ! a C pointer and specifying its shape when the pointer is an array.
    ! The specific procedure c_f_pointer_scalar is for scalar pointer argument.
    ! The four specific procedures c_f_pointer_array1, etc. are for array
    ! pointer argument and only differ by the type of the shape argument.
    !
    ! The specific procedures c_f_pointer_array1, etc. are implemented by
    ! differently named module procedures c_f_pointer_private1, etc.
    ! The names need to be different since their argument declarations do not
    ! match (c_f_pointer_private1, etc. declare the pointer argument directly
    ! as array descriptors).  The ALIAS attribute is used to associate the
    ! specific procedures with their implementation procedures via internal
    ! names.
    !
    ! In the specific procedures c_f_pointer_array1, etc. the NO_ARG_CHECK
    ! attribute is used to allow various types and shapes for the Fortran
    ! pointer.  The NULLIFY attribute is used to nullify the pointer argument
    ! before the call and to ensure that the rank and length fields of the
    ! descriptor are properly initialized.
    ! ---------------------------------------------------------------------
    INTERFACE c_f_pointer
        MODULE PROCEDURE c_f_pointer_scalar

        SUBROUTINE c_f_pointer_array1 (cptr, fptr, shape)


            IMPLICIT NONE
    TYPE :: C_PTR
        INTEGER(4) :: ptr
    END TYPE C_PTR
            TYPE(c_ptr), INTENT(IN) :: cptr

            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(1), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array1

        SUBROUTINE c_f_pointer_array2 (cptr, fptr, shape)


            IMPLICIT NONE
    TYPE :: C_PTR
        INTEGER(4) :: ptr
    END TYPE C_PTR
            TYPE(c_ptr), INTENT(IN) :: cptr

            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(2), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array2

        SUBROUTINE c_f_pointer_array4 (cptr, fptr, shape)


            IMPLICIT NONE
    TYPE :: C_PTR
        INTEGER(4) :: ptr
    END TYPE C_PTR
            TYPE(c_ptr), INTENT(IN) :: cptr

            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(4), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array4

        SUBROUTINE c_f_pointer_array8 (cptr, fptr, shape)


            IMPLICIT NONE
    TYPE :: C_PTR
        INTEGER(4) :: ptr
    END TYPE C_PTR
            TYPE(c_ptr), INTENT(IN) :: cptr

            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(8), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array8

    END INTERFACE

    PRIVATE :: c_f_pointer_private1
    PRIVATE :: c_f_pointer_private2
    PRIVATE :: c_f_pointer_private4
    PRIVATE :: c_f_pointer_private8

! -------------------------------------------------------------------------
! procedures
! -------------------------------------------------------------------------
CONTAINS

    ! ---------------------------------------------------------------------
    ! Indicates the association status of c_ptr_1 or indicates whether
    ! c_ptr_1 and c_ptr_2 are associated with the same entity.
    ! Both c_ptr_1 and c_ptr_2 are of type C_PTR
    ! ---------------------------------------------------------------------
    FUNCTION c_associated_ptr (c_ptr_1, c_ptr_2)
        LOGICAL(4) :: c_associated_ptr
        TYPE(c_ptr) :: c_ptr_1
        TYPE(c_ptr), OPTIONAL :: c_ptr_2

        IF (.NOT. PRESENT(c_ptr_2)) THEN
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_ptr = .FALSE.
            ELSE
                c_associated_ptr = .TRUE.
            END IF

        ELSE  ! c_ptr_2 is present
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_ptr = .FALSE.
            ELSE IF (c_ptr_1%ptr == c_ptr_2%ptr) THEN
                c_associated_ptr = .TRUE.
            ELSE 
                c_associated_ptr = .FALSE.
            END IF
        END IF
    END FUNCTION c_associated_ptr

    ! ---------------------------------------------------------------------
    ! Indicates the association status of c_ptr_1 or indicates whether
    ! c_ptr_1 and c_ptr_2 are associated with the same entity.
    ! Both c_ptr_1 and c_ptr_2 are of type C_FUNPTR
    ! ---------------------------------------------------------------------
    FUNCTION c_associated_funptr (c_ptr_1, c_ptr_2)
        LOGICAL(4) :: c_associated_funptr
        TYPE(c_funptr) :: c_ptr_1
        TYPE(c_funptr), OPTIONAL :: c_ptr_2

        IF (.NOT. PRESENT(c_ptr_2)) THEN
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_funptr = .FALSE.
            ELSE
                c_associated_funptr = .TRUE.
            END IF

        ELSE  ! c_ptr_2 is present
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_funptr = .FALSE.
            ELSE IF (c_ptr_1%ptr == c_ptr_2%ptr) THEN
                c_associated_funptr = .TRUE.
            ELSE 
                c_associated_funptr = .FALSE.
            END IF
        END IF
    END FUNCTION c_associated_funptr

    ! ---------------------------------------------------------------------
    ! Associates a scalar pointer with the target of a C pointer.
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_scalar (cptr, fptr)

        INTEGER, POINTER , INTENT(IN):: cptr  ! use pointer of any type here
        INTEGER, POINTER , INTENT(OUT):: fptr  ! use pointer of any type here

        fptr => cptr
    END SUBROUTINE c_f_pointer_scalar

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(1).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private1 (caddr, fdesc, shape)

        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(1), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo

    END SUBROUTINE c_f_pointer_private1

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(2).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private2 (caddr, fdesc, shape)

        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(2), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private2

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(4).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private4 (caddr, fdesc, shape)

        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(4), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private4

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(8).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private8 (caddr, fdesc, shape)

        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(8), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private8

    ! ---------------------------------------------------------------------
    ! Associate a procedure pointer with the target of a C function pointer.
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_procpointer (cptr, fptr)

        INTEGER, POINTER , INTENT(IN):: cptr  ! use pointer of any type here
        !PROCEDURE(), POINTER , INTENT(OUT):: fptr   ! not yet supported
        INTEGER, POINTER , INTENT(OUT):: fptr  ! use pointer of any type here

        fptr => cptr
    END SUBROUTINE c_f_procpointer

    ! ---------------------------------------------------------------------
    ! Returns the C address of the procedure argument.
    ! ---------------------------------------------------------------------
    FUNCTION c_funloc (x)
        TYPE(c_funptr) :: c_funloc

        INTEGER :: x   ! use any type here, since the procedure pointer is
                       ! not yet supported
!        INTRINSIC :: LOC

        ! Since x has the NO_ARG_CHECK attribute, the actual address (not
        ! any descriptor address) is passed in.
        c_funloc%ptr = LOC(x)
    END FUNCTION c_funloc

    ! ---------------------------------------------------------------------
    ! Returns the C address of the argument.
    ! ---------------------------------------------------------------------
    FUNCTION c_loc (x)
        TYPE(c_ptr) :: c_loc

        INTEGER :: x   ! use any type here
!        INTRINSIC :: LOC

        ! Since x has the NO_ARG_CHECK attribute, the actual address (not
        ! any descriptor address) is passed in.
        c_loc%ptr = LOC(x)
    END FUNCTION c_loc
    

END MODULE ISO_C_BINDING
