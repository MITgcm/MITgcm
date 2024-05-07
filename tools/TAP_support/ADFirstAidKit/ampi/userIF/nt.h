/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_NT_H_
#define _AMPI_NT_H_

/**
 * \file 
 * \ingroup UserInterfaceHeaders
 * NT= "no tansformation" or "no trace" prototypes for wrapper routines that could have an adjoint action but in particular contexts do not because they are called outside of the transformed or traced section of the source code;  the signatures are identical to their MPI originals and they pass the parameters through to the MPI routines
 */ 

/** \ingroup UserInterfaceDeclarations
 * @{
 */


/**
 * this wrapper variant of \ref AMPI_Init has no adjoint transformation / trace functionality; to be used outside of the transformed/traced code section
 */
int AMPI_Init_NT(int* argc, 
		 char*** argv);

/**
 * this wrapper variant of \ref AMPI_Finalize has no adjoint transformation / trace functionality; to be used outside of the transformed/traced code section
 */
int AMPI_Finalize_NT(void);

/**
 * this wrapper variant of \ref AMPI_Buffer_attach has no adjoint transformation / trace functionality; to be used outside of the transformed/traced code section
 */
int AMPI_Buffer_attach_NT(void *buffer, 
			  int size); 

/**
 * this wrapper variant of \ref AMPI_Buffer_detach has no adjoint transformation / trace functionality; to be used outside of the transformed/traced code section
 */
int AMPI_Buffer_detach_NT(void *buffer, 
			  int *size);

int AMPI_Type_contiguous_NT(int count,
			    MPI_Datatype oldtype,
			    MPI_Datatype* newtype);

int AMPI_Type_create_struct_NT(int count,
			       int array_of_blocklengths[],
			       MPI_Aint array_of_displacements[],
			       MPI_Datatype array_of_types[],
			       MPI_Datatype *newtype);

int AMPI_Type_create_resized_NT(MPI_Datatype oldtype,
				MPI_Aint lb,
				MPI_Aint extent,
				MPI_Datatype *newtype);

int AMPI_Type_commit_NT(MPI_Datatype *datatype);

int AMPI_Op_create_NT(MPI_User_function *function,
		      int commute,
		      MPI_Op *op);

int AMPI_Type_free_NT(MPI_Datatype *datatype);

int AMPI_Op_free_NT(MPI_Op *op);

int AMPI_Comm_dup_NT(MPI_Comm comm, MPI_Comm *dupComm) ;

/**
 * this wrapper variant of \ref AMPI_Comm_dup manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade); to be used outside of the transformed/traced code section
 */
int TLS_AMPI_Comm_dup_NT(MPI_Comm comm, MPI_Comm *dupComm) ;

int AMPI_Comm_split_NT(MPI_Comm comm, int color, int key, MPI_Comm *dupComm) ;

/**
 * this wrapper variant of \ref AMPI_Comm_split manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade); to be used outside of the transformed/traced code section
 */
int TLS_AMPI_Comm_split_NT(MPI_Comm comm, int color, int key, MPI_Comm *dupComm) ;

int AMPI_Comm_create_NT(MPI_Comm comm, MPI_Group group, MPI_Comm *dupComm) ;

/**
 * this wrapper variant of \ref AMPI_Comm_create manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade); to be used outside of the transformed/traced code section
 */
int TLS_AMPI_Comm_create_NT(MPI_Comm comm, MPI_Group group, MPI_Comm *dupComm) ;

int AMPI_Comm_free_NT(MPI_Comm *comm) ;

/**
 * this wrapper variant of \ref AMPI_Comm_free manages the shadow communicators if code is differentiated in tangent mode with ST-AD with shadow variables (e.g. Tapenade); to be used outside of the transformed/traced code section
 */
int TLS_AMPI_Comm_free_NT(MPI_Comm *comm) ;

#ifdef AMPI_FORTRANCOMPATIBLE
/** Must be defined in the fortranSupport.F of the particular AD tool */
void adtool_ampi_fortransetuptypes_(MPI_Fint* adouble, MPI_Fint* areal);
#endif

/** @} */

#endif
