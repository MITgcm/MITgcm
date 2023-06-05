/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_AMPI_H_
#define _AMPI_AMPI_H_

/**
 * \file
 * \ingroup UserInterfaceHeaders
 * One-stop header file for all AD-tool-independent AMPI routines; this is the file to replace mpi.h in the user code.
 */

/**
 * \defgroup UserInterfaceHeaders User-Interface header files
 * This set contains all the header files with declarations relevant to the user; header files not listed in this group
 * are internal to AdjoinableMPI or relate to support to be provided by a given AD tool.
 */

/**
 * \defgroup UserInterfaceDeclarations User-Interface declarations
 * This set contains all declarations relevant to the user; anything in the source files not listed in this group
 * is internal to AdjoinableMPI or relates to support to be provided by a given AD tool.
 */

/** \mainpage 
 * The Adjoinable MPI (AMPI) library provides a modified set if MPI subroutines
 * that are constructed such that an adjoint in the context of algorithmic
 * differentiation (AD) can be computed. The library is designed to be supported
 * by a variety of AD tools and to enable also the computation of  (higher-order)
 * forward derivatives.
 * \authors <b>Laurent Hasco&euml;t</b> 
 * (currently at INRIA Sophia-Antipolis; <a href="http://fr.linkedin.com/pub/laurent-hascoët/86/821/a04">LinkedIn</a> - <a href="mailto:Laurent.Hascoet@sophia.inria.fr?subject=AMPI">e-mail</a>)
 * \authors <b>Michel Schanen</b> 
 * (currently at RWTH Aachen; <a href="http://www.stce.rwth-aachen.de/people/Michel.Schanen.html">home page</a> - <a href="mailto:schanen@stce.rwth-aachen.de?subject=AMPI">e-mail</a>)
 * \authors <b>Jean Utke</b> 
 * (until March 2014 at Argonne National Laboratory; <a href="http://www.linkedin.com/pub/jean-utke/5/645/7a">LinkedIn</a> - <a href="mailto:utkej1@gmail.com?subject=AMPI">e-mail</a>)
 *
 * Contributions informing the approach implemented in AMPI were made by the co-authors of&nbsp;\cite Utke2009TAM  <b>P. Heimbach, C. Hill, U. Naummann</b>. 
 * 
 * Significant contributions were made by <b>Anton Bovin</b> (summer student at Argonne National Laboratory in 2013;<a href="http://www.linkedin.com/pub/anton-bovin/86/b1b/847">LinkedIn</a>).
 *
 * <b>Please refer to the \ref UserGuide for information regarding the use of the library in a given application.</b>
 *
 * Information regarding the library design, library internal functionality and the interfaces of methods to
 * be supported by a given AD tool are given in \ref LibraryDevelopmentGuide
 *
 * \section links Links to Ressources
 * 
 *  - <a href="https://trac.mcs.anl.gov/projects/AdjoinableMPI/wiki">TRAC  page</a> for bug and feature tracking, links to presentations
 *  - <a href="http://mercurial.mcs.anl.gov/ad/AdjoinableMPI/">mercurial repository</a> for source code and change history
 *  - <a href="http://www.mcs.anl.gov/~utke/AdjoinableMPI/regression/tests.shtml">regression tests</a> 
 *
 */

/**
 * \page UserGuide User Guide
 * \tableofcontents
 * \section Introduction
 * 
 * The Adjoinable MPI (AMPI) library provides a modified set of MPI subroutines
 * that are constructed such that:
 *  - an adjoint in the context of algorithmic differentiation (AD) can be computed,
 *  - it can be supported by a variety of AD tools,
 *  - it enable also the computation of  (higher-order) forward derivatives,
 *  - it provides an implementation for a straight pass-through to MPI such that the switch to AMPI can be made permanent
 * without forcing compile dependencies on any AD tool.
 *
 * There are principal recipes for the construction of the adjoint of 
 * a given communication, see \cite Utke2009TAM . 
 * The practical implementation of these recipes, however, faces the following 
 * challenges.
 *  - the target language may prevent some implementation options
 *   - exposing an MPI_Request augmented with extra information as a structured type (not supported by Fortran 77)
 *   - passing an array of buffers (of different length), e.g. to \ref AMPI_Waitall, as an additional argument to  (not supported in any Fortran version)
 *  - the AD tool implementation could be based on 
 *   - operator overloading
 *    - original data and (forward) derivatives co-located (e.g. Rapsodia,dco)
 *    - original data and (forward) derivatives referenced (e.g. Adol-C)
 *   - source transformation
 *    - association by address (e.g. OpenAD)
 *    - association by name (e.g. Tapenade)
 * 
 * The above choices imply certain consequences on the complexity for implementing  
 * the adjoint (and forward derivative) action and this could imply differences in the AMPI design.
 * However, from a user's perspective it is a clear advantage to present a <b>single, AD tool implementation independent
 * AMPI library</b> such that switching AD tools is not hindered by AMPI while also promoting a common understanding of the
 * differentiation through MPI calls.
 * We assume the reader is familiar with MPI and AD concepts.
 *
 * \section sources Getting the library sources
 * 
 * The sources can be accessed through the <a href="http://mercurial.mcs.anl.gov/ad/AdjoinableMPI/">AdjoinableMPI mercurial repository</a>. Bug tracking, feature requests
 * etc. are done via <a href="http://trac.mcs.anl.gov/projects/AdjoinableMPI">trac</a>.
 * In the following we assume the sources are cloned (cf <a href="http://mercurial.selenic.com/">mercurial web site</a> for details about mercurial)
 * into a directory `AdjoinableMPI` by invoking
 * \code
 * hg clone http://mercurial.mcs.anl.gov/ad/AdjoinableMPI
 * \endcode
 * 
 * \section configure Library - Configure,  Build, and Install
 * 
 * Configuration, build, and install follows the typical GNU autotools chain. Go to the source directory
 * \code
 * cd AdjoinableMPI
 * \endcode
 * If the sources were obtained from the mercurial repository, then one first needs to run the autotools via invoking
 * \code
 * ./autogen.sh
 * \endcode
 * In the typical `autoconf` fashion invoke
 * \code
 *  configure --prefix=<installation directory> ...
 * \endcode
 * in or outside the source tree.
 * The AD tool supporting AMPI should provide information which detailed AMPI
 * configure settings are required if any.
 * Build the libaries with
 * \code
 * make
 * \endcode
 * Optionally, before installing, one can do a sanity check by running:  `make check` .
 *
 * To install the header files and compiled libraries follow with
 * \code
 *  make install
 * \endcode
 * after which in the installation directory one should find under <tt>\<installation directory\></tt> the following.
 *  - header files: see also  \ref dirStruct
 *  - libraries:
 *    - libampiPlainC - for pass through to MPI, no AD functionality
 *    - libampiCommon - implementation of AD functionality shared between all AD tools supporting AMPI
 *    - libampiBookkeeping - implementation of AD functionality needed by some AD tools (see the AD tool documentation)
 *    - libampiTape - implementation of AD functionality needed by some AD tools (see the AD tool documentation)
 *
 * Note, the following libraries are AMPI internal:
 *  - libampiADtoolStubsOO - stubs for operator overloading AD tools not needed by the user
 *  - libampiADtoolStubsST - stubs for source transformation AD tools not needed by the user
 *
 * \section mpiToAmpi Switching from MPI to Adjoinable MPI
 *
 * For a given MPI-parallelized source code the user will replace all calls to MPI_... routines with the respective  AMPI_...
 * equivalent provided in \ref UserInterfaceDeclarations.
 * To include the declarations replace
 *  - in C/C++: includes of <tt>mpi.h</tt> with
 *  \code
 *  #include <ampi/ampi.h>
 *  \endcode
 *  - in Fortran: includes of <tt>mpif.h</tt> with
 *  \code
 *  #include <ampi/ampif.h>
 *  \endcode
 *
 * respectively.
 *
 * Because in many cases certain MPI calls (e.g. for initialization and finalization) take place outside the scope of
 * the original computation and its AD-derivatives and therefore do not themselves become part of the AD process,
 * see the explanations in \ref differentiableSection.
 * Each routine in this documentation lists to the changes to the parameters
 * relative to the MPI standard. These changes impact parameters specifying
 *  - MPI_Datatype parameters, see \ref datatypes
 *  - MPI_Request parameters, see \ref requests
 *
 * Some routines require new parameters specifying the pairing two-sided communications, see \ref pairings.
 * Similarly to the various approaches (preprocessing, templating, using <tt>typedef</tt>)
 * employed to effect a change to an active type for overloading-based AD tools, this switch
 * from MPI to AMPI routines should be done as a one-time effort.
 * Because  AMPI provides an implementation for a straight pass-through to MPI it is possible to make this switch
 * permanent and retain builds that are completely independent of any AD tool and use AMPI as a thin wrapper library to AMPI.
 *
 * \section appCompile Application - compile and link
 *
 * After the switch described in \ref mpiToAmpi is done, the application should be recompiled with the include path addition
 * \code
 * -I<installation directory>/include
 * \endcode
 * and linked with the link path extension
 * \code 
 * -L<installation directory>/lib[64]
 * \endcode 
 * Note, the name of the subdirectory (lib or lib64 ) depends on the system;
 * the appropriate set of libraries, see \ref configure; the optional ones in square brackets depend on the AD tool:
 * \code
 * -libampicommon [ -libampiBookkeeping -lampiTape ]
 * \endcode 
 * <b>OR</b> if instead of differentiation by AD a straight pass-through to MPI is desired, then
 * \code
 * -libampiPlainC
 * \endcode
 * instead.
 * 
 * \section dirStruct Directory and File Structure
 * All locations discussed below are relative to the top level source directory. 
 * The top level header file to be included in place of the usual  "mpi.h" is located in  
 * ampi/ampi.h
 *
 * It references the header files in <tt>ampi/userIF</tt> , see also \ref UserInterfaceHeaders which are organized to contain
 *  - unmodified pass through to MPI in <tt>ampi/userIF/passThrough.h</tt> which exists to give the extent of the original MPI we cover  
 *  - variants of routines that in principle need adjoint logic but happen to be called outside of the code section that is adjoined and therefore 
 *    are not transformed / not traced (NT) in  <tt>ampi/userIF/nt.h</tt>
 *  - routines that are modified from the original MPI counterparts because their behavior in the reverse sweep differs from their behavior in the 
 *    forward sweep and they also may have a modified signatyre; in <tt>ampi/userIF/modified.h</tt>
 *  - routines that are specific for some variants of source transformation (ST) approaches in <tt>ampi/userIF/st.h</tt>; 
 *    while these impose a larger burden for moving from MPI to AMPI on the user, they also enable a wider variety of transformations 
 *    currently supported by the tools; we anticipate that the ST specific versions may become obsolete as the source transformation tools evolve to 
 *    support all transformations via the routines in <tt>ampi/userIF/modified.h</tt> 
 *
 * Additional header files contain enumerations used as arguments to AMPI routines. All declarations that are part of the user
 * interface are grouped in \ref UserInterfaceDeclarations. All other declarations in header files in the library are not to be used directly in the user code.
 * 
 * A library that simply passes through all AMPI calls to their MPI counterparts for a test compilation and execution without any involvement of 
 * and AD tool is implemented in the source files in the <tt>PlainC</tt> directory.
 * 
 * \section differentiableSection Using subroutine variants NT vs non-NT relative to the differentiable section
 * 
 * The typical assumption of a program to be differentiated is that there is some top level routine <tt>head</tt> which does the numerical computation 
 * and communication which is called from some main <tt>driver</tt> routine. The <tt>driver</tt> routine would have to be manually adjusted to initiate 
 * the derivative computation, retrieve, and use the derivative values.
 * Therefore only <tt>head</tt> and everything it references would be <em>adjoined</em> while <tt>driver</tt> would not. Typically, the <tt>driver</tt>
 * routine also includes the basic setup and teardown with MPI_Init and MPI_Finalize and consequently these calls (for consistency) should be replaced 
 * with their AMPI "no trace/transformation"  (NT) counterparts \ref AMPI_Init_NT and \ref AMPI_Finalize_NT. 
 * The same approach should be taken for all resource allocations/deallocations (e.g. \ref AMPI_Buffer_attach_NT and \ref AMPI_Buffer_detach_NT) 
 * that can exist in the scope enclosing the adjointed section alleviating 
 * the need for the AD tool implementation to tackle them. 
 * For cases where these routines have to be called within the adjointed code section the variants without the <tt>_NT</tt> suffix will ensure the
 * correct adjoint behavior.
 * 
 * \section general General Assumptions on types and Communication Patterns
 *
 * \subsection datatypes Datatype consistency
 * 
 * Because the MPI standard passes buffers as <tt>void*</tt>  (aka choice) the information about the type of
 * the buffer and in particular the distinction between active  and passive data (in the AD sense) must be
 * conveyed via the <tt>datatype</tt> parameters and be consistent with the type of the buffer. To indicate buffers of
 * active type the library predefines the following
 * - for C/C++
 *   - \ref AMPI_ADOUBLE  as the active variant of the passive MPI_DOUBLE
 *   - \ref AMPI_AFLOAT as the active variant of the passive MPI_FLOAT
 * - for Fortran
 *   - \ref AMPI_ADOUBLE_PRECISION as the active variant of the passive MPI_DOUBLE_PRECISION
 *   - \ref AMPI_AREAL as the active variant of the passive MPI_REAL
 *
 * Passive buffers can be used as parameters to the AMPI interfaces with respective passive data type values.
 *
 * \subsection requests Request Type
 *
 * Because additional information has to be attached to the MPI_Request instances  used in nonblocking communications, there
 * is an expanded data structure to hold this information. Even though in some contexts (F77) this structure cannot be exposed
 * to the user code the general approach is to declare variables that are to hold requests as \ref AMPI_Request (instead of
 * MPI_Request).
 *
 * \subsection pairings Pairings
 *
 * Following the explanations in \cite Utke2009TAM it is clear that context information about the 
 * communication pattern, that is the pairing of MPI calls, is needed to achieve 
 * -# correct adjoints, i.e. correct send and receive end points and deadlock free
 * -# if possible retain the efficiency advantages present in the original MPI communication for the adjoint.
 *
 * In AMPI pairings are conveyed via additional <tt>pairedWith</tt> parameters which may be set to \ref AMPI_PairedWith enumeration values , see e.g. \ref AMPI_Send or \ref AMPI_Recv.
 * The need to convey the pairing imposes restrictions because in a given code the pairing may not be static.
 * For a example a given <tt>MPI_Recv</tt> may be paired with 
 * \code{.cpp}
 * if (doBufferedSends)  
 *   MPI_Bsend(...); 
 * else  
 *   MPI_Ssend(...);
 * \endcode 
 *
 * but the AD tool has to decide on the send mode once the reverse sweep needs to adjoin the orginal <tt>MPI_Recv</tt>.  
 * Tracing such information in a global data structure is not scalable and piggybacking the send type onto the message 
 * so it can be traced on the receiving side is conceivable but not trivial and currently not implemented. 
 * 
 * \restriction Pairing of send and receive modes must be static.
 *
 * Note that this does not prevent the use of wild cards for source, or tag.
 *
 * \section examples Examples
 * A set of examples organized to illustrate the uses of AMPI together with setups for AD tools that also serve as
 * regression tests are collected in  `AdjoinableMPIexamples` that can be obtained similarly to the AMPI sources themselves
 * by cloning
 *\code
 * hg clone http://mercurial.mcs.anl.gov/ad/AdjoinableMPIexamples
 * \endcode
 * The daily regression tests based on these examples report the results on the page linked via the main page of this documentation.  
 *
 */

/**
 * \page LibraryDevelopmentGuide Library Development Guide
 * \tableofcontents
 * \section naming Naming Conventions - Code Organization
 * Directories and libraries are organized as follows:
 *  - user interface header files, see  \ref dirStruct; should not contain anything else (e.g. no internal helper functions)
 *  - `PlainC` :  pass through to MPI implementations of the user interface; no reference to ADTOOL interfaces; to be renamed
 *  - `Tape` : sequential access storage mechanism default implementation (implemented as doubly linked list) to enable forward/reverse
 *  reading; may not reference ADTOOL or AMPI symbols/types; may reference MPI
 *  - `Bookkeeping` : random access storage for AMPI_Requests (but possibly also other objects that could be opaque)
 *  - `Common` : the AD enabled workhorse; here we have all the common functionality for MPI differentiation;
 *
 * Symbol prefixes:
 *  - `AMPI_` to be used for anything in MPI replacing the `MPI_` prefix; not to be used for symbols outside of the user interface
 *  - `TAPE_AMPI_` to be used for the `Tape` sequential access storage mechanism declared in ampi/tape/support.h
 *  - `BK_AMPI_`:  `Bookkeeping`  random access storage mechanism declared in ampi/bookkeeping/support.h
 *  - `ADTOOL_AMPI_` to be
 *
 *
 *
 * \section nonblocking Nonblocking Communication and Fortran Compatibility
 * 
 * A central concern is the handling of non-blocking sends and receives in combination with their respective completion,
 * e.g. wait,  waitall, test. 
 * Taking as an example 
 * \code{.cpp}
 * MPI_Irecv(&b,...,&r);
 * // some other code in between 
 * MPI_Wait(&r,MPI_STATUS_IGNORE); 
 * \endcode
 * The adjoint action for <tt>MPI_Wait</tt> will have to be the <tt>MPI_Isend</tt> of the adjoint data associated with 
 * the data in buffer <tt>b</tt>. 
 * The original <tt>MPI_Wait</tt> does not have any of the parameters required for the send and in particular it does not 
 * have the buffer. The latter, however, is crucial in particular in a source transformation context because, absent a correct syntactic 
 * representation for the buffer at the <tt>MPI_Wait</tt> call site one has to map the address <tt>&b</tt> valid during the forward 
 * sweep to the address of the associated adjoint buffer during the reverse sweep. 
 * In some circumstances, e.g. when the buffer refers to stack variable and the reversal mode follows a strict <em>joint</em> scheme 
 * where one does not leave the stack frame of a given subroutine until the reverse sweep has completed, it is possible to predetermine 
 * the address of the respective adjoint buffer even in the source transformation context.  
 * In the general case, e.g. allowing for <em>split</em> mode reversal 
 * or  dynamic memory deallocation before the adjoint sweep commences such predetermination 
 * requires a more elaborate mapping algorithm. 
 * This mapping is subject of ongoing research and currently not supported. 
 * 
 * On the other hand, for operator overloading based tools, the mapping to a reverse sweep address space is an integral part of the 
 * tool because there the reverse sweep is executed as interpretation of  a trace of the execution that is entirely separate from the original program 
 * address space. Therefore all addresses have to be mapped to the new adjoint address space to begin with and no association to some 
 * adjoint program variable is needed. Instead, the buffer address can be conveyed via the request parameter (and AMPI-userIF bookkeeping) 
 * to the <tt>MPI_Wait</tt> call site, traced there and is then recoverable during the reverse sweep.  
 * Nevertheless, to allow a common interface this version of the AMPI library has the buffer as an additional argument to in the source-transformation-specific \ref AMPI_Wait_ST 
 * variant of \ref AMPI_Wait.  
 * In later editions, when source transformation tools can fully support the address mapping, the  of the AMPI library the \ref AMPI_Wait_ST variant  may be dropped.  
 * 
 * Similarly to conveying the buffer address via userIF bookkeeping associated with the request being passed, all other information such as source or destination, tag, 
 * data type, or the distinction if a request originated with a send or receive  will be part of the augmented information attached to the request and be subject to the trace and recovery as the buffer address itself. 
 * In the source transformation context, for cases in which parameter values such as source, destination, or tag are constants or loop indices the question could be asked if these values couldn't be easily recovered in
 * the generated adjoint code without having to store them. 
 * Such recovery following a TBR-like approach would, however, require exposing the augmented request instance as a structured data type to the TBR analysis in the languages other than Fortran77. 
 * This necessitates the introduction of the \ref AMPI_Request, which in Fotran77 still maps to just an integer address. 
 * The switching between these variants is done via  configure flags, see \ref configure.
 * 
 * \section bookkeeping Bookkeeping of Requests
 * 
 * As mentioned in \ref nonblocking the target language may prevent the augmented request from being used directly.  
 * In such cases the augmented information has to be kept internal to the library, that is we do some bookkeeping to convey the necessary information between the nonblocking sends or receives and
 * the and respective completion calls. Currently the bookkeeping has a very simple implementation as a doubly-linked list implying linear search costs which is acceptable only as long as the 
 * number of icomplete nonblocking operations per process remains moderate. 
 *
 * Whenever internal handles are used to keep trace (or correspondence) of a given internal object
 * between two distant locations in the source code (e.g. file identifier to keep trace of an opened/read/closed file,
 * or address to keep trace of a malloc/used/freed dynamic memory, or request ID to keep trace of a Isend/Wait...)
 * we may have to arrange the same correspondence during the backward sweep.
 * Keeping the internal identifier in the AD stack is not sufficient because there is no guarantee that
 * the mechanism in the backward sweep will use the same values for the internal handle.
 * The bookkeeping we use to solve this problem goes as follows:
 * - standard TBR mechanism makes sure that variables that are needed in the BW sweep and are overwritten
 *    are pushed onto the AD stack before they are overwritten
 * - At the end of its life in the forward sweep, the FW handle is pushed in the AD stack
 * - At the beginning of its backward life, we obtain a BW handle, we pop the FW handle,
 *    and we keep the pair of those in a table (if an adjoint handle is created too, we keep the triplet).
 * - When a variable is popped from the AD stack, and it is an internal handle,
 *    the popped handle is re-based using the said table.
 *
 * Simple workaround for the "request" case:
 * This method doesn't rely on TBR.
 * - Push the FW request upon acquisition (e.g. just after the Isend)
 * - Push the FW request upon release (e.g. just before the Wait)
 * - Pop the FW request upon adjoint of release, and get the BW request from the adjoint of release
 * - Add the BW request into the bookkeeping, with the FW request as a key.
 * - Upon adjoint of acquisition, pop the FW request, lookup in the bookkeeping to get the BW request.
 *
 * \section bundling  Tangent-linear mode bundling the derivatives or shadowing the communication
 * A central question for the implementation of tangent-linear mode becomes
 * whether to bundle the original buffer <tt>b</tt> with the  derivative <tt>b_d</tt> as  pair and communicate the pair
 * or to send separate messages for the derivatives.
 * - shadowing messages avoid the bundling/unbundling if <tt>b</tt> and <tt>b_d</tt>
 * are already given as separate entities as is the case in association by name, see \ref Introduction.
 * - for one-sided passive communications there is no hook to do the bundling/unbundling on the target side; therefore
 * it would be inherently impossible to achieve semantically correct behavior with any bundling/unbundling scheme.
 * The example here is a case where a put on the origin side and subsequent computations on the target side are synchronized
 * via a barrier which by itself does not have any obvious link to the target window by which one could trigger an unbundling.
 * - the bundling operation itself may incur nontrivial overhead for large buffers
 *
 * An earlier argument against message shadowing was the difficulty of correctly associating message pairs while using wildcards.
 * This association can, however, be ensured when a the shadowing message for the <tt>b_d</tt> is received on a communicator
 * <tt>comm_d</tt> that duplicates the original communicator <tt>comm</tt> and uses the
 * actual src and tag values obtained from the receive of the shadowed message as in the following example:
 *
 * \code{.cpp}
 * if ( myRank==1) {
 *   send(x,...,0,tag1,comm); // send of the original data
 *   send(x_d,...,0,tag1,comm_d); // shadowing send of the derivatives
 * else if ( myRank==2) {
 *   send(y,...,0,tag2,comm);
 *   send(y_d,...,0,tag2,comm_d);
 * else if ( myRank==0) {
 *   do {
 *      recv(t,...,ANY_SOURCE, ANY_TAG,comm,&status); // recv of the original data
 *      recv(t_d,...,status.SOURCE,status.TAG,comm_d,STATUS_IGNORE); // shadowing recv with wildcards disambiguated
 *      z+=t; // original operation
 *      z_d+=t_d; // corresponding derivative operation
 *   }
 * }
 * \endcode
 *
 * This same approach can be applied to (user-defined) reduction operations, see \ref reduction, in that the binomial
 * tree traversal for the reduction is shadowed in the same way and a user defined operation with derivatives can be invoked
 * by passing the derivatives as separate arguments.
 *
 * The above approach is to be taken by any tool in which <tt>b</tt> and <tt>b_d</tt> are not already paired in consecutive
 * memory such as association by name as in Tapenade or by implementation choice such as forward interpreters in Adol-C where
 * the 0-th order Taylor coefficients live in a separate array from the first-  and higher-order Taylor coefficients.
 * Tools with association by address (OpenAD, Rapsodia) would have the data already given in paired form and therefore not
 * need messsage shadowing but communicate the paired data.
 *
 * \section badOptions Rejected design options
 * About MPI_Types and the "active" boolean:
 * One cannot get away with just an "active" boolean to indicate the structure of
 * the MPI_Type of the bundle. Since the MPI_Type definition of the bundle type
 * has to be done anyway in the differentiated application code, and is passed
 * to the communication call, the AMPI communication implementation will
 * check this bundle MPI_Type to discover activity and trace/not trace accordingly.
 *
 * For the operator overloading, the tool needs to supply the active MPI types
 * for the built-in MPI_datatypes and using the active types, one can achieve
 * type conformance between the buffer and the type parameter passed.
 *
 * \section onesided One-Sided Active Targets
 * Idea - use an <tt>AMPI_Win</tt> instance (similar to the \ref AMPI_Request ) to attach more 
 * information about the things that are applied to the window and completed on the fence; 
 * we execute/trace/collect-for-later-execution operations on the window in the following fashion
 * 
 * forward: 
 * - MPI_Get record op/args on the window (buffer called 'x')  
 * - MPI_Put/MPI_Accumulate(z,,...): record op/args on the window; during forward: replace with MPI_Get of the remote target value into temporary 't' ; postpone to the  fence;
 * 
 * upon hitting a fence in the forward sweep:
 * 1. put all ops on the stack 
 * 2. run the fence 
 * 3. for earch accum/put:
 * 3.1:  push 't'
 * 3.2: do the postponed accumulate/put
 * 4. run a fence for 3.2
 * 5. for each accum*: 
 * 5.1 get accumlation result 'r'
 * 6. run a fence for 5.1 
 * 7. for each accum*:
 * 7.1 push 'r'
 * 
 * for the adjoint of a fence :
 * 0. for each operation on the window coming from the previous fence: 
 * 0.1 op isa GET then x_bar=0.0
 * 0.2 op isa PUT/accum= then x_bar+=t21
 * 0.3 op isa accum+ then x_bar+=t22
 * 1. run a fence
 * 2. pop  op from the stack and put onto adjoint window
 * 2.1 op isa PUT/accum=: then   GET('t21')
 * 2.2 op isa accum+; then get('t22') from adjoint target 
 * 2.3 op isa accum*, then pop('r'),  GET('t23') from adjoint target
 * 3. run a fence
 * 4. for each op on the adjoint window
 * 4.1 op isa GET, then accum+ into remote
 * 4.2 op isa PUT/accum: pop(t); accu(t,'=') to the value in the target
 * 4.3 op isa PUT/accum=; then acc(0.0,'=') to adjoint target
 * 4.4 op isa accum*: then accumulate( r*t23/t,'=', to the target) AND do z_bar+=r*t23/z  (this is the old local z ); 
 * 
 * \section derived Handling of Derived Types
 * (Written mostly in the context of ADOL-C.) MPI allows the user to create typemaps for arbitrary structures in terms of a block
 * count and arrays of block lengths, block types, and displacements. For sending an array of active variables, we could get by with
 * a pointer to their value array; in the case of a struct, we may want to send an arbitrary collection of data as well as some active
 * variables which we'll need to "dereference". If a struct contains active data, we must manually pack it into a new array because
 * -# the original datamap alignment is destroyed when we convert active data to real values
 * -# we would like to send completely contiguous messages
 * 
 * When received, the struct is unpacked again.
 * 
 * When the user calls the \ref AMPI_Type_create_struct_NT wrapper with a datamap, the map is stored in a structure of type
 * \ref derivedTypeData; the wrapper also generates an internal typemap that describes the packed data. The packed typemap is used
 * whenver a derived type is sent and received; it's also used in conjunction with the user-provided map to pack and unpack data.
 * This typemap is invisible to the user, so the creation of derived datatypes is accomplished entirely with calls to the
 * \ref AMPI_Type_create_struct and \ref AMPI_Type_commit_NT wrappers.
 * 
 * \image html dtype_illustration.png
 * \image latex dtype_illustration.png
 * 
 * AMPI currently supports sending structs with active elements and structs with embedded structs. Packing is called recursively.
 * Functions implemented are \ref AMPI_Type_create_struct_NT and \ref AMPI_Type_contiguous_NT. A wrapper for _Type_vector can't be
 * implemented now because the point of that function is to send noncontiguous data and, for simplicity and efficiency, we're assuming
 * that the active variables we're sending are contiguous.
 * 
 * Worth noting: if we have multiple active variables in a struct and we want to send an array of these structs, we have to send every
 * active element to ensure that our contiguity checks don't assert false.
 * 
 * \section reduction Reduction operations
 * 
 * Since operator overloading can't enter MPI routines, other AMPI functions extract the double values from active variables,
 * transfer those, and have explicit adjoint code that replaces the automated transformation. This is possible because we know the
 * partial derivative of the result. For reductions, we can also do this with built-in reduction ops (e.g., sum, product). But
 * we can't do this for user-defined ops because we don't know the partial derivative of the result.
 * 
 * (Again explained in the context of ADOL-C.) So we have to make the tracing machinery enter the Reduce and perform taping every
 * time the reduction op is applied. As it turns out, MPICH implements Reduce for derived types as a binary tree of Send/Recv pairs,
 * so we can make our own Reduce by replicating the code with AMPI_Send/Recv functions. (Note that derived types are necessarily
 * reduced with user-defined ops because MPI doesn't know how to accumulate them with its built-in ops.) So AMPI_Reduce is implemented
 * for derived types as the aforementioned binary tree with active temporaries used between steps for applying the reduction op.
 * See \ref AMPI_Op_create_NT.
 * 
 * 
 */




#include <mpi.h>
#if defined(__cplusplus)
extern "C" {
#endif

#include "ampi/userIF/passThrough.h"
#include "ampi/userIF/nt.h"
#include "ampi/userIF/modified.h"
#include "ampi/userIF/st.h"

#include "ampi/libCommon/modified.h"

#if defined(__cplusplus)
}
#endif

#endif
