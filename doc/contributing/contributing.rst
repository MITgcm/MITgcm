.. _chap_contributing:

Contributing to the MITgcm
**************************

The MITgcm is an open source project that relies on the participation of its users,
and we welcome contributions. This chapter sets out how you can contribute to the MITgcm.

Bugs and feature requests
=========================

If you think you've found a bug, the first thing to check that you're using the
latest version of the model. If the bug is still in the latest version, then
think about how you might fix it and file a ticket in the
`GitHub issue tracker <https://github.com/MITgcm/MITgcm/issues>`_. Please
include as much detail as possible. At a minimum your ticket should include:

 - what the bug does;
 - the location of the bug: file name and line number(s); and
 - any suggestions you have for how it might be fixed.

To request a new feature, or guidance on how to implement it yourself, please
open a ticket with the following details:

 - a clear explanation of what the feature will do; and
 - a summary of the equations to be solved.

.. _using_git_and_github:

Using Git and Github
========================

To contribute to the source code of the model you will need to fork the repository
and place a pull request on GitHub. The two following sections describe this
process in different levels of detail. If you are unfamiliar with git, you may
wish to skip the quickstart guide and use the detailed instructions. All
contributions to the source code are expected to conform with the
:ref:`sec_code_style_guide`. Contributions to the manual should follow
the same procedure and conform with :numref:`contrib_manual`.

Quickstart Guide
----------------

**0.** As a precursor, if you have not done so already, set up
`ssh keys <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/about-ssh>`_ for GitHub
`command line authentication <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/about-authentication-to-github#authenticating-with-the-command-line>`_
(or alteratively, authenticate using a 
`personal access token <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token>`_).

**1.** Fork the project on GitHub (using the fork button).

**2.** Create a local clone (we strongly suggest keeping a separate
repository for development work). If you are using ssh keys
for command line authentication:

::

    % git clone git@github.com:«GITHUB_USERNAME»/MITgcm.git

Alternatively, if you are using a personal access token for authentication:

::

    % git clone https://github.com/«GITHUB_USERNAME»/MITgcm.git


**3.** Move into your local clone directory (cd MITgcm) and and set
up a remote that points to the original:

::

    % git remote add upstream https://github.com/MITgcm/MITgcm.git

**4.** Make a new branch from ``upstream/master`` (name it something
appropriate, such as ‘bugfix’ or ‘newfeature’ etc.) and make edits on this branch:

::

   % git fetch upstream
   % git checkout -b «YOUR_NEWBRANCH_NAME» upstream/master

**5.** When edits are done, do all git add’s and git commit’s. In the commit message,
make a succinct (<70 char) summary of your changes. If you need more space to
describe your changes, you can leave a blank line and type a longer description,
or break your commit into multiple smaller commits. Reference any outstanding
issues addressed using the syntax ``#«ISSUE_NUMBER»``.

**6.** Push the edited branch to the origin remote (i.e. your fork) on GitHub:

::

    % git push -u origin «YOUR_NEWBRANCH_NAME»

**7.** On GitHub, go to your fork and hit the compare and pull request (PR) button,
provide the requested information about your PR (in particular, a non-trivial change to the model
requires a suggested addition to :filelink:`doc/tag-index`)
and wait for the MITgcm head developers with merge privileges to review your proposed changes.
In addition to the MITgcm head developers, a broader group of reviewers
and developers will peruse and try to respond to a new PR within
a week or two. The reviewers may accept the PR as is, or more typically, may request edits and
changes. Occasionally the review team will reject changes that are not
sufficiently aligned with and do not fit with the code structure.
The review team is always happy to discuss their decision, but want to
avoid people investing extensive effort in code that has a fundamental
design flaw. As such, we **strongly** suggest opening an
`issue <https://github.com/MITgcm/MITgcm/issues>`_
on GitHub to discuss any proposed contributions beforehand.  

The current pull request discussion and review team is Jean-Michel Campin, 
Ed Doddridge, Chris Hill, Oliver Jahn, Jon Lauderdale,
Martin Losch, Jeff Scott, Timothy Smith, and Ou Wang. Please
contact anyone on this team with questions about a proposed pull request.

If you want to update your code branch before submitting a PR (or any point
in development), follow the recipe below. It will ensure that your GitHub
repo stays up to date with the main repository. Note again that your edits
should always be to your development branch, not the master branch.

::

    % git checkout master
    % git pull upstream master
    % git push origin master
    % git checkout «YOUR_NEWBRANCH_NAME»
    % git merge master

If you prefer, you can rebase rather than merge in the final step above;
just be careful regarding your rebase syntax!

Detailed guide for those less familiar with Git and GitHub
----------------------------------------------------------

What is `Git <https://en.wikipedia.org/wiki/Git>`_? Git is a version
control software tool used to help coordinate work among the many
MITgcm model contributors. Version control is a management system to
track changes in code over time, not only facilitating ongoing changes
to code, but also as a means to check differences and/or obtain
code from any past time in the project history. Without such a tool,
keeping track of bug fixes and new features submitted by the global
network of MITgcm contributors would be virtually impossible. If you
are familiar with the older form of version control used by the
MITgcm (CVS), there are many similarities, but we now take advantage
of the modern capabilities offered by Git.

Git itself is open source linux software (typically included with any
new linux installation, check with your sys-admin if it seems to be
missing) that is necessary for tracking changes in files, etc. through
your local computer’s terminal session. All Git-related terminal commands
are of the form ``git «arguments»``.  Important functions include syncing
or updating your code library, adding files to a collection of files
with edits, and commands to “finalize” these changes for sending back to
the MITgcm head developers. There are numerous other Git command-line
tools to help along the way (see man pages via ``man git``).

The most common git commands are:

 - ``git clone`` download (clone) a repository to your local machine
 - ``git status`` obtain information about the local git repository
 - ``git diff`` highlight differences between the current version of a file and the version from the most recent commit
 - ``git add`` stage a file, or changes to a file, so that they are ready for ``git commit``
 - ``git commit`` create a commit. A commit is a snapshot of the repository with an associated message that describes the changes.

What is GitHub then? GitHub is a website that has three major purposes: 1) Code Viewer: through your browser, you can view
all source code and all changes to such over time; 2) “Pull Requests”: facilitates the process whereby code developers submit
changes to the primary MITgcm head developers; 3) the “Cloud”: GitHub functions as a cloud server to store different copies of the code.
The utility of #1 is fairly obvious. For #2 and #3, without GitHub, one might envision making a big tarball of edited files and
emailing the head developers for inclusion in the main repository. Instead, GitHub effectively does something like this for you in a
much more elegant way.  Note unlike using (linux terminal command) git, GitHub commands are NOT typed in a terminal, but are
typically invoked by hitting a button on the web interface, or clicking on a webpage link etc.

To contribute edits to MITgcm,
**the first step is to obtain a GitHub account**, if you have not done so already; it’s free. Second, as a 'developer' you will need to
`authenticate <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/about-authentication-to-github#authenticating-with-the-command-line>`_
your terminal command line sessions in GitHub. There are two ways this can be done, either using
`ssh keys <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/about-ssh>`_
or via a `personal access token <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token>`_.
A personal access token functions similar to a password; ssh keys require some upfront configuration
(`generating the key <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent>`_
and then `adding to your Github account <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account>`_),
but most developers prefer the ease of this approach once it is set up.

Before you start working with git, make sure you identify yourself. From your terminal, type:

::

    % git config --global user.email «your_email@example.edu»
    % git config --global user.name «‘John Doe’»

(note the required quotes around your name). You should also personalize your profile associated with your GitHub account.

There are many online tutorials to using Git and GitHub
(see for example https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project );
here, we are just communicating the basics necessary to submit code changes to the MITgcm. Spending some time learning the more
advanced features of Git will likely pay off in the long run, and not just for MITgcm contributions,
as you are likely to encounter it in all sorts of different projects.

To better understand this process, :numref:`git_setup` shows a conceptual map of the Git setup. Note three copies of the code:
the main MITgcm repository sourcecode “upstream” (i.e., owned by the MITgcm head developers) in the GitHub cloud, a copy of the
repository “origin” owned by you, also residing in the GitHub cloud, and a local copy on your personal computer or compute cluster
(where you intend to compile and run). The Git and GitHub commands to create this setup are explained more fully below.

 .. figure:: figs/git_setup.*
    :width: 70%
    :align: center
    :alt: Conceptual model of GitHub
    :name: git_setup

    A conceptual map of the GitHub setup. Git terminal commands are shown in red, GitHub commands are shown in green.

One other aspect of Git that requires some explanation to the uninitiated: your
local linux copy of the code repository can contain different “branches”,
each branch being a different copy of the code repository (this can occur
in all git-aware directories). When you switch branches, basic unix commands
such as ``ls`` or ``cat`` will show a different set of files specific to
current branch. In other words, Git interacts with your local file system
so that edits or newly created files only appear in the current branch, i.e.,
such changes do not appear in any other branches. So if you swore you
made some changes to a particular file, and now it appears those changes
have vanished, first check which branch you are on (``git status`` is a
useful command here), all is probably not lost. NOTE: for a file to be “assigned” to a specific Git branch,
Git must first be “made aware” of the file, which occurs after a ``git add`` and ``git commit`` (see :ref:`below <doing_stuff_in_git>`).
Prior to this, the file will appear in the current folder independently, i.e., regardless of which git branch you are on.

A detailed explanation of steps for contributing MITgcm repository edits:

**1.** On GitHub, create a local copy of the repository in your GitHub cloud user space:
from the main repository (https://github.com/MITgcm/MITgcm) hit the **Fork** button.
As mentioned, your GitHub copy “origin” is necessary to streamline the collaborative
development process -- you need to create a place for your edits in the GitHub cloud,
for developers to peruse. (Note: this step is only necessary the first time you contribute a pull request,
as this forked copy will remain permanently in your Github space.)

**2.** Download the code onto your local computer using the git clone command.
Even if you previously downloaded the code through a “git-aware” method
(i.e., a git clone command, see :numref:`git-aware_download`),
we **STRONGLY SUGGEST** you download a fresh repository, to a separate
disk location, for your development work (keeping your research work separate). 
If you are using ssh keys for command line authentication (see above), in your terminal window type:

::

    % git clone git@github.com:«GITHUB_USERNAME»/MITgcm.git

Alternatively, if you are using a personal access token for authentication:

::

    % git clone https://github.com/«GITHUB_USERNAME»/MITgcm.git


(technically, here you are copying the forked “origin”
version from the cloud, not the “upstream” version, but these will be identical at this point).

**3.** Move into the local clone directory on your computer:

::

    % cd MITgcm

We need to set up a remote that points to the main repository:

::

    % git remote add upstream https://github.com/MITgcm/MITgcm.git

This means that we now have two "remotes" of the project. A remote is
just a pointer to a repository not on your computer, i.e., in the GitHub
cloud, one pointing to your GitHub user space (“origin”), and this new
remote pointing to the original (“upstream”). You can read and write
into your "origin" version (since it belongs to you, in the cloud),
but not into the "upstream" version. This command just sets up this
remote, which is needed in step #4 -- no actual file manipulation
is done at this point. If in doubt, the command ``git remote -v``
will list what remotes have been set up.

**4.**  Next make a new branch.

::

    % git fetch upstream
    % git checkout -b «YOUR_NEWBRANCH_NAME» upstream/master

You will make edits on this new branch, to keep these new edits completely
separate from all files on the master branch. The first command
``git fetch upstream`` makes sure your new branch is the latest code
from the main repository; as such, you can redo step 4 at any time to
start additional, separate development projects (on a separate, new branch).
Note that this second command above not only creates this new branch,
from the ``upstream/master`` branch, it also switches you onto this newly
created branch.  Naming the branch something descriptive like ‘newfeature’
or ‘bugfix’ (preferably, be even more descriptive) is helpful.

.. _doing_stuff_in_git:

**5.** Doing stuff! This usually comes in one of three flavors:

|   i) cosmetic changes, formatting, documentation, etc.;
|   ii) fixing bug(s), or any change to the code which results in different numerical output; or
|   iii) adding a feature or new package.
|
|   To do this you should:

    - edit the relevant file(s) and/or create new files. Refer to :ref:`sec_code_style_guide` for details on expected documentation
      standards and code style requirements. Of course, changes should be thoroughly tested to ensure they compile and run successfully!
    - type ``git add «FILENAME1» «FILENAME2» ...`` to stage the file(s) ready for a commit command (note both existing and
      brand new files need to be added). “Stage” effectively means to notify Git of the the list of files you plan to “commit”
      for changes into the version tracking system. Note you can change other files and NOT have them sent to model developers;
      only staged files will be sent. You can repeat this ``git add`` command as many times as you like and it will continue
      to augment the list of files.  ``git diff`` and ``git status`` are useful commands to see what you have done so far.
    - use ``git commit`` to commit the files. This is the first step in bundling a collection of files together to be sent
      off to the MITgcm head developers. When you enter this command, an editor window will pop up. On the top line, type a succinct
      (<70 character) summary of what these changes accomplished. If your commit is non-trivial and additional explanation is required,
      leave a blank line and then type a longer description of why the action in this commit was appropriate etc.
      It is good practice to link with known issues using the syntax ``#ISSUE_NUMBER`` in either the summary line or detailed comment.
      Note that all the changes do not have to be handled in a single commit (i.e. you can git add some files, do a commit,
      than continue anew by adding different files, do another commit etc.); the ``git commit`` command itself does
      not (yet) submit anything to head developers.
    - if you are fixing a more involved bug or adding a new feature, such that many changes are required,
      it is preferable to break your contribution into multiple commits (each documented separately) rather than submitting one massive commit;
      each commit should encompass a single conceptual change to the code base, regardless of how many files it touches.
      This will allow the MITgcm head developers to more easily understand your proposed changes and will expedite the review process.

When your changes are tested and documented, continue on to step #6, but read all of step #6 and #7 before proceeding;
you might want to do an optional “bring my development branch up to date” sequence of steps before step #6.

**6.** Now we “push” our modified branch with committed changes onto the origin remote in the GitHub cloud.
This effectively updates your GitHub cloud copy of the MITgcm repo to reflect the wonderful changes you are contributing.

::

    % git push -u origin «YOUR_NEWBRANCH_NAME»

Some time might elapse during step #5, as you make and test your edits, during which continuing development occurs in the main MITgcm repository.
In contrast with some models that opt for static, major releases, the MITgcm is in a constant state of improvement and development.
It is very possible that some of your edits occur to files that have also been modified by others. Your local clone however will not
know anything about any changes that may have occurred to the MITgcm repo in the cloud, which may cause an issue in step #7 below,
when one of three things will occur:

   - the files you have modified in your development have **NOT** been modified in the main repo during this elapsed time,
     thus git will have no conflicts in trying to update (i.e. merge) your changes into the main repo.
   - during the elapsed time, the files you have modified have also been edited/updated in the main repo,
     but you edited different places in these files than those edits to the main repo, such that git is
     smart enough to be able to merge these edits without conflict.
   - during the elapsed time, the files you have modified have also been edited/updated in the main repo,
     but git is not smart enough to know how to deal with this conflict (it will notify you of this problem during step #7).

One option is to NOT attempt to bring your development code branch up to date, instead simply proceed with steps #6 and #7 and
let the maintainers (i.e., the MITgcm head developers with merge priviledges) assess and resolve any conflict(s),
should such occur (there is a checkbox ‘Allow edits by maintainers’
that is checked by default when you do step #7). If very little time elapsed during step #5, such conflict is less likely.
However, if step #5 takes on the order of months, we do suggest you follow this recipe below to update the code and merge yourself.
And/or during the development process, you might have reasons to bring the latest changes in the main repo into your
development branch, and thus might opt to follow these same steps.

Development branch code update recipe:

::

    % git checkout master
    % git pull upstream master
    % git push origin master
    % git checkout «YOUR_NEWBRANCH_NAME»
    % git merge master

This first command switches you from your development branch to the master branch. The second command above will synchronize
your local master branch with the main MITgcm repository master branch (i.e. “pull” any new changes that might have occurred
in the upstream repository into your local clone). Note you should not have made any changes to your clone’s master branch;
in other words, prior to the pull, master should be a stagnant copy of the code from the day you performed step #1 above.
The ``git push`` command does the opposite of pull, so in the third step you are synchronizing your GitHub cloud copy (“origin”)
master branch to your local clone’s master branch (which you just updated). Then, switch back to your development branch via
the second ``git checkout`` command. Finally, the last command will merge any changes into your development branch.
If conflicts occur that git cannot resolve, git will provide you a list of the problematic file names, and in these files,
areas of conflict will be demarcated. You will need to edit these files at these problem spots (while removing git’s demarcation text),
then do a ``git add «FILENAME»`` for each of these files, followed by a final ``git commit`` to finish off the merger.

Some additional ``git diff`` commands to help sort out file changes, in case you want to assess the scope of development changes,
are as follows. ``git diff master upstream/master`` will show you all differences between your local master branch and the main
MITgcm repo, i.e., so you can peruse what parallel MITgcm changes have occurred while you were doing your development (this assumes
you have not yet updated your clone’s master branch).
You can check for differences on individual files via ``git diff master upstream/master  «FILENAME»``.
If you want to see all differences in files you have modified during your development, the command
is ``git diff master``. Similarly, to see a combined list of both your changes and those occurring to the main repo, ``git diff upstream/master``.

Aside comment: if you are familiar with git, you might realize there is an alternate way to merge, using the “rebase” syntax.
If you know what you are doing, feel free to use this command instead of our suggested merge command above.

**7.** Finally create a “pull request” (a.k.a. “PR”; in other words, you are requesting that the
MITgcm head developers with merge privileges pull your changes into the main code repository).
In GitHub, go to the fork of the project that you made (https://github.com/«GITHUB_USERNAME»/MITgcm.git).
There is a button for "Compare and Pull" in your newly created branch. Click the button!
Now you can add a final succinct summary description of what you've done in your commit(s),
flag up any issues, and respond to the remaining questions on the PR template form. If you have made non-trivial changes to
the code or documentation, we will note this in the MITgcm change log, :filelink:`doc/tag-index`. Please suggest how to note your
changes in :filelink:`doc/tag-index`; we will not accept the PR if this field is left blank.
The MITgcm PR discussion team (in addition to the MITgcm head developers, a broader group of reviewers) will now be notified
and be able to peruse your changes! In general, the PR review team will try to respond to a new PR within a week or two.
While the PR remains open, you can go back to step #5 and make additional edits, git adds,
git commits, and then redo step #6; such changes will be added to the PR (and head developers re-notified), no need to redo step #7.

Your pull request remains open until either the MITgcm head developers with merge privileges fully accept and
merge your code changes into the main repository, or decide to reject your changes.
Occasionally, the review team will reject changes that are not
sufficiently aligned with and do not fit with the code structure;
the review team is always happy to discuss their decision, but want to
avoid people investing extensive additional effort in code that has a fundamental design flaw.
But much more likely than outright rejection, you will instead be asked to respond to feedback,
modify your code changes in some way, and/or clean up your code to better satisfy our style requirements, etc.,
and the pull request will remain open.
In some cases, the head developers might take initiative to make some changes to your pull request
(such changes can then be incorporated back into your local branch simply by typing ``git pull`` from your branch), but
more typically you will be asked to undertake the majority of the necessary changes.
Note we **strongly** suggest opening an `issue <https://github.com/MITgcm/MITgcm/issues>`_
on GitHub to discuss any proposed contributions beforehand.

It is possible for other users (besides the PR review team) to examine
or even download your pull request; see :ref:`sec_pullreq`.

The current pull request discussion and review team is Jean-Michel Campin, 
Ed Doddridge, Chris Hill, Oliver Jahn, Jon Lauderdale,
Martin Losch, Jeff Scott, Timothy Smith, and Ou Wang. Please feel free
to contact anyone on this team with questions about a proposed pull request.

.. _sec_code_style_guide:

Coding style guide
==================

**Detailed instructions or link to be added.**

Creating MITgcm packages
========================

Optional parts of code are separated from
the MITgcm core driver code and organized into
packages. The packaging structure provides a mechanism for
maintaining suites of code, specific to particular
classes of problem, in a way that is cleanly
separated from the generic fluid dynamical engine. An overview of available MITgcm
packages is presented in :numref:`packagesI`, as illustrated in :numref:`fig_package_organigramme`.
An overview of how to include and use MITgcm packages in your setup is presented in :numref:`using_packages`,
with specific details on using existing packages spread throughout :numref:`packagesI`, :numref:`outp_pack`, and :numref:`chap_state_estimation`.
This sub-section includes information necessary to create your own package for use with MITgcm.

The MITgcm packaging structure is described
below using generic package names ``${pkg}``.
A concrete examples of a package is the code
for implementing GM/Redi mixing:  this code uses
the package names ``${PKG} = GMREDI``, ``${pkg} = gmredi``, and ``${Pkg} = gmRedi``.

Package structure
-----------------

•  Compile-time state: Given that each package is allowed to be compiled or not
   (e.g., all ``${pkg}`` listed in ``packages.conf`` are compiled, see :numref:`pkg_inclusion_exclusion`),
   :filelink:`genmake2 <tools/genmake2>` keeps track of each package's compile-state in PACKAGES_CONFIG.h
   with CPP option ``ALLOW_${PKG}`` being defined (``#define``) or not (``#undef``).
   Therefore, in the MITgcm core code (or code from other included packages), calls to package-specific
   subroutines and package-specific header file ``#include`` statements
   must be protected within ``#ifdef ALLOW_${PKG}`` ... ... ``#endif /* ALLOW_${PKG} */``
   (see :ref:`below <example_pkg_call_from_outside>`) to ensure that the model compiles when this ${pkg}
   is not compiled.

•  Run-time state: The core driver part of the model can check
   for a run-time on/off switch of individual package(s)
   through the Fortran logical flag ``use${Pkg}``.
   The information is loaded from a
   global package setup file called ``data.pkg``. Note a
   ``use${Pkg}`` flag is NOT used within the
   package-local subroutine code (i.e., ``${pkg}_«DO_SOMETHING».F`` package source code).

•  Each package gets its runtime configuration
   parameters from a file named ``data.${pkg}``.
   Package runtime configuration options are imported
   into a common block held in a header file
   called ``${PKG}.h``.
   Note in some packages, the header file ``${PKG}.h`` is split
   into ``${PKG}_PARAMS.h``, which contains the package parameters, and
   ``${PKG}_VARS.h`` for the field arrays. The ``${PKG}.h`` header file(s) can be imported
   by other packages to check dependencies and requirements
   from other packages (see :numref:`package_boot_sequence`).

In order for a package’s run-time state ``use${Pkg}`` to be set to true (i.e., “on”),
the code build must have its compile-time state ``ALLOW_${PKG}`` defined (i.e., “included”),
else mitgcmuv will terminate (cleanly) during initialization. A package’s run-time state
is not permitted to change during a model run.

Every call to a package routine from **outside** the package
requires a check on BOTH compile-time and run-time states:

.. _example_pkg_call_from_outside:

::

   #include "PACKAGES_CONFIG.h"
   #include "CPP_OPTIONS.h"
         .
         .
   #ifdef ALLOW_${PKG}
   #  include "${PKG}_PARAMS.h"
   #endif
         .
         .
         .

   #ifdef ALLOW_${PKG}
         IF ( use${Pkg} ) THEN
            .
            .
            CALL ${PKG}_DO_SOMETHING(...)
            .
         ENDIF
   #endif

**Within** an individual package, the header file ``${PKG}_OPTIONS.h``
is used to set CPP flags specific to that package. This header file should include
``PACKAGES_CONFIG.h`` and :filelink:`CPP_OPTIONS.h <model/inc/CPP_OPTIONS.h>`, as shown in this example:

::

   #ifndef ${PKG}_OPTIONS_H
   #define ${PKG}_OPTIONS_H
   #include "PACKAGES_CONFIG.h"
   #include "CPP_OPTIONS.h"

   #ifdef ALLOW_${PKG}
         .
         .
         .
   #define ${PKG}_SOME_PKG_SPECIFIC_CPP_OPTION
         .
         .
         .
   #endif /* ALLOW_${PKG} */
   #endif /* ${PKG}_OPTIONS_H */

See for example :filelink:`GMREDI_OPTIONS.h <pkg/gmredi/GMREDI_OPTIONS.h>`.

.. _package_boot_sequence:

Package boot sequence
---------------------

All packages follow a required "boot" sequence outlined here:

::

        S/R PACKAGES_BOOT()

        S/R PACKAGES_READPARMS()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_READPARMS( retCode )
           #endif

        S/R PACKAGES_INIT_FIXED()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_INIT_FIXED( retCode )
           #endif

        S/R PACKAGES_CHECK()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_CHECK( retCode )
           #else
              IF ( use${Pkg} ) CALL PACKAGES_CHECK_ERROR('${PKG}')
           #endif

        S/R PACKAGES_INIT_VARIABLES()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_INIT_VARIA( )
           #endif

- :filelink:`PACKAGES_BOOT() <model/src/packages_boot.F>`
   determines the logical state of all ``use${Pkg}`` variables, as defined in the file ``data.pkg``.

- ${PKG}_READPARMS()
   is responsible for reading
   in the package parameters file ``data.${pkg}`` and storing
   the package parameters in ``${PKG}.h`` (or in ``${PKG}_PARAMS.h``).
   ${PKG}_READPARMS is called in S/R :filelink:`packages_readparms.F <model/src/packages_readparms.F>`,
   which in turn is called from S/R :filelink:`initialise_fixed.F <model/src/initialise_fixed.F>`.

- ${PKG}_INIT_FIXED()
   is responsible for completing the internal setup of a package, including adding any package-specific
   variables available for output in :filelink:`pkg/diagnostics` (done in S/R ${PKG}_DIAGNOSTICS_INIT).
   ${PKG}_INIT_FIXED is called in S/R :filelink:`packages_init_fixed.F <model/src/packages_init_fixed.F>`,
   which in turn is called from S/R :filelink:`initialise_fixed.F <model/src/initialise_fixed.F>`.
   Note: some packages instead use ``CALL ${PKG}_INITIALISE``  (or the old form ``CALL ${PKG}_INIT``).

- ${PKG}_CHECK()
   is responsible for validating
   basic package setup and inter-package dependencies.
   ${PKG}_CHECK can also import parameters from other packages that it may
   need to check; this is accomplished through header files ``${PKG}.h``.
   (It is assumed that parameters owned by other packages
   will not be reset during ${PKG}_CHECK !!!)
   ${PKG}_CHECK is called in S/R :filelink:`packages_check.F <model/src/packages_check.F>`,
   which in turn is called from S/R :filelink:`initialise_fixed.F <model/src/initialise_fixed.F>`.

- ${PKG}_INIT_VARIA()
   is responsible for initialization of all package variables, called after the core model state has been completely
   initialized but before the core model timestepping starts.
   This routine calls ${PKG}_READ_PICKUP, where any package variables required to restart the model
   will be read from a pickup file.
   ${PKG}_INIT_VARIA is called in :filelink:`packages_init_variables.F <model/src/packages_init_variables.F>`,
   which in turn is called from S/R :filelink:`initialise_varia.F <model/src/initialise_varia.F>`.
   Note: the name ${PKG}_INIT_VARIA is not yet standardized across all packages;
   one can find other S/R names such as ${PKG}_INI_VARS or ${PKG}_INIT_VARIABLES or ${PKG}_INIT.

Package S/R calls
-----------------

Calls to package subroutines within the core code timestepping
loop can vary. Below we show an example of calls to do calculations, generate output
and dump the package state (for pickup):

::

        S/R DO_OCEANIC_PHYS()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_DO_SOMETHING( )
           #endif

        S/R DO_THE_MODEL_IO()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_OUTPUT( )
           #endif

        S/R PACKAGES_WRITE_PICKUP()
           #ifdef ALLOW_${PKG}
              IF ( use${Pkg} ) CALL ${PKG}_WRITE_PICKUP( )
           #endif

- ${PKG}_DO_SOMETHING()
   refers to any local package source code file, which may be called from any :filelink:`model/src` routine
   (or, from any subroutine in another package). An specific example would be the
   S/R call :filelink:`gmredi_calc_tensor.F <pkg/gmredi/gmredi_calc_tensor.F>` from within the core S/R
   :filelink:`model/src/do_oceanic_phys.F`.

- ${PKG}_OUTPUT()
   is responsible for writing time-average fields to output files
   (although the cumulating step is done within other package subroutines).
   May also call other output routines (e.g., CALL ${PKG}_MONITOR)
   and write snapshot fields that are held in common blocks. Other
   temporary fields are directly dumped to file where they are available.
   Note that :filelink:`pkg/diagnostics` output of ${PKG} variables
   is generated in :filelink:`pkg/diagnostics` subroutines.
   ${PKG}_OUTPUT() is called in S/R :filelink:`do_the_model_io.F <model/src/do_the_model_io.F>`
   NOTE: 1) the S/R ${PKG}_DIAGS is used in some packages
   but is being replaced by ${PKG}_OUTPUT
   to avoid confusion with :filelink:`pkg/diagnostics` functionality.
   2) the output part is not yet in a standard form.

- ${PKG}_WRITE_PICKUP()
   is responsible for writing a package pickup file, used in packages where such is necessary for
   a restart. ${PKG}_WRITE_PICKUP is called in :filelink:`packages_write_pickup.F <model/src/packages_write_pickup.F>`
   which in turn is called from :filelink:`the_model_main.F <model/src/the_model_main.F>`.

Note: In general, subroutines in one package (pkgA) that only contains code which
is connected to a 2nd package (pkgB) will be named pkgA_pkgB_something.F
(e.g., :filelink:`gmredi_diagnostics_init.F <pkg/gmredi/gmredi_diagnostics_init.F>`).

Package “mypackage”
-------------------

In order to simply creating the infrastructure required for a new package, we have created :filelink:`pkg/mypackage`
as essentially an existing package (i.e., all package variables defined, proper boot sequence, output generated) that
does not do anything. Thus, we suggest you start with this “blank” package’s code infrastructure and add your new package functionality
to it, perusing the existing mypackage routines and editing as necessary, rather than creating a new package from scratch.

.. _code_testing_protocols:

MITgcm code testing protocols
=============================

:filelink:`verification` directory  includes  many examples
intended  for  regression  testing (some of which are tutorial experiments presented in detail in :numref:`chap_modelExamples`).
Each  one  of  these  test-experiment  directories  contains  "known-good"  standard output  files (see :numref:`reference_output`)
along  with  all  the  input  (including  both  code  and  data  files)  required  for  their  re-calculation.  Also  included  in
:filelink:`verification` is the shell script :filelink:`testreport <verification/testreport>` to perform regression tests.

Test-experiment directory content
---------------------------------

Each test-experiment directory («TESTDIR», see :filelink:`verification` for
the full list of choices) contains several standard subdirectories and files which
:filelink:`testreport <verification/testreport>` recognizes and uses when running a regression test.
The directories and files that :filelink:`testreport <verification/testreport>`
uses are different for a forward test and an adjoint test (``testreport -adm``, see :numref:`testreport_utility`) and
some test-experiments are set-up for only one type of regression test
whereas others allow both types of tests (forward and adjoint).
Also some test-experiments allow, using the same MITgcm executable, multiple tests using
different parameters and input files, with a primary input set-up (e.g., ``input/`` or ``input_ad/``)  and  corresponding
results (e.g., ``results/output.txt`` or ``results/output_adm.txt``)  and with one or several secondary inputs
(e.g., ``input.«OTHER»/`` or ``input_ad.«OTHER»/``) and corresponding results (e.g., ``results/output.«OTHER».txt`` or ``results/output_adm.«OTHER».txt``).

directory «TESTDIR»/code/
   Contains the test-experiment specific source code (i.e., files that have been modified from the standard
   MITgcm repository version) used to build the MITgcm executable (``mitgcmuv``)
   for forward-test (using ``genmake2 -mods=../code``).

   It  can  also  contain  specific  source  files  with  the  suffix  ``_mpi``  to  be  used  in  place  of  the  corresponding  file
   (without suffix) for an MPI test (see :numref:`testreport_utility`). The presence or absence of ``SIZE.h_mpi``
   determines whether or not an MPI test on this test-experiment is performed or skipped.
   Note that the original ``code/SIZE.h_mpi``
   is not directly used as :filelink:`SIZE.h <model/inc/SIZE.h>` to build an MPI-executable; instead, a local copy
   ``build/SIZE.h.mpi`` is derived from ``code/SIZE.h_mpi`` by adjusting the number
   of processors (:varlink:`nPx`, :varlink:`nPy`) according  to «NUMBER_OF_PROCS»
   (see :numref:`testreport_utility`, ``testreport -MPI``); then it is
   linked to :filelink:`SIZE.h <model/inc/SIZE.h>` (``ln -s SIZE.h.mpi SIZE.h``) before building the MPI-executable.

directory «TESTDIR»/code_ad/
   Contains  the  test-experiment  specific  source  code  used  to  build  the  MITgcm  executable  (``mitgcmuv_ad``)  for
   adjoint-test  (using ``genmake2 -mods=../code_ad``).  It  can  also  contain  specific  source  files  with  the  suffix
   ``_mpi`` (see above).

directory «TESTDIR»/build/
   Directory where :filelink:`testreport <verification/testreport>`
   will build the MITgcm executable for forward and adjoint tests. It is initially empty except in some cases
   will contain an experiment specific ``genmake_local`` file (see :numref:`genmake2_desc`).
directory TESTDIR/input/
   Contains the input and parameter files used to run the primary forward test of this test-experiment.

   It can also contain specific parameter files with the suffix ``.mpi`` to be used in place of the corresponding file
   (without suffix) for MPI tests, or with suffix ``.mth`` to be used for
   multi-threaded tests (see :numref:`testreport_utility`). The presence or absence of
   ``eedata.mth`` determines whether or not a multi-threaded test on this test-experiment is
   performed or skipped, respectively.

   To save disk space and reduce downloading time, multiple copies of the same input file are avoided by using a
   shell script ``prepare_run``. When such a script is found in ``TESTDIR/input/``,
   :filelink:`testreport <verification/testreport>` runs this script in
   directory ``TESTDIR/run/`` after linking all the input files from ``TESTDIR/input/``.

directory «TESTDIR»/input_ad/
   Contains the input and parameter files used to run the primary adjoint test of this test-experiment. It can also
   contain specific parameter files with the suffix ``.mpi`` and shell script ``prepare_run`` as described above.

directory «TESTDIR»/input.«OTHER»/
    Contains the input and parameter files used to run the secondary OTHER forward test of this test-experiment.
    It can also contain specific parameter files with suffix ``.mpi`` or ``.mth`` and shell script
    ``prepare_run`` (see above).

    The presence or absence the file ``eedata.mth`` determines whether or not a secondary multi-threaded test on this
    test-experiment is performed or skipped.

directory «TESTDIR»/input_ad.«OTHER»/
    Contains the input and parameter files used to run the secondary OTHER adjoint test of this test-experiment. It
    can also contain specific parameter files with the suffix ``.mpi`` and shell script ``prepare_run`` (see above).

directory «TESTDIR»/results/
  Contains reference standard output used for test comparison.
  ``results/output.txt`` and ``results/output_adm.txt``,
  respectively, correspond to primary forward and adjoint test run on the reference
  platform  (currently villon.mit.edu) on one processor  (no  MPI,  single  thread)  using  the
  reference  compiler  (currently  the  `GNU  Fortran  compiler gfortran <https://gcc.gnu.org/fortran>`_).
  The  presence  of  these  output files  determines
  whether or not :filelink:`testreport <verification/testreport>`
  is testing or skipping this test-experiment. Reference standard output for secondary tests
  (``results/output.«OTHER».txt`` or ``results/output_adm.«OTHER».txt``) are also expected here.

directory «TESTDIR»/run/
    Initially empty directory where :filelink:`testreport <verification/testreport>`
    will run the MITgcm executable for primary forward and adjoint tests.

    Symbolic links (using command ``ln -s``) are made for input and parameter files
    (from ``../input/`` or from ``../input_ad/``) and for MITgcm executable (from
    ``../build/``) before the run proceeds. The sequence of links (function
    ``linkdata`` within shell script :filelink:`testreport <verification/testreport>`)
    for a forward test is:

    - link and rename or remove links to special files with suffix ``.mpi`` or ``.mth`` from ``../input/``
    - link files from ../input/
    - execute ``../input/prepare_run`` (if it exists)

    The sequence for an adjoint test is similar, with ``../input_ad/`` replacing ``../input/``.

directory «TESTDIR»/tr_run.«OTHER»/
    Directory created by :filelink:`testreport <verification/testreport>`
    to run the MITgcm executable for secondary "OTHER" forward or adjoint tests.

    The sequence of links for a forward secondary test is:

    - link and rename or remove links to special files with suffix ``.mpi`` or ``.mth`` from ``../input.OTHER/``
    - link files from ``../input.OTHER/``
    - execute ``../input.OTHER/prepare_run`` (if it exists)
    - link files from ``../input/``
    - execute ``../input/prepare_run`` (if it exists)

    The  sequence  for  an  adjoint  test  is  similar,  with ``../input_ad.OTHER/``
    and ``../input_ad/`` replacing ``../input.OTHER/`` and ``../input/``.

.. _testreport_utility:

The testreport utility
----------------------

The shell script :filelink:`testreport <verification/testreport>`, which was written to work with
:filelink:`genmake2 <tools/genmake2>`, can be used to build different versions of MITgcm code,
run the various examples, and compare the output.
On some systems, the :filelink:`testreport <verification/testreport>`
script can be run with a command line as simple as:

::

   % cd verification
   % ./testreport -optfile ../tools/build_options/linux_amd64_gfortran

The :filelink:`testreport <verification/testreport>` script accepts a number of command-line options which can be listed using the
``-help`` option. The most important ones are:

``-ieee`` (default) / ``-fast``
   If allowed by the compiler (as defined in the specified optfile), use IEEE arithmetic (``genmake2 -ieee``).
   In contrast, ``-fast`` uses the optfile default for compiler flags.

``-devel``
   Use optfile development flags (assumes specified in optfile).

``-optfile «/PATH/FILENAME»`` (or ``-optfile ’«/PATH/F1» «/PATH/F2» ...’``)
   This specifies a list of "options files" that will be passed to :filelink:`genmake2 <tools/genmake2>`.
   If multiple options files are used (for example, to test different compilers
   or different sets of options for the same compiler), then each options file will be used
   with each of the test directories.

``-tdir «TESTDIR»`` (or ``-tdir ’«TDIR1» «TDIR2» ...’``)
   This option specifies the test directory or list of test directories that should be used.
   Each of these entries should exactly match (note: they are case sensitive!) the names of directories in
   :filelink:`verification`. If this option is omitted, then all directories that are
   properly formatted (that is, containing an input subdirectory and a ``results/output.txt``
   file) will be used.

``-skipdir «TESTDIR»`` (or ``-skipdir ’«TDIR1» «TDIR2» ...’``)
   This option specifies a test directory or list of test directories to skip. The default is to test **ALL**
   directories in :filelink:`verification`.

``-MPI «NUMBER_OF_PROCS»`` (or ``-mpi``)
   If the necessary file ``«TESTDIR»/code/SIZE.h_mpi`` exists, then use it (and all
   ``TESTDIR/code/*_mpi`` files) for  an  MPI-enabled  run.  The option
   ``-MPI`` followed  by  the  maximum  number  of  processors  enables to
   build  and  run  each  test-experiment  using  different  numbers
   of  MPI  processors (specific number chosen by:
   multiple  of nPx*nPy from ``«TESTDIR»/code/SIZE.h_mpi`` and not larger than
   «NUMBER_OF_PROCS»). The short option (``-mpi``) can only be used to
   build and run on 2 MPI processors (equivalent to ``-MPI 2``).

   Note that the use of MPI typically requires a special command option (see "-command" below) to invoke the
   MPI executable.

``-command=’«SOME COMMANDS TO RUN»’``
   For some tests, particularly MPI runs, a specific command might be needed to run the executable.
   This option allows a more general command (or shell script) to be invoked.

   The default here is for «SOME COMMANDS TO RUN» to be replaced by
   ``mpirun -np TR_NPROC mitgcmuv``. If on your system you require
   something other than ``mpirun``, you will need to use the option
   and specify your computer’s syntax. Because the number of MPI processors
   varies according to each test-experiment, the keyword TR_NPROC
   will be replaced by its effective value, the actual number of MPI processors
   needed to run the current test-experiment.

``-mth``
   Compile with ``genmake2 -omp`` and run with multiple threads (using ``eedata.mth``).

``-adm``
   Compile and test the adjoint suite of verification runs using TAF.

``-clean``
   Clean out all files/progress from any previously executed :filelink:`testreport <verification/testreport>` runs.

``-match «NUMBER»``
   Set matching criteria to «NUMBER» of significant digits (default is 10 digits).

Additional :filelink:`testreport <verification/testreport>` options are available
to pass options to :filelink:`genmake2 <tools/genmake2>` (called during :filelink:`testreport <verification/testreport>` execution)
as well as additional options to skip specific steps of the
:filelink:`testreport <verification/testreport>` shell script. See
``testreport -help`` for a detailed list.

In the :filelink:`verification/` directory, the :filelink:`testreport <verification/testreport>` script will create an output
directory «tr_NAME_DATE_N», with your computer hostname substituted for
NAME, the current date for DATE, followed by a suffix number N to distinguish
from previous :filelink:`testreport <verification/testreport>`
output directories. Unless you specify otherwise using the ``-tdir`` or ``-skipdir`` options described above,
all sub-directories (i.e., TESTDIR experiments) in :filelink:`verification` will be tested.
:filelink:`testreport <verification/testreport>` writes progress to the screen (stdout) and
reports into the «tr_NAME_DATE_N/TESTDIR» sub-directories as it runs. In particular,
one can find, in each TESTDIR subdirectory, a
``summary.txt`` file in addition to log and/or error file(s) (depending how the run failed, if this occurred).
``summary.txt``  contains information about the run and a comparison of the current
output with “reference output” (see :ref:`below <reference_output>` for information on how this reference output is generated).
The test comparison involves several output model variables. By default, for a forward test, these are the 2D
solver initial residual ``cg2d_init_res`` and 3D state variables
(T, S, U, V) from :filelink:`pkg/monitor` output; by default
for an adjoint test, the cost-function and gradient-check. However, some test-experiments
use some package-specific variables from :filelink:`pkg/monitor` according to the file
``«TESTDIR»/input[_ad][.«OTHER»]/tr_checklist`` specification. Note that at this time,
the only variables that are compared by :filelink:`testreport <verification/testreport>`
are those dumped in standard output via :filelink:`pkg/monitor`, not output produced
by :filelink:`pkg/diagnostics`.  Monitor output produced from **ALL** run time steps are compared
to assess significant digit match; the worst match is reported.
At the end of the testing process, a composite
``summary.txt`` file is generated in the top «tr_NAME_DATE_N» directory as a compact, combined version of the ``summary.txt``
files located in all TESTDIR sub-directories
(a slightly more condensed version of this information is also written to file ``tr_out.txt`` in the top :filelink:`verification/` directory;
note this file is overwritten upon subsequent :filelink:`testreport <verification/testreport>` runs).
:numref:`testreport_output` shows an excerpt from the composite ``summary.txt``, created by running the full testreport suite (in the example here, on a linux cluster, using gfortran):

.. figure:: figs/testreport_output.*
    :width: 100%
    :align: center
    :alt: output text from summary.txt
    :name: testreport_output

    Example output from testreport ``summary.txt``

The four columns on the left are build/run results (successful=Y, unsuccessful=N). Explanation of these columns is as follows:

  - Gen2: did genmake2 build the makefile for this experiment without error?
  - Dpnd: did the ``make depend`` for this experiment complete without error?
  - Make: did the ``make`` successfully generate a ``mitgcmuv`` executable for this experiment?
  - Run: did execution of this experiment startup and complete successfully?

The next sets of columns shows the number of significant digits matched from the monitor
output “cg2d”, “min”, “max”, “mean”, and “s d” (standard deviation) for variables T, S, U, and V (see column headings), as compared with the reference output.
NOTE: these column heading labels are for the default list of variables, even if different variables are specified in a ``tr_checklist`` file
(for reference, the list of actual variables tested for a specific TESTDIR experiment is output near the end of the file  ``summary.txt``
appearing in the specific TESTDIR experiment directory).
For some experiments, additional variables are tested, as shown in “PTR 01”, “PTR 02” sets of columns;
:filelink:`testreport <verification/testreport>` will detect if tracers are active
in a given experiment and check digit match on their concentration values.
A match to near-full machine precision is 15-16 digits; this generally will occur when a similar type of computer,
similar operating system, and similar version of Fortran compiler are used for the test. Otherwise, different round-off can occur,
and due to the chaotic nature of ocean and climate models, fewer digits (typically, 10-13 digits) are matched. A match of 22 digits generally is
due to output being exactly 0.0. In some experiments, some variables may not be used or meaningful, which causes the ‘0’ and ‘4’ match results
in several of the adjustment experiments above.

While the significant digit match for many variables is tested and displayed in ``summary.txt``,
only one of these is used to assess pass/fail (output to the right of the match test results) -- the number bracketed by ``>`` and ``<``.
For example, see above for experiment :filelink:`advect_cs <verification/advect_cs>` the pass/fail test occurs on variable “T: s d”
(i.e., standard deviation of potential temperature), the first variable in the list specified in
:filelink:`verification/advect_cs/input/tr_checklist`. By default (i.e., if no file ``tr_checklist`` is present),
pass/fail is assessed on the cg2d monitor output.
See the :filelink:`testreport <verification/testreport>` script for a list of
permissible variables to test and a guide to their abbreviations. See ``tr_checklist`` files in the input subdirectories of several TESTDIR
experiments (e.g., :filelink:`verification/advect_xz/input/tr_checklist`) for examples of syntax (note, a ``+`` after a variable in a ``tr_checklist file`` is shorthand to compare the
mean, minimum, maximum, and standard deviation for the variable).

.. _reference_output:

Reference Output
~~~~~~~~~~~~~~~~

Reference output is currently generated using the linux server ``villon.mit.edu`` which employs an Intel Haswell processor running Ubuntu 18.04.3 LTS.
For each verification experiment in the MITgcm repository, this reference output is stored in the file ``«TESTDIR»/results/output.txt``,
which is the standard output generated by running :filelink:`testreport <verification/testreport>`
(using a single process) on ``villon.mit.edu`` using the gfortran (`GNU Fortran <https://gcc.gnu.org/fortran>`_) compiler version 7.4.0.

Using a different gfortran version (or a different Fortran compiler entirely), and/or running with MPI,
a different operating system, or a different processor (cpu) type will generally result in output that differs to machine precision.
The greater the number of such differences between your platform and this reference platform, typically the fewer digits of matching output precision.

The do_tst_2+2 utility
----------------------

The shell script :filelink:`tools/do_tst_2+2` can be used to check the accuracy of the restart procedure.
For each experiment that has been run through testreport,
:filelink:`do_tst_2+2 <tools/do_tst_2+2>` executes three additional short runs using the tools/tst2+2 script.
The first run makes use of the pickup files output
from the run executed by :filelink:`testreport <verification/testreport>` to restart and run
for four time steps, writing pickup files upon completion. The second run
is similar except only two time steps are executed, writing pickup files.
The third run restarts from the end of the second run, executing two additional time steps,
writing pickup files upon completion.
In order to successfully pass :filelink:`do_tst_2+2 <tools/do_tst_2+2>`, not only must all three runs execute and complete successfully,
but the pickups generated at the end the first run must be identical to the pickup files from the end of the third run.
Note that a prerequisite to running :filelink:`do_tst_2+2 <tools/do_tst_2+2>`
is running :filelink:`testreport <verification/testreport>`, both to build the executables used by :filelink:`do_tst_2+2 <tools/do_tst_2+2>`,
and to generate the pickup files from which :filelink:`do_tst_2+2 <tools/do_tst_2+2>` begins execution.

The :filelink:`tools/do_tst_2+2` script should be called from the :filelink:`verification/` directory, e.g.:

::

   % cd verification
   % ../tools/do_tst_2+2

The :filelink:`do_tst_2+2 <tools/do_tst_2+2>` script accepts a number of command-line options which can be listed using the
``-help`` option. The most important ones are:

``-t «TESTDIR»``
   Similar to :filelink:`testreport <verification/testreport>` option ``-tdir``, specifies the test directory or list of test directories that should be used.
   If omitted, the test is attempted in all sub-directories.

``-skd «TESTDIR»``
   Similar to :filelink:`testreport <verification/testreport>` option ``-skipdir``, specifies a test directory or list of test directories to skip.

``-mpi``
   Run the tests using MPI; requires the prerequisite :filelink:`testreport <verification/testreport>`
   run to have been executed with the ``-mpi`` or ``-MPI «NUMBER_OF_PROCS»`` flag.
   No argument is necessary, as the :filelink:`do_tst_2+2 <tools/do_tst_2+2>` script will determine the
   correct number of processes to use for your executable.

``-clean``
   Clean up any output generated from the :filelink:`do_tst_2+2 <tools/do_tst_2+2>`.
   This step is necessary if one wants to do additional :filelink:`testreport <verification/testreport>` runs
   from these directories.

Upon completion, :filelink:`do_tst_2+2 <tools/do_tst_2+2>` will generate a file ``tst_2+2_out.txt``
in the :filelink:`verification/` directory which summarizes the results.
The top half of the file includes information from the composite ``summary.txt`` file from the prerequisite :filelink:`testreport <verification/testreport>` run.
In the bottom half, new results from each verification experiment are given:
each line starts with four Y/N indicators indicating if pickups from
the :filelink:`testreport <verification/testreport>` run were available,
and whether runs 1, 2 and 3, completely successfully, respectively,
followed by a pass or fail from the output pickup file comparison test, followed by the TESTDIR experiment name.
In each ``«TESTDIR»/run`` subdirectory
:filelink:`do_tst_2+2 <tools/do_tst_2+2>` also creates a log file ``tst_2+2_out.log`` which contains additional information.
During :filelink:`do_tst_2+2 <tools/do_tst_2+2>` execution a separate directory of  summary information,
including log files for all failed tests, is created in an output directory «rs_NAME_DATE_N»
similar to the syntax for the :filelink:`testreport <verification/testreport>`  output directory name.
Note however this directory is deleted by default
upon :filelink:`do_tst_2+2 <tools/do_tst_2+2>` completion, but can be saved
by adding the :filelink:`do_tst_2+2 <tools/do_tst_2+2>` command line option ``-a NONE``.

Daily Testing of MITgcm
-----------------------

On a daily basis, MITgcm runs a full suite of :filelink:`testreport <verification/testreport>`
(i.e., forward and adjoint runs, single process, single-threaded and mpi) on an array of different
clusters, running using different operating systems, testing several different Fortran compilers.
The reference machine ``villon.mit.edu`` is one of such daily test machines.
When changes in output occur from previous runs, even if as minor as changes
in numeric output to machine precision, MITgcm head developers are automatically notified.

Links to summary results from the daily testing are posted at http://mitgcm.org/public/testing.html.

Required Testing for MITgcm Code Contributors
---------------------------------------------

Using testreport to check your new code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before submitting your pull request for approval, if you have made any changes to MITgcm code, however trivial, you **MUST** complete the following:

- Run :filelink:`testreport <verification/testreport>` (on all experiments) on an unmodified master branch of MITgcm. We suggest using the ``-devel`` option
  and gfortran (typically installed in most linux environments) although neither is strictly necessary for this test.
  Depending how different your platform
  is from our reference machine setup, typically most tests will pass but some match tests may fail; it is possible one or more experiments might not even
  build or run successfully. But even if there are multiple experiment fails or unsuccessful builds or runs, do not despair, the purpose at this
  stage is simply to generate a reference report on your local platform using the master code.
  It may take one or more hours for :filelink:`testreport <verification/testreport>` to complete.

- Save a copy of this summary output from running  :filelink:`testreport <verification/testreport>` on the mastrer branch: from the verification
  directory, type ``cp tr_out.txt tr_out_master.txt``. The file ``tr_out.txt`` is simply a condensed version of the
  composite ``summary.txt`` file located in the «tr_NAME_DATE_N» directory. Note we are not making this file "git-aware",
  as we have no desire to check this into the repo,
  so we are using an old-fashioned copy to save the output here for later comparison.

- Switch to your pull request branch, and repeat the :filelink:`testreport <verification/testreport>` sequence using the same options.

- From the verification directory, type ``diff tr_out_master.txt tr_out.txt``
  which will report any differences in :filelink:`testreport <verification/testreport>` output from the above tests.
  If no differences occur (other than timestamp-related), see below if you are required
  to do a :filelink:`do_tst_2+2 <tools/do_tst_2+2>` test; otherwise, you
  are clear for submitting your pull request.

Differences might occur due to one or more of the following reasons:

- Your modified code no longer builds properly in one or more experiments. This is likely due to a Fortran syntax error; examine output and log files
  in the failed experiment TESTDIR to identify and fix the problem.

- The run in the modified code branch terminates due to a numerical exception error. This too requires further investigation into the cause of the error,
  and a remedy, before the pull request should be submitted.

- You have made changes which require changes to input parameters
  (e.g., renaming a namelist parameter, changing the units or function of an input parameter, etc.)
  This by definition is a “breaking change”, which must be noted when completing the PR template -- but should not deter you from
  submitting your PR. Ultimately, you and the MITgcm head developers will likely have to make changes to one or more verification experiments, but as a first
  step we will want to review your PR.

- You have made algorithmic changes which change model output in some or all setups; this too is a “breaking change” that should be noted in
  the PR template. As usual recourse, if the PR is accepted, the head developers will re-generate reference output and push to the affected
  ``«TESTDIR»/results/`` directories when the PR is merged.

Most typically, running testreport using a single process is a sufficient test. However, any code changes which call MITgcm
routines (such as :filelink:`eesupp/src/global_sum.F`) employing low-level MPI-directives
should run :filelink:`testreport <verification/testreport>` with the ``-mpi`` option enabled.

Using do_tst_2+2 to check your new code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you make any kind of algorithmic change to the code, or modify anything related to generating or reading pickup files,
you are also required to also complete a :filelink:`do_tst_2+2 <tools/do_tst_2+2>`. Again, run the test on both the unmodified master branch and your
pull request branch (after you have run :filelink:`testreport <verification/testreport>` on both branches).
Verify that the output ``tst_2+2_out.txt`` file is identical between branches, similar to the above procedure for the file ``tr_out.txt``.
If the files differ, attempt to identify and fix what is causing the problem.

Automatic testing with GitHub Actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once your PR is submitted onto GitHub, the continuous integration service
`GitHub Actions <https://docs.github.com/en/actions>`_ runs additional tests on your PR submission.
On the ‘Pull request’ tab in GitHub (https://github.com/MITgcm/MITgcm/pulls), find your pull request; initially you will see a yellow circle
to the right of your PR title, indicating testing in progress. Eventually this will change to a green checkmark (pass) or a red X (fail).
If you get a red X, click the X and then click on ‘Details’ to list specifics tests that failed; these can be clicked to produce a screenshot
with error messages.

Note that `GitHub Actions <https://docs.github.com/en/actions>`_ builds documentation (both html and latex) in addition to code testing, so if you have
introduced syntax errors into the documentation files,
these will be flagged at this stage. Follow the same procedure as above to identify the error messages so the problem(s) can be fixed. Make any
appropriate edits to your pull request, re-``git add`` and re-``git commit`` any newly modified files, re-``git push``. Anytime changes are pushed to the PR,
`GitHub Actions <https://docs.github.com/en/actions>`_ will re-run its tests.

The MITgcm head developers will not review your PR until all `GitHub Actions <https://docs.github.com/en/actions>`_ tests pass.

.. _contrib_manual:

Contributing to the manual
==========================

Whether you are simply correcting typos or describing undocumented packages,
we welcome all contributions to the manual. The following information will
help you make sure that your contribution is consistent with the style of
the MITgcm documentation. (We know that not all of the current documentation
follows these guidelines - we're working on it)

The manual is written in **rst** format, which is short for ReStructuredText
directives. rst offers many wonderful features: it automatically does much of
the formatting for you, it is reasonably well documented on the web (e.g.,
primers available `here <http://www.sphinx-doc.org/en/stable/rest.html>`__ and
`here <http://docutils.sourceforge.net/docs/user/rst/quickref.html>`__), it
can accept raw latex syntax and track equation labelling for you, in addition
to numerous other useful features. On the down side however, it can be very
fussy about formatting, requiring exact spacing and indenting, and seemingly
innocuous things such as blank spaces at ends of lines can wreak havoc. We
suggest looking at the existing rst files in the manual to see exactly how
something is formatted, along with the syntax guidelines specified in this
section, prior to writing and formatting your own manual text.

The manual can be viewed either of two ways: interactively (i.e., web-based),
as hosted by read-the-docs (https://readthedocs.org/),
requiring an html format build, or downloaded as a pdf file.
When you have completed your documentation edits, you should double
check both versions are to your satisfaction, particularly noting that
figure sizing and placement may be rendered differently in the pdf build.
See :ref:`building_the_manual` for detail.

Section headings
----------------

- Chapter headings - these are the main headings with integer numbers - underlined with ``****``
- section headings - headings with number format X.Y - underlined with ``====``
- Subsection headings - headings with number format X.Y.Z - underlined with ``----``
- Subsubsection headings - headings with number format X.Y.Z.A - underlined with ``~~~~``
- Paragraph headings - headings with no numbers - underlined with ``^^^^``

N.B. all underlinings should be the same length as the heading. If they are too short an error will be produced.

.. _referencing:

Internal document references
----------------------------

rst allows internal referencing of figures, tables, section headings, and
equations, i.e. clickable links that bring the reader to the respective
figure etc. in the manual.
To be referenced, a unique label is required. To reference figures, tables, or section headings by number,
the rst (inline) directive is ``:numref:`«LABELNAME»```. For example,
this syntax would write out ``Figure XX`` on a line (assuming «LABELNAME» referred to a figure),
and when clicked, would relocate your position
in the manual to figure XX.  Section headings can also be referenced
so that the name is written out instead of the section number, instead using this
directive ``:ref:`«LABELNAME»```.

Equation references have a slightly different inline syntax: ``:eq:`«LABELNAME»```
will produce a clickable equation number reference,  surrounded by parentheses.

For instructions how to assign a label to tables and figures, see
:ref:`below <how_to_figures>`. To label a section heading,
labels go above the section heading they refer to, with the format ``.. _«LABELNAME»:``.
Note the necessary leading underscore. You can also place a clickable
link to *any* spot in the text (e.g., mid-section),
using this same syntax to make the label, using the syntax
``:ref:`«SOME TEXT TO CLICK ON» <«LABELNAME»>``` for the link.

Citations
---------

In the text, references should be given using the standard “Author(s) (Year)” shorthand followed by a link
to the full reference in the manual bibliography. This link is accomplished using the syntax
``:cite:`«BIB_REFERENCE»```; this will produce clickable text, usually some variation on the authors’ initials or names, surrounded by brackets.

Full references are specified in the file :filelink:`doc/manual_references.bib`
using standard `BibTeX <http://www.bibtex.org>`_ format.
Even if unfamiliar with `BibTeX <http://www.bibtex.org>`_, it is relatively easy
to add a new reference by simply examining other entries. Furthermore, most
publishers provide a means to download BibTex formatted references directly from their website.
Note this file is in approximate alphabetic order by author name.
For all new references added to the manual, please include a `DOI <https://www.doi.org>`_ or
a URL in addition to journal name, volume and other
standard reference infomation. An example JGR journal article reference is
reproduced below; note the «BIB_REFERENCE» here is “bryan:79” so the syntax in the rst file format would be ``“Bryan and Lewis (1979) :cite:`bryan:79```,
which will appear in the manual as Bryan and Lewis (1979) :cite:`bryan:79`.

| @Article{bryan:79,
|   author =	 {Bryan, K. and L.J. Lewis},
|   title =	 {A water mass model of the world ocean},
|   journal =	 jgr,
|   volume =	 84,
|   number =       {C5},
|   pages =	 {2503--2517},
|   doi =          {10.1029/JC084iC05p02503},
|   year =	 1979,
| }

Other embedded links
--------------------

**Hyperlinks:** to reference a (clickable) URL, simply enter the full URL.
If you want to have a different,
clickable text link instead of displaying the full URL, the syntax
is ```«CLICKABLE TEXT» <«URL»>`_``  (the ‘<’ and ‘>’ are literal characters,
and note the trailing underscore).
For this kind of link, the clickable text has to be unique for each URL.  If
you would like to use non-unique text (like ‘click here’), you should use
an ‘anonymous reference’ with a double trailing underscore:
```«CLICKABLE TEXT» <«URL»>`__``.

**File references:** to create a link to pull up MITgcm code (or any file in the repo)
in a code browser window, the syntax is ``:filelink:`«PATH/FILENAME»```.
If you want to have a different text link to click on (e.g., say you
didn’t want to display the full path), the syntax is
``:filelink:`«CLICKABLE TEXT» <«PATH/FILENAME»>```
(again, the ‘<‘ and ‘>’ are literal characters). The top
directory here is https://github.com/MITgcm/MITgcm ,
so if for example you wanted to pop open the file
:filelink:`dynamics.F <model/src/dynamics.F>`
from the main model source directory, you would specify
``model/src/dynamics.F`` in place of «PATH/FILENAME».

**Variable references:** to create a link to bring up a webpage
displaying all MITgcm repo references to a particular variable
name (for this purpose we are using the LXR Cross Referencer),
the syntax is ``:varlink:`«NAME_OF_VARIABLE»```. This will work
on CPP options as well as FORTRAN identifiers (e.g., common block
names, subroutine names).

.. _symbolic_notation:

Symbolic Notation
-----------------

Inline math is done with ``:math:`«LATEX_HERE»```

Separate equations, which will be typeset on their own lines, are produced with::

  .. math::
     «LATEX_HERE»
     :label: «EQN_LABEL_HERE»

Labelled separate equations are assigned an equation number, which may be
referenced elsewhere in the document (see :numref:`referencing`). Omitting the ``:label:`` above
will still produce an equation on its own line, except without an equation label.
Note that using latex formatting ``\begin{aligned}`` ...  ``\end{aligned}``
across multiple lines of equations will not work in conjunction with unique
equation labels for each separate line
(any embedded formatting ``&`` characters will cause errors too). Latex alignment
will work however if you assign a single label for the multiple lines of equations.

There is a software tool ‘universal document converter’ named `pandoc <https://pandoc.org/>`_
that we have found helpful in converting raw latex documents
into rst format. To convert a ``.tex`` file into ``.rst``, from a terminal window type:

::

    % pandoc -f latex -t rst -o «OUTPUT_FILENAME».rst «INPUT_FILENAME».tex

Additional conversion options are available, for example if you have your equations or text in another format;
see the `pandoc documentation <https://pandoc.org/MANUAL.html>`_.

Note however we have found that a fair amount of clean-up is still
required after conversion, particularly regarding
latex equations/labels (pandoc has the unfortunate tendency to add
extra spaces, sometimes confusing the rst ``:math:`` directive, other
times creating issues with indentation).

.. _how_to_figures:

Figures
-------

The syntax to insert a figure is as follows::

 .. figure:: «PATHNAME/FILENAME».*
    :width: 80%
    :align: center
    :alt: «TEXT DESCRIPTION OF FIGURE HERE»
    :name: «MY_FIGURE_NAME»

    The figure caption goes here as a single line of text.

``figure::``: The figure file is located in subdirectory ``pathname`` above; in practice, we have located figure files in subdirectories ``figs``
off each manual chapter subdirectory.
The wild-card ``*`` is used here so that different file formats can be used in the build process.
For vector graphic images, save a ``pdf`` for the pdf build plus a ``svg`` file for the html build.
For bitmapped images, ``gif``, ``png``, or ``jpeg`` formats can be used for both builds,
no wild-card necessary, just substitute the actual extension
(see `here <http://www.sphinx-doc.org/en/stable/builders.html>`__ for more info
on compatible formats). [Note: A repository for figure source .eps needs to be created]

``:width:``:  used to scale the size of the figure, here specified as 80% scaling factor
(check sizing in both the pdf and html builds, as you may need to adjust the figure size within the pdf file independently).

``:align:``: can be right, center, or left.

``:name:``  use this name when you refer to the figure in the text, i.e. ``:numref:`«MY_FIGURE_NAME»```.

Note the indentation and line spacing employed above.

Tables
------

There are two syntaxes for tables in reStructuredText. Grid tables are more flexible but cumbersome to create. Simple
tables are easy to create but limited (no row spans, etc.).  The raw rst syntax is shown first, then the output.

Grid Table Example:

::

    +------------+------------+-----------+
    | Header 1   | Header 2   | Header 3  |
    +============+============+===========+
    | body row 1 | column 2   | column 3  |
    +------------+------------+-----------+
    | body row 2 | Cells may span columns.|
    +------------+------------+-----------+
    | body row 3 | Cells may  | - Cells   |
    +------------+ span rows. | - contain |
    | body row 4 |            | - blocks. |
    +------------+------------+-----------+

+------------+------------+-----------+
| Header 1   | Header 2   | Header 3  |
+============+============+===========+
| body row 1 | column 2   | column 3  |
+------------+------------+-----------+
| body row 2 | Cells may span columns.|
+------------+------------+-----------+
| body row 3 | Cells may  | - Cells   |
+------------+ span rows. | - contain |
| body row 4 |            | - blocks. |
+------------+------------+-----------+

Simple Table Example:

::

    =====  =====  ======
       Inputs     Output
    ------------  ------
      A      B    A or B
    =====  =====  ======
    False  False  False
    True   False  True
    False  True   True
    True   True   True
    =====  =====  ======

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
False  False  False
True   False  True
False  True   True
True   True   True
=====  =====  ======

Note that the spacing of your tables in your ``.rst`` file(s) will not match the generated output; rather,
when you build the final output, the rst builder (Sphinx) will determine how wide the columns need to be and space them appropriately.

Other text blocks
-----------------

Conventionally, we have used the rst ‘inline literal’ syntax around any literal computer text (commands, labels, literal computer syntax etc.)
Surrounding text with double back-quotes `````` results in output html ``like this``.

To set several lines apart in an whitespace box, e.g. useful for showing lines in from a terminal session, rst uses ``::`` to set off a ‘literal block’.
For example::

   ::

       % unix_command_foo
       % unix_command_fum

(note the ``::`` would not appear in the output html or pdf) A splashier way to outline a block, including a box label,
is to employ what is termed in rst as an ‘admonition block’.
In the manual these are used to show calling trees and for describing subroutine inputs and outputs. An example of
a subroutine input/output block is as follows:

.. admonition:: This is an admonition block showing subroutine in/out syntax
   :class: note

   |   .. admonition:: :filelink:`SUBROUTINE_NAME </model/src/subroutine_name.F>`
   |     :class: note
   |
   |     | :math:`var1` : **VAR1** ( :filelink:`WHERE_VAR1_DEFINED.h </model/inc/where_var1_defined.h>`)
   |     | :math:`var2` : **VAR1** ( :filelink:`WHERE_VAR2_DEFINED.h </model/inc/where_var2_defined.h>` )
   |     | :math:`var3` : **VAR1** ( :filelink:`WHERE_VAR3_DEFINED.h </model/inc/where_var3_defined.h>` )

An example of a subroutine in/out admonition box in the documentation is :ref:`here <correction_step_sr_in-out>`.

An example of a calling tree in the documentation is :ref:`here <call-tree-press-meth>`.

To show text from a separate file (e.g., to show lines of code, show comments from a Fortran file, show a parameter file etc.),
use the ``literalinclude`` directive. Example usage is shown here:

   ::

        .. literalinclude:: «FILE_TO_SHOW»
            :start-at: String indicating where to start grabbing text
            :end-at: String indicating where to stop grabbing text

Unlike the ``:filelink:`` and ``:varlink:`` directives, which assume a file path starting at the top of the MITgcm repository,
one must specify the path relative to the current directory of the file (for example, from the doc directory, it would require
``../../`` at the start of the file path to specify the base directory of the MITgcm repository).
Note one can instead use ``:start-after:`` and ``:end-before:`` to get text from the file between (not including) those lines.
If one omits the ``start-at`` or ``start-after``, etc. options the whole file is shown.
More details for this directive can be found `here <http://www.sphinx-doc.org/en/stable/markup/code.html#directive-literalinclude>`__.
Example usage in this documentation is :ref:`here <model_main_call_tree>`,
where the lines to generate this are:

   ::

        .. literalinclude:: ../../model/src/the_model_main.F
            :start-at: C Invocation from WRAPPER level...
            :end-at: C    |                 :: events.

.. _subsec_manual_style_guide:

Other style conventions
-----------------------

Units should be typeset in normal text, with a space between a numeric value and the unit, and exponents added with the ``:sup:`` command.

::

  9.8 m/s\ :sup:`2`

will produce 9.8 m/s\ :sup:`2`. If the exponent is negative use two dashes ``--`` to make the minus sign sufficiently long.
The backslash removes the space between the unit and the exponent. Similarly, for subscripts the command is ``:sub:``.

Alternatively, latex ``:math:`` directives (see :ref:`above <symbolic_notation>`) may also be used to display units, using the ``\text{}`` syntax to display non-italic characters.

Line length: as recommended in the
`sphinx style guide <https://documentation-style-guide-sphinx.readthedocs.io/en/latest/style-guide.html#line-length>`_,
lines of raw rst text should be kept to fewer than 80 characters (this
restriction does not apply to tables, URLs, etc. where a line break might
cause difficulty).

- Todo: determine how to break up sections into smaller files

.. _building_the_manual:

Building the manual
-------------------

Once you've made your changes to the manual, you should build it locally to
verify that it works as expected.  To do this you will need a working python
installation with the following packages installed:

 - sphinx
 - sphinxcontrib-bibtex
 - sphinxcontrib-programoutput
 - sphinx_rtd_theme
 - numpy

These packages can be installed from the Python Package Index using pip. If you
have an existing python installation using `Anaconda
<https://www.anaconda.com/>`_ or one of its variants (e.g., `miniconda
<https://docs.conda.io/en/latest/miniconda.html>`_), we recommend that you can
create (and use) a clean environment with the required packages like this:

::

   cd MITgcm
   conda create --name mitgcm_build_the_docs --channel conda-forge --file doc/requirements.txt
   conda activate mitgcm_build_the_docs
   [...] # do the work
   conda deactivate

If you don't yet have a python installation on your computer, we recommend
following the `Anaconda installation procedure
<https://www.anaconda.com/products/individual#Downloads>`_, then following the
recipe above.  You do not need to learn python to build the manual; just note
you should type ``conda activate mitgcm_build_the_docs`` in a shell when
starting up a manual editing session, and ``conda deactivate`` when you finish
(also note you only need to perform the ``conda create ...`` step above when
you **initially** follow the recipe). This will maintain a clean, separate
python `virtual environment
<https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html>`_
for manual compilation and won't interfere with your python setup should you
decide to learn python in the future.

Once these modules are installed you can build the html version of the manual
by running ``make html`` in the ``doc`` directory.

To build the pdf version of the manual you will also need a working version of
LaTeX that includes `several packages
<http://www.sphinx-doc.org/en/master/usage/builders/index.html#sphinx.builders.latex.LaTeXBuilder>`_
that are not always found in minimal LaTeX installations. The command to build
the pdf version is ``make latexpdf``, which should also be run in the ``doc``
directory.

.. _sec_pullreq:

Reviewing pull requests
=======================

The only people with write access to the main repository are a small number of core MITgcm developers. They are the people that
will eventually merge your pull requests. However, before your PR gets merged,
it will undergo the automated testing on Travis-CI, and it will be assessed by the MITgcm community.

**Everyone can review and comment on pull requests.** Even if you are not one of the core developers you can still comment on a pull request.

The simplest way to examine a pull request is to `use GitHub <https://github.com/MITgcm/MITgcm/pulls>`_. You can look at changes made to files
(GitHub will show you a standard linux ``diff`` for each file changed), read though commit messages, and/or peruse any comments
the MITgcm community has made regarding this pull request.

If you are reviewing changes to the documentation, most likely you will also want to review the rendered manual in html format.
While this is not available at GitHub, you can view html builds based on the pull request documentation
using `this link <https://readthedocs.org/projects/mitgcm/builds>`_ at readthedocs.org. Here you will need to click on the appropriate
pull request (as labeled by the pull request number), then click on "View docs"
(not the green button near the top of the page, but the text in the middle of the page
on the right side).

Finally, if you want to test pull requests locally (i.e., to compile or run the code),
you should download the pull request branch. You can do this either by cloning the branch from the pull request.
If you are using ssh keys for command line authentication:

::

    git clone -b «THEIR_DEVELOPMENT_BRANCHNAME» git@github.com:«THEIR_GITHUB_USERNAME»/MITgcm.git

If you are using a personal access token for authentication:

::

    git clone -b «THEIR_DEVELOPMENT_BRANCHNAME» https://github.com/«THEIR_GITHUB_USERNAME»/MITgcm.git

where «THEIR_GITHUB_USERNAME» is replaced by the username of the person proposing the pull request,
and «THEIR_DEVELOPMENT_BRANCHNAME» is the branch from their pull request.

Alternatively, you can add the repository of the user proposing the pull request as a remote to
your existing local repository. Navigate to your local repository and type

::

    git remote add «THEIR_GITHUB_USERNAME» https://github.com/«THEIR_GITHUB_USERNAME»/MITgcm.git

where «THEIR_GITHUB_USERNAME» is replaced by the user name of the person who has made the
pull request. Then download their pull request changes

::

    git fetch «THEIR_GITHUB_USERNAME»

and switch to the desired branch

::

    git checkout --track «THEIR_GITHUB_USERNAME»/«THEIR_DEVELOPMENT_BRANCHNAME»

You now have a local copy of the code from the pull request and can run tests locally.
If you have write access to the main repository you can push fixes or changes directly
to the pull request.

None of these steps, apart from pushing fixes back to the pull request, require
write access to either the main repository or the repository of the person proposing
the pull request. This means that anyone can review pull requests. However, unless
you are one of the core developers you won't be able to directly push changes. You
will instead have to make a comment describing any problems you find.
