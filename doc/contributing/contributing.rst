.. _chap_contributing:

Contributing to the MITgcm
**************************

The MITgcm is an open source project that relies on the participation of its users, and we welcome contributions. This chapter sets out how you can contribute to the MITgcm.


Bugs and feature requests
=========================

If you think you've found a bug, the first thing to check that you're using the latest version of the model. If the bug is still in the latest version, then think about how you might fix it and file a ticket in the `GitHub issue tracker <https://github.com/altMITgcm/MITgcm/issues>`_. Please include as much detail as possible. At a minimum your ticket should include:

 - what the bug does;
 - the location of the bug: file name and line number(s); and
 - any suggestions you have for how it might be fixed.

To request a new feature, or guidance on how to implement it yourself, please open a ticket with the following details:
 
 - a clear explanation of what the feature will do; and
 - a summary of the equations to be solved.

.. _using_git_and_github:

Using Git and Github
========================

To contribute to the source code of the model you will need to fork the repository and place a pull request on GitHub. The two following sections describe this process in different levels of detail. If you are unfamiliar with git, you may wish to skip the quickstart guide and use the detailed instructions. All contributions to the source code are expected to conform with the :ref:`sec_code_style_guide`. Contributions to the manual should follow the same procedure and conform with :numref:`contrib_manual`.


Quickstart Guide
----------------

**1.** Fork the project on GitHub (using the fork button).

**2.** Create a local clone (we strongly suggest keeping a separate repository for development work):

::

    % git clone https://github.com/user_name/MITgcm.git

**3.** Move into your local clone directory (cd MITgcm) and and set up a remote that points to the original:

::

    % git remote add upstream https://github.com/altMITgcm/MITgcm.git

**4.** Make a new branch from ``upstream/master`` (name it something appropriate, here we call the new feature branch ``newfeature``) and make edits on this branch:

::

   % git fetch upstream
   % git checkout -b newfeature upstream/master

**5.** When edits are done, do all git add’s and git commit’s. In the commit message, make a succinct (<70 char) summary of your changes. If you need more space to describe your changes, you can leave a blank line and type a longer description, or break your commit into multiple smaller commits. Reference any outstanding issues addressed using the syntax ``#ISSUE_NUMBER``.

**6.** Push the edited branch to the origin remote (i.e. your fork) on GitHub:

::

    % git push -u origin newfeature

**7.** On GitHub, go to your fork and hit the pull request (PR) button, and wait for the MITgcm head developers to review your proposed changes. 
In general the MITgcm code reviewers try to respond to a new PR within
a week. A response may accept the changes, or may request edits and
changes. Occasionally the review team will reject changes that are not
sufficiently aligned with and do not fit with the code structure. The
review team is always happy to discuss their decisions, but wants to
avoid people investing extensive effort in code that has a fundamental
design flaw. The current review team is Jean-Michel Campin, Ed Doddridge, Chris
Hill and Oliver Jahn.

If you want to update your code branch before submitting a PR (or any point in development), follow the recipe below. It will ensure that your GitHub repo stays up to date with the main repository. Note again that your edits should always be to a development branch (here, ``newfeature``), not the master branch.

::

    % git checkout master
    % git pull upstream master
    % git push origin master
    % git checkout newfeature
    % git merge master


If you prefer, you can rebase rather than merge in the final step above; just be careful regarding your rebase syntax!   


Detailed guide for those less familiar with Git and GitHub
----------------------------------------------------------

What is `Git <https://en.wikipedia.org/wiki/Git>`_? Git is a version control software tool used to help coordinate work among the many MITgcm model contributors. Version control is a management system to track changes in code over time, not only facilitating ongoing changes to code, but also as a means to check differences and/or obtain code from any past time in the project history. Without such a tool, keeping track of bug fixes and new features submitted by the global network of MITgcm contributors would be virtually impossible. If you are familiar with the older form of version control used by the MITgcm (CVS), there are many similarities, but we now take advantage of the modern capabilities offered by Git.  

Git itself is open source linux software (typically included with any new linux installation, check with your sys-admin if it seems to be missing) that is necessary for tracking changes in files, etc. through your local computer’s terminal session. All Git-related terminal commands are of the form ``git <arguments>``.  Important functions include syncing or updating your code library, adding files to a collection of files with edits, and commands to “finalize” these changes for sending back to the MITgcm maintainers. There are numerous other Git command-line tools to help along the way (see man pages via ``man git``).

The most common git commands are:

 - ``git clone`` download (clone) a repository to your local machine
 - ``git status`` obtain information about the local git repository
 - ``git diff`` highlight differences between the current version of a file and the version from the most recent commit
 - ``git add`` stage a file, or changes to a file, so that they are ready for ``git commit``
 - ``git commit`` create a commit. A commit is a snapshot of the repository with an associated message that describes the changes.

What is GitHub then? GitHub is a website that has three major purposes: 1) Code Viewer: through your browser, you can view all source code and all changes to such over time; 2) “Pull Requests”: facilitates the process whereby code developers submit changes to the primary MITgcm maintainers; 3) the “Cloud”: GitHub functions as a cloud server to store different copies of the code. The utility of #1 is fairly obvious. For #2 and #3, without GitHub, one might envision making a big tarball of edited files and emailing the maintainers for inclusion in the main repository. Instead, GitHub effectively does something like this for you in a much more elegant way.  Note unlike using (linux terminal command) git, GitHub commands are NOT typed in a terminal, but are typically invoked by hitting a button on the web interface, or clicking on a webpage link etc. To contribute edits to MITgcm, you need to obtain a github account. It’s free; do this first if you don’t have one already. 

Before you start working with git, make sure you identify yourself. From your terminal, type:

::

    % git config --global user.email your_email@example.edu
    % git config --global user.name ‘John Doe’ 

(note the required quotes around your name). You should also personalize your profile associated with your GitHub account.

There are many online tutorials to using Git and GitHub (see for example https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project ); here, we are just communicating the basics necessary to submit code changes to the MITgcm. Spending some time learning the more advanced features of Git will likely pay off in the long run, and not just for MITgcm contributions, as you are likely to encounter it in all sorts of different projects.

To better understand this process, :numref:`git_setup` shows a conceptual map of the Git setup. Note three copies of the code: the main MITgcm repository sourcecode “upstream” (i.e., owned by the MITgcm maintainers) in the GitHub cloud, a copy of the repository “origin” owned by you, also residing in the GitHub cloud, and a local copy on your personal computer or compute cluster (where you intend to compile and run). The Git and GitHub commands to create this setup are explained more fully below.


 .. figure:: figs/git_setup.*
    :width: 70%
    :align: center
    :alt: Conceptual model of GitHub
    :name: git_setup

    A conceptual map of the GitHub setup. Git terminal commands are shown in red, GitHub commands are shown in green.

One other aspect of Git that requires some explanation to the uninitiated: your local linux copy of the code repository can contain different “branches”, each branch being a different copy of the code repository (this can occur in all git-aware directories). When you switch branches, basic unix commands such as ``ls`` or ``cat`` will show a different set of files specific to current branch. In other words, Git interacts with your local file system so that edits or newly created files only appear in the current branch, i.e., such changes do not appear in any other branches. So if you swore you made some changes to a particular file, and now it appears those changes have vanished, first check which branch you are on (``git status`` is a useful command here), all is probably not lost.


A detailed explanation of steps for contributing MITgcm repository edits:

**1.** On GitHub, create a local copy of the repository in your GitHub cloud user space: from the main repository (https://github.com/altMITgcm/MITgcm) hit the **Fork** button.
As mentioned, your GitHub copy “origin” is necessary to streamline the collaborative development process -- you need to create a place for your edits in the GitHub cloud, for developers to peruse.

**2.** Download the code onto your local computer using the git clone command. Even if you previously downloaded the code through a “git-aware” method (i.e., a git clone command, see :numref:`git-aware_download`),
we **STRONGLY SUGGEST** you download a fresh repository, to a separate disk location, for your development work (keeping your research work separate). Type:

::

    % git clone https://github.com/your_github_user_name/MITgcm.git

from your terminal (technically, here you are copying the forked “origin” version from the cloud, not the “upstream” version, but these will be identical at this point).

**3.** Move into the local clone directory on your computer:

::

    % cd MITgcm

We need to set up a remote that points to the main repository:

::

    % git remote add upstream https://github.com/altMITgcm/MITgcm.git

This means that we now have two "remotes" of the project. A remote is just a pointer to a repository not on your computer, i.e., in the GitHub cloud, one pointing to your GitHub user space (“origin”), and this new remote pointing to the original (“upstream”). You can read and write into your "origin" version (since it belongs to you, in the cloud), but not into the "upstream" version. This command just sets up this remote, which is needed in step #4 -- no actual file manipulation is done at this point. If in doubt, the command ``git remote -v`` will list what remotes have been set up.

**4.**  Next make a new branch.

::
  
    % git fetch upstream
    % git checkout -b newfeature upstream/master

You will make edits on this new branch, to keep these new edits completely separate from all files on the master branch. The first command ``git fetch upstream`` makes sure your new branch is the latest code from the main repository; as such, you can redo step 4 at any time to start additional, separate development projects (on a separate, new branch). Note that this second command above not only creates this new branch, which we name ``newfeature``, from the ``upstream/master`` branch, it also switches you onto this newly created branch.  Naming the branch something more descriptive than ‘newfeature’ is helpful. 
 

**5.** Doing stuff! This usually comes in one of three flavors: 

|   i) cosmetic changes, formatting, documentation, etc.; 
|   ii) fixing bug(s), or any change to the code which results in different numerical output; or 
|   iii) adding a feature or new package.
| 
|   To do this you should:


    - edit the relevant file(s) and/or create new files. Refer to :ref:`sec_code_style_guide` for details on expected documentation standards and code style requirements. Of course, changes should be thoroughly tested to ensure they compile and run successfully!
    - type ``git add <FILENAME1> <FILENAME2> ...`` to stage the file(s) ready for a commit command (note both existing and brand new files need to be added). “Stage” effectively means to notify Git of the the list of files you plan to “commit” for changes into the version tracking system. Note you can change other files and NOT have them sent to model developers; only staged files will be sent. You can repeat this ``git add`` command as many times as you like and it will continue to augment the list of files.  ``git diff`` and ``git status`` are useful commands to see what you have done so far.
    - use ``git commit`` to commit the files. This is the first step in bundling a collection of files together to be sent off to the MITgcm maintainers. When you enter this command, an editor window will pop up. On the top line, type a succinct (<70 character) summary of what these changes accomplished. If your commit is non-trivial and additional explanation is required, leave a blank line and then type a longer description of why the action in this commit was appropriate etc. It is good practice to link with known issues using the syntax ``#ISSUE_NUMBER`` in either the summary line or detailed comment. Note that all the changes do not have to be handled in a single commit (i.e. you can git add some files, do a commit, than continue anew by adding different files, do another commit etc.); the ``git commit`` command itself does not (yet) submit anything to maintainers.  
    - if you are fixing a more involved bug or adding a new feature, such that many changes are required, it is preferable to break your contribution into multiple commits (each documented separately) rather than submitting one massive commit; each commit should encompass a single conceptual change to the code base, regardless of how many files it touches. This will allow the MITgcm maintainers to more easily understand your proposed changes and will expedite the review process. 
    - if you make any change to the code, however small, i.e., flavor ii or iii above, we expect you to add your changes to the top of :filelink:`doc/tag-index` (starting at line 4), which is a running history of all development of the MITgcm. Again, be concise, describing your changes in one or several lines of text. We will not accept code changes without this edit.

When your changes are tested and documented, continue on to step #6, but read all of step #6 and #7 before proceeding; you might want to do an optional “bring my development branch up to date” sequence of steps before step #6.

**6.** Now we “push” our modified branch with committed changes onto the origin remote in the GitHub cloud. This effectively updates your GitHub cloud copy of the MITgcm repo to reflect the wonderful changes you are contributing.

::

    % git push -u origin newfeature

Some time might elapse during step #5, as you make and test your edits, during which continuing development occurs in the main MITgcm repository. In contrast with some models that opt for static, major releases, the MITgcm is in a constant state of improvement and development. It is very possible that some of your edits occur to files that have also been modified by others; in fact, it is very likely :filelink:`doc/tag-index` will have been updated in the main repo if even a week has elapsed. Your local clone however will not know anything about any changes that may have occurred to the MITgcm repo in the cloud, which may cause an issue in step #7 below, when one of three things will occur:
 
   - the files you have modified in your development have **NOT** been modified in the main repo during this elapsed time, thus git will have no conflicts in trying to update (i.e. merge) your changes into the main repo.
   - during the elapsed time, the files you have modified have also been edited/updated in the main repo, but you edited different places in these files than those edits to the main repo, such that git is smart enough to be able to merge these edits without conflict.
   - during the elapsed time, the files you have modified have also been edited/updated in the main repo, but git is not smart enough to know how to deal with this conflict (it will notify you of this problem during step #7).

One option is to NOT attempt to bring your development code branch up to date, instead simply proceed with steps #6 and #7 and let the maintainers assess and resolve any conflict(s), should such occur (there is a checkbox ‘Allow edits by maintainers’ that is checked by default when you do step #7). If very little time elapsed during step #5, such conflict is less likely (exception would be to :filelink:`doc/tag-index`, which the maintainers can easily resolve). However, if step #5 takes on the order of months, we do suggest you follow this recipe below to update the code and merge yourself. And/or during the development process, you might have reasons to bring the latest changes in the main repo into your development branch, and thus might opt to follow these same steps.

Development branch code update recipe:

::

    % git checkout master
    % git pull upstream master
    % git push origin master
    % git checkout newfeature
    % git merge master
    
This first command switches you from your development branch to the master branch. The second command above will synchronize your local master branch with the main MITgcm repository master branch (i.e. “pull” any new changes that might have occurred in the upstream repository into your local clone). Note you should not have made any changes to your clone’s master branch; in other words, prior to the pull, master should be a stagnant copy of the code from the day you performed step #1 above. The ``git push`` command does the opposite of pull, so in the third step you are synchronizing your GitHub cloud copy (“origin”) master branch to your local clone’s master branch (which you just updated). Then, switch back to your development branch via the second ``git checkout`` command. Finally, the last command will merge any changes into your development branch. If conflicts occur that git cannot resolve, git will provide you a list of the problematic file names, and in these files, areas of conflict will be demarcated. You will need to edit these files at these problem spots (while removing git’s demarcation text),
then do a ``git add FILENAME`` for each of these files, followed by a final ``git commit`` to finish off the merger. 

Some additional ``git diff`` commands to help sort out file changes, in case you want to assess the scope of development changes, are as follows. ``git diff master upstream/master`` will show you all differences between your local master branch and the main MITgcm repo, i.e., so you can peruse what parallel MITgcm changes have occurred while you were doing your development (this assumes you have not yet updated your clone’s master branch). You can check for differences on individual files via ``git diff master upstream/master  <FILENAME>``. If you want to see all differences in files you have modified during your development, the command is ``git diff master``. Similarly, to see a combined list of both your changes and those occurring to the main repo, ``git diff upstream/master``. 

Aside comment: if you are familiar with git, you might realize there is an alternate way to merge, using the “rebase” syntax. If you know what you are doing, feel free to use this command instead of our suggested merge command above.


**7.** Finally create a “pull request” (a.k.a. “PR”; in other words, you are requesting that the maintainers pull your changes into the main code repository). In GitHub, go to the fork of the project that you made (https://github.com/your_github_user_name/MITgcm.git). There is a button for "Compare and Pull" in your newly created branch. Click the button! Now you can add a final succinct summary description of what you've done in your commit(s), and flag up any issues. The maintainers will now be notified and be able to peruse your changes! In general, the maintainers will try to respond to a new PR within
a week. While the PR remains open, you can go back to step #5 and make additional edits, git adds,
git commits, and then redo step #6; such changes will be added to the PR (and maintainers re-notified), no need to redo step #7. 

Your pull request remains open until either the maintainers fully accept and
merge your code changes into the main repository, or decide to reject your changes
(occasionally, the review team will reject changes that are not
sufficiently aligned with and do not fit with the code structure).
But much more likely than the latter, you will instead be asked to respond to feedback, 
modify your code changes in some way, and/or clean up your code to better satisfy our style requirements, etc.,
and the pull request will remain open instead of outright rejection. 
The review team is always happy to discuss their decisions, but wants to
avoid people investing extensive effort in code that has a fundamental
design flaw. 

It is possible for other users (besides the maintainers) to examine 
or even download your pull request; see :ref:`sec_pullreq`.

The current review team is Jean-Michel Campin, Ed Doddridge, Chris
Hill and Oliver Jahn.

.. _sec_code_style_guide:

Coding style guide
==================

**Detailed instructions or link to be added.**

Automatic testing with Travis-CI
--------------------------------

The MITgcm uses the continuous integration service Travis-CI to test code before it is accepted into the repository. When you submit a pull request your contributions will be automatically tested. However, it is a good idea to test before submitting a pull request, so that you have time to fix any issues that are identified. To do this, you will need to activate Travis-CI for your fork of the repository.

**Detailed instructions or link to be added.**

.. _contrib_manual:

Contributing to the manual
==========================

Whether you are simply correcting typos or describing undocumented packages, we welcome all contributions to the manual. The following information will help you make sure that your contribution is consistent with the style of the MITgcm documentation. (We know that not all of the current documentation follows these guidelines - we're working on it)

The manual is written in **rst** format, which is short for ReStructuredText directives. rst offers many wonderful features: it automatically does much of the formatting for you, it is reasonably well documented on the web (e.g. primers available `here <http://www.sphinx-doc.org/en/stable/rest.html>`_ and
`here <http://docutils.sourceforge.net/docs/user/rst/quickref.html>`_), it can accept raw latex syntax and track equation labelling for you, in addition to numerous other useful features. On the down side however, it can be very fussy about formatting, requiring exact spacing and indenting, and seemingly innocuous things such as blank spaces at ends of lines can wreak havoc. We suggest looking at the existing rst files in the manual to see exactly how something is formatted, along with the syntax guidelines specified in this section, prior to writing and formatting your own manual text.

The manual can be viewed either of two ways: interactively (i.e., web-based), as hosted by read-the-docs (https://readthedocs.org/),
requiring an html format build, or downloaded as a pdf file. 
When you have completed your documentation edits, you should double check both versions are to your satisfaction, particularly noting that figure sizing and placement
may be render differently in the pdf build.

Section headings
----------------

- Chapter headings - these are the main headings with integer numbers - underlined with ``****``
- section headings - headings with number format X.Y - underlined with ``====``
- Subsection headings - headings with number format X.Y.Z - underlined with ``---``
- Subsubsection headings - headings with number format X.Y.Z.A - underlined with ``+++``
- Paragraph headings - headings with no numbers - underlined with ``###``

N.B. all underlinings should be the same length as the heading. If they are too short an error will be produced.

.. _referencing:

Internal document references
----------------------------

rst allows internal referencing of figures, tables, section headings, and equations, i.e. clickable links that bring the reader to the respective figure etc. in the manual.
To be referenced, a unique label is required. To reference figures, tables, or section headings by number,
the rst (inline) directive is ``:numref:`LABELNAME```. For example, this syntax would write out ``Figure XX`` on a line (assuming LABELNAME referred to a figure),
and when clicked, would relocate your position
in the manual to figure XX.  Section headings can also be referenced so that the name is written out instead of the section number, instead using this 
directive ``:ref:`LABELNAME```.

Equation references have a slightly different inline syntax: ``:eq:`LABELNAME``` will produce a clickable equation number reference,  surrounded by parentheses. 

For instructions how to assign a label to tables and figures, see :ref:`below <how_to_figures>`. To label a section heading,
labels go above the section heading they refer to, with the format ``.. _LABELNAME:``.
Note the necessary leading underscore. You can also place a clickable link to *any* spot in the text (e.g., mid-section),
using this same syntax to make the label, and using the syntax
``:ref:`some_text_to_clickon <LABELNAME>``` for the link.

Other embedded links
--------------------

**Hyperlinks:** to reference a (clickable) URL, simply enter the full URL. If you want to have a different,
clickable text link instead of displaying the full URL, the syntax
is ```clickable_text <URL>`_``  (the ‘<’ and ‘>’ are literal characters, and note the trailing underscore). 

**File references:** to create a link to pull up MITgcm code (or any file in the repo) in a code browser window, the syntax is ``:filelink:`path/filename```.
If you want to have a different text link to click on (e.g., say you didn’t want to display the full path), the syntax is ``:filelink:`clickable_text <path/filename>```
(again, the ‘<‘ and ‘>’ are literal characters). The top directory here is https://github.com/altMITgcm/MITgcm ,
so if for example you wanted to pop open the file :filelink:`dynamics.F <model/src/dynamics.F>`
from the main model source directory, you would specify ``model/src/dynamics.F`` in place of path/filename.

**Variable references:** to create a link to bring up a webpage displaying all MITgcm repo references to a particular variable name (for this purpose we are using the LXR Cross Referencer),
the syntax is ``:varlink:`name_of_variable```.

.. _symbolic_notation:

Symbolic Notation
-----------------

Inline math is done with ``:math:`LATEX_HERE```

Separate equations, which will be typeset on their own lines, are produced with::

  .. math::
     LATEX_HERE
     :label: EQN_LABEL_HERE


Labelled separate equations are assigned an equation number, which may be referenced elsewhere in the document (see :numref:`referencing`). Omitting the ``:label:`` above
will still produce an equation on its own line, except without an equation label.
Note that using latex formatting ``\begin{aligned}`` ...  ``\end{aligned}`` across multiple lines of equations will not work in conjunction with unique equation labels for each separate line
(any embedded formatting ``&`` characters will cause errors too). Latex alignment will work however if you assign a single label for the multiple lines of equations.

Discuss conversion of .tex files.

.. _how_to_figures:

Figures
-------

The syntax to insert a figure is as follows::

 .. figure:: pathname/filename.*
    :width: 80%
    :align: center
    :alt: text description of figure here
    :name: myfigure

    The figure caption goes here as a single line of text.

``figure::``: The figure file is located in subdirectory ``pathname`` above; in practice, we have located figure files in subdirectories ``figs``
off each manual chapter subdirectory.
The wild-card is used here so that different file formats can be used in the build process.
For vector graphic images, save a ``pdf`` for the pdf build plus a ``svg`` file for the html build. 
For bitmapped images, ``gif``, ``png``, or ``jpeg`` formats can be used for both builds, no wild-card necessary
(see `here <http://www.sphinx-doc.org/en/stable/builders.html>`_ for more info
on compatible formats).

``:width:``:  used to scale the size of the figure, here specified as 80% scaling factor
(check sizing in both the pdf and html builds, as you may need to adjust the figure size within the pdf file independently).

``:align:``: can be right, center, or left.

``:name:``  use this name when you refer to the figure in the text, i.e. ``:numref:`myfigure```.

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

To set several lines apart in an whitespace box, e.g. useful for showing lines in from a terminal session, rst uses ``::`` to set off a ‘literal block’.
For example::

   ::

       % unix_command_foo
       % unix_command_fum 



(note the ``::`` would not appear in the output html) A splashier way to outline a block, including a box label,
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



.. _subsec_manual_style_guide:


Other style conventions
-----------------------

Units should be typeset in normal text, with a space between a numeric value and the unit, and exponents added with the ``:sup:`` command. 

::

  9.8 m/s\ :sup:`2`

will produce 9.8 m/s\ :sup:`2`. If the exponent is negative use two dashes ``--`` to make the minus sign sufficiently long.
The backslash removes the space between the unit and the exponent.

Alternatively, latex ``:math:`` directives (see :ref:`above <symbolic_notation>`) may also be used to display units, using the ``\text{}`` syntax to display non-italic characters.


- double quotes for inline literal computer command, variables, syntax etc.

- discuss how to break up sections into smaller files

- discuss | lines

Building the manual
-------------------

Once you've made your changes to the manual, you should build it locally to verify that it works as expected. To do this you will need a working python installation with the following modules installed (use :code:`pip install MODULE` in the terminal):

 - sphinx
 - sphinxcontrib-bibtex
 - sphinx_rtd_theme

Then, run :code:`make html` in the :code:`docs` directory.



.. _sec_pullreq:

Reviewing pull requests
=======================

The only people with write access to the main repository are a small number of core MITgcm developers. They are the people that will eventually merge your pull requests. However, before your PR gets merged, it will undergo the automated testing on Travis-CI, and it will be assessed by the MITgcm community.

**Everyone can review and comment on pull requests.** Even if you are not one of the core developers you can still comment on a pull request.

To test pull requests locally you should download the pull request branch. You can do this either by cloning the branch from the pull request:

::
    
    git clone -b BRANCHNAME https://github.com/USERNAME/MITgcm.git

where `USERNAME` is replaced by the username of the person proposing the pull request, and `BRANCHNAME` is the branch from the pull request.

Alternatively, you can add the repository of the user proposing the pull request as a remote to your existing local repository. Move directories in to your local repository and then

::
    
    git remote add USERNAME https://github.com/USERNAME/MITgcm.git

where USERNAME is replaced by the user name of the person who has made the pull request. Then download the branch from the pull request 

::
    
    git fetch USERNAME 

and switch to the desired branch

::
    
    git checkout --track USERNAME/foo


You now have a local copy of the code from the pull request and can run tests locally. If you have write access to the main repository you can push fixes or changes directly to the pull request.

None of these steps, apart from pushing fixes back to the pull request, require write access to either the main repository or the repository of the person proposing the pull request. This means that anyone can review pull requests. However, unless you are one of the core developers you won't be able to directly push changes. You will instead have to make a comment describing any problems you find.
