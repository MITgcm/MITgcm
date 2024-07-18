"""
Sphinx extension for the MITgcm documentation

Provides two custom roles:

:filelink:`path/to/file`
  inserts a link to the file on github.  The full path will be shown unless an
  explicit title is given as in :filelink:`title <path/to/file>` or the path is
  prefixed with a tilde, :filelink:`~path/to/file`, in which case only the last
  path component (file) is shown.

:varlink:`identifier`
  inserts a link to a code search based on lxr.

Slightly modified from http://protips.readthedocs.io/link-roles.html.
"""

from docutils import nodes, utils

from sphinx.util.nodes import split_explicit_title


def setup(app):
    app.add_role(
        'filelink',
        filelink('https://github.com/MITgcm/MITgcm/blob/master/%s'))
    app.add_role(
        'varlink',
        autolink('http://lxr.mitgcm.org/lxr2/ident/MITgcm?_i=%s'))

def filelink(pattern):
    """
    Return a role processor for external links to files based on a URL pattern.

    %s in *pattern* will be replaced by the role text, a file path, with two
    modifications:

    - an explicit title can be specified as for internal sphinx cross
      references: :role:`title <target>` will display *title* but link to a URL
      formed from *target*.
    - if the role text is prefixed with ~, only the last path component will be
      displayed: :role:`~path/to/file` will display *file* but link to
      path/to/file.
    """
    def role(name, rawtext, text, lineno, inliner, options={}, content=[]):
        # extract explicit title if provided
        text = utils.unescape(text)
        has_explicit_title, title, part = split_explicit_title(text)
        # if not, check for initial tilde
        if not has_explicit_title and part[:1] == '~':
            # remove it
            part = part[1:]
            # and extract last path component
            title = part.split('/')[-1]
        # form link target
        url = pattern % (part,)
        # make link node
        node = nodes.reference(rawtext, title, refuri=url, **options)
        # and return it (no messages)
        return [node], []
    return role

def autolink(pattern):
    """
    Return a role processor for external links based on a URL pattern.

    %s in *pattern* will be replaced by the role text.
    """
    def role(name, rawtext, text, lineno, inliner, options={}, content=[]):
        url = pattern % (text,)
        node = nodes.reference(rawtext, text, refuri=url, **options)
        return [node], []
    return role
