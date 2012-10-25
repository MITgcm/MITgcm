import string
__doc__ = """
label = iolabel(itracer)

    maps integers 1..3843 to length-two strings:

    1..99      =>  01..99
    100..619   =>  0a..0Z,1a..9Z
    620..3843  =>  aa..ZZ

itracer = iolabel2num(label)

    does the inverse.
"""

_iolabel_set10 = string.digits
_iolabel_set52 = string.ascii_letters
_iolabel_set62 = _iolabel_set10 + _iolabel_set52

def iolabel(i):
    ''' Map tracer number (1..3843) to 2-character I/O label. '''
    if i < 100:
        return '{0:02d}'.format(i)
    elif i < 620:  # 100 + 10*52
        a,b = divmod(i-100, 52)
        return '{0:d}{1:s}'.format(a,_iolabel_set52[b])
    elif i < 3844:  # 100 + 10*52 + 52*62
        a,b = divmod(i-620, 62)
        return '{0:s}{1:s}'.format(_iolabel_set52[a],
                                   _iolabel_set62[b])
    else:
        raise ValueError('Tracer numbers > 3883 not supported.')


def iolabel2num(s):
    ''' Map 2-character IO label to tracer number '''
    assert len(s) == 2
    try:
        i = int(s)
    except ValueError:
        try:
            i1 = int(s[0])
        except ValueError:
            i1 = _iolabel_set52.index(s[0])
            i2 = _iolabel_set62.index(s[1])
            i = 620 + i1*62 + i2
        else:
            i2 = _iolabel_set52.index(s[1])
            i = 100 + i1*52 + i2
    return i


