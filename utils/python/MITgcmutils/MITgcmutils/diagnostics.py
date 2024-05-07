import re
import numpy as np

nstats = 5

def readstats(fname):
    '''
    statsPerLayer,statsVertInt,itrs = readstats(fname)

    Read a diagstats text file into record arrays (or dictionaries).

    Parameters
    ----------
    fname : string
        name of diagstats file to read

    Returns
    -------
    statsPerLayer : record array or dict of arrays
        statistics per layer, shape (len(itrs), len(nReg), Nr, 5)
    statsVertInt  : record array or dict of arrays
        column integrals, shape (len(itrs), len(nReg), 5)
    itrs : list of int
        iteration numbers found in the file

    Notes
    -----
    - The 5 columns of the resulting arrays are
      average, std.dev, min, max and total volume.
    - There is a record (or dictionary key) for each field found in the file.
    - Regional axis is omitted if nReg == 1

    '''
    flds = []
    regs = [0]
    with open(fname) as f:
        for line in f:
            if line.startswith('# end of header'):
                break

            m = re.match(r'^# ([^: ]*) *: *(.*)$', line.rstrip())
            if m:
                var,val = m.groups()
                if var.startswith('Fields'):
                    flds.extend(val.split())
                elif var == 'Regions':
                    regs = val.split()

        nreg = len(regs)
        res = dict((fld,[[] for reg in regs]) for fld in flds)
        itrs = dict((fld,[[] for reg in regs]) for fld in flds)

        fieldline = None
        for line in f:
            if line.strip() == '':
                continue

            if line.startswith('# records'):
                break

            if fieldline is not None:
                assert line.startswith(' k')
                # parse field information from saved line and discard 'k' line
                line = fieldline

            m = re.match(r' field : *([^ ]*) *; Iter = *([0-9]*) *; region # *([0-9]*) ; nb\.Lev = *([0-9]*)', line)
            if m:
                fld,itr,reg,nlev = m.groups()
                ireg = regs.index(reg)
                itrs[fld][ireg].append(int(itr))
                tmp = np.zeros((int(nlev)+1,nstats))
                fieldline = None
                kmax = 0
                for line in f:
                    if line.startswith(' k'):
                        continue

                    if line.strip() == '':
                        break

                    if line.startswith(' field :'):
                        fieldline = line
                        break

                    cols = line.strip().split()
                    k = int(cols[0])
                    tmp[k] = [float(s) for s in cols[1:]]
                    kmax = max(kmax, k)

                res[fld][ireg].append(tmp[:kmax+1])
            else:
                raise ValueError('readstats: parse error: ' + line)

    # assume all regions have the same iteration numbers
    for fld in itrs:
        itrs[fld] = itrs[fld][0]

    # if shapes differ between fields, we return dictionaries instead
    # of record array
    asdict = False
    shp = None
    for fld in res:
        res[fld] = np.array(res[fld])
        if nreg == 1:
            # remove region axis
            res[fld].shape = res[fld].shape[1:]
        else:
            # iteration axis first, then region
            res[fld] = res[fld].swapaxes(0,1)

        if res[fld].size == 0:
            # avoid indexing error later
            res[fld].shape = res[fld].shape + (1,5)

        if shp is None:
            shp = res[fld].shape
        else:
            if res[fld].shape != shp:
                asdict = True

    if asdict:
        statsVertInt  = dict((fld,res[fld][...,0,:]) for fld in flds)
        statsPerLayer = dict((fld,res[fld][...,1:,:]) for fld in flds)
    else:
        ra = np.rec.fromarrays([res[fld] for fld in flds], names=flds)
        statsVertInt  = ra[...,0,:]
        statsPerLayer = ra[...,1:,:]

    return statsPerLayer,statsVertInt,itrs
