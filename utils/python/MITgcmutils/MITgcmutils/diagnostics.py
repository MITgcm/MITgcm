import re
import numpy as np

nstats = 5

def readstats(fname):
    '''
    locals,totals,itrs = readstats(fname)

    Read a diagstats text file into record arrays (or dictionaries).

    Parameters
    ----------
    fname : string
        name of diagstats file to read

    Returns
    -------
    locals : record array or dict of arrays
        local statistics, shape (len(itrs), Nr, 5)
    totals : record array or dict of arrays
        column integrals, shape (len(itrs), 5)
    itrs : list of int
        iteration numbers found in the file

    Notes
    -----
    - The 5 columns of the resulting arrays are average, std.dev, min, max and total volume.
    - There is a record (or dictionary key) for each field found in the file.

    '''
    flds = []
    with open(fname) as f:
        for line in f:
            if line.startswith('# end of header'):
                break

            m = re.match(r'^# ([^:]*) *: *(.*)$', line.rstrip())
            if m:
                var,val = m.groups()
                if var.startswith('Fields'):
                    flds = val.split()

        res = dict((fld,[]) for fld in flds)
        itrs = dict((fld,[]) for fld in flds)

        for line in f:
            if line.strip() == '':
                continue

            if line.startswith('# records'):
                break

            m = re.match(r' field : *([^ ]*) *; Iter = *([0-9]*) *; region # *([0-9]*) ; nb\.Lev = *([0-9]*)', line)
            if m:
                fld,itr,reg,nlev = m.groups()
                itrs[fld].append(int(itr))
                tmp = np.zeros((int(nlev)+1,nstats))
                for line in f:
                    if line.startswith(' k'):
                        continue

                    if line.strip() == '':
                        break

                    cols = line.strip().split()
                    k = int(cols[0])
                    tmp[k] = [float(s) for s in cols[1:]]

                res[fld].append(tmp)

            else:
                raise ValueError('readstats: parse error: ' + line)

    try:
        all = np.rec.fromarrays([np.array(res[fld]) for fld in flds], names=flds)
        return all[:,1:],all[:,0],itrs
    except:
        totals = dict((fld,np.array(res[fld])[:,0]) for fld in flds)
        locals = dict((fld,np.array(res[fld])[:,1:]) for fld in flds)
        return locals,totals,itrs

