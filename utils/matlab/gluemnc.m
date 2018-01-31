function foo = gluemnc(diags,nIter0)
% gluemnc.m
% written by david wang, ldeo
%
% purpose: use mnc_assembly.m to glue the multi-tile *.*.nc mnc output 
%	   into a single "global" nc file, which is subject to further 
%	   manipulation (e.g., adding/modifying coordiates/attributes)
%	   if necessary. 
%
%	   diags:  diagnostics name
%          nIter0: 10-digit iterate #
% EXAMPLE:
%	   foo = gluemnc('state','0000000000');

if nargin ~= 2, error('there have to be two input arguments!'); end

nc_in    = [diags,'.',nIter0,'.t%03d.nc'];
nc_inone = [diags,'.',nIter0,'.t001.nc'];
nc_inall = [diags,'.',nIter0,'.t*.nc'];
nc_out   = [diags,'_',nIter0,'.nc'];

varlist = ncload(nc_inone);
nvars =   length(varlist);
vars = struct([]);

for i = 1:nvars,
  vars(i).name = char(varlist(i));
end

[nt,nf] = mnc_assembly(nc_in, vars, nc_out);

reply = input('delete the original tiled files? [y/n] ','s');
if isempty(reply), reply = 'y'; end
if strcmpi(reply,'y'), delete(nc_inall); end

return
