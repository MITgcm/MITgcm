function mm=mmax(a)
% function MMAX(A)
% Return maximum value of matrix A
%
% See also MINMAX MMIN MMEAN

% D. Menemenlis (dimitri@ocean.mit.edu), 21 aug 94

ix=find(~isnan(a));
mm=max(a(ix));
