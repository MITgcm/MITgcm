function y = nansum(x)
%NANSUM Sum ignoring NaNs.
%   NANSUM(X) returns the sum treating NaNs as missing values.  
%   For vectors, NANSUM(X) is the sum of the non-NaN elements in
%   X. For matrices, NANSUM(X) is a row vector containing the sum 
%   of the non-NaN elements in each column of X. 
%
%    See also NANMEDIAN, NANSTD, NANMIN, NANMAX, NANMEAN.

%   Copyright 1993-2000 The MathWorks, Inc. 
%   $Revision: 1.3 $  $Date: 2006/08/12 20:25:13 $
%
% $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_latlon/diags_matlab/nansum.m,v 1.3 2006/08/12 20:25:13 jmc Exp $
% $Name:  $

% Replace NaNs with zeros.
nans = isnan(x);
i = find(nans);
x(i) = zeros(size(i));

% Protect against an entire column of NaNs
y = sum(x);
i = find(all(nans));
y(i) = i + NaN;

