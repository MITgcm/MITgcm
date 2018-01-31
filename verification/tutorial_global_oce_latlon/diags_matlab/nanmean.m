function y = nanmean(x)
%NANMEAN Average or mean ignoring NaNs.
%   NANMEAN(X) returns the average treating NaNs as missing values.  
%   For vectors, NANMEAN(X) is the mean value of the non-NaN
%   elements in X.  For matrices, NANMEAN(X) is a row vector
%   containing the mean value of each column, ignoring NaNs.
%
%   See also NANMEDIAN, NANSTD, NANMIN, NANMAX, NANSUM.

%   Copyright 1993-2000 The MathWorks, Inc. 
%
if isempty(x) % Check for empty input.
    y = NaN;
    return
end

% Replace NaNs with zeros.
nans = isnan(x);
i = find(nans);
x(i) = zeros(size(i));

if min(size(x))==1,
  count = length(x)-sum(nans);
else
  count = size(x,1)-sum(nans);
end

% Protect against a column of all NaNs
i = find(count==0);
count(i) = ones(size(i));
y = sum(x)./count;
y(i) = i + NaN;
