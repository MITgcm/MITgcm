function [tlev] = find_all_iters(fall, vname)

% Function [tlev] = find_all_iters(fall)
%
% INPUTS
%   fall     either a string containing a file name
%              or a cell array of file names
%   vanme    variable name for locating iters (DEF: 'iter')
%
% OUTPUTS
%   tlev     struct with a single field (tlev.iters) containing 
%              an array of all model iteration values found 
%              across all specified files
%
%
%  Ed Hill
if nargin < 2 || isempty(vname)
  vname = 'iter';
end

tlev.iters = [];
for ii = 1:length(fall)
  nc = netcdf(fall{ii},'read');
  tlev.iters = union(tlev.iters, nc{vname}(:));
  nc = close(nc);
end
tlev.iters = sort(tlev.iters);
