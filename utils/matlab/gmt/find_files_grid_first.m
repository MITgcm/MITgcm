function [flist] = find_files_grid_first(fpat)

% Function [flist] = find_files_grid_first(fpat)
%
% INPUTS
%   fpat     either a string containing a file pattern
%              (eg. 'state.*.nc) or a cell array of file patterns
%
% OUTPUTS
%   flist    cell array of file names
%
%
%  Ed Hill
files = {};
fdirs = {};
if ischar(fpat)
  tmp = fpat;
  fpat = {};
  fpat = { tmp };
end
for ip = 1:length(fpat)
  d = dir(fpat{ip});
  r = regexp(fpat{ip},'^(?<dirname>.*/)[^/]+$','names');
  for i = 1:length(d)
    if (not(d(i).isdir))
      if (not(isempty(r)))
        fdirs{end+1} = r.dirname;
      else
        fdirs{end+1} = '';
      end
      files{end+1} = d(i).name;
    end
  end
end
[fall,iu] = unique(files);
dall = {};
for i = 1:length(iu)
  dall{end+1} = fdirs{iu(i)};
end

%  Order all the files with any grid files first and the rest
%  alphabetically

fordered = {};
notg = [];
for i = 1:length(fall)
  if strncmp(fall{i},'grid.',5)
    fordered{end+1} = [ dall{i} fall{i} ];
  else
    notg = [ notg ; i ];
  end
end

for i = 1:length(notg)
  fordered{end+1} = [ dall{notg(i)} fall{notg(i)} ];
end

flist = fordered;

