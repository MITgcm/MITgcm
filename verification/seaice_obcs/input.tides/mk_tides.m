% Generate example tidal input files.

% Tidal input files are real*4 IEEE big-endian binary
% with dimenstion OBlength * tidalComponents,
% where OBlength is the length of the open boundary
% and tidalComponents is the number of tidal components
% specified in OBCS_PARAMS.h.

% OB[N,S,E,W][am,ph]File :: Files with boundary conditions,
%                           the letter combinations mean:
%              N/S/E/W   :: northern/southern/eastern/western boundary
%              am/ph     :: tidal amplitude (m/s) / phase (s)

% Tidal periods are specified using variable tidalPeriod in data.obcs
% Tidal amplitude is the maximum tidal velocity in m/s.
% Tidal phase indicates time in s of maximum positive tide relative
% to model startTime=0.

% readbin.m and writebin.m are in MITgcm/utils/matlab/cs_grid/read_cs

% create tidal input files
nx=10; ny=8;
tidalComponents=10;
for ob={'N','S','E','W'}
    OBlength=ny;
    if any(strcmp(ob,{'N','S'}))
        OBlength=nx;
    end
    for fld={'am','ph'}
        fnm=['OB' ob{1} fld{1} '.seaice_obcs'];
        tmp=randn(OBlength,tidalComponents)/1000;

        % specify (0.1 m/s, 2 hr) for North boundary tidal component 1
        if strcmp(ob,'N')
            if strcmp(fld,'am')
                tmp(:,1) = tmp(:,1) + 0.1;
            else
                tmp(:,1) = tmp(:,1) + 2 * 3600;
            end
        end

        % specify (0.1 m/s, 4 hr) for South boundary tidal component 2
        if strcmp(ob,'S')
            if strcmp(fld,'am')
                tmp(:,2) = tmp(:,2) + 0.1;
            else
                tmp(:,2) = tmp(:,2) + 4 * 3600;
            end
        end

        % specify (0.1 m/s, 6 hr) for East boundary tidal component 3
        if strcmp(ob,'E')
            if strcmp(fld,'am')
                tmp(:,3) = tmp(:,3) + 0.1;
            else
                tmp(:,3) = tmp(:,3) + 6 * 3600;
            end
        end

        % specify (0.1 m/s, 8 hr) for West boundary tidal component 4
        if strcmp(ob,'W')
            if strcmp(fld,'am')
                tmp(:,4) = tmp(:,4) + 0.1;
            else
                tmp(:,4) = tmp(:,4) + 8 * 3600;
            end
        end

        writebin(fnm,tmp)
    end
end
