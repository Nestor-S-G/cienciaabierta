%  *********************************************************************
%
%  peqMain.m
%
%  Main program for "Information-Constrained State-Dependent Pricing"
%      by Michael Woodford
%
%      Runs algorithm and saves results for a single parameter calibration
%      (kappa, theta, siga). Used as standalone, or called by peqMainAll.
%
%      Requires CompEcon Toolbox
%
%  *********************************************************************

% If 'baseline' variable doesn't exist, running peqMain as standalone:
if ~exist('baseline', 'var')
    clear
    fprintf('\nICSDP: Partial Equilibrium with Idiosyncratic Shocks\n');
    fprintf(datestr(today));
    fprintf('\nRunning single parameter calibration...\n\n');
end

tic
format compact

% Add COMPECON toolbox:
cepath='compecon/'; path([cepath 'CEtools'],path);
path([cepath 'CEdemos'],path)


% Set initial values and parameters, grids, shock processes, and
% history of shocks to be used in the simulation:
peqParam


% Solve for firm's optimal policy and resulting average frequency of price
% changes:
peqPolicy


% Compute and store statistics:
peqStat


% Generate plots only if running single calibration:
if ~exist('baseline', 'var')
    peqPlot
    fprintf('\nAll done!\n');
else
    fprintf('\nTo generate plots, load %s as standalone in peqPlot, or as part of peqPlotAll.\n',fileout);
end


% Save results:
save(fileout);

toc

