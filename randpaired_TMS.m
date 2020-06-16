function randpaired_TMS(subID)

% overwrit check
if ~exist('subID','var')
    subID=99;
end
fileName=['randpaired_TMS_' num2str(subID) '.txt'];
if exist(fileName, 'file')
    resp=input(['the file ' fileName 'already exists. overwrite it? [Type ok for overwrite]'], 's');
    if ~strcmp(resp, 'ok')
        disp('experiment aborted')
        return
    end
end 

% Paramater of trial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% stimuli onset after
soa=[0, 0.003, 0.010];

% number of condition * number of loop = number of trial
tloop=15;
ntrain=length(soa)*tloop;

% trial order
rng('shuffle');
repcond=repmat(1:length(soa), 1, tloop);
repind=randperm(ntrain);
torder=repcond(repind);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make results table
resultsHeaders={'trial_No', 'soa'};
results=NaN*ones(ntrain, length(resultsHeaders));

try
% initialize
ListenChar(2);
HideCursor;
keyIsDown=0;
config_io;
outp(54240, 0); 

% priority change
Priority(2);

WaitSecs(1);
disp('waiting key input')
KbWait(0);

% trial loop %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

WaitSecs(1);

for i=1:ntrain
    % initialize
    outp(53240, 0); 

    % TTL output
    if soa(torder(i)) ~= 0
         outp(53240, 1);
         WaitSecs(0.0001);
         outp(53240, 0)
    end

    WaitSecs(soa(torder(i)) - 0.001);
    
         outp(53240, 2);
         WaitSecs(0.0001);
         outp(53240, 0)
    
    disp(i)
    disp(soa(torder(i)))
    
    
    %write results
    results(i,:)=[i, soa(torder(i))*1000];
    
    WaitSecs(7);
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% output text file
dlmwrite(fileName, results, 'delimiter', ',', 'precision', 6);


%終了処理
Priority(0);
Screen('CloseAll');
ShowCursor;
ListenChar(0);

catch % 正常に終了した場合は、catch以下は実行されません。
Screen('CloseAll');
outp(54240, 0); 
ShowCursor;
ListenChar(0);
Priority(0);
psychrethrow(psychlasterror);
end