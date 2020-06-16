function checkpassed = EMGcheck( fileind, threshold,timerange, channels )
%BG EMG‰ðÍ—p
%Checkpassed=1:Under threshold, Checkpassed=0:over threshold 

rawresults = dircat2txt(fileind);

maxval = max(rawresults(timerange(1):timerange(2),:));
minval = min(rawresults(timerange(1):timerange(2),:));
absval = maxval-minval;
checkpassed = NaN(1, length(absval));

for i=1:length(checkpassed)
    if absval(i)>threshold
        checkpassed(i)=0;
    else
        checkpassed(i)=1;
    end
end

totaltraials = size(rawresults, 2);
rejectedtrials = totaltraials-sum(checkpassed);

checkpassed = reshape(checkpassed, channels,size(rawresults,2)/channels);
dlmwrite('EMGcheckpassind.txt', checkpassed)
totaltraials = size(rawresults, 2);

report = {'rejected ' num2str(rejectedtrials) ' of total ' num2str(totaltraials) ' trials'};
dlmwrite('EMGcheck_log.txt', report)
