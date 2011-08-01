% Typometrics
% - intented to quantify and measure user typing behaviour of passwords
% Two Main Factors are consider - Hold Time and Flight Time
% Samir Ahmed - First Started 2010 Dec 27 2010
% Samir Ahmed - Finished 2010 Jan 3rd 2011
% Added Instructions - 2010 Jan 11th 2011


% BSD License
%  
%  Samir Ahmed
%  www.samir-ahmed.com
%  2011
%  
%  In the original BSD license, both occurrences of the phrase "COPYRIGHT HOLDERS AND CONTRIBUTORS" in the disclaimer read "REGENTS AND CONTRIBUTORS".
%  
%  Here is the license template:
%  
%  Copyright (c) 2011, Samir Ahmed
%  All rights reserved.
%  
%  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
%  
%  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
%  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer
%  in the documentation and/or other materials provided with the distribution.
%  
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
%  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
%  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
%  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



%% TypoMetrics MainScreen

function typoMetrics
% Main screen

clear all
close all
clc

%Get Screen Info
screenSize=get(0,'MonitorPositions');
screenSize=[.02*screenSize(3) 0.05*screenSize(4) 0.95*screenSize(3) 0.85*screenSize(4)];
main=figure;
set(main,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'NumberTitle','off','Name','TypoMetrics',...
    'UserData',{-1});

heightVec=linspace(.8,.45,7);
thick=abs(diff(heightVec(1:2)));

% Title
titleBox=uicontrol('Style','Text','Units','Normalized',...
    'Position',[0.01 0.85 0.25 0.125],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','TypoMetrics');

% Compile Button on Main Screen
compileBox=uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(4) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor',[.5 .5 .5],...
    'String','Compile',...
    'Callback',{@compile,main});

% Type Button On Main Screen
saveBox=uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(3) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor',[.5 .5 .5],...
    'String','Save Data',...
    'Callback',{@saveFcn,titleBox,main,compileBox});

% Analysis Button On Main Screen
analysisBox=uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(2) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor',[.5 .5 .5],...
    'String','Analysis',...
    'Callback',{@analyzer,main,titleBox,saveBox});

% Type Button On Main Screen
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(1) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','yellow',...
    'String','Type',...
    'Callback',{@prelim,main,analysisBox});

% Exit Button On Main Screen
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(5) 0.11 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','y',...
    'String','Export',...
    'Callback',{@exportFcn,main});

% Import Button On Main Screen
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.13 heightVec(5) 0.1 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','y',...
    'String','Import',...
    'Callback',{@importFcn,main,compileBox});

% Instructions Button On Main Screen
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(6) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','y',...
    'String','What Is Typometrics?',...
    'Callback',{@whatIsFcn});

% Exit Button On Main Screen
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.03 heightVec(7) 0.2 thick],...
    'FontUnit','Normalized',...
    'FontSize',.5,'FontName','Calibri',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','yellow',...
    'String','Exit',...
    'Callback',{@endsession,main});

for i=1:5
    cbStr=sprintf('@instruct%d',i);
    uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.005 heightVec(i)+.01 0.02 thick-.02],...
    'FontUnit','Normalized',...
    'FontSize',.4,'FontName','Calirbi','FontWeight','bold',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','i',...
    'Callback',{eval(cbStr)});
end

setappdata(main,'Counter',1);
setappdata(main,'CompareMode',false);
end

%% Type Callback

function prelim(~,~,main,analysisBox)
% Preliminary Buffer Window
% This screen is necessary because the collection is timing specific so
% this merely acts as a buffer where all the calculations take places after

% If Analysis Graphs Exists, this will clear them
comparemode=getappdata(main,'CompareMode');
if comparemode==false
    if isappdata(main,'Handles')
        mainH=getappdata(main,'Handles');
        for k=length(mainH):-1:1
            if ishandle(mainH(k))
                delete(mainH(k));
            end
        end
        setappdata(main,'Handles',[])
    end
end
% Getting Screen Info again / Setting up Figure Window...
screenSize=get(0,'MonitorPositions');
screenSize=[0.3*screenSize(3:4) 0.3*screenSize(3) 0.3*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

% Here I choose an arbritary 25 for preallocation of string vector size,
% this number should be large enough to accommodate a password and fit on
% the screen.
box=25;

%Creating an information string;
infoStr=sprintf('INSTRUCTIONS\n\n -Click Start Button \n -Enter A Password, No CapsLock Key Please \n -Hit Enter When Finished');

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.05 0.4 0.9 0.5],...
    'FontUnit','Normalized',...
    'FontSize',.15,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.3],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Start',...
    'Callback',@collect);

% Calls keylog subfunction and then extracts data from figure userdata
    function collect(~,~)
        keylog(box,f1);
        %         set(collectBox,'Visible','on',...
        %             'string','Quit','Position',[0.25 0.05 0.5 0.1],...
        %             'FontSize',.7);
        extractCell=getappdata(f1,'typeData');
        
        % Culling empty zeros and unwanted blanks
        datamat=extractCell{1};
        %         dataStr=deblank(extractCell{2});
        killIndices=(datamat(:,1)==0);
        datamat(killIndices,:)=[];
        dataStr=extractCell{2};
        dataStr(:,killIndices)='';
        disp(datamat)
        disp(dataStr)
        % If press string is not the same as the release string, it is sorted
        if any(dataStr(1,:)~=dataStr(2,:))
            [sortmat sortStr]=sortByPress(datamat,dataStr);
            dataCell={sortmat ; sortStr}';
            if all(sortmat(1)==-1) && strcmp(sortStr,'sortfail')
                dataCell={-1};
            end
        else
            disp('No Sorting Needed')
            dataCell={datamat ; dataStr}';
        end
        
        endsession(main,dataCell,f1)
        disp('--End Log--')
        if comparemode==false
            set(analysisBox,'ForegroundColor','yellow');
        else
            uiresume
        end
        
    end

end

function [dataStr datamat]=keylog(box,f1)
% Inputs are the max char preallocating value Box and buffer figure
% window function handle
% Output are the press and release time stamps and press and release
% character strings

disp('--Start Log--')

% Setting new figure window information ...
screenSize=get(0,'MonitorPositions');
screenSize=[0.3*screenSize(3:4) 0.5*screenSize(3) 0.2*screenSize(4) ];
set(f1,'Visible','Off')
f2=figure;
set(f2,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

% textbox with directions
directBox=uicontrol('Style','text','Units','Normalized',...
    'Position',[0.05 0.35 0.9 0.5],...
    'FontUnit','Normalized',...
    'FontSize',.3,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Type Text and Hit Enter');

% Another text box that looks like a password display will be shown
displayBox = uicontrol('Style','text','Units','Normalized',...
    'Position',[0.05 0.1 0.9 0.5],...
    'FontUnit','Normalized',...
    'FontSize',1,'FontName','Arial',...
    'Backgroundcolor',[.1 .1 .1],...
    'Foregroundcolor','yellow');

% Preallocating / Setting variables for scope accross nested functions

% matrix 1st column stores press time stamps
% - 2nd column release time stamps
datamat=zeros(box,2);

% string matrix - 1st row: character pressed
% - 2nd row: character released
dataStr=[blanks(box); blanks(box)];

% String to show little dots for each character entered
userStr=blanks(box);

% Counters to move through the matrices, press release and string...
pCount=1;
rCount=1;
sCount=1;

% Because KeyReleaseFcn cannot capture modifier values, pmodCell will hold
% the modifier value...
pmodCell=1;

%defining the keypress and keyrelease fcn...
set(f2,'KeyPressFcn',@pressInfo);
set(f2,'KeyReleaseFcn',@releaseInfo);

% Starting the clock, t = 0
tzero=tic;

% Stop iteration of function, will resume when collection of data finishes
uiwait

% pressfunction - stores information to respective places immediately
    function pressInfo(~,pStruct)
        % Immediately captures the time of pressing
        
        tempVar=toc(tzero);
        if ~strcmp(pStruct.Character,char(13))
            datamat(pCount,1) = tempVar;
            if ~isempty(pStruct.Character);
                dataStr(1,pCount)=pStruct.Character;
                userStr(sCount)=char(183);
                % Unnecessary Print line...
                fprintf('Press    %c - %2.5f\n',pStruct.Character,tempVar)
                set(displayBox,'String',userStr);
                sCount=sCount+1;
            else
                % To capture shift/capslock/alt... Matlab gives a cell with the key
                % name in it... they all look unique, so i took the first letter,
                % added enough to push out of the regular ascii values, >128 and
                % then use will convert from the character values later on.
                pmodCell=pStruct.Modifier;
                dataStr(1,pCount)=char(double(pmodCell{1}(1))+31);
                fprintf('Press    %c - %2.5f\n',dataStr(1,pCount),tempVar)
            end
            pCount=pCount+1;
        else
            % fake processing step pauses for 2 minutes
            % incase the enter is released before the last
            % key is it...
            if datamat(rCount,2)==0 && datamat(rCount,1)~=0
                set(directBox,'string','Processing')
                pause(2)
            end
            % Turn on the first figure window and close the current
            endsession(f1,{datamat; dataStr},f2)
            uiresume
        end
        
    end

% release function - stores information to repsective slots immediately
% works just as before
    function releaseInfo(~,rStruct)
        releaseTime=toc(tzero);
        if ~strcmp(rStruct.Character,char(13))
            datamat(rCount,2)=releaseTime;
            if ~isempty(rStruct.Character);
                dataStr(2,rCount)=rStruct.Character;
                fprintf('Release  %c - %2.5f\n',rStruct.Character,releaseTime)
            else
                % Functionality to extract modifier info is not present, so the
                % most recent press function modifier info is copied... works fine
                dataStr(2,rCount)=char(double(pmodCell{1}(1))+31);
                fprintf('Release  %c - %2.5f\n',dataStr(2,rCount),releaseTime)
            end
            rCount=rCount+1;
        end
    end

end

function [mat string] = sortByPress(datamat,dataStr)
%Monster Function
% for sorting the release data via the Press information so that
%the two match up. Commenting excess because this whole function confuses
%me.

% Hypothetical situation, somebody types password. They hold onto the w key
%for too long, i.e we have the press string looking like, password just the
%way it should, but the release string looks like passORWd ...

% I have to account for this by substitution and release time information
% ...

% 1st of all, I finding all the instances where the my two strings do no
% match i.e 'password' and 'passorwd' then I extract the dataStr into two
% string vectors copyVec for the release info, copyVecr for the press
% info... i know the naming is bad...
% dupe is a variable i use for the duplicates, just put it outside the loop
% for scope
mismatch=find((dataStr(1,:)~=dataStr(2,:)));
copyVec = dataStr(2,:);
copyVecp= dataStr(1,:);
dupe=0;

% Now I need a robust method to find any duplicate letters within the
% mismatch, then I must substitute all the possible duplicate with
% different symbols, I choose those characters 160 and up...
if length(unique(dataStr(2,mismatch)))~=length(dataStr(2,mismatch))
    counter = 0;
    for i=1:length(mismatch)
        dupe=find(copyVec(mismatch(i))==copyVec(mismatch));
        
        % If I find a duplicate, length is > 1
        if length(dupe)>1
            extension=length(dupe);
            % This just gives me the indMat, too lazy to make output ...
            % nested function below
            sortByRelease
            % Finding where in the press string I have these letters, so
            % that I can substitute them in the press string too, logical
            % indexing for efficiency
            copyVecp(mismatch(copyVec(mismatch(i))==copyVecp(mismatch)))...
                =char((161+counter):(160+counter+extension));
            
            % Altering changing the order of the press string based on the
            % release information, i.e if the press string was catcatcat and the
            % release string was ctactacta, I use the release time
            % information to distinguish between the duplicated t's and a's
            % then pass that to indMat
            copyVec(mismatch(dupe))=copyVec(mismatch(indMat));
            
            % Substituting the values in the release string ...
            copyVec(mismatch(dupe))=char((161+counter):(160+counter+extension));
            
            % updating my counter ...
            counter=counter+extension;
        end
    end
end
% Just as i made a copy before, I copy the substituted vector to
% copyvec2
copyVec2= copyVec;
% Index vector to move around
indvec=1:length(dataStr(2,:));

%Looping through the release string mismatched terms
%so that I can compare terms and switch them if they
%match with the press string ... until the order is right
for j=1:length(mismatch)-1
    for i=1:length(mismatch)
        % Watching these disp() in the command window shows you how it works
        disp(copyVec); disp(copyVecp);
        if copyVec(mismatch(i))==copyVecp(mismatch(j))
            % If statement to compare, next 3 for switching
            temp=indvec(mismatch(j));
            indvec(mismatch(j))=indvec(mismatch(i));
            indvec(mismatch(i))=temp;
            % Rearranging values with new index vector
            copyVec(:)=copyVec2(indvec);
        end
    end
end
% Finally using the index vector into the original release string
dataStr(2,:)=dataStr(2,indvec);

% Check to make sure release string = press string and then indexing
% the release time data to rearrange correctly
if all(dataStr(2,:)==dataStr(1,:))
    mat = datamat;
    mat(:,2)=datamat(indvec,2);
    string = dataStr;
    disp(mat); disp(string);
    disp('Succesfully Sorted')
else
    
    % incase it doesnt work out...
    mat=-1;
    string='sortfail';
    
    disp('Sorting Failed')
end

    function sortByRelease
        % Nested Function to sort the duplicates by the order they were
        % clicked, therefore (I dont know how one could type like this) theres
        % no chance of mixing up variables when we substitute and then index
        % them.
        copyMat = datamat(dupe,2);
        [~, indMat] = sort(copyMat);
        indMat =dupe(indMat);
    end
end

%%  Analyzer Functions

function analyzer(analysisBox,~,main,~,saveBox)
% Analzer Function
% INPUTS - pushbutton handle, (~eventdata), mainscreen figure handle
% OUTPUTS - no outputs, saved to pushbutton 'analysisBox' userdata property
% This function analyzes the data doing HOLD and FLIGHT stats /
% visualizations...

% Extracts press/release timesstamps and strings
dataCell=getappdata(main,'typeData');
comparemode=getappdata(main,'CompareMode');
% If the data is empty or unsortable - no action... If sucessful continues
if dataCell{1}(1)~=-1 && isempty(get(analysisBox,'Userdata'))
    
    % Putting extracting press / release Time vectors + press char string
    pressVec=dataCell{1}(:,1);
    releaseVec=dataCell{1}(:,2);
    pressStr=dataCell{2}(1,:);
    
    % Looping through to determine the Hold Time for each key
    holdTimeSec=zeros(1,length(pressVec));
    for i=1:length(pressVec)
        holdTimeSec(i)=diff(dataCell{1}(i,:));
    end
    % Multiplying by 1000 to work with milli second
    holdTime=holdTimeSec*1000;
    
    % Determining Flight Time
    flightTimeSec = zeros(1,(length(pressVec)-1));
    for i=2:length(flightTimeSec)+1
        flightTimeSec(i-1)=pressVec(i)-releaseVec(i-1);
    end
    
    % Multiplying by 1000 to work with milli seconds
    flightTime = flightTimeSec*1000;
    
    if comparemode==false
        
        % Making analysis button look inactive / save looks active
        set(analysisBox,'ForeGroundColor',[.5 .5 .5]);
        set(saveBox,'Foregroundcolor','yellow');
        
        % Calling Analysis Functions ; all functions give out handles so they
        % maybe deleted later
        
        % PressCell is each term in a Cell for Tick Label Purposes
        % This function generates a HOLD bar graph
        [pressCell barhfH] = barHoldFlight(pressStr,holdTime,main,flightTime);
        
        % This function displays relevant statistics ?!
        statH = statDisplay(pressVec,releaseVec,pressStr,holdTime,main,...
            pressCell,flightTime);
        
        % Creates the Visualization of the Typing Sequence
        
        visualH = visualTyping(holdTime',flightTime',pressStr,pressCell,main,[]);
        
        % Saving handles to UserData property of the analysis box
        setappdata(main,'Handles',[barhfH statH visualH]);
        
        % Printing HOLD and FLIGHT Data for confirmation / status update
        fprintf('\n--Analysis--\n%7s  %7s\n','Hold','Flight');
        fprintf('%13f  %13f\n',[holdTime [flightTime inf]]);
        
        % Saving Variables for debugging .... might remove later
        setappdata(main,'saveData',{holdTime;flightTime;pressStr;pressCell})
    elseif comparemode==true
        setappdata(main,'testData',{holdTime;flightTime;pressStr});
    end
end

end

function outCell = decodeModifier(modifier)
% This function decodes the pressStr incase they are modifiers present
% Input single character from string
% Output same character, or (shift or ?) depending on the modifier used
if double(modifier)>128 || strcmp(modifier,char(32))
    if strcmp('s',char((double(modifier)-31)))
        outCell='Shift';
    else
        % Shift is the only modifier I know off that can be used, the other
        % is control, MatLab has no functionality for capslock with key
        % press fcn .... this is for space
        outCell='Space';
    end
else
    outCell=modifier;
end
end

function [pressCell barH]=barHoldFlight(pressStr,holdTime,main,flightTime)
% This function generates a bar graph for HOLD/FLIGHT time information
% INPUTS - press character string; HOLD/FLIGHT times; main figure handle;
% OUTPUTS- press characters in cell array; bar graph handle

% Cell arraying each letter of pressStr
pressCell=cell(1,length(pressStr));
for i=1:length(pressStr)
    pressCell{i}=decodeModifier((pressStr(i)));
end

% Setting up Axes
holdAxes=axes('parent',main,'position',[0.31 0.08 0.3 0.3]);

% Bar charting Hold information,
holdBar = bar(holdAxes,holdTime,'FaceColor',[1 0.6471 0],...
    'EdgeColor','none');

% Setting information for asthetics
set(get(holdBar,'BaseLine'),'Color',[.9 .9 .9])
set(gca,'XColor',[.9 .9 .9],'YColor',[.9 .9 .9],'Color',[.3 .3 .3],...
    'FontUnits','Normalized','FontSize',.06,...
    'XTick',1:length(pressCell),'XLim',[0 (length(pressCell)+1)],...
    'XTickLabel',pressCell);
xlabel('Key','Color',[.9 .9 .9]);
ylabel('Time [ms]','Color',[.9 .9 .9]);
title('Hold Times','Color',[.9 .9 .9]);

% Setting up Flight Axes
flightAxes=axes('parent',main,'position',[0.67 0.08 0.3 0.3]);

% Bar charting Flight Information
flightBar = bar(flightAxes,flightTime,'FaceColor',[0.6784 1 0.1843],...
    'EdgeColor','none');

%  Setting information for asthetics ...
set(get(flightBar,'BaseLine'),'Color',[.9 .9 .9])
set(gca,'XColor',[.9 .9 .9],'YColor',[.9 .9 .9],'Color',[.3 .3 .3],...
    'FontUnits','Normalized','FontSize',.06,...
    'XTick',1:length(pressCell),'XLim',[0 (length(pressCell)+1)],...
    'XTickLabel',pressCell);
xlabel('Key','Color',[.9 .9 .9]);
ylabel('Time [ms]','Color',[.9 .9 .9]);
title('Flight Times','Color',[.9 .9 .9]);


% Declaring output handles
barH=[holdAxes flightAxes];

end

function statH=statDisplay(pressVec,releaseVec,...
    pressStr,holdTime,main,pressCell,flightTime)

% Multiple Strings are generated

infoCell=cell(1,9);
% Word Typed
infoCell{1}=sprintf('Word Typed: %s\n',pressStr(double(pressStr)<128));

% Key strokes
infoCell{2}=sprintf('Key Strokes: %2d\n',length(pressVec));

% Total Typing Time
infoCell{3}=sprintf('Total Typing Time : %6.5f s\n\n',...
    range([pressVec ; releaseVec]));

% Longest Held Key
[val, ind]=max(holdTime);
infoCell{4}=sprintf('Longest Hold: %s %7.3f ms\n',...
    (pressCell{ind}),val);

% Shortest Held key
[val, ind]=min(holdTime);
infoCell{5}=sprintf('Shortest Hold: %s %7.3f\n',...
    pressCell{ind},val);

% Average Hold Time
infoCell{6}=sprintf('Mean Hold: %10.2f ms\n\n',...
    mean(holdTime));

% Longest Flight Time
[val, ind]=max(flightTime);
infoCell{7}=sprintf('Longest Flight: %s  %7.3f ms\n',...
    [pressCell{ind} '-' pressCell{ind+1}],val);

% Shortest Flight Time
[val, ind]=min(flightTime);
infoCell{8}=sprintf('Shortest Flight: %s  %7.3f ms\n',...
    [pressCell{ind} '-' pressCell{ind+1}],val);

% Average Flight Time
infoCell{9}=sprintf('Mean Flight: %10.2f ms',...
    mean(flightTime));

% Generating Giant String
infoString=sprintf('STATISTICS\n');
for i=1:length(infoCell)
    infoString=sprintf('%s%s',infoString,infoCell{i});
end
% Creating an Axis
statAxes=axes('parent',main,'position',[0.04 0.07 0.1 0.1],...
    'Units','Normalized','color',[.2 .2 .2],'xcolor',[.2 .2 .2],...
    'ycolor',[.2 .2 .2]);

% Making Text Box
text(-.3,1.5,infoString,'Parent',statAxes,'Color',[.9 .9 .9],...
    'BackgroundColor',[.1 .1 .1],'FontName','Courier New');
axis([0 1 0 1])

%Declaring Output Handles
statH=statAxes;

end

%%  Visualizer Functions

function visualH=visualTyping(hold,flight,pressStr,pressCell,main,stdMat)

[~, x]=size(hold');
% Extracting Color Map Autumn information to get a factor to divide through
colormap(autumn)
cmap=colormap;
colorFactor=floor(length(cmap)/x);

% Setting Axes, Ygrid is kinda ugly
vAxes = axes('parent',main,'Position',[ 0.3 0.5 0.65 0.45],...
    'Color',[.35 .35 .35],...
    'XColor',[.9 .9 .9],...
    'yColor',[.9 .9 .9],...
    'Ytick',1:x);
%     'yGrid','on','GridLineStyle',':',...

% Preallocating cells
flight=[flight; 0];
rect=cell(1,x);
textLabel=rect;
flightLine=cell(1,x-1);
xPosition=0;
% Looping to make a rectangle of height 1; width = HOLD time, starting x
% postion press time.
% Make a text annotation centered in each rectangle
% Callback Function Displays more info ... turns on/ off with click
for i=1:x
    line([0+10 sum([hold' flight'])],...
        [(x-i+1) (x-i+1)],...
        'Color',[.3 .3 .3],'LineWidth',0.1,...
        'parent',vAxes);
    
    rect{i}=rectangle('parent',vAxes,...
        'position',[xPosition (x-i) (hold(i)) 1],...
        'FaceColor',cmap(colorFactor*i,:),...
        'EdgeColor','none',...
        'ButtonDownFcn',{@infoNote,pressCell{i},vAxes,flight(i),stdMat,i});
    
    textLabel{i}=text(mean([xPosition (xPosition+hold(i)-10-10*(length(pressCell{i})))]),...
        x-i+0.5,pressCell{i},...
        'parent',vAxes,...
        'Color',[0 0 0],...
        'FontWeight','bold',...
        'ButtonDownFcn',{@infoNote,pressCell{i},vAxes,flight(i),stdMat,i,rect{i}});
    xPosition=xPosition+hold(i)+flight(i) ;
end
flight(end)=inf;
xPosition=hold(1);
for i=1:x-1
    flightLine{i}=line([xPosition xPosition+flight(i)],...
        [(x-i) (x-i)],...
        'Color',[.9 .9 .9],'Marker','.',...
        'LineWidth',2,'parent',vAxes);
    xPosition=xPosition+hold(i+1)+flight(i) ;
end

% Out of Looping X,Y, Label + Title beautification.
xlabel('Time [ms]','Color',[.9 .9 .9]);
ylabel('Key','Color',[.9 .9 .9]);
maxLim=sum([hold' flight(1:x-1)']);
% depending on when I call the title will be different
if isempty(stdMat)
    titleStr=sprintf('Visualization of Typing : ''%s''',pressStr(double(pressStr)<128));
else
    titleStr=sprintf('Statistical Mean Visualization of Typing : ''%s''',pressStr(double(pressStr)<128));
    setappdata(main,'visualtextHandles',textLabel)
    setappdata(main,'visualrectHandles',rect)
    setappdata(main,'visualFlineHandles',flightLine)
    setappdata(main,'maxLims',maxLim)
end

title(titleStr,'Color',[.9 .9 .9],'FontWeight','bold');


% Set the axis labels (its upsidedown because it loops moving down until 0)
set(vAxes,'YTickLabel',fliplr(pressCell));
% Set the axis limits
axis([0 maxLim 0 x])

% Declare output handle
visualH=vAxes;

end

function infoNote(rH,~,character,axesH,individualFlightTime,stdMat,i,varargin)
% Cool little Function that turns on and off an annotation for each
% rectangle
% INPUT - rectangle buttondown handle, (~eventdata), press character;
% stdMat for something...
% Visualization handle ; varargin (0 or 1) - depending on if user clicks
% on text annotation or rectangle; if exists it holds the information
% originally in rectangle buttondown handle.

% Figuring out which one is the rectangles function handle
% Stealing rectangles position data
if nargin==7
    positionVec=(get(rH,'position'));
else
    positionVec=(get(varargin{1},'position'));
end

openNotes=get(axesH,'UserData');
if ~isempty(openNotes)
    delete(openNotes);
end

% Making a whole bunch of strings
charText=sprintf('Key:%14s\n',character);
startTime=sprintf('Start Time:%8.2f ms\n',positionVec(1));
hTime=sprintf('Hold Time:%9.2f ms\n',positionVec(3));
endTime=sprintf('End Time:%10.2f ms\n',(positionVec(1)+positionVec(3)));
fTime=sprintf(  'Flight Time:%7.2f ms\n',individualFlightTime);
endText=sprintf('\n(click to close)');
if ~isempty(stdMat)
    hTime=sprintf('%sHold Std:%10.2f ms\n',hTime,stdMat(i,1));
    fTime=sprintf('%sFlight Std:%8.2f ms\n',fTime,stdMat(i,2));
end
% Joining the strings
dispStr=sprintf('%s%s%s%s%s%s%s',charText,startTime,...
    endTime,hTime,fTime,endText);

axesPos=get(axesH,'Position');
% Making the text box, buttondownfcn closes the text box
infoNoteH=text((10*axesPos(3)),(2*axesPos(4)),...
    dispStr,'parent',axesH,...
    'BackgroundColor',[.1 .1 .1],'Color',[.9 .9 .9],...
    'ButtonDownFcn',@closetext,'FontSize',9,'FontName','Courier New');

set(axesH,'UserData',infoNoteH)

%just deletes the box
    function closetext(handle,~)
        delete(handle);
        set(axesH,'UserData',[]);
    end
end

function visualEdit(main,visualH,scatterH,x,tHoldMat,tFlightMat)

pressCell=getappdata(main,'pressCell');
stdMat=[];
xPosition=0;

tFlight=[tFlightMat(end,:) 0];
tHold=tHoldMat(end,:);

colormap(autumn)
cmap=(colormap)*.875;
colorFactor=floor(length(cmap)/x);

if isappdata(main,'visualtextHandles')
    vtH=getappdata(main,'visualtextHandles');
    for i=1:x-1
        delete(vtH{i});
    end
    delete(vtH{x});
    rmappdata(main,'visualtextHandles');
end

if isappdata(main,'Visualrect2H')
    vGlH=getappdata(main,'VisualGLinesH');
    vr2H=getappdata(main,'Visualrect2H');
    vrD=getappdata(main,'VisualredDot');
    vF2H=getappdata(main,'VisualFline2H');
    deleteCell={vGlH, vr2H, vrD,vF2H};
    for i=1:length(deleteCell)
        for j=1:length(deleteCell{i})
            if ishandle(deleteCell{i}{j})
                delete(deleteCell{i}{j});
            end
        end
    end
    rmappdata(main,'VisualGLinesH');
    rmappdata(main,'VisualFline2H');
    rmappdata(main,'VisualredDot');
    rmappdata(main,'Visualrect2H');
end

rect2=cell(1,x);
rect=getappdata(main,'visualrectHandles');

for i=1:x
    rect2{i}=rectangle('parent',visualH,...
        'position',[xPosition (x+.5-i) (tHold(i)) .5],...
        'FaceColor',cmap(colorFactor*i,:),...
        'EdgeColor',[.3 .3 .3],...
        'ButtonDownFcn',{@infoNote,pressCell{i},visualH,tFlight(i),stdMat,i});
    positionVec=get(rect{i},'Position');
    positionVec(4)=.5;
    set(rect{i},'Position',positionVec);
    
    xPosition=xPosition+tHold(i)+tFlight(i);
end

counter=0;
linesH=cell(1,x*2);
xLims=getappdata(main,'maxLims');
xLims=[5 max([xLims sum([tFlight tHold])])];
for i=1:x
    for j=1:2
        counter=counter+1;
        linesH{counter}=line(xLims,...
            [(x-i+.5*j) (x-i+.5*j)],...
            'Color',[.3 .3 .3],'LineWidth',1,...
            'parent',visualH);
    end
end

fLines2=cell(1,2*(x-1));
fLines1=getappdata(main,'visualFlineHandles');
xPosition=tHold(1);
for i=1:x-1
    set(fLines1{i},'YData',[(x-i+.25) (x-i+.25)],...
        'Marker','none',...
        'LineStyle',':');
    xdata=get(fLines1{i},'XData');
    line([xdata(2) xdata(2)],[(x-i+.25) (x-i-.75)],...
        'Color',[.9 .9 .9],...
        'LineWidth',2,...
        'LineStyle',':',...
        'parent',visualH);
    fLines2{i}=line([xPosition xPosition+tFlight(i)],...
        [(x-i+.75) (x-i+.75)],...
        'Color',[.9 .9 .9],...
        'LineStyle',':',...
        'LineWidth',2,'parent',visualH);
    fLines2{i+x-1}=line([xPosition+tFlight(i) xPosition+tFlight(i)],...
        [(x-i+.75) (x-i-.25)],...
        'Color',[.9 .9 .9],...
        'LineStyle',':',...
        'LineWidth',2,'parent',visualH);
    
    xPosition=xPosition+tHold(i+1)+tFlight(i);
end
set(visualH,'Xlim',xLims);

redDots=cell(1,x);
for j=1:x-1
    redDots{j}=plot([(j) (j+.5)],[tHold(j) tFlight(j)],'o',...
        'MarkerSize',4,...
        'Color',[1 0 0],...
        'MarkerFaceColor',[0.8235 0 0],...
        'parent',scatterH);
    if j==x-1
        redDots{j+1}=plot((j+1),tHold(j+1),'o',...
            'MarkerSize',4,...
            'Color',[0.8235 0 0],...
            'MarkerFaceColor',[1 0 0],...
            'parent',scatterH);
    end
end

setappdata(main,'VisualGLinesH',linesH)
setappdata(main,'VisualFline2H',fLines2)
setappdata(main,'VisualredDot',redDots)
setappdata(main,'Visualrect2H',rect2)
end

%% Save Function

function saveFcn(saveH,~,~,main,compileBox)
% Function to save holdtime/flighttime to app data
% Inputs - 1,2 pushbutton - then titlebox handle, main figure window handle
% Outputs - None, saves data to appdata of main window, holdmat
% If somebody types in terms and tries to save dissimilar strings, it
% explains that the typed words arent the same in a error figure window

% If there is data in titleBox, we extract the press string to compare it
comparemode=getappdata(main,'CompareMode');
if isappdata(main,'saveData') || isappdata(main,'testData')
    count=getappdata(main,'Counter');
    if comparemode==false
        saveData=getappdata(main,'saveData');
    else
        saveData=getappdata(main,'testData');
    end
    checkStr=getappdata(main,'pressStr');
    if xor(strcmp(saveData{3},checkStr),~isappdata(main,'pressStr'))
        % concatenate holdMat/flightMat
        if comparemode==true
            setappdata(main,'tHoldMat',[getappdata(main,'tHoldMat'); saveData{1}]);
            setappdata(main,'tFlightMat',[getappdata(main,'tFlightMat'); saveData{2}]);
            
            % If this is the first time, we save pressStr and pressCell too
            if ~isappdata(main,'pressStr')
                setappdata(main,'pressStr',saveData{3});
            end
            setappdata(main,'testData',[])
        else
            
            setappdata(main,'holdMat',[getappdata(main,'holdMat'); saveData{1}]);
            setappdata(main,'flightMat',[getappdata(main,'flightMat'); saveData{2}]);
            
            % If this is the first time, we save pressStr and pressCell too
            if ~isappdata(main,'pressStr')
                setappdata(main,'pressStr',saveData{3});
                setappdata(main,'pressCell',saveData{4});
            end
            
            % Make the button look inactive and empty the titlebox user data to
            % make the button fail the first if statement after 1 click so it
            % is inactive
            set(saveH,'Foregroundcolor',[.5 .5 .5])
            setappdata(main,'saveData',[]);
            if count>2
                set(compileBox,'Foregroundcolor','y')
            end
            setappdata(main,'Counter',count+1)
        end
        sampleCount(getappdata(main,'holdMat'),main);
    elseif ~strcmp(saveData{3},checkStr)
        % Explaination of error - window
        errorStr=sprintf('Oops ... Looking for ''%s'' but you typed ''%s!''',...
            checkStr,saveData{3});
        setappdata(main,'CompareMode',false);
        
        screenSize=get(0,'MonitorPositions');
        screenSize=[0.3*screenSize(3:4) 0.5*screenSize(3) 0.2*screenSize(4) ];
        whyNoSave=figure;
        set(whyNoSave,'Position',screenSize,'Color',[0.2 0.2 0.2],...
            'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');
        
        % textbox with directions
        uicontrol('Style','text','Units','Normalized',...
            'Position',[0.05 0.35 0.9 0.5],...
            'FontUnit','Normalized',...
            'FontSize',.3,'FontName','Calibri',...
            'Backgroundcolor',[.2 .2 .2],...
            'Foregroundcolor','yellow',...
            'String',errorStr);
        
        % Another text box that looks like a password display will be shown
        uicontrol('Style','pushbutton','Units','Normalized',...
            'Position',[0.25 0.1 0.5 0.4],...
            'FontUnit','Normalized',...
            'FontSize',.3,'FontName','Arial',...
            'Backgroundcolor',[.1 .1 .1],...
            'Foregroundcolor','yellow',...
            'String','Close','Callback',{@endsession,whyNoSave});
    end
end
end

function sampleCount(holdMat,main)
[sampleSize ~]=size(holdMat);
sampleStr= sprintf('Sample Count: %d',sampleSize);
    sampleAxes=axes('parent',main,'position',[0.03 0.04 0.2 0.001],...
        'Units','Normalized',...
        'color',[.2 .2 .2],...
        'xcolor',[.2 .2 .2],...
        'ycolor',[.2 .2 .2]);
    axis([0 1 0 1]);
    text(0,0,sampleStr,'Parent',sampleAxes,...
        'Color','y',...
        'BackgroundColor',[.2 .2 .2],...
        'FontName','Arial',...
        'FontSize',10);
    setappdata(main,'Handles',[getappdata(main,'Handles') sampleAxes]);



end

%% Compiler Functions

function compile(~,~,main)

% We check to see if the counter is greater than the minimum required for
% chi^2 GOF tests about 50 in this case
count=getappdata(main,'Counter');
if count>2
    
    % we Extract the data
    holdMat=getappdata(main,'holdMat');
    flightMat=getappdata(main,'flightMat');
    pressStr=getappdata(main,'pressStr');
    pressCell=getappdata(main,'pressCell');
    
    % Clear some variables
    if isappdata(main,'Handles')
        mainH=getappdata(main,'Handles');
        for k=1:length(mainH)
            if ishandle(mainH(k))
                delete(mainH(k));
            end
        end
        setappdata(main,'Handles',[])
    end
    
    %Statistical analysis
    statVec=compilestats(holdMat,flightMat);
    %Visualization
    visualH=visualTyping(statVec{1},statVec{2},pressStr,pressCell,main,...
        [statVec{3}'; [statVec{4}; inf]']');
    
    
    %Scatter Plot
    scatterH=scatterHoldFlight(holdMat,flightMat,statVec,pressCell,main);
    
    % Store the Statistics
    setappdata(main,'Statistics',statVec);
    save allvar
    
    % Setting up some push buttons: to record data/test the data
    clearH=uicontrol('Style','pushbutton','Units','Normalized',...
        'Position',[0.03 0.17 0.2 .07],...
        'FontUnit','Normalized',...
        'FontSize',.5,'FontName','Calibri',...
        'Backgroundcolor',[.1 .1 .1],...
        'Foregroundcolor',[.5 .5 .5],...
        'String','Clear Results',...
        'Callback',{@clearRecords,main});
    
    recordH=uicontrol('Style','pushbutton','Units','Normalized',...
        'Position',[0.03 0.24 0.2 .07],...
        'FontUnit','Normalized',...
        'FontSize',.5,'FontName','Calibri',...
        'Backgroundcolor',[.1 .1 .1],...
        'Foregroundcolor',[.5 .5 .5],...
        'String','Record Result',...
        'Callback',{@recorder,main,clearH});
    
    testH=uicontrol('Style','pushbutton','Units','Normalized',...
        'Position',[0.03 0.31 0.2 .07],...
        'FontUnit','Normalized',...
        'FontSize',.5,'FontName','Calibri',...
        'Backgroundcolor',[.1 .1 .1],...
        'Foregroundcolor','y',...
        'String','Test','Parent',main,...
        'Callback',{@tester,main,statVec,recordH,visualH,scatterH});  
    
    slideH=uicontrol('Style','slider',...
        'Units','Normalized',...
        'Position',[0.67 0.1 0.2 .04],...
        'Min',-1.5,'Max',.5,...
        'Value',-1,...
        'Parent',main,...
        'Callback',@slideupdate,...
        'BackGroundColor',[.1 .1 .1]);
    %Storing handles
    setappdata(main,'Handles',[ recordH ...
        testH visualH scatterH slideH clearH]);
    setappdata(main,'p',get(slideH,'Value'))
    matchStats(main);
    sampleCount(holdMat,main);
end

    function slideupdate(h,~)
        setappdata(main,'p',get(h,'Value'))
        matchStats(main);
    end

end

function statVec=compilestats(holdMat,flightMat)

% Calculate Means
avgFlight=mean(flightMat,1)';
avgHold=mean(holdMat,1)';
stdFlight=std(flightMat,1)';
stdHold=std(holdMat,1)';

% find dimension for looping variables
[numFiles x]=size(holdMat);

if numFiles>45
    % Calculate the Probabilites based on their GOF probability
    % 2/3 the weighting comes from the GOF, then 1/3 from existence
    % Done once for hold data, and once for flight data
    probHvec=-1*ones(x,2);
    for i=1:x
        [h p]=chi2gof(holdMat(:,i));
        if h==0;
            probHvec(i,:)=[p i];
        end
    end
    probHvec((probHvec(:,1)==-1),:)=[];
    
    
    probFvec=-1*ones(x,2);
    for i=1:x-1
        [h p]=chi2gof(flightMat(:,i));
        if h==0;
            probFvec(i,:)=[p i];
        end
    end
    probFvec((probFvec(:,1)==-1),:)=[];
    
else
    %weighting every category equally if their is not enough information
    probHvec=[ ones(1,x)*(1/x) ; (1:x) ]';
    probFvec=[ ones(1,x-1)*(1/(x-1)) ; (1:(x-1)) ]';
end
% A weighting vector is extract from the probability
weightH=probHvec(:,1)/(sum(probHvec(:,1)))*100;
weightF=probFvec(:,1)/(sum(probFvec(:,1)))*100;
% preallocating a correlation matrix; one column for HOLD one for FLIGHT

corMat=zeros(numFiles,2);

for test = 1:numFiles
    
    %Looping through each row of the holdMatrix
    %Deteriming the probabilities by finding the probabilty of being between
    %the two, and then find the anti-probability? i have no idea how i came up
    %with this concept but it makes sense to me
    testHold=holdMat(test,:);
    
    matchProb=zeros(length(weightH),1);
    counter = 1;
    for i=probHvec(:,2)'
        matchProb(counter)=(0.5-abs(diff(normcdf([avgHold(i) testHold(i)],avgHold(i),stdHold(i)))))*2;
        counter = counter+1;
    end
    matchH=(matchProb.*weightH);
    holdCorrelation=sum(matchH);
    
    testFlight=flightMat(test,:);
    
    matchProb=zeros(length(weightF),1);
    counter = 1;
    for i=probFvec(:,2)'
        matchProb(counter)=(0.5-abs(diff(normcdf([avgFlight(i) testFlight(i)],avgFlight(i),stdFlight(i)))))*2;
        counter = counter+1;
    end
    matchF=(matchProb.*weightF);
    flightCorrelation=sum(matchF);
    corMat(test,:)=[holdCorrelation, flightCorrelation];
end

% to determine the false positive rate, I figure out a likely false
% positive rate

falsePositive=100*(numel(find(((corMat(:,1)<mean(corMat(:,1))-std(corMat(:,1))).*(corMat(:,2)<mean(corMat(:,2))-std(corMat(:,2))))==true))/numFiles);
fprintf('False Positive Rate: %.2f%%\n',falsePositive);
disp(corMat)

% Declare Variable output by storing it in giant cell array
statVec= {avgHold ;avgFlight ;stdHold ;stdFlight ;corMat ; ...
    falsePositive; probHvec; probFvec; weightH; weightF};
end

function scatterH=scatterHoldFlight(holdMat,flightMat,statVec,pressCell,main)

% Setting up Axes
scAxes=axes('parent',main,'position',[0.31 0.08 0.3 0.3]);

% Bar charting Hold information,
[counter, x]=size(holdMat);

% setting up color Map
colormap(winter)
cmap=colormap;
colorFactor=floor(length(cmap)/x);
hold on

% Plotting each line + a mean for the whole group
for j=1:x
    for i=1:counter
        line([(j+0.875-1) (j+1.125-1)],[holdMat(i,j) holdMat(i,j)],...
            'Color',cmap(colorFactor*j,:),...
            'parent',scAxes,'LineWidth',.3);
    end
    plot((j),statVec{1}(j),'o',...
        'MarkerSize',4,...
        'Color',[.9 .9 .9],...
        'MarkerFaceColor',[.9 .9 .9],...
        'parent',scAxes);
end

% Changing color map to autumn for FLIGHT data
colormap(autumn)
cmap=colormap;
colorFactor=floor(length(cmap)/x);

% Same Idea here, loop through and make  line to represent each stroke
% measured
for j=1:x-1
    for i=1:counter
        line([(j+0.375) (j+.625)],[flightMat(i,j) flightMat(i,j)],...
            'Color',cmap(colorFactor*j,:),...
            'parent',scAxes,'LineWidth',.3);
    end
    plot((j+.5),statVec{2}(j),'o',...
        'MarkerSize',4,...
        'Color',[.9 .9 .9],...
        'MarkerFaceColor',[1 1 1],...
        'Parent',scAxes);
end

set(gca,'XColor',[.9 .9 .9],'YColor',[.9 .9 .9],'Color',[.3 .3 .3],...
    'FontUnits','Normalized','FontSize',.06,...
    'XTick',1:length(pressCell),'XLim',[0 (x+1)],...
    'XTickLabel',pressCell);

% Setting information for asthetics
ylabel('Key','Color',[.9 .9 .9]);
xlabel('Time [ms]','Color',[.9 .9 .9]);
title('Scatter Plot: Hold and Flight Times','Color',[.9 .9 .9]);
axis([ 0 x+.5 min([min(holdMat) min(flightMat)])...
    max([max(holdMat) max(flightMat)]) ]);
% Declaring OutPut
scatterH=scAxes;
end

function clearRecords(clearH,~,main)
if isappdata(main,'ResultVec')
    setappdata(main,'ResultVec',[]);
    set(clearH,'foregroundColor',[.5 .5 .5])
    rAxes=getappdata(main,'rAxes');
    if ishandle(rAxes)
    delete(rAxes)
    end 
end
end

function recorder(recordBox,~,main,clearH)
% super simple function to records the test result storing to appdata then
% change the little anotation status box.

if isappdata(main,'Match')
    match=getappdata(main,'Match');
    setappdata(main,'ResultVec',[getappdata(main,'ResultVec') match])
    resultVec=getappdata(main,'ResultVec');
    resultStr=sprintf('Positive:  %d   Negative:  %d',...
        numel(find(resultVec==1)),numel(find(resultVec==0)));
    
    rAxes=axes('parent',main,'position',[0.043 0.14 0.2 0.1],...
        'Units','Normalized',...
        'color',[.2 .2 .2],...
        'xcolor',[.2 .2 .2],...
        'ycolor',[.2 .2 .2]);
    axis([0 1 0 1]);
    text(0,0,resultStr,'Parent',rAxes,...
        'Color','y',...
        'BackgroundColor',[.2 .2 .2],...
        'FontName','Arial',...
        'FontSize',14,...
        'EdgeColor','y');
    setappdata(main,'Match',[]);
    
    % Make box look inactive
    set(recordBox,'foregroundcolor',[.5 .5 .5]);
    set(clearH,'foregroundColor','y')
    setappdata(main,'rAxes',rAxes);
    %saving axes handle for future deletion
    setappdata(main,'Handles',[getappdata(main,'Handles') rAxes]);
end

end

%% End Session Function

function endsession(buttonH,dataCell,current)
if ~isempty(dataCell)
    setappdata(buttonH,'typeData',dataCell);
end
close(current)
fprintf('*Window_Closed*\n')
end

%% Test & Compare Functions

function tester(testH,~,main,statVec,recordH,visualH,scatterH)

% Turn on the CompareMode so that we can reuse functions but with little
% tweakers..
setappdata(main,'CompareMode',true)

% Calls the typing functions for data collection
prelim([],[],main,[])

% This is necessary; after data collection i resume running
uiwait

% using the collected data and CompareMode versions of the following
% functions
analyzer([],[],main,[],[])
saveFcn([],[],[],main,[])
% the save function might turn of comparemode, so I have this if to check
if getappdata(main,'CompareMode')==true
    
    % testerStats outputs the match sucess/ 1 or 0
    [match x]=testerStats(getappdata(main,'tHoldMat'),...
        getappdata(main,'tFlightMat'),statVec,main);
    
    % store the data in appdata
    setappdata(main,'Match',match);
    % Make record button look active
    set(recordH,'Foregroundcolor','Yellow')
    
    visualEdit(main,visualH,scatterH,x,...
        getappdata(main,'tHoldMat'),getappdata(main,'tFlightMat'));
    
    matchDisp(main,match)
    matchStats(main)
    % Turn comparemode off, keep button looking active
    set(testH,'ForeGroundColor','Yellow')
    setappdata(main,'CompareMode',false)
end

end

function [match x]=testerStats(testHold,testFlight,statVec,main)
% Takes in the stats and test data
% Fires the test data against the stats
% Probabilities multiplied by the weightings
% Then depending on the p factor the threshold is set for where to cut off
% bad and good values. The threshold data comes from the test sample
% correlation matrix values (how good the values are against themselves)

% Out Puts match 0 or 1 based on how it fairs against the threshold;

% Extracting data from statvec
testHold=(testHold(end,:));
testFlight=testFlight(end,:);
avgHold=statVec{1};
avgFlight=statVec{2};
stdHold=statVec{3};
stdFlight=statVec{4};
corMat=statVec{5};

probHvec=statVec{7};
probFvec=statVec{8};
weightH=statVec{9};
weightF=statVec{10};
matchProb=zeros(length(weightH),1);
counter = 1;

x=length(avgHold);

for i=probHvec(:,2)'
    matchProb(counter)=(0.5-abs(diff(normcdf([avgHold(i) testHold(i)],avgHold(i),stdHold(i)))))*2;
    counter = counter+1;
end
matchH=(matchProb.*weightH);
holdCorrelation=sum(matchH);

matchProb=zeros(length(weightF),1);
counter = 1;
for i=probFvec(:,2)'
    matchProb(counter)=(0.5-abs(diff(normcdf([avgFlight(i) testFlight(i)],avgFlight(i),stdFlight(i)))))*2;
    counter = counter+1;
end
matchF=(matchProb.*weightF);
flightCorrelation=sum(matchF);

corVec(1,:)=[holdCorrelation, flightCorrelation];
p=getappdata(main,'p');
if ((corVec(1)<(mean(corMat(:,1))+(p*std(corMat(:,1)))))*...
        (corVec(2)<(mean(corMat(:,2))+(p*std(corMat(:,2))))))==true;
    match=0;
else
    match=1;
end
% Tacked on this for loop for to ensure that Nothing is more than 3 std
for i=1:x-1
    matchProb(counter)=(0.5-abs(diff(normcdf([avgHold(i) testHold(i)],avgHold(i),stdHold(i)))))*2;
    if matchProb<.03
        match=0;
    end
end
for i=1:(x-1)
    matchProb(counter)=(0.5-abs(diff(normcdf([avgFlight(i) testFlight(i)],avgFlight(i),stdFlight(i)))))*2;
    if matchProb<.01
        match=0;
    end
end
setappdata(main,'corFlight',flightCorrelation);
setappdata(main,'corHold',holdCorrelation);
end

function matchDisp(main,match)
if match==1
    matchStr=('MATCH');
    mColor=[0 0.8 0];
    pVec=[0.1 0.32 0.2 0.1];
else
    matchStr=('NO MATCH');
    mColor=[0.8 0 0];
    pVec=[0.09 0.32 0.2 0.1];
end
if isappdata(main,'matchH')
    delete(getappdata(main,'matchH'))
end
matchH=axes('parent',main,'position',pVec,...
    'Units','Normalized',...
    'color',[.2 .2 .2],...
    'xcolor',[.2 .2 .2],...
    'ycolor',[.2 .2 .2]);
axis([0 1 0 1]);
text(0,1,matchStr,...
    'Parent',matchH,...
    'Color',mColor,...
    'BackgroundColor',[.2 .2 .2],...
    'FontName','Arial',...
    'FontWeight','bold',...
    'EdgeColor',mColor,...
    'FontSize',15);
setappdata(main,'matchH',matchH);
setappdata(main,'Handles',[getappdata(main,'Handles') matchH])
end

function matchStats(main)
if isappdata(main,'corFlight')
    corFlight=getappdata(main,'corFlight');
    corHold=getappdata(main,'corHold');
    rmappdata(main,'corFlight')
    rmappdata(main,'corHold')
    statStr=sprintf('Hold Correlation: %7.1f %% \nFlight Correlation: %5.1f %%\n',corHold,corFlight);
else
    statStr=sprintf('\n\n');
end
p=getappdata(main,'p');
corMat=getappdata(main,'Statistics');
corMat=corMat{5};

holdThres=(mean(corMat(:,1))+p*std(corMat(:,1)));
flightThres=(mean(corMat(:,2))+p*std(corMat(:,2)));

statStr=sprintf('%sHold Threshold: %9.1f %% \nFlight Threshold: %7.1f %% \n',statStr,holdThres,flightThres);

matchStatH=axes('parent',main,'position',[0.67 0.1 0.2 0.1],...
    'Units','Normalized',...
    'color',[.2 .2 .2],...
    'xcolor',[.2 .2 .2],...
    'ycolor',[.2 .2 .2]);
axis([0 1 0 1]);
text(0,1,statStr,...
    'Parent',matchStatH,...
    'Color','y',...
    'BackgroundColor',[.2 .2 .2],...
    'FontName','Courier New',...
    'FontSize',16);
setappdata(main,'Handles',[getappdata(main,'Handles') matchStatH]);
end

%% Export Import & What Is functions

function exportFcn(~,~,main)

holdMat=getappdata(main,'holdMat');
flightMat=getappdata(main,'flightMat');
pressStr=getappdata(main,'pressStr');
pressCell=getappdata(main,'pressCell');
saveStr=sprintf('%s.mat',pressStr);
uisave({'holdMat', 'flightMat', 'pressStr', 'pressCell'},saveStr);

end

function importFcn(importH,~,main,compileBox)
uiopen('.mat')

setappdata(main,'holdMat',holdMat);
setappdata(main,'flightMat',flightMat);
setappdata(main,'pressStr',pressStr);
setappdata(main,'pressCell',pressCell);
[numFiles ~]=size(holdMat);
set(importH,'ForegroundColor','y');
setappdata(main,'Counter',numFiles);
set(compileBox,'ForegroundColor','y');
end

function whatIsFcn(~,~)

screenSize=get(0,'MonitorPositions');
screenSize=[0.25*screenSize(3:4) 0.5*screenSize(3) 0.5*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('TypoMetrics\n\n -This program attempts to measure an individuals unique typing style \n');
infoStr=sprintf('%s- By quantifying typing style, one can possibly set an extra layer of security in password protection\n',infoStr);
infoStr=sprintf('%s- It does this by measuring HOLD and FLIGHT times\n',infoStr);
infoStr=sprintf('%s- HOLD time measures how long a button is pressed down\n',infoStr);
infoStr=sprintf('%s- FLIGHT time measures the time different between releasing one key and press down the next\n',infoStr);
infoStr=sprintf('%s- This program enables you to type any password then analyze it,\n',infoStr);
infoStr=sprintf('%ssave the data, compile all the saved data and test how well\n',infoStr);
infoStr=sprintf('%sTypometrics identifies a person through HOLD and FLIGHT characterstics\n',infoStr);
infoStr=sprintf('%s\n For specific instructions click on the information buttons on the left hand side!\n\n',infoStr);
infoStr=sprintf('%s  Created By Samir Ahmed \n January 2011',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.7],...
    'FontUnit','Normalized',...
    'FontSize',.05,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end

%% Instruction Callback Functions

function instruct1(~,~)
screenSize=get(0,'MonitorPositions');
screenSize=[0.25*screenSize(3:4) 0.5*screenSize(3) 0.5*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('Type Function\n\n -Clicking this button will allow you to type in any word. They are a few limitations and restrictions! \n');
infoStr=sprintf('%s- DO NOT Hit CapsLock because MATLAB does not support certain functionality required for this button\n',infoStr);
infoStr=sprintf('%s- DO NOT attempt to cancel what you tried with backspace, just start typing again!\n',infoStr);
infoStr=sprintf('%s- Click Start to Begin, and hit the ENTER key to end\n',infoStr);
infoStr=sprintf('%s- TypoMetrics is only concerned about the first and last key you hit, so dont worry about the time before you start typing or how long it takes to hit the ENTER key\n',infoStr);
infoStr=sprintf('%s- After you type you may hit Analysis\n',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.7],...
    'FontUnit','Normalized',...
    'FontSize',.06,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end

function instruct2(~,~)
screenSize=get(0,'MonitorPositions');
screenSize=[0.25*screenSize(3:4) 0.5*screenSize(3) 0.5*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('Analysis\n\n -This button will provide a full analysis of the last word typed\n');
infoStr=sprintf('%s- The visualization shows how long and when each key is hit\n',infoStr);
infoStr=sprintf('%s- Flight Times are represented by the white lines joining.\n- For detailed information click on box!\n',infoStr);
infoStr=sprintf('%s- HOLD times and FLIGHT times are shown in bar graphs\n',infoStr);
infoStr=sprintf('%s- Key Statistics (pun intended) are also provided!\n\n',infoStr);
infoStr=sprintf('%s- You Must hit ANALYSIS before you SAVE DATA!\n',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.7],...
    'FontUnit','Normalized',...
    'FontSize',.06,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end

function instruct3(~,~)
screenSize=get(0,'MonitorPositions');
screenSize=[0.25*screenSize(3:4) 0.5*screenSize(3) 0.5*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('SAVE DATA\n\n -This button will save your word and HOLD and FLIGHT information temporarily\n');
infoStr=sprintf('%s- After saving a sample count will be displayed at the button\n',infoStr);
infoStr=sprintf('%s- You must save at least 3 times before you can compile\n- For Good Statistics Typometrics needs 50 samples to work with!\n',infoStr);
infoStr=sprintf('%s- You can only save the same word as the first word you saved\n',infoStr);
infoStr=sprintf('%s- Restart the program to change the word!\n\n',infoStr);
infoStr=sprintf('%s- You may you save to add samples to imported data - dont forget to export to make it permanent!\n',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.7],...
    'FontUnit','Normalized',...
    'FontSize',.06,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end

function instruct4(~,~)

screenSize=get(0,'MonitorPositions');
screenSize=[0.2*screenSize(3:4) 0.65*screenSize(3) 0.6*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('COMPILE\n\n -Compile will take all the data you imported or saved and stastically analyze it \n');
infoStr=sprintf('%s- Compile initially provides a statistical mean visualization and a scatter plot HOLD and FLIGHT times\n',infoStr);
infoStr=sprintf('%s- Visualization shows the HOLD times as boxes and FLIGHT times as white lines\n',infoStr);
infoStr=sprintf('%s- Scatter Plot shows the mean HOLD/FLIGHT time as WHITE dots, all other instances are displayed as thin lines\n',infoStr);
infoStr=sprintf('%s- The threshold is automatically calculated as one standard deviation below the mean correlation of the sample\n',infoStr);
infoStr=sprintf('%s- The slide may be used to adjust the threshold to make matching correlation more and less strict\n',infoStr);
infoStr=sprintf('%s- The Test button may be used to type a word and compare it to the sample mean.\n',infoStr);
infoStr=sprintf('%s- Test results are shown as RED dots in scatterplot\n',infoStr);
infoStr=sprintf('%s- Test results are shown just above the average results in the visualization \n',infoStr);
infoStr=sprintf('%s- Correlation percentages of HOLD and FLIGHT times are shown on the right hand side\n',infoStr);
infoStr=sprintf('%s- Either HOLD OR FLIGHT times must be above their respoective thresholds to qualify for a MATCH!\n',infoStr);
infoStr=sprintf('%s- For specific instructions click on the information buttons on the left hand side!\n',infoStr);
infoStr=sprintf('%s- To record a result hit the RECORD RESULT button\n',infoStr);
infoStr=sprintf('%s- To clear the result count hit the CLEAR RECORD button!\n',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.75],...
    'FontUnit','Normalized',...
    'FontSize',.04,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end

function instruct5(~,~)
screenSize=get(0,'MonitorPositions');
screenSize=[0.25*screenSize(3:4) 0.5*screenSize(3) 0.5*screenSize(4)  ];
f1=figure;
set(f1,'Position',screenSize,'Color',[0.2 0.2 0.2],...
    'WindowStyle','modal','NumberTitle','off','Name','TypoMetrics');

%Creating an information string;
infoStr=sprintf('EXPORT/IMPORT\n\n -EXPORT allows you to save the data you have collected to a .mat file\n');
infoStr=sprintf('%s- IMPORT allows you to load up data previously saved by clicking EXPORT\n',infoStr);
infoStr=sprintf('%s- Importing will overwrite any data saved with the SAVE DATA button\n',infoStr);

uicontrol('Style','text','Units','Normalized',...
    'Position',[0.025 0.2 0.95 0.7],...
    'FontUnit','Normalized',...
    'FontSize',.065,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String',infoStr);

% pushbutton to start data collection
uicontrol('Style','pushbutton','Units','Normalized',...
    'Position',[0.25 0.05 0.5 0.1],...
    'FontUnit','Normalized',...
    'FontSize',.6,'FontName','Calibri',...
    'Backgroundcolor',[.2 .2 .2],...
    'Foregroundcolor','yellow',...
    'String','Close',...
    'Callback',{@endsession,f1});

end