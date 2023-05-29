function Gamma_simulation()

dbstop if error
aantalLoops=100;
dim=9;
newsDummy=1;

volatileDays=[];
gammaTraders=2*[-40 -30 -20 -10 0 10 20 30 40];
hedgeUrgency=0*[  1 1 1 1 1 1 1 1 1 1];
hedgeStrategy=0*[  0 0 0 0 0 2 2 2 2 2];
extremePerc=0*0.02*[1 1 1 1 1 1 1 1 1 1];
extremeVolume=25;


immediacy=0.25;
informedness=1*0.3*[1 1 1 1 1 1 1 1 1 1];
decayRate=0.05;

constantStrike=0;

newsTrend=0.000;
newsSigma=0.0;
eventStudy=0;



hedgeStrategy=hedgeStrategy(1:dim); gammaTraders=gammaTraders(1:dim); hedgeUrgency=hedgeUrgency(1:dim);informedness=informedness(1:dim);

tickSize=0.01;
MeanOS=1;

%simulation_details
days=20;
hedge_frequency=4; %how often dealers hedge, has to be 4
hours_per_day=10;
periods=hours_per_day*hedge_frequency;
lambda= 160/hedge_frequency ;%orderArrivals per period
n=days*periods*lambda*1.5*(1+(periods/days)); %sligthly larger than n to accomodate gamma orders

drieDpriceStorer=zeros(days*periods,dim,aantalLoops);


trend=-0.1;
orderVol=0.1;
vol=sqrt(periods)*0.014;

optionLength=2;


descriptiveStorer=zeros(27,dim,aantalLoops);
marketFailed=zeros(dim,aantalLoops);
programFailed=zeros(dim,aantalLoops);
Failed_location=zeros(aantalLoops,1);
tic

for superLoop=1:aantalLoops
    
    %generate OB
    CB_Port=zeros(2,dim);
    p0=5;
    p=p0;
    fp= p0*(1-eventStudy) + (4*eventStudy);
    maxPrice=10;OB=[[0:tickSize:maxPrice]',zeros(maxPrice/tickSize+1,dim)];
    orderBookPdf=zeros(maxPrice/tickSize+1,dim);
    orderBookPdf_fundm=zeros(maxPrice/tickSize+1,dim);
    superOB_pdf=zeros(maxPrice/tickSize+1,dim);
    news=round(normrnd(newsTrend,newsSigma,[days periods]),2)*newsDummy;
    %other statistics
    orderVolStorer=zeros(n,dim); orderPXStorer=zeros(n,dim);
    bidVolume=zeros(n,dim); offerVolume=zeros(n,dim); LOS=zeros(n,dim);
    timeStamp=zeros(n,dim);
    %gammaTrading portfolios
    gammaPort=zeros(4,3,dim);
    gammaPort(2,2,:)=0.9;
    gammaPort(3,2,:)=1.1; %first 'puts and calls must be  out of the money
    
    %InitializeOBs
    
    %more statistics
    bestBid=zeros(n,dim);bestOffer=zeros(n,dim);L_bidOffer=zeros(n,dim);
    L_volume=zeros(n,dim);midPrice=p*ones(n,dim);
    transActionLedger1=[0,0,0];transActionLedger2=[0,0,0];transActionLedger3=[0,0,0];transActionLedger4=[0,0,0];transActionLedger5=[0,0,0];
    transActionLedger6=[0,0,0];transActionLedger7=[0,0,0];transActionLedger8=[0,0,0];transActionLedger9=[0,0,0];transActionLedger10=[0,0,0];
    
    immediacy_Port=zeros(2,2,dim);  % [momentum cash, momentum pos ; value cash, value pos]
    
    
    %gammaTrader behavior storer
    underlyingStorer=zeros(days*periods,dim);
    deltaStorer=zeros(days*periods,dim);
    gammaStorer=zeros(days*periods,dim);
    wegHaalMatrix=zeros(days*periods,dim);
    gammaBidStorer=zeros(days*periods,dim);
    aggressionStorer=zeros(days*periods,dim);
    
    
    %initialize OBS
    for j=1:dim
        for i=1:3
            OB(round((p+tickSize*i*2)/tickSize+1,0),1+j)=-10;
            OB(round((p+tickSize*-i*2)/tickSize+1,0),1+j)=10;
        end
        bestBid(1,j)=OB(round((p-2*tickSize)/tickSize+1,0),1);
        bestOffer(1,j)=OB(round((p+2*tickSize)/tickSize+1,0),1);
    end
    
    
    
    
    
    
    % the indicator i is the sequencing order, is not looped through but added
    % via i=i+1;
    i=2;
    
    %empty previousOrder
    previousOrder=zeros(dim,4); %<p*,v*,t,v**>
    
    cashStorer_CB=zeros(days,dim);
    cashStorer_MM=zeros(days,dim);
    failed=zeros(dim,1);
    
    for day=1:days
        %%%%%%%%%%% EVERY DAY  %%%%%%%%%%%%%%%
        for j=1:dim
            if failed(j)==0
                %gammaport datastructure: 4x4xj cube, of which each j' a slice
                % #underlying positie
                % #putContracts , strike , maturity
                % #calContracts , strike , maturity
                % cash balance
                
                
                
                %sell old options and portfolio
                strike=midPrice(i-1,j) *(constantStrike==0) + (1-(constantStrike==0))*p0  ;
                
                
                deltaNPV=gammaPort(1,1,j)*strike;
                CB_Port(1,j)=CB_Port(1,j)+gammaPort(1,1,j); %CB takes over delta position
                CB_Port(2,j)=CB_Port(2,j)-deltaNPV; %CB adjusts cash
                gammaPort(1,1,j)=0;  %dealer loses delta inventory
                gammaPort(4,1,j)=gammaPort(4,1,j)+deltaNPV;  %dealer adjusts cash
                %CB also buys the whole gamma portfolio at mid
                currentPrice=strike;
                oldPut=BS(0 ,vol,max(0,gammaPort(2,3,j)-day),currentPrice, gammaPort(2,2,j), 0, -1);
                oldCall=BS(0 ,vol,max(0,gammaPort(3,3,j)-day),currentPrice,gammaPort(3,2,j), 0, 1);
                gammaPort(4,1,j)=gammaPort(4,1,j)+(oldPut+oldCall)*gammaTraders(j);
                CB_Port(2,j)=CB_Port(2,j)-(oldPut+oldCall)*gammaTraders(j);
                gammaPort(2:3,:,j)=zeros(2,3);
                
                %buy new Options
                
                [putPrice]=BS(0 ,vol,optionLength,strike, strike, 0, -1);
                [callPrice]=BS(0 ,vol,optionLength,strike, strike, 0, 1);
                gammaPort(2,:,j)=[gammaTraders(j),strike,day+optionLength]; %maturity t+2
                gammaPort(3,:,j)=[gammaTraders(j),strike,day+optionLength];
                %adjust cash balances:
                gammaPort(4,1,j)=gammaPort(4,1,j)-(putPrice+callPrice)*gammaTraders(j);
                CB_Port(2,j)=CB_Port(2,j)+(putPrice+callPrice)*gammaTraders(j);
                cashStorer_CB(day,j)=CB_Port(2,j);
                cashStorer_MM(day,j)=gammaPort(4,1,j);
            end %end while failed statement
        end %end j loop
        
        for time=1:periods
            fp=(fp+news(day,time))*(1-eventStudy)+ eventStudy*((day>1)*4+(day<2)*5);
            
            
            arrivals =poissrnd(lambda*ones(periods,1));
            %arrivals = lambda*ones(periods,1);%
            %%%%%%%%%%% EVERY HOUR  %%%%%%%%%%%%%%%
            %for each parallel simulation place order
            for j=1:dim
                if failed(j)==0
                    %check deltas in portfolio
                    
                    %first remove the previous gamma order
                    
                    if ((day-1)+(time-1))>0 && gammaTraders(j)~=0
                        
                        p_star=previousOrder(j,1);
                        p_starLoc=round((p_star/tickSize+1),0);
                        v_star=previousOrder(j,2);
                        v_starstar=previousOrder(j,4);
                        previousDay=(day+(time-1)/periods);
                        if j==1;  v_traded=sum([transActionLedger1(:,2)].*[transActionLedger1(:,2)*sign(v_star)<0].*[round(transActionLedger1(:,1),2)==round(p_star,2)].*[transActionLedger1(:,3)==previousDay])    ;  end
                        if j==2;  v_traded=sum([transActionLedger2(:,2)].*[transActionLedger2(:,2)*sign(v_star)<0].*[round(transActionLedger2(:,1),2)==round(p_star,2)].*[transActionLedger2(:,3)==previousDay])    ;  end
                        if j==3;  v_traded=sum([transActionLedger3(:,2)].*[transActionLedger3(:,2)*sign(v_star)<0].*[round(transActionLedger3(:,1),2)==round(p_star,2)].*[transActionLedger3(:,3)==previousDay])    ;  end
                        if j==4;  v_traded=sum([transActionLedger4(:,2)].*[transActionLedger4(:,2)*sign(v_star)<0].*[round(transActionLedger4(:,1),2)==round(p_star,2)].*[transActionLedger4(:,3)==previousDay])    ;  end
                        if j==5;  v_traded=sum([transActionLedger5(:,2)].*[transActionLedger5(:,2)*sign(v_star)<0].*[round(transActionLedger5(:,1),2)==round(p_star,2)].*[transActionLedger5(:,3)==previousDay])    ;  end
                        if j==6;  v_traded=sum([transActionLedger6(:,2)].*[transActionLedger6(:,2)*sign(v_star)<0].*[round(transActionLedger6(:,1),2)==round(p_star,2)].*[transActionLedger6(:,3)==previousDay])    ;  end
                        if j==7;  v_traded=sum([transActionLedger7(:,2)].*[transActionLedger7(:,2)*sign(v_star)<0].*[round(transActionLedger7(:,1),2)==round(p_star,2)].*[transActionLedger7(:,3)==previousDay])    ;  end
                        if j==8;  v_traded=sum([transActionLedger8(:,2)].*[transActionLedger8(:,2)*sign(v_star)<0].*[round(transActionLedger8(:,1),2)==round(p_star,2)].*[transActionLedger8(:,3)==previousDay])    ;  end
                        if j==9;  v_traded=sum([transActionLedger9(:,2)].*[transActionLedger9(:,2)*sign(v_star)<0].*[round(transActionLedger9(:,1),2)==round(p_star,2)].*[transActionLedger9(:,3)==previousDay])    ;  end
                        if j==10;  v_traded=sum([transActionLedger10(:,2)].*[transActionLedger10(:,2)*sign(v_star)<0].*[round(transActionLedger10(:,1),2)==round(p_star,2)].*[transActionLedger10(:,3)==previousDay]);  end
                        
                        
                        dir=sign(v_star); % 1=buy, -1=sell
                        aggresief=sign(v_star*v_starstar)==-1; %so you know if first order is aggressive
                        
                        
                        
                        gehandeld_door_mm=dir*min(max(abs(v_traded)-abs(v_starstar),0),abs(v_star));
                        weghalen=gehandeld_door_mm-v_star;
                        
                        
                        
                        gammaPort(1,1,j)=gammaPort(1,1,j)+gehandeld_door_mm;
                        gammaPort(4,1,j)=gammaPort(4,1,j)-gehandeld_door_mm*p_star;
                        wegHaalMatrix((day*periods+time)-periods,j)=weghalen;
                        bOffer=OB(find(OB(:,j+1)<0,1,  'first'),1);
                        bBid= OB(find(OB(:,j+1) >0,1,  'last'),1);
                        if bBid>bOffer
                            failed(j)=1;
                            programFailed(dim,superLoop)=1;
                            Failed_location(superLoop)=periods*day+time;
                        end
                        if aggresief ~=1
                            if weghalen==0 %nested if statement for debugging
                                %doe niks
                                if abs(weghalen+OB(p_starLoc,j+1))<0.01
                                    OB(p_starLoc,j+1)=0;
                                else
                                    OB(p_starLoc,j+1)=OB(p_starLoc,j+1)+weghalen;
                                end
                            end
                            
                        end
                        bOffer=OB(find(OB(:,j+1)<0,1,  'first'),1);
                        bBid= OB(find(OB(:,j+1) >0,1,  'last'),1);
                        
                        if bBid>bOffer
                            failed(j)=1;
                            programFailed(j,superLoop)=1;
                            Failed_location(superLoop)=periods*day+time;
                        end
                        
                    end
                    
                    maturity_time=gammaPort(2,3,j)-(day+time/periods);
                    put_delta= gammaTraders(j)*(calcDelta2(0 ,vol,maturity_time,0.5*(bestOffer(i-1,j)+bestBid(i-1,j)), gammaPort(2,2,j), -0.5*vol^2)-1);
                    call_delta=gammaTraders(j)*calcDelta2(0 ,vol,maturity_time,0.5*(bestOffer(i-1,j)+bestBid(i-1,j)), gammaPort(3,2,j),-0.5*vol^2);
                    
                    
                    delta=put_delta+call_delta+gammaPort(1,1,j);
                    gamma=2*gammaTraders(j)*calcGamma(0 ,vol,maturity_time,0.5*(bestOffer(i-1,j)+bestBid(i-1,j)), gammaPort(3,2,j), -0.5*vol^2);
                    BB=bestBid(i-1,j);
                    OO=bestOffer(i-1,j);
                    
                    polynomial_expr=(mod(time,hedge_frequency)/hedge_frequency)+ ((mod(time,hedge_frequency)/hedge_frequency)==0)*1;
                    gamma_hedge_aggression= (32/3)*(polynomial_expr)^3 - 16*(polynomial_expr)^2 + (28/3)*(polynomial_expr)-2;
                    gamma_hedge_price=(delta<0)*(BB+gamma_hedge_aggression*(OO-BB)) + (delta>0)*(OO-gamma_hedge_aggression*(OO-BB));
                    

                    BidoverStrike=BB>=gammaPort(2,2,j);
                    OfferOnderStrike=OO<=gammaPort(2,2,j);
                    
                    proper_price=round(gamma_hedge_price/tickSize,0)*tickSize;
                    
                    aggressionStorer((day*periods+time)-periods,j)=gamma_hedge_aggression;
                    
                    
                    slimmeOrder= (delta>0)*BidoverStrike*BB+ (delta<0)* (OfferOnderStrike*OO+(OfferOnderStrike==0)*maxPrice );
                    
                    
                    gamma_order(j,:)=[(hedgeStrategy(j)==0)*proper_price + (hedgeStrategy(j)==1) *maxPrice*(delta<0) + (hedgeStrategy(j)==2)*slimmeOrder,-delta]; %alleen de gamma prijs hier bekend, de delta is afhankelijk
                    
                    deltaStorer((day*periods+time)-periods,j)=delta;
                    gammaStorer((day*periods+time)-periods,j)=gamma;
                    underlyingStorer((day*periods+time)-periods,j)=gammaPort(1,1,j);
                    if gammaTraders(j)~=0
                        gammaBidStorer((day*periods+time)-periods,j)=0.5*(bestOffer(i-1,j)+bestBid(i-1,j))-proper_price;
                    end
                end %end for failed while loop
            end % end for j loop
            
            %now generate all arivals for this period:
            prices=round(normrnd(trend,orderVol,arrivals(time),1)/tickSize,0)*tickSize;
            sizes =2*MeanOS*rand(arrivals(time),1);
            direction=(2*((rand(arrivals(time),1)>0.5)-0.5));
            immediacy_vector=rand(arrivals(time),1)<(immediacy+1*ismember(day,volatileDays)/10);
            extreme_vector=rand(arrivals(time),1);
            informedness_vector=rand(arrivals(time),1); %seed for informedness
            for k=0:arrivals(time)
                for j=1:dim
                    if failed(j)==0
                        %generates the order for each path
                        if k~=0
                            if i>2; previousPrice=midPrice(i-2,j); else previousPrice=p0; end
                            
                            
                            %dummies
                            
                            infor_d=(informedness_vector(k)<informedness(j));
                            
                            %immediate pricen: 0 of maxPrice
                            
                            imm_random_p= maxPrice*(direction(k)>0)+0;
                            imm_inf_p= maxPrice*(fp>midPrice(i-1,j))+0; %reversion to fundamental price
                            imm_prijs=(1-infor_d)*imm_random_p + infor_d*(imm_inf_p);
                            %immediate direction: 1 of -1
                            imm_dir= (imm_prijs>0) - (imm_prijs==0) ;
                            
                            %non immediate prices:
                            uninformed_nonimm=prices(k)*direction(k)+ midPrice(i-1,j);% uninformed
                            informed_nonimm=prices(k)*direction(k)+ fp; %informed
                            
                            
                            non_imm_prijs=informed_nonimm*(informedness_vector(k)<informedness(j)) + uninformed_nonimm *(1-(informedness_vector(k)<informedness(j)));
                            
                            prijs=immediacy_vector(k)* imm_prijs + (1-immediacy_vector(k))* non_imm_prijs;
                            imm_volume=(sizes(k)+extremeVolume*(day>2)*(extremePerc(j)>extreme_vector(k)))*imm_dir;
                            
                            non_imm_volume=sizes(k)*direction(k);
                            volume=immediacy_vector(k)* (imm_volume) + (1-immediacy_vector(k))*non_imm_volume;
                            order=[prijs;volume];
                            
                            
                            
                            immediacy_Port(1,:,j)=immediacy_Port(1,:,j)+immediacy_vector(k)*(1-infor_d)*[(-bOffer*volume*(volume>0)+bBid*volume*(volume<0)),volume];
                            immediacy_Port(2,:,j)=immediacy_Port(2,:,j)+immediacy_vector(k)*infor_d*[(-bOffer*volume*(volume>0)+bBid*volume*(volume<0)),volume];
                            
                            
                        else   
                            order=gamma_order(j,:);
                     
                            if gammaTraders(j)~=0
                                previousOrder(j,:)= [gamma_order(j,:),day+time/periods,OB(round((gamma_order(j,1)/tickSize+1),0),j+1)];
                            end
                            
                        end 
                        if order(1)<0 || order(1)>maxPrice
                            failed(j)=1; % failure
                            marketFailed(j,superLoop)=1;
                            Failed_location(superLoop)=periods*day+time;
                        end
                        timeStamp(i,j)=day+time/periods-1;
                        orderVolStorer(i,j)=order(2); orderPXStorer(i,j)=order(1);
                        tempLedger=[];
                        transAction=[];
                        
                        
                        
                        
                        % volumes
                        bidVolume(i,j)=sum((OB(:,1+j)>0).*OB(:,1+j));
                        offerVolume(i,j)=sum((OB(:,1+j)<0).*OB(:,1+j));
                        LOS_intermediate=OB(:,1+j)./(midPrice(i-1,j)-OB(:,1));
                        LOS_intermediate(isnan(LOS_intermediate))=0;
                        LOS(i,j)=sum(LOS_intermediate);
                        
                        while abs(order(2))>0
                            [OB(:,[1 1+j]),order,transAction]=transact(OB(:,[1 1+j]),order,i, day+time/periods);
                            if sum(transAction(:,1))>0 
                                if j==1;transActionLedger1=[transActionLedger1;transAction]; tempLedger=transActionLedger1; end
                                if j==2;transActionLedger2=[transActionLedger2;transAction];tempLedger=transActionLedger2;end
                                if j==3;transActionLedger3=[transActionLedger3;transAction];tempLedger=transActionLedger3;end
                                if j==4;transActionLedger4=[transActionLedger4;transAction];tempLedger=transActionLedger4;end
                                if j==5;transActionLedger5=[transActionLedger5;transAction];tempLedger=transActionLedger5;end
                                if j==6;transActionLedger6=[transActionLedger6;transAction]; tempLedger=transActionLedger6; end
                                if j==7;transActionLedger7=[transActionLedger7;transAction];tempLedger=transActionLedger7;end
                                if j==8;transActionLedger8=[transActionLedger8;transAction];tempLedger=transActionLedger8;end
                                if j==9;transActionLedger9=[transActionLedger9;transAction];tempLedger=transActionLedger9;end
                                if j==10;transActionLedger10=[transActionLedger10;transAction];tempLedger=transActionLedger10;end
                            end
                        end
                        bOffer=OB(find(OB(:,j+1)<0,1,  'first'),1);
                        bBid= OB(find(OB(:,j+1) >0,1,  'last'),1);
                        if isempty(bBid) || isempty(bOffer)
                            failed(j)=1;
                            marketFailed(j,superLoop)=1;
                            Failed_location(superLoop)=periods*day+time;
                        else
                            
                            
                            mid=0.5*(bOffer+bBid);
                            
                            %stukje code voor order-book PDF constructie
                            
                            shift=floor((p0-mid)/tickSize+rand());
                            if shift>0
                                orderBookPdf(:,j)=orderBookPdf(:,j)+[zeros(shift,1);max(OB(1:(end-(shift)),j+1),0)];
                            elseif shift<0
                                orderBookPdf(:,j)=orderBookPdf(:,j)+[max(OB(-shift+1:end,j+1),0);zeros(-shift,1)];
                            else
                                orderBookPdf(:,j)=orderBookPdf(:,j)+max(OB(:,j+1),0);
                            end
                            
                            shift_fundamental=floor((p0-p)/tickSize+rand());
                            if shift_fundamental>0
                                orderBookPdf_fundm(:,j)=orderBookPdf_fundm(:,j)+[zeros(shift_fundamental,1);max(OB(1:(end-(shift_fundamental)),j+1),0)];
                            elseif shift_fundamental<0
                                orderBookPdf_fundm(:,j)=orderBookPdf_fundm(:,j)+[max(OB(-shift_fundamental+1:end,j+1),0);zeros(-shift_fundamental,1)];
                            else
                                orderBookPdf_fundm(:,j)=orderBookPdf_fundm(:,j)+max(OB(:,j+1),0);
                            end
                            
                            
                            
                            
                            
                            
                            bestBid(i,j)=  bBid;bestOffer(i,j)= bOffer;L_bidOffer(i,j)=bestOffer(i,j)-bestBid(i,j);L_volume(i,j)=sum(abs(OB(:,1+j)));
                            midPrice(i,j)=mid;
                            %decay function
                            ignoreGammaOrderVector=[round(gamma_order(j,1),2)~=round(OB(:,1),2) ];
                            OB(:,j+1)=round(OB(:,j+1).*exp(-decayRate*ignoreGammaOrderVector.*(OB(:,1)-midPrice(i-1,j)).^2),4);
                        end
                    end % end failed loop
                    
                end % end j loop
                
                
                i=i+1;
                
            end % k-loop
            
            %fprintf('We are in day %d and time %d \n', day, time)
            
            
        end %time loop
    end % day loop
    aantalTicks=i;
    dealerPNL=[];
    valuePNL=[];
    randomPNL=[];
    valuePos=[];
    randomPos=[];
    for j=1:dim
        dealerPNL=[dealerPNL,gammaPort(4,1,j)];
        randomPNL=[randomPNL,immediacy_Port(1,1,j)];
        valuePNL=[valuePNL,immediacy_Port(2,1,j)];
        randomPos=[randomPos,immediacy_Port(1,2,j)];
        valuePos=[valuePos,immediacy_Port(2,2,j)];
    end
    CB_Port(2,:)=CB_Port(2,:)+CB_Port(1,:).*(midPrice(i-1,:));
    valuePNL= valuePNL+valuePos.*(midPrice(i-1,:));
    randomPNL= randomPNL+randomPos.*(midPrice(i-1,:));
    
    fprintf('We are in loop %d of  %d \n', superLoop, aantalLoops)
    toc
    %aantalIteraties=i;
    simLength= (1-max(failed))* days*time + max(failed)*(Failed_location(superLoop)-periods); %if the simulation fails, aggregate less
    
    timeStampsUnique=unique(timeStamp);
    timeStampsUnique=timeStampsUnique(2:end);
    realized_volMatrix=zeros(simLength,dim);
    bidAskMatrix=zeros(simLength,dim);
    aggregated_midPrice=zeros(simLength,dim);
    bidVolume_t=zeros(simLength,dim);
    offerVolume_t=zeros(simLength,dim);
    fundamentalPrice=zeros(simLength,1);
    RT_three_B=zeros(simLength,dim);    RT_three_S=zeros(simLength,dim);    RT_ten_B=zeros(simLength,dim);    RT_ten_S=zeros(simLength,dim);
    
    aantalOrders=i;
    aantalperiods=length(timeStampsUnique);
    F=news';
    F=F(:);
    tic
    for i=1:simLength
        fundamentalPrice(i)=p0+sum(F(1:i-1));
        aggregated_midPrice(i,:)=mean(midPrice(timeStamp(:,1)==timeStampsUnique(i),:));
        realized_volMatrix(i,:)=std(midPrice(timeStamp(:,1)==timeStampsUnique(i),:));
        bidAskMatrix(i,:)=mean(bestOffer(timeStamp(:,1)==timeStampsUnique(i),:))-mean(bestBid(timeStamp(:,1)==timeStampsUnique(i),:));
        bidVolume_t(i,:)=mean(bidVolume(timeStamp(:,1)==timeStampsUnique(i),:));
        offerVolume_t(i,:)=mean(offerVolume(timeStamp(:,1)==timeStampsUnique(i),:));
        
    end
    toc
    
    
    
    % bar(orderBookPdf(450:550,[1 2 3 4 5]));
    % legend('-100 contracts','-10 contracts','0','10 contracts','100 contracts')
    %orderBookPdf(497:520,:)
    
    %OB-statistics
    
    pdf_sum=sum( (orderBookPdf.*OB(:,1))).*(1./sum(orderBookPdf))-p0;
    pdf_std=std( (orderBookPdf.*OB(:,1)).*(1./sum(orderBookPdf))) ;
    pdf_skew=skewness( (orderBookPdf.*OB(:,1)).*(1./sum(orderBookPdf))) ;
    pdf_kurt=kurtosis((orderBookPdf.*OB(:,1)).*(1./sum(orderBookPdf))) ;
    absoluteDistanceStorer=(aggregated_midPrice-fundamentalPrice).^2;
    
    if aantalLoops==superLoop && dim>2 && 5==3;
        
        subplot(2,2,1)
        plot([fundamentalPrice  aggregated_midPrice(:,[1 5 9])],'-.');
        legend({'Fundamental Price','-80 Option contracts','0 Option contracts','80 Option contracts'},'Location','SouthEast')
        title('Price development of a single simulation')
        subplot(2,2,2)
        plot([bidVolume_t(:,[1 5 9 ]) ,offerVolume_t(:,[1 5 9])]);
        legend({'bid volume -80 Option contracts','bid volume 0 Option contracts','bid volume 80 Option contracts','offer volume -80 Option contracts','offer volume 0 Option contracts','offer volume 80 Option contracts'},'Location','East')
        title('Order book volume development of a single simulation')
        subplot(2,2,3)
        bar(linspace(-0.42,0.38,540-460+1),orderBookPdf(460:540,[1 5 9])/aantalLoops);
        title('Order book frequency: bid-side, scaled around mid price')
        legend('-40 Option contracts','0 Option contracts','40 Option contracts')
        
        subplot(2,2,4)
        bar(linspace(4.6,5.4,540-460+1),orderBookPdf_fundm(460:540,[1 5 9])/aantalLoops);
        title('Order book frequency: bid-side, scaled around fundamental price')
        legend('-40 Option contracts','0 Option contracts','40 Option contracts')
        
        
        
    end
    
    
    
    notFirstDay=timeStampsUnique(1:simLength)>1;
    mids=[aggregated_midPrice(notFirstDay,:),fundamentalPrice(notFirstDay,1)];
    [n m]=size(mids);
    shocks=[zeros(1,m);mids(2:n,:)-mids(1:n-1,:)];    EODshocks=aggregated_midPrice ;
    if sum(failed)==0
        lastDays= aggregated_midPrice((mod([1:(hedge_frequency*days*hours_per_day)],40)==0),:);
        dayShocks=[lastDays(1,:)-5;lastDays(2:end,:)-lastDays(1:end-1,:)];
        dayShocks_sorted=sort(dayShocks);
        
        
        sorted_shocks=sort(shocks);
        VAR95=sorted_shocks(round(0.05*n,0),:);
        VAR99=sorted_shocks(round(0.01*n,0),:);
        VAR_daily=dayShocks_sorted(2,:);
    end
    ac_matrix=corr([[zeros(1,m);shocks],[shocks;zeros(1,m)]]);
    ac=diag(ac_matrix(m+1:2*m-1,1:m-1));
    price_disc_corr=ac_matrix(end,1:dim);
    stdev_px=std(midPrice);
    heteroSkedasticity=zeros(2,9);

    
    highs=max(aggregated_midPrice(notFirstDay,:));
    lows=min(aggregated_midPrice(notFirstDay,:));
    absDelta=mean(abs(deltaStorer(notFirstDay(2:simLength),:)));
    meanGamma=mean(gammaStorer(notFirstDay(2:simLength),:));
    tradeVolumes=0.001*[sum(abs(transActionLedger1(:,2))),sum(abs(transActionLedger2(:,2))),sum(abs(transActionLedger3(:,2))),sum(abs(transActionLedger4(:,2))),sum(abs(transActionLedger5(:,2))),sum(abs(transActionLedger6(:,2))),sum(abs(transActionLedger7(:,2))),sum(abs(transActionLedger8(:,2))),sum(abs(transActionLedger9(:,2))),sum(abs(transActionLedger10(:,2)))];
    tradeVolumes=tradeVolumes(:,1:dim);
    %%einde impactstuk
    des_des=[gammaTraders(1:dim);hedgeUrgency;failed';mean(shocks(:,1:dim));absDelta;meanGamma]; %6
    stab_des=[stdev_px;highs-lows;ac';mean(absoluteDistanceStorer(notFirstDay,:));price_disc_corr;VAR95(2:10);VAR99(2:10);VAR_daily]; %5
    lid_des=[mean(bidAskMatrix(notFirstDay,:));tradeVolumes;max(mean(bidVolume_t(notFirstDay,:)),0)-min(mean(offerVolume_t(notFirstDay,:)),0);mean(LOS(notFirstDay,:))]; %4
    pdf_des= [pdf_sum;pdf_std;pdf_skew;pdf_kurt]; %4
    cost_des=[dealerPNL;CB_Port(2,:);randomPNL;valuePNL;-(dealerPNL+CB_Port(2,:)+randomPNL+valuePNL)]; %3 (CB port is 2d);
    
    des=[des_des;stab_des;lid_des;pdf_des;cost_des]; %26
    descriptiveStorer(:,:,superLoop)=des;
    drieDpriceStorer(:,:,superLoop)=aggregated_midPrice;
end % end of the superLOOP


meanResults=zeros(27,dim);
stdxResults=zeros(27,dim);
meanResults(1:3,:)=[descriptiveStorer(1:2,:,1);mean(descriptiveStorer(3,:,:),3)];


for k=1:24 %amount of variables -3
    for j=1:dim
        
        vector=reshape(descriptiveStorer(3+k,j,:),1,aantalLoops);
        
        meanResults(k+3,j)=mean(vector);
        stdxResults(k+3,j)=-std(vector); %negative for formatting
        
        
    end
    
end

inputParameters=[tickSize, aantalLoops, days, hours_per_day, hedge_frequency, lambda, immediacy(1), informedness(1),  trend, orderVol, vol, 0, 1 , p0, newsTrend,newsSigma, decayRate ];
parameterTable=array2table(inputParameters','RowNames',{'tickSize','N', 'T','H', 'Q', 'lambda', 'immediacy', 'informedness', 'orderVol', 'trend', 'vol',  'vMin','vMax', 'p0','Trend news','sigma news', 'decayRate'})



des_names={'#options','.1.','hedge distance','.2.','failure-rate','.3.','returns','.4.','mean absolute delta','.5.','average gamma','.6.'}; %6
stab_names={ 'stdev of price','.7.','high-low spread','.8.','autocorrelation of shocks','.9.','squared deviation from fundamental price','.10.','corr(news/shock)','.221.','VAR95','.222.','VAR99','.223.','VARDAILY','.224.',}; %9
liq_names={'bidAsk','.12.','total trade volume','.13.','summed OB volume','.14.','LOS','.99.'};
pdf_names={'bid_mean','.15.','bid_std','.16.','bid_skewness','.17.','bid_kurtosis','.18.'};
cost_des={'pgc: dynamic hedger PnL','.19.','pgc: non-hedger PnL','.20.','noise trader with high demand for immediacy','.21.','informed trader with high demand for immediacy','.22.','participants with no demand for immediacy (passive orders)','.23.'};





des_names(length(des_names)+1:length(des_names)+length(stab_names))=stab_names;
des_names(length(des_names)+1:length(des_names)+length(liq_names))=liq_names;
des_names(length(des_names)+1:length(des_names)+length(pdf_names))=pdf_names;%
des_names(length(des_names)+1:length(des_names)+length(cost_des))=cost_des;


finalResults=zeros(54,dim);
for k=1:26
    finalResults(2*k-1,:)=meanResults(k,:);
    finalResults(2*k,:)=stdxResults(k,:)/sqrt(aantalLoops);

end



desTable=array2table(finalResults, 'RowNames',des_names);


desTable
parameterTable

end








gamma_code.txt
gamma_code.txt wordt weergegeven.