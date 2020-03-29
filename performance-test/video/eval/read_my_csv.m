d=dir;
d(1:2)=[];
indy=[]
for tt=1:length(d);
    if ~isempty(strfind(d(tt).name,'.m')) | ~isempty(strfind(d(tt).name,'.asv')) | ~isempty(strfind(d(tt).name,'function'))
       indy=[indy tt];     
    end
end
d(indy)=[];
error=0;


for la=1:length(d);
    d2=dir([d(la).name '/data']);
    d2(1:2)=[];
    for le=1:length(d2);
        clear run;
        path=[d(la).name '/data/' d2(le).name '/json']
        
        fid=fopen([path '/cqm_inid.csv']);
        C = textscan(fid,'%s %s %s %s %s %f','Delimiter',',');
            for i=1:size(C{1})
                run(i).video=C{1}{i};
                run(i).config=C{2}{i};
                run(i).start=C{3}{i};
                run(i).abr=C{4}{i};
                run(i).policy=C{5}{i};
                run(i).inid=C{6}(i);
            end;
            fclose(fid);
            clear C,fid
            fid1=fopen([path '/cqm_sparr.csv']);
            fid2=fopen([path '/cqm_sdarr.csv']);
            C = textscan(fid1,'%*s %*s %s %s %s %*f %f','Delimiter',',');
            D = textscan(fid2,'%*s %*s %s %s %s %*f %f','Delimiter',',');

            for i=1:size(run,2)
                indy=strcmp(C{1},run(i).start);
                if(sum(indy)==0)
                    run(i).sparr=[];
                    run(i).sdarr=[];
                else
                    run(i).sparr=C{4}(indy);
                    run(i).sdarr=D{4}(indy);
                end
            end;
            fclose(fid1);
            fclose(fid2);

        clear C,D,fid1,fid2

        fid=fopen([path '/cqm_sqarr.csv']);
        C = textscan(fid,'%*s %*s %s %s %s %*f %f','Delimiter',',');

        for i=1:size(run,2)
            indy=strcmp(C{1},run(i).start);
            run(i).sqarr=C{4}(indy)';
        end;
        fclose(fid);

%% compute cqv;

        for i=1:size(run,2);
            run(i).cqm=F_CQM(run(i).inid,run(i).sqarr,run(i).sparr,run(i).sdarr);
            % plot(run(i).cqm,'Color',cols(i,:));
        end;
        fid=fopen([path '/cqm_vals.csv'],'wb');
        for i=1:size(run,2);
        %formatspec = 
            for k=1:size(run(i).cqm,2);
                %fprintf(fid,[run(i).video ',' run(i).config ',' run(i).start ',' run(i).cqm(k) '\n']); 
                fprintf(fid,'%s,%s,%s,%1.4f\n',run(i).video, run(i).config, run(i).start,run(i).cqm(k));
            end;
        end
        if size(run,2)<18
            error=error+1;
            myerrorlog(error).run=run;
        end;
        run(i).cqm=[];
        %legend([{run.policy}])
        fclose(fid);
    end
end
        %%end










