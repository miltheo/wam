# Study analysis and presentation; paths supplied by config.R.
wam_features <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(tidyr);library(tibble);library(digest)})
    out<-file.path(cfg$output,"features")
    dir.create(out,recursive=TRUE,showWarnings=FALSE)
    fun<-new.env(parent=globalenv())
    sys.source(file.path(cfg$root,"code/feature_statistics.R"),envir=fun)
    fun$bootstrap_reps<-5000L
    registry<-read.csv(file.path(cfg$root,"code/feature_pipelines.csv"),stringsAsFactors=FALSE)
    arms<-list(primary_native=c('features_primary','Native','primary'),primary_calibrated=c('features_primary','Native + VM calibration','primary'),secondary_native=c('features_secondary_native','Native','secondary'),secondary_calibrated=c('features_secondary_calibrated','Native + VM calibration','secondary'))
    summary_all<-list();checks<-list();input_files<-list()
    for(arm in names(arms)){
     message('Analysing ',arm)
     a<-arms[[arm]];path<-file.path(cfg$masters,paste0(a[1],'.rds'));master<-readRDS(path)
     input_files[[arm]]<-data.frame(arm=arm,path=path,sha256=digest(readBin(path,'raw',n=file.info(path)$size),serialize=FALSE,algo='sha256'))
     devices<-if(a[3]=='primary')c('AX3','GENEActiv','LEAP','SENS')else c('LEAP','wGT3X-BT','GT9X Link')
     expected_trials<-if(a[3]=='primary')3L else 2L;npairs<-choose(length(devices),2)
     raw<-master|>filter(instrument%in%devices,signal_pipeline==a[2],metric%in%registry$metric,was_computed,is.finite(mean_value),rpm%in%seq(50,500,50))|>
      transmute(instrument,device_id=as.character(device_id),trial=as.character(trial_start_utc),rpm=as.numeric(rpm),metric,value=as.numeric(mean_value))|>
      left_join(registry,by='metric')|>mutate(unit_id=paste(instrument,device_id,sep='_'),device=ifelse(instrument=='SENS','SENS motion',instrument),input_value=ifelse(input_type=='frequency',rpm/60,fun$centripetal_g(rpm)))
     stopifnot(!anyDuplicated(raw[c('unit_id','trial','rpm','metric')]))
     unit<-raw|>group_by(metric,metric_label,metric_family,metric_unit,input_type,performance_rank,instrument,device,unit_id,rpm,input_value)|>summarise(value=mean(value),n_trials=n_distinct(trial),.groups='drop')
     audit<-unit|>count(metric,instrument,rpm,name='n_units')
     stopifnot(all(unit$n_trials==expected_trials),all(audit$n_units==15L),nrow(unit)==16L*length(devices)*15L*10L)
     stage_mean<-unit|>group_by(metric,instrument,rpm)|>summarise(mean_value=mean(value),.groups='drop')
     spans<-stage_mean|>filter(rpm%in%c(50,500))|>pivot_wider(names_from=rpm,values_from=mean_value,names_prefix='rpm_')|>mutate(span=abs(rpm_500-rpm_50))|>group_by(metric)|>summarise(dynamic_span=median(span),.groups='drop')
     tests<-bind_rows(lapply(c(5,10,15),function(q){unit|>left_join(spans,by='metric')|>mutate(margin_pct=q,absolute_margin=dynamic_span*q/100)|>group_by(metric,rpm,margin_pct,absolute_margin)|>group_modify(function(dat,key){bind_rows(lapply(combn(sort(unique(dat$instrument)),2,simplify=FALSE),function(pair){fun$welch_tost(dat$value[dat$instrument==pair[1]],dat$value[dat$instrument==pair[2]],key$absolute_margin[[1]])|>mutate(device_1=pair[1],device_2=pair[2])}))})|>ungroup()}))
     stopifnot(nrow(tests)==16L*10L*npairs*3L,!anyNA(tests$p_equivalence))
     stage<-tests|>group_by(metric,rpm,margin_pct)|>summarise(pairs_passing=sum(equivalent),pairs_total=n(),all_pairs_equivalent=all(equivalent),max_tost_p=max(p_equivalence),.groups='drop')|>left_join(registry,by='metric')
     response<-unit|>group_by(metric,instrument,device,unit_id,input_type)|>group_modify(function(dat,key)fun$fit_response(dat,key$input_type[[1]]))|>ungroup()
     set.seed(20260713)
     device_response<-response|>group_by(metric,instrument,device)|>group_modify(function(dat,key)fun$bootstrap_response(dat,reps=5000L))|>ungroup()
     stopifnot(all(device_response$n_units==15L),!anyNA(device_response$inl_ci_high))
     qualification<-stage|>group_by(metric,margin_pct)|>summarise(passing=sum(pairs_passing),total=sum(pairs_total),universal_equivalence=all(all_pairs_equivalent),global_iut_p=max(max_tost_p),.groups='drop')|>
      left_join(device_response|>group_by(metric)|>summarise(lowest_median_r2=min(median_r2),highest_upper_inl_pct=max(inl_ci_high),all_positive_slopes=all(slope_ci_low>0),.groups='drop'),by='metric')|>
      mutate(adequate_response=all_positive_slopes&highest_upper_inl_pct<=margin_pct,both_criteria=universal_equivalence&adequate_response)|>left_join(registry,by='metric')
     regional<-stage|>mutate(region=case_when(rpm<=200~'50-200 RPM',rpm<=350~'250-350 RPM',TRUE~'400-500 RPM'))|>group_by(metric,margin_pct,region)|>summarise(passing=sum(pairs_passing),total=sum(pairs_total),all_pairs_equivalent=all(all_pairs_equivalent),.groups='drop')|>left_join(registry,by='metric')
     # Independent reconstruction of all TOST decisions from their 90% limits.
     stopifnot(all(tests$equivalent==(tests$ci90_low> -tests$margin & tests$ci90_high<tests$margin & tests$p_equivalence<.05)))
     checks[[arm]]<-data.frame(arm=arm,unit_stage_rows=nrow(unit),units=n_distinct(unit$unit_id),trials_per_unit=expected_trials,pair_stage_tests=nrow(tests))
     folder<-file.path(out,arm);dir.create(folder,recursive=TRUE,showWarnings=FALSE)
     objects<-list(unit_stage=unit,spans=spans,pairwise_tost=tests,stage_equivalence=stage,unit_response=response,device_response=device_response,qualification=qualification,regional=regional)
     for(n in names(objects))write.csv(objects[[n]],file.path(folder,paste0(n,'.csv')),row.names=FALSE)
     saveRDS(objects,file.path(folder,'analysis.rds'));summary_all[[arm]]<-qualification|>mutate(arm=arm)
     message('Completed ',arm,': ',nrow(raw),' retained trial-stage-feature rows, ',nrow(tests),' TOSTs')
    }
    write.csv(bind_rows(summary_all),file.path(out,'all_arm_qualification.csv'),row.names=FALSE)
    write.csv(bind_rows(checks),file.path(out,'analysis_checks.csv'),row.names=FALSE)
    write.csv(bind_rows(input_files),file.path(out,'input_provenance.csv'),row.names=FALSE)
    writeLines(capture.output(sessionInfo()),file.path(out,'session.txt'))
}
