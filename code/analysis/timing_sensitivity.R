# Study analysis and presentation; paths supplied by config.R.
wam_pairing_sensitivity <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(nlme)})
    roles<-c('native_uncalibrated','native_calibrated','filtered_uncalibrated','filtered_calibrated')
    out<-file.path(cfg$output,"timing_sensitivity/pairing")
    dir.create(out,recursive=TRUE,showWarnings=FALSE)
    d<-read.csv(file.path(cfg$masters,"diagnostics/pairing_trials.csv"),stringsAsFactors=FALSE)
    stopifnot(nrow(d)==720)
    write.csv(d,file.path(out,'trial_comparison.csv'),row.names=FALSE)
    summary <- d |> group_by(device,role) |> summarise(n=n(),
     max_abs_change_pp=max(abs(change_pp)),max_saved_disagreement=max(abs(saved_error-original)),
     original_max_cross_stage=sum(original_max_cross_stage),
     original=mean(original),stage_matched=mean(stage_matched),change_pp=mean(change_pp),.groups='drop')
    write.csv(summary,file.path(out,'summary.csv'),row.names=FALSE)
    # Same composite model and within-pipeline Holm device contrasts as WAM.
    fit_comparisons <- function(value) {
     dat <- d |> mutate(instrument=factor(device,levels=c('AX3','GENEActiv','LEAP','SENS motion')),
      pipeline_label=factor(role,levels=roles),device_uid=paste(device,id),outcome=log1p(.data[[value]]))
     fit <- nlme::lme(outcome~instrument*pipeline_label,random=~1|device_uid,data=dat,method='ML',
      control=nlme::lmeControl(maxIter=250,msMaxIter=250,opt='optim'))
     reduced <- nlme::lme(outcome~instrument+pipeline_label,random=~1|device_uid,data=dat,method='ML',
      control=nlme::lmeControl(maxIter=250,msMaxIter=250,opt='optim'))
     grid <- expand.grid(instrument=levels(dat$instrument),pipeline_label=levels(dat$pipeline_label))
     grid$instrument <- factor(grid$instrument,levels=levels(dat$instrument));grid$pipeline_label <- factor(grid$pipeline_label,levels=roles)
     mat <- model.matrix(delete.response(terms(fit)),grid); beta <- nlme::fixef(fit); mat <- mat[,names(beta),drop=FALSE]
     vc <- vcov(fit); df <- min(fit$fixDF$X,na.rm=TRUE)
     pairs <- bind_rows(lapply(roles,function(role) {
      ids <- which(grid$pipeline_label==role); cmb <- combn(ids,2)
      bind_rows(lapply(seq_len(ncol(cmb)),function(j) {
       a<-cmb[1,j];b<-cmb[2,j];c<-mat[a,]-mat[b,];est<-sum(c*beta);se<-sqrt(as.numeric(c%*%vc%*%c))
       data.frame(role=role,device1=grid$instrument[a],device2=grid$instrument[b],estimate=est,p=2*pt(abs(est/se),df,lower.tail=FALSE))
      })) |> mutate(p_holm=p.adjust(p,'holm'))
     }))
     list(pairs=pairs,lrt=as.data.frame(anova(reduced,fit)))
    }
    before <- fit_comparisons('original');after <- fit_comparisons('stage_matched')
    comparisons <- merge(before$pairs,after$pairs,by=c('role','device1','device2'),suffixes=c('_original','_stage')) |>
     mutate(significance_changed=(p_holm_original<.05)!=(p_holm_stage<.05),direction_changed=sign(estimate_original)!=sign(estimate_stage))
    write.csv(comparisons,file.path(out,'device_comparisons.csv'),row.names=FALSE)
    write.csv(before$lrt,file.path(out,'interaction_original.csv'),row.names=FALSE)
    write.csv(after$lrt,file.path(out,'interaction_stage_matched.csv'),row.names=FALSE)
    print(summary,n=16,width=Inf)
    print(comparisons |> filter(significance_changed|direction_changed))
    cat('Maximum baseline discrepancy:',max(abs(d$saved_error-d$original)),'\n')
}

# Study analysis and presentation; paths supplied by config.R.
wam_event_sensitivity <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(nlme)})
    roles<-c('native_uncalibrated','native_calibrated','filtered_uncalibrated','filtered_calibrated')
    out<-file.path(cfg$output,"timing_sensitivity/event")
    dir.create(out,recursive=TRUE,showWarnings=FALSE)
    o<-read.csv(file.path(cfg$masters,"diagnostics/event_trial_outcomes.csv"),stringsAsFactors=FALSE)
    s<-read.csv(file.path(cfg$masters,"diagnostics/event_stage_outcomes.csv"),stringsAsFactors=FALSE)
    # Small outcome-level models only; no raw-data processing.
    device_order<-c('AX3','GENEActiv','LEAP','SENS motion')
    model_audit<-function(dat,metric,arm,composite=FALSE) {
     dat<-dat |> mutate(instrument=factor(device,levels=device_order),
      pipeline_label=factor(role,levels=roles),rpm_factor=if('rpm' %in% names(dat))factor(rpm) else factor(1),
      device_uid=paste(device,id),trial_uid=paste(device,id,trial),
      response=if(metric=='scale_factor').data[[metric]] else log1p(.data[[metric]]))
     fixed<-if(composite)response~instrument*pipeline_label else response~instrument*rpm_factor
     reduced_fixed<-if(composite)response~instrument+pipeline_label else response~instrument+rpm_factor
     random<-if(composite)~1|device_uid else ~1|device_uid/trial_uid
     control<-nlme::lmeControl(maxIter=250,msMaxIter=250,opt='optim')
     full<-nlme::lme(fixed,random=random,data=dat,method='ML',control=control)
     reduced<-nlme::lme(reduced_fixed,random=random,data=dat,method='ML',control=control)
     grid<-if(composite)expand.grid(instrument=device_order,pipeline_label=roles) else
      expand.grid(instrument=device_order,rpm_factor=levels(dat$rpm_factor))
     grid$instrument<-factor(grid$instrument,levels=device_order)
     if(composite)grid$pipeline_label<-factor(grid$pipeline_label,levels=roles) else grid$rpm_factor<-factor(grid$rpm_factor,levels=levels(dat$rpm_factor))
     mat<-model.matrix(delete.response(terms(full)),grid); beta<-nlme::fixef(full);mat<-mat[,names(beta),drop=FALSE]
     vc<-vcov(full);df<-min(full$fixDF$X,na.rm=TRUE)
     grid$condition<-if(composite)as.character(grid$pipeline_label) else as.character(grid$rpm_factor)
     pairs<-bind_rows(lapply(unique(grid$condition),function(condition) {
      cmb<-combn(which(grid$condition==condition),2)
      bind_rows(lapply(seq_len(ncol(cmb)),function(j) {
       a<-cmb[1,j];b<-cmb[2,j];ct<-mat[a,]-mat[b,];estimate<-sum(ct*beta);se<-sqrt(as.numeric(ct%*%vc%*%ct))
       data.frame(metric=metric,arm=arm,condition=condition,device1=as.character(grid$instrument[a]),device2=as.character(grid$instrument[b]),
        estimate=estimate,p_raw=2*pt(abs(estimate/se),df,lower.tail=FALSE))
      })) |> mutate(p_holm=p.adjust(p_raw,'holm'))
     }))
     lrt<-as.data.frame(anova(reduced,full))
     list(pairs=pairs,interaction=data.frame(metric=metric,arm=arm,LR=lrt$L.Ratio[2],p=lrt[['p-value']][2]))
    }
    audits<-list()
    for(arm_name in c('index_stage','timestamp_unshifted','event_aligned')) {
     message('Outcome models: ',arm_name)
     audits[[length(audits)+1]]<-model_audit(o |> filter(arm==arm_name),'composite_error',arm_name,TRUE)
     for(metric in c('non_linearity','abs_bias','scale_factor')) {
      audits[[length(audits)+1]]<-model_audit(s |> filter(arm==arm_name,role=='native_uncalibrated'),metric,arm_name)
     }
    }
    pairs<-bind_rows(lapply(audits,`[[`,'pairs'))
    write.csv(pairs,file.path(out,'device_pairwise_tests.csv'),row.names=FALSE)
    write.csv(bind_rows(lapply(audits,`[[`,'interaction')),file.path(out,'interactions.csv'),row.names=FALSE)
    comparisons<-bind_rows(lapply(c('index_stage','timestamp_unshifted'),function(baseline) {
     merge(pairs |> filter(arm==baseline),pairs |> filter(arm=='event_aligned'),
      by=c('metric','condition','device1','device2'),suffixes=c('_before','_aligned')) |>
      mutate(baseline=baseline,significance_changed=(p_holm_before<.05)!=(p_holm_aligned<.05),
       direction_changed=sign(estimate_before)!=sign(estimate_aligned))
    }))
    write.csv(comparisons,file.path(out,'comparison_changes.csv'),row.names=FALSE)
    counts<-comparisons |> group_by(baseline,metric) |> summarise(contrasts=n(),
     significance_changes=sum(significance_changed),direction_changes=sum(direction_changed),.groups='drop')
    write.csv(counts,file.path(out,'comparison_change_summary.csv'),row.names=FALSE)
    print(counts,width=Inf)
    print(o |> filter(arm!='index_stage') |> group_by(device,role,arm) |> summarise(mean=mean(composite_error),.groups='drop'),n=32)
}
