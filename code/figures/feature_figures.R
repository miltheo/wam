# Study analysis and presentation; paths supplied by config.R.
wam_feature_figures <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(gridExtra);library(ragg)})
    root<-file.path(cfg$output,"features");options(device=function(...)ragg::agg_capture())
    output<-root
    response_only<-FALSE
    dir.create(output,recursive=TRUE,showWarnings=FALSE)
    theme_pub<-theme_minimal(base_family='Arial',base_size=10.5)+theme(plot.title=element_text(face='bold',hjust=0),plot.subtitle=element_text(hjust=0),axis.title=element_text(face='bold'),panel.grid.minor=element_blank(),legend.title=element_text(face='bold'),plot.margin=margin(8,10,8,10))
    cols<-c('AX3'='#56B4E9','GENEActiv'='#009E73','LEAP'='#CC79A7','SENS motion'='#E69F00','wGT3X-BT'='#F0E442','GT9X Link'='#D55E00')
    status_cols<-c('Both full-range criteria met'='#009E73','Response criterion only'='#0072B2','Equivalence criterion only'='#E69F00','Neither criterion met'='#999999')
    axis_audits<-list();response_audits<-list()
    qualifier<-function(metric,arm)ifelse(metric%in%c('ROCAM','SVMGS','SVMGSEC'),'\u2020',ifelse(grepl('^primary',arm)&metric=='SVMBP_02_15','*',''))
    response_breaks<-function(limits,q){
     ticks<-pretty(limits)
     if(max(limits)<=2)sort(unique(c(ticks[abs(ticks-.95)>.07],.95)))else sort(unique(c(ticks,q)))
    }
    save_both<-function(p,file,w=9.2,h=11.2){ggsave(paste0(file,'.png'),p,width=w,height=h,units='in',dpi=300,device=ragg::agg_png,bg='white');ggsave(paste0(file,'.pdf'),p,width=w,height=h,units='in',device=cairo_pdf,bg='white')}
    for(arm in c('primary_native','primary_calibrated','secondary_native','secondary_calibrated')){
     message('Rendering ',arm);x<-readRDS(file.path(root,arm,'analysis.rds'));np<-unique(x$stage_equivalence$pairs_total);total<-10*np
     dir<-file.path(output,arm,'figures');dir.create(dir,recursive=TRUE,showWarnings=FALSE)
     order<-x$qualification|>filter(margin_pct==10)|>arrange(performance_rank)|>pull(metric_label)
     rpm<-seq(50,500,50);context<-c('lie','sit','v.light','light','mod','h.mod','vig','v.vig','v.vig','ext')
     labels<-paste0(rpm,' RPM\n',sprintf('%.2f',rpm/60),' Hz\n',sprintf('%.3f',(.019/2)*(2*pi*rpm/60)^2/9.81),' g\n',context)
     profiles<-x$unit_stage|>group_by(metric,metric_label,metric_unit,device,rpm,input_value)|>summarise(mean_value=mean(value),lo=mean(value)-qt(.975,n()-1)*sd(value)/sqrt(n()),hi=mean(value)+qt(.975,n()-1)*sd(value)/sqrt(n()),.groups='drop')|>
      mutate(metric_title=factor(paste0(metric_label,' [',metric_unit,']'),levels=paste0(order,' [',x$qualification$metric_unit[match(order,x$qualification$metric_label)],']')))
     for(q in 10){
      # Measure arranged grobs on a full-sized device before saving; the default
      # capture device can underestimate title and axis heights in later panels.
      metrics_file<-tempfile(pattern='wam_layout_',tmpdir=dir,fileext='.png')
      ragg::agg_png(metrics_file,width=9.2,height=11.2,units='in',res=100)
      heat<-x$stage_equivalence|>filter(margin_pct==q)|>mutate(metric_label=factor(metric_label,levels=rev(order)),x=match(rpm,seq(50,500,50)))
      pa<-ggplot(heat,aes(x,metric_label,fill=pairs_passing))+geom_tile(colour='white',linewidth=.45,width=1,height=1)+geom_text(aes(label=paste0(pairs_passing,'/',pairs_total)),size=3)+geom_vline(xintercept=c(4.5,7.5),linewidth=.65,colour='#333333')+
       annotate('text',x=c(2.5,6,9),y=17.15,label=c('Lower excitation\nInactivity to light','Intermediate excitation\nModerate to vigorous','Higher excitation\nVery vigorous to extreme'),family='Arial',fontface='bold',size=3,lineheight=1.05)+
       scale_fill_gradientn(colours=c('#FDE0DD','#FEC44F','#A1D99B','#238B45'),limits=c(0,np),breaks=0:np,name='Equivalent\ndevice pairs')+
       scale_x_continuous(breaks=1:10,labels=wam_gravity_labels(labels),expand=expansion(add=.5))+scale_y_discrete(expand=expansion(add=c(.5,2.1)))+
       labs(title=paste0('A. Stage-specific inter-device equivalence at +/-',q,'%'),subtitle=paste0('Each cell shows Welch-TOST-equivalent device pairs out of ',np),x='Orbital shaker condition',y=NULL)+theme_pub+theme(axis.text.x=element_text(size=8.4,lineheight=.95),panel.grid=element_blank())
      band<-x$qualification|>mutate(metric_label=factor(metric_label,levels=rev(order)),margin=factor(paste0('+/-',margin_pct,'%'),levels=c('+/-5%','+/-10%','+/-15%')),pass_pct=100*passing/total)
      pb<-ggplot(band,aes(pass_pct,metric_label,colour=margin,shape=margin))+geom_vline(xintercept=100,linetype='dashed',colour='grey50')+geom_point(aes(size=margin),stroke=.7)+
       scale_size_manual(values=c('+/-5%'=1.7,'+/-10%'=3.4,'+/-15%'=4.6),guide='none')+scale_colour_manual(values=c('+/-5%'='#0072B2','+/-10%'='#E69F00','+/-15%'='#009E73'))+scale_shape_manual(values=c('+/-5%'=16,'+/-10%'=2,'+/-15%'=0))+
       scale_x_continuous(limits=c(0,102),breaks=seq(0,100,25))+labs(title='B. Sensitivity of pairwise equivalence to the tolerance band',subtitle=paste0('Percentage of ',total,' device-pair-by-stage TOSTs passing; dashed line = full-range equivalence'),x='Passing pair-by-stage TOSTs (%)',y=NULL,colour='Tolerance',shape='Tolerance')+theme_pub+theme(legend.position='bottom')
      if(!response_only)save_both(arrangeGrob(pa,pb,ncol=1,heights=c(1.22,1)),file.path(dir,paste0('equivalence_',q)))
      qualification<-x$qualification|>filter(margin_pct==q)|>mutate(status=case_when(both_criteria~'Both full-range criteria met',adequate_response~'Response criterion only',universal_equivalence~'Equivalence criterion only',TRUE~'Neither criterion met'))
      dr<-x$device_response|>left_join(qualification|>select(metric,metric_label,status),by='metric')
      rd<-bind_rows(dr|>group_by(metric)|>slice_min(median_r2,n=1,with_ties=FALSE)|>transmute(metric_label,status,statistic='R2',estimate=median_r2,lower=r2_ci_low,upper=r2_ci_high),
       dr|>group_by(metric)|>slice_max(inl_ci_high,n=1,with_ties=FALSE)|>transmute(metric_label,status,statistic='Response INL (%)',estimate=median_inl_pct,lower=inl_ci_low,upper=inl_ci_high))|>mutate(metric_label=factor(metric_label,levels=rev(order)))
      display_names<-setNames(paste0(order,qualifier(qualification$metric[match(order,qualification$metric_label)],arm)),order)
      pp<-ggplot(profiles,aes(input_value,mean_value,colour=device,linetype=device))+geom_errorbar(aes(ymin=lo,ymax=hi),width=0,linewidth=.3)+geom_line(linewidth=.7,alpha=.9)+geom_point(size=1.45)+
       facet_wrap(~metric_title,scales='free',ncol=5,labeller=as_labeller(function(x) as.character(wam_gravity_labels(vapply(x,
           function(label) paste(strwrap(label,width=24),collapse="\n"),character(1)))),default=label_parsed))+scale_colour_manual(values=cols)+scale_linetype_manual(values=c('AX3'='solid','GENEActiv'='longdash','LEAP'='dashed','SENS motion'='dotted','wGT3X-BT'='solid','GT9X Link'='longdash'))+
       labs(title='A. Response profiles of evaluated feature pipelines',subtitle='Metric-specific y-axis scales retain each feature\'s native unit',x=expression("Predicted centripetal acceleration ("*italic(g)*"); programmed Hz for frequency"),y='Native feature value',colour='Device',linetype='Device')+theme_pub+theme(legend.position='bottom',strip.text=element_text(size=7.2),plot.margin=margin(12,10,15,10))
      pr<-ggplot(rd,aes(estimate,metric_label,colour=status))+geom_vline(data=tibble(statistic=c('R2','Response INL (%)'),threshold=c(.95,q)),aes(xintercept=threshold),linetype='dashed',colour='grey45')+
       geom_errorbar(aes(xmin=lower,xmax=upper),width=.18,orientation='y',linewidth=.5)+geom_point(size=2.5)+facet_wrap(~statistic,scales='free_x',nrow=1)+scale_colour_manual(values=status_cols)+
       scale_x_continuous(breaks=function(limits)response_breaks(limits,q))+scale_y_discrete(labels=display_names)+
       labs(title='B. Least-favourable device controlled-condition response',subtitle='Points and whiskers show median and 95% bootstrap confidence interval',x=NULL,y=NULL,colour=NULL)+theme_pub+theme(legend.position='bottom',legend.text=element_text(size=7),legend.key.width=grid::unit(.3,'cm'))+guides(colour=guide_legend(nrow=2,byrow=TRUE))
      # Check the rendered facet scales, including their units, against the input data.
      built<-ggplot_build(pp)
      for(j in seq_len(nrow(built$layout$layout))){
       facet<-built$layout$layout[j,];z<-profiles[profiles$metric_title==facet$metric_title,]
       lim<-built$layout$panel_scales_x[[facet$SCALE_X]]$range$range
       stopifnot(length(unique(z$metric))==1L,max(abs(lim-range(z$input_value)))<1e-10)
       if(unique(z$metric)=='DOMFREQ_CLASS_5S')stopifnot(abs(max(lim)-500/60)<1e-10)else stopifnot(max(lim)<3)
       axis_audits[[length(axis_audits)+1L]]<-data.frame(arm=arm,tolerance=q,metric=unique(z$metric),input_unit=if(unique(z$metric)=='DOMFREQ_CLASS_5S')'Hz'else'g',input_min=min(lim),input_max=max(lim),conditions=length(unique(z$rpm)))
      }
      rb<-ggplot_build(pr)
      for(j in seq_len(nrow(rb$layout$layout))){
       facet<-rb$layout$layout[j,];threshold<-if(facet$statistic=='R2').95 else q
       stopifnot(any(abs(rb$layout$panel_scales_x[[facet$SCALE_X]]$get_breaks()-threshold)<1e-10))
      }
      response_audits[[length(response_audits)+1L]]<-mutate(rd,arm=arm,tolerance=q)
      pp<-pp+labs(title=pp$labels$title,subtitle=pp$labels$subtitle,x=pp$labels$x,y=pp$labels$y)
      pr<-pr+labs(title=pr$labels$title,subtitle=pr$labels$subtitle)
      save_both(arrangeGrob(pp,pr,ncol=1,heights=c(1.90,1.12)),file.path(dir,paste0('response_',q)))
      pair<-x$pairwise_tost|>filter(margin_pct==q)|>left_join(qualification|>select(metric,metric_label),by='metric')|>mutate(pair=paste(device_1,device_2,sep=' - '),metric_label=factor(metric_label,levels=rev(order)))
      ps<-pair|>group_by(metric_label,pair)|>summarise(passing=sum(equivalent),.groups='drop')
      p1<-ggplot(ps,aes(pair,metric_label,fill=passing))+geom_tile(colour='white')+geom_text(aes(label=passing),size=3)+scale_fill_gradient(low='#FDE0DD',high='#238B45',limits=c(0,10),name='Passing\nstages')+labs(title=paste0('A. Pair-specific passing stages at +/-',q,'%'),x=NULL,y=NULL)+theme_pub+theme(axis.text.x=element_text(angle=25,hjust=1))
      p2<-ggplot(pair,aes(factor(rpm),metric_label,fill=equivalent))+geom_tile(colour='white')+facet_wrap(~pair,ncol=3)+scale_fill_manual(values=c('FALSE'='#FDE0DD','TRUE'='#238B45'),name='Equivalent')+labs(title='B. Individual stage decisions',x='Shaker condition (RPM)',y=NULL)+theme_pub+theme(axis.text.x=element_text(angle=90,size=7),axis.text.y=element_text(size=6),strip.text=element_text(size=8))
      if(!response_only)save_both(arrangeGrob(p1,p2,ncol=1,heights=c(1,1.4)),file.path(dir,paste0('pairs_',q)),h=12)
      dev.off();unlink(metrics_file)
     }
    }
    if(!response_only)for(cohort in c('primary','secondary'))for(q in 10){
     native<-readRDS(file.path(root,paste0(cohort,'_native'),'analysis.rds'))$qualification|>filter(margin_pct==q)
     calibrated<-readRDS(file.path(root,paste0(cohort,'_calibrated'),'analysis.rds'))$qualification|>filter(margin_pct==q)
     order<-native|>arrange(performance_rank)|>pull(metric_label)
     z<-bind_rows(native|>mutate(pipeline='Native'),calibrated|>mutate(pipeline='Dynamic VM calibration'))|>mutate(metric_label=factor(metric_label,levels=rev(order)))
     p<-ggplot(z,aes(passing,metric_label,colour=pipeline,shape=pipeline))+geom_vline(xintercept=max(z$total),linetype='dashed',colour='grey50')+geom_point(size=2.6)+scale_colour_manual(values=c('Native'='#000000','Dynamic VM calibration'='#009E73'))+scale_shape_manual(values=c('Native'=16,'Dynamic VM calibration'=17))+scale_x_continuous(limits=c(0,max(z$total)+2),breaks=seq(0,max(z$total),length.out=5))+labs(title=paste0('Processing-pipeline sensitivity at +/-',q,'%'),subtitle=paste0('Passing Welch TOSTs out of ',max(z$total),' device-pair-by-stage comparisons'),x='Passing device-pair-by-stage TOSTs',y=NULL,colour='Pipeline',shape='Pipeline')+theme_pub+theme(legend.position='bottom')
     save_both(p,file.path(output,paste0(cohort,'_pipeline_',q)),h=7.8)
    }
    write.csv(bind_rows(axis_audits),file.path(output,'response_axis_checks.csv'),row.names=FALSE)
    write.csv(bind_rows(response_audits),file.path(output,'response_display_values.csv'),row.names=FALSE)
    message('All figures rendered from completed statistical outputs.')
}
