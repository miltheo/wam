# Study analysis and presentation; paths supplied by config.R.
wam_threshold_transfer <- function(cfg) {
    suppressPackageStartupMessages({library(dplyr);library(ggplot2);library(gridExtra);library(ragg)})
    script_dir<-file.path(cfg$root,"code/settings")
    static_file<-file.path(cfg$output,"static/static_clean_master.rds");feature_root<-file.path(cfg$output,"features");output_root<-file.path(cfg$output,"threshold_transfer")
    out<-file.path(output_root,'data');dir.create(out,recursive=TRUE,showWarnings=FALSE)
    ci95<-function(x)qt(.975,length(x)-1)*sd(x)/sqrt(length(x))
    st <- readRDS(static_file)
    st$device <- as.character(st$device)
    st$device[st$device=='GT3X-BT']<-'wGT3X-BT'
    stopifnot(nrow(st)==90,all(table(st$device)==15))
    tr <- bind_rows(lapply(1:60,function(day){
      st |> transmute(device,unit_id=as.character(id),day=day,elapsed_h=(day-1)*24,
                     bias_mg=vm_offset_mg+(day-1)*24*vm_drift_mg_h)
    }))
    ss <- tr |> group_by(device,day,elapsed_h) |>
      summarise(mean=mean(bias_mg),lo=mean(bias_mg)-ci95(bias_mg),hi=mean(bias_mg)+ci95(bias_mg),n=n(),.groups='drop')
    write.csv(tr,file.path(out,'static_60day_projection_by_unit.csv'),row.names=FALSE)
    write.csv(ss,file.path(out,'static_60day_projection_summary.csv'),row.names=FALSE)# Fixed published ENMO/MAD cutoffs; SVMbp cutoffs are analytically extrapolated.
    cuts <- read.csv(file.path(script_dir,'threshold_cutoffs.csv'),stringsAsFactors=FALSE)
    cuts$source_ENMO_cutoff_mg <- ifelse(cuts$metric=='ENMO',cuts$cutoff_mg,NA_real_)
    cuts$derivation <- 'Published cutoff transferred to native mechanical data'
    s_cuts <- cuts[cuts$metric=='ENMO',]
    s_cuts$metric <- 'SVMBP_069_10'
    # Numerical values E,S are in g: E=sqrt(1+a^2)-1; S=a under unity filter gain.
    # This does not transform measured ENMO values and then reclassify them.
    s_cuts$cutoff_g <- sqrt((s_cuts$source_ENMO_cutoff_mg/1000)*(s_cuts$source_ENMO_cutoff_mg/1000+2))
    s_cuts$cutoff_mg <- 1000*s_cuts$cutoff_g
    s_cuts$source <- paste0('Extrapolated ideal-orbital cutoffs from ',s_cuts$source)
    s_cuts$placement <- 'Ideal-orbital extrapolation; no validated behavioural placement'
    s_cuts$derivation <- 'S=sqrt(E*(E+2)); E and S in g; horizontal circular acceleration, 1-g gravity, unit filter gain'
    cuts <- bind_rows(cuts,s_cuts)
    boundary_levels <- c('Light or above','Moderate or above','Vigorous')
    cuts$boundary <- factor(cuts$boundary,levels=boundary_levels)
    write.csv(cuts,file.path(out,'threshold_provenance.csv'),row.names=FALSE)
    write.csv(s_cuts,file.path(out,'extrapolated_ENMO_to_SVMbp_cutoffs.csv'),row.names=FALSE)
    
    rpm <- seq(50,350,50);context<-c('lie','sit','v.light','light','mod','h.mod','vig')
    conditions <- data.frame(rpm,frequency_hz=rpm/60,acceleration_g=.0095*(2*pi*rpm/60)^2/9.81,behaviour=context)
    condition_labels <- setNames(paste0(rpm,' RPM\n',sprintf('%.2f',conditions$frequency_hz),' Hz, ',
                                      sprintf('%.3f',conditions$acceleration_g),' g\n',context),rpm)
    write.csv(conditions,file.path(out,'condition_mapping.csv'),row.names=FALSE)
    features <- bind_rows(lapply(c('primary_native','secondary_native'),function(arm){
      z<-read.csv(file.path(feature_root,arm,'unit_stage.csv'),stringsAsFactors=FALSE)
      z<-z[z$metric%in%c('ENMO','MAD','SVMBP_069_10')&z$rpm%in%rpm,]
      z$cohort<-if(arm=='primary_native')'Primary comparison'else'Secondary comparison';z
    }))
    stopifnot(nrow(features)==2205,all(is.finite(features$value)),all(features$value>=0))
    profiles <- features |> group_by(metric,cohort,device,rpm) |>
      summarise(mean=mean(value)/1000,lo=(mean(value)-ci95(value))/1000,
                hi=(mean(value)+ci95(value))/1000,n=n(),.groups='drop')
    unit_minutes <- bind_rows(lapply(seq_len(nrow(cuts)),function(k){
      features |> filter(metric==cuts$metric[k]) |> group_by(metric,cohort,device,unit_id) |>
        summarise(minutes_above=sum(value>=cuts$cutoff_mg[k])*60/7,.groups='drop') |>
        mutate(boundary=as.character(cuts$boundary[k]),cutoff_mg=cuts$cutoff_mg[k])
    }))
    minute_summary <- unit_minutes |> group_by(metric,cohort,device,boundary,cutoff_mg) |>
      summarise(mean=mean(minutes_above),minimum=min(minutes_above),maximum=max(minutes_above),.groups='drop')
    write.csv(profiles,file.path(out,'native_profiles.csv'),row.names=FALSE)
    write.csv(unit_minutes,file.path(out,'unit_minutes_all_cutoffs.csv'),row.names=FALSE)
    write.csv(minute_summary,file.path(out,'minutes_summary.csv'),row.names=FALSE)
    
    # Independent paired classification comparison: actual ENMO and actual SVMbp.
    paired <- inner_join(features |> filter(metric=='ENMO') |> select(cohort,device,unit_id,rpm,ENMO_mg=value),
                         features |> filter(metric=='SVMBP_069_10') |> select(cohort,device,unit_id,rpm,SVMbp_mg=value),
                         by=c('cohort','device','unit_id','rpm'))
    stopifnot(nrow(paired)==735)
    agreement <- bind_rows(lapply(1:3,function(k){
      paired |> mutate(boundary=as.character(s_cuts$boundary[k]),ENMO_cutoff_mg=s_cuts$source_ENMO_cutoff_mg[k],
                       SVMbp_cutoff_mg=s_cuts$cutoff_mg[k],ENMO_above=ENMO_mg>=ENMO_cutoff_mg,
                       SVMbp_above=SVMbp_mg>=SVMbp_cutoff_mg,agree=ENMO_above==SVMbp_above)
    }))
    agreement_summary <- agreement |> group_by(cohort,device,boundary,ENMO_cutoff_mg,SVMbp_cutoff_mg) |>
      summarise(paired_unit_stages=n(),agree_n=sum(agree),agreement_pct=100*mean(agree),
                both_above=sum(ENMO_above&SVMbp_above),ENMO_only=sum(ENMO_above&!SVMbp_above),
                SVMbp_only=sum(!ENMO_above&SVMbp_above),both_below=sum(!ENMO_above&!SVMbp_above),.groups='drop')
    agreement_cohort <- agreement |> group_by(cohort,boundary,ENMO_cutoff_mg,SVMbp_cutoff_mg) |>
      summarise(paired_unit_stages=n(),agree_n=sum(agree),agreement_pct=100*mean(agree),.groups='drop')
    duration_agreement <- inner_join(unit_minutes |> filter(metric=='ENMO') |> select(cohort,device,unit_id,boundary,ENMO_minutes=minutes_above),
                                     unit_minutes |> filter(metric=='SVMBP_069_10') |> select(cohort,device,unit_id,boundary,SVMbp_minutes=minutes_above),
                                     by=c('cohort','device','unit_id','boundary')) |>
      mutate(SVMbp_minus_ENMO_min=SVMbp_minutes-ENMO_minutes)
    write.csv(agreement,file.path(out,'ENMO_SVMbp_paired_classifications.csv'),row.names=FALSE)
    write.csv(agreement_summary,file.path(out,'ENMO_SVMbp_classification_agreement.csv'),row.names=FALSE)
    write.csv(agreement_cohort,file.path(out,'ENMO_SVMbp_cohort_agreement.csv'),row.names=FALSE)
    write.csv(duration_agreement,file.path(out,'ENMO_SVMbp_paired_unit_minutes.csv'),row.names=FALSE)
    
    write.csv(features,file.path(out,'unit_stage_features.csv'),row.names=FALSE)
    previous<-out;out<-output_root
    capture_device<-ragg::agg_capture(width=16,height=9.8,units='in',res=120)
    cols<-c('AX3'='#56B4E9','GENEActiv'='#009E73','LEAP'='#CC79A7',
            'SENS motion'='#E69F00','wGT3X-BT'='#F0E442','GT9X Link'='#D55E00')
    boundary_levels<-c('Light or above','Moderate or above','Vigorous')
    line_types<-c('Light or above'='dotted','Moderate or above'='dashed','Vigorous'='longdash')
    theme_p<-theme_minimal(base_family='Arial',base_size=11)+
      theme(plot.title=element_text(face='bold',size=13),plot.subtitle=element_text(size=10),
            panel.grid.minor=element_blank(),legend.position='none',legend.title=element_blank(),
            legend.text=element_text(size=10),plot.margin=margin(10,12,10,10),
            axis.title.y=element_text(margin=margin(r=14)),axis.text.y=element_text(margin=margin(r=5)))
    save_fig<-function(fig,name,w,h){
      ggsave(file.path(out,paste0(name,'.png')),fig,width=w,height=h,dpi=300,device=ragg::agg_png,bg='white')
      ggsave(file.path(out,paste0(name,'.pdf')),fig,width=w,height=h,device=cairo_pdf,bg='white')
    }
    
    ss<-read.csv(file.path(previous,'static_60day_projection_summary.csv'),stringsAsFactors=FALSE)
    profiles<-read.csv(file.path(previous,'native_profiles.csv'),stringsAsFactors=FALSE)
    minutes<-read.csv(file.path(previous,'minutes_summary.csv'),stringsAsFactors=FALSE)
    cuts<-read.csv(file.path(previous,'threshold_provenance.csv'),stringsAsFactors=FALSE)
    conditions<-read.csv(file.path(previous,'condition_mapping.csv'),stringsAsFactors=FALSE)
    stopifnot(nrow(ss)==360,nrow(profiles)==147,nrow(minutes)==63,nrow(cuts)==9,nrow(conditions)==7)
    for(name in c('static_60day_projection_summary.csv','native_profiles.csv','minutes_summary.csv',
                  'threshold_provenance.csv','condition_mapping.csv')){
      stopifnot(file.copy(file.path(previous,name),file.path(out,name),overwrite=TRUE))
    }
    write.csv(data.frame(device=names(cols),hex=unname(cols)),file.path(out,'device_palette.csv'),row.names=FALSE)
    
    # Panel A: same 60-day projection, lighter dashed curves, no repeated legend.
    a<-ggplot(ss,aes(day,mean,colour=device,fill=device,group=device))+
      geom_hline(yintercept=0,colour='grey60',linewidth=.3)+
      geom_vline(xintercept=c(15,30,45),linetype='dotdash',colour='grey65',linewidth=.3)+
      geom_ribbon(aes(ymin=lo,ymax=hi),alpha=.055,colour=NA,show.legend=FALSE)+
      geom_line(linewidth=.55,linetype='dashed')+
      geom_point(data=ss|>filter(day==1),shape=21,fill='white',colour='grey25',size=1.9,stroke=.45)+
      geom_point(data=ss|>filter(day==60),shape=21,colour='grey25',size=2.1,stroke=.4)+
      scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+
      scale_x_continuous(breaks=c(1,15,30,45,60),expand=expansion(mult=c(.025,.03)))+
      labs(title='A. Projected static VM offset bias',subtitle='60-day span; conditional 95% CIs',
           x='Measurement day',y='Signed 1-g VM offset bias (mg)')+theme_p
    
    group_order<-c(paste('Primary comparison',c('AX3','GENEActiv','LEAP','SENS motion'),sep=': '),
                   paste('Secondary comparison',c('LEAP','wGT3X-BT','GT9X Link'),sep=': '))
    device_labels<-sub('^.*: ','',group_order)
    duration_plot<-function(metric,letter,title,subtitle){
      tm<-minutes|>filter(.data$metric==.env$metric)
      tc<-cuts|>filter(.data$metric==.env$metric)
      tm$boundary<-factor(tm$boundary,levels=boundary_levels)
      tm$y<-8-match(paste(tm$cohort,tm$device,sep=': '),group_order)
      values<-if(metric=='SVMBP_069_10')sprintf('%.1f',tc$cutoff_mg)else format(tc$cutoff_mg,trim=TRUE)
      labs_facet<-setNames(paste0(tc$boundary,'\n',values,' mg'),tc$boundary)
      ggplot(tm,aes(mean,y,colour=device,fill=device))+
        geom_errorbar(aes(xmin=minimum,xmax=maximum),orientation='y',width=.12,linewidth=.5)+
        geom_point(aes(shape=cohort),colour='grey25',size=2.5,stroke=.4)+
        scale_shape_manual(values=c('Primary comparison'=21,'Secondary comparison'=24),guide='none')+
        facet_grid(cohort~boundary,scales='free_y',space='free_y',
          labeller=labeller(boundary=labs_facet,cohort=c('Primary comparison'='Primary\ncomparison',
                                                      'Secondary comparison'='Secondary\ncomparison')))+
        scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+
        scale_y_continuous(breaks=7:1,labels=device_labels,expand=expansion(add=.5))+
        scale_x_continuous(limits=c(0,60),breaks=c(0,30,60))+
        labs(title=paste0(letter,'. ',title),subtitle=subtitle,x='Minutes at or above cutoff',y=NULL)+theme_p+
        theme(strip.text.x=element_text(size=8.5),strip.text.y=element_text(size=8,angle=0),
              panel.spacing.x=grid::unit(10,'pt'))
    }
    b<-duration_plot('ENMO','B','ENMO (published cutoffs)','60 min equally across seven conditions')
    c<-duration_plot('SVMBP_069_10','C','SVMbp (0.69-10 Hz; extrapolated cutoffs)','60 min equally across seven conditions')
    
    # Cohort response estimates share axes within metric without averaging cohorts.
    # Circles identify the primary comparison and triangles the secondary comparison.
    # Native responses occupy one horizontal row. Metric-specific numeric scales
    # and cutoff lines stay in their own facets; numerical features are not pooled.
    features<-bind_rows(lapply(c('primary_native','secondary_native'),function(arm){
      z<-read.csv(file.path(feature_root,arm,'unit_stage.csv'),stringsAsFactors=FALSE)
      z<-z[z$metric%in%c('ENMO','MAD','SVMBP_069_10')&z$rpm%in%conditions$rpm,]
      z$cohort<-if(arm=='primary_native')'Primary comparison'else'Secondary comparison';z
    }))
    offsets<-data.frame(cohort=c(rep('Primary comparison',4),rep('Secondary comparison',3)),
                       device=c('AX3','GENEActiv','LEAP','SENS motion','LEAP','wGT3X-BT','GT9X Link'),
                       offset=c(-.30,-.20,-.10,.10,0,.20,.30))
    profiles_plot<-profiles|>left_join(offsets,by=c('cohort','device'))|>mutate(y=match(rpm,conditions$rpm)+offset)
    features_plot<-features|>left_join(offsets,by=c('cohort','device'))|>mutate(y=match(rpm,conditions$rpm)+offset)
    write.csv(profiles_plot,file.path(out,'profile_plot_coordinates.csv'),row.names=FALSE)
    condition_labels<-paste0(conditions$rpm,' RPM\n',conditions$behaviour)
    metric_labels<-c('ENMO'='ENMO','MAD'='MAD','SVMBP_069_10'='SVMbp 0.69-10 Hz')
    native_plot<-function(metrics,letter,title,subtitle){
      tp<-profiles_plot|>filter(metric%in%metrics)
      td<-features_plot|>filter(metric%in%metrics)
      tc<-cuts|>filter(metric%in%metrics)
      display_levels<-unname(metric_labels[metrics])
      for(v in c('tp','td','tc')){
        obj<-get(v);obj$metric_label<-factor(unname(metric_labels[obj$metric]),levels=display_levels)
        if('cohort'%in%names(obj))obj$cohort<-factor(obj$cohort,levels=c('Primary comparison','Secondary comparison'))
        assign(v,obj)
      }
      tc$boundary<-factor(tc$boundary,levels=boundary_levels)
      axis_limits<-bind_rows(lapply(metrics,function(m){
        expand.grid(metric=m,x=c(-.001,if(m=='SVMBP_069_10')1.6 else 1),y=4,stringsAsFactors=FALSE)
      }))
      axis_limits$metric_label<-factor(unname(metric_labels[axis_limits$metric]),levels=display_levels)
      cutoff_breaks<-function(limits){
        metric_for_axis<-if(length(metrics)==1)metrics[1]else if(max(limits)>1.3)'SVMBP_069_10'else'ENMO'
        cuts$cutoff_g[cuts$metric==metric_for_axis]
      }
      ggplot(tp,aes(mean,y,colour=device,fill=device))+
        geom_blank(data=axis_limits,aes(x,y),inherit.aes=FALSE)+
        geom_vline(data=tc,aes(xintercept=cutoff_g,linetype=boundary),inherit.aes=FALSE,colour='grey40',linewidth=.4)+
        geom_point(data=td,aes(value/1000,y,shape=cohort),size=.85,stroke=.2,alpha=.12,show.legend=FALSE)+
        geom_errorbar(aes(xmin=lo,xmax=hi),orientation='y',width=.045,linewidth=.4,show.legend=FALSE)+
        geom_point(aes(shape=cohort),colour='grey25',size=2.2,stroke=.35)+
        facet_grid(~metric_label,scales='free_x')+
        scale_shape_manual(values=c('Primary comparison'=21,'Secondary comparison'=24),name=NULL)+
        scale_colour_manual(values=cols,guide='none')+
        scale_fill_manual(values=cols,breaks=names(cols),name=NULL)+
        scale_linetype_manual(values=line_types,name=NULL)+
        scale_y_continuous(breaks=1:7,labels=condition_labels,expand=expansion(add=.60))+
        scale_x_continuous(breaks=function(limits){if(limits[2]>1.3)c(0,.5,1,1.5)else c(0,.25,.5,.75,1)},
                           expand=expansion(mult=c(.02,.02)),
                           sec.axis=dup_axis(name=NULL,breaks=cutoff_breaks,labels=function(x)sprintf('%.4f',x)))+
        labs(title=paste0(letter,'. ',title),subtitle=subtitle,x='Native feature value (g)',y='Orbital shaker condition')+theme_p+
        theme(strip.text=element_text(size=9.5,face='bold'),strip.placement='outside',panel.spacing.x=grid::unit(18,'pt'),
              plot.subtitle=element_text(size=10,margin=margin(b=10)),
              axis.text.y=element_text(size=8.5,lineheight=1,margin=margin(r=7)),
              axis.text.x.top=element_text(size=8,angle=60,hjust=0,vjust=0,colour='grey30',margin=margin(b=3)),
              axis.ticks.x.top=element_line(colour='grey40',linewidth=.35),
              axis.ticks.length.x.top=grid::unit(3,'pt'),
              axis.title.y=element_text(margin=margin(r=18)),legend.position='bottom',legend.box='vertical',
              legend.key.width=grid::unit(24,'pt'),legend.key.spacing.x=grid::unit(12,'pt'),legend.margin=margin(0,0,0,0))+
        guides(fill=guide_legend(order=1,nrow=1,override.aes=list(shape=21,size=2.8,alpha=1)),
               shape=guide_legend(order=2,nrow=1,override.aes=list(fill='grey75',colour='grey25',size=2.8,alpha=1)),
               linetype=guide_legend(order=3,nrow=1,override.aes=list(linewidth=.65)))
    }
    d<-native_plot(c('ENMO','SVMBP_069_10'),'D','Native response profiles and cutoffs',
                   'Unit values, model means and 95% CIs; upper ticks: cutoffs (g)')
    main<-arrangeGrob(arrangeGrob(a,b,c,ncol=3,widths=c(.95,1.15,1.15)),d,ncol=1,heights=c(1,1.16))
    save_fig(main,'threshold-transfer',16,9.8)
    
    mad_a<-duration_plot('MAD','A','MAD (published cutoffs)','60 min equally across seven conditions')
    mad_b<-native_plot('MAD','B','Native MAD response profiles and cutoffs',
                      'Unit values, model means and 95% CIs; upper ticks: cutoffs (g)')
    supp<-arrangeGrob(mad_a,mad_b,ncol=1,heights=c(1,1.55))
    save_fig(supp,'mad-threshold-transfer',12.5,9.2)
    
    audit<-data.frame(figure=c('main','MAD supplement'),top_row_panels=c(3,1),
      response_panels=c(nrow(ggplot_build(d)$layout$layout),nrow(ggplot_build(mad_b)$layout$layout)),
      response_facet_rows=c(length(unique(ggplot_build(d)$layout$layout$ROW)),length(unique(ggplot_build(mad_b)$layout$layout$ROW))),
      drift_present=c(TRUE,FALSE),drift_linewidth_mm=c(.55,NA),drift_linetype=c('dashed',NA))
    write.csv(audit,file.path(out,'layout_audit.csv'),row.names=FALSE)
    
    invisible(dev.off())
    message('Approved main ENMO/SVMbp and supplementary MAD scenarios completed.')
}
