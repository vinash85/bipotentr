
################
# final figure
if(F){
  temp=local({load("/liulab/xmwang/oxphos_proj/loading_data/./ALL/trim_auc_sum.RData");environment()})
  sum.model = temp$sum.model
  comb.imm = readRDS("/liulab/xmwang/oxphos_proj/loading_data//TRIM_model/comb.imm.rds")
  comb.imm2 = comb.imm 
  comb.imm2[,"Estimate"] = -comb.imm[,"Estimate"]
  
  show.names<- c("ESRRA","IKZF1","STAT4","STAT1","IRF4","IRF1","CDK7")

}

get.plot <- function(comb.imm2, sum.model, show.names) {
  
  merge(comb.imm2,sum.model,by.x=0,by.y="TF") %>%
    dplyr::mutate(coef_sd =case_when(Estimate>0 ~ Estimate-1.96 *`SE`,
                                     Estimate<0 ~ -(abs(Estimate)-1.96 *`SE`)) )%>%
    dplyr::mutate(coef_sd = if_else(Estimate>0 & coef_sd <0,0,coef_sd),
                  coef_sd = if_else(Estimate<0 & coef_sd >0,0,coef_sd))->plot.data
  colnames(plot.data)[1]="object"
  thres.y.up = quantile(plot.data$auc_ci,.85,na.rm=T)
  thres.x.up = quantile(plot.data$coef_sd,.85,na.rm=T)
  thres.x.down = quantile(plot.data$coef_sd,.15,na.rm=T)
  plot.data %>%
    dplyr::rename(pvalue_immune = "p.val") %>%
    dplyr::mutate(color_col =if_else( pvalue_immune <.05 ,
                                      case_when(coef_sd>thres.x.up  & auc_ci>thres.y.up ~ "Stimulatory",
                                                coef_sd<thres.x.down  & auc_ci>thres.y.up ~ "Suppressive",
                                                TRUE ~"Not_signif"),
                                      "Not_signif")) %>% 
    dplyr::mutate(color_col = factor(color_col,
                                     levels = c("Stimulatory",
                                                "Suppressive",
                                                "Not_signif"))) ->plot.data
  
  loc = which(plot.data$color_col!="Not_signif" )
  
show.names<- c(plot.data[loc,]$object,show.names)
  
  plot.data %>%
    ggscatter(x="coef_sd",y="auc_ci",
              xlab = "Immune-modulatory potential",
              ylab = "Regulation potential",
              label = "object",#size = "pvalue_auc",
              repel = T,
              color = "color_col",alpha=0.5,
              #shape ="shape_col",
              label.select =show.names,
              legend.title = ""
    )+
    scale_color_manual(values =c("#7570B3", "#D95F02", "grey"),
                       name="bipotent \n targets",
                       # breaks=c("ctrl", "trt1", "trt2"),
                       breaks=c("Stimulatory", "Suppressive")
    )+
    theme(legend.position = "left")+
    geom_hline(yintercept = .5,size=.3,color="black",linetype="dashed")
  
}


#' Get volcano plot from data.table
#'
#' @param avi.dt contains following column
#'   stat regulatory_P       auc         ci   TF                         Pathway    auc.ci
# p.adj logpadjust label  col
# 46.46336    -25.39508 0.5801362 0.01025035 EGR1 KEGG_GLYCOLYSIS_GLUCONEOGENESIS 0.5698859
#  5.351241e-09  -8.271545  <NA> grey
#'
#' @return ggplot object
#' @export
#'
#' @examples
get.volcanoplot <- function(avi.dt) {
  # browser()
  avi.dt[,auc.ci:=auc-ci]
  avi.dt[,p.adj:=exp(regulatory_P) %>% p.adjust()] %>% 
    .[,logpadjust:=log10(p.adj)]
  
  auc.ci.thr = sort(avi.dt$auc.ci,decreasing = T)[30] 
  pthr = sort(avi.dt$logpadjust)[50] 
  avi.dt[,label:=ifelse(auc.ci>auc.ci.thr, TF,NA)]
  avi.dt[,col:=ifelse(logpadjust<pthr, ifelse( auc.ci>auc.ci.thr, "red", "green"), "grey")]
  p=ggplot(data=avi.dt, aes(x=auc.ci, y=-logpadjust, col=col, label=label)) +
    geom_point() + 
    geom_text_repel(max.overlaps = 20) +
    scale_color_manual(values=c("blue", "darkgrey", "darkred")) +
    geom_vline(xintercept=auc.ci.thr, col="red") +
    geom_hline(yintercept=-pthr, col="red") + 
    xlab("Regulatory Potential") + 
    ylab("Adjusted P value (-log10)") +
    ggpubr::theme_classic2()+
    theme(legend.position = "none") 
  p
}



string.match.plus <- function(x) {
  return(tolower(gsub("[^[:alpha:]]", "", x)))
}
