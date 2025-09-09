library(tidyverse)
library(data.table)
library(trelliscopejs)
library(cowplot)
library(magick)

draw_fun <- function(x, y) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = 0.9, size = 16)}

#########################
## DIMENSION REDUCTION ##
#########################

# Generate data.frame to build display
dr <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Dimension_Reduction.csv") %>%
  rename(Root = Path) %>%
  group_by(Root) %>%
  summarise(`Number of Clusters` = n()) %>%
  mutate(
    Root = gsub("_Annotations", "", Root, fixed = T),
    Original = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Root, ".png"),
    Target = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/",
                    Root, "_Annotations.png"),
    KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", Root, "_KCC.png"),
    PCA_KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PCA_KCC_PNG/", Root, "_PCA_KCC.png"),
    tSNE_KCC =paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/tSNE_KCC_PNG/", Root, "_tSNE_KCC.png"),
    SVD_KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/SVD_KCC_PNG/", Root, "_SVD_KCC.png")
  )

# Build display
dr %>% select(Root, Original) %>% mutate(Original = map2_plot(Original, "Original Image", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)
dr %>% select(Root, Target) %>% mutate(Target = map2_plot(Target, "Target", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)
dr %>% select(Root, KCC) %>% mutate(KC = map2_plot(KCC, "KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)
dr %>% select(Root, PCA_KCC) %>% mutate(KC = map2_plot(PCA_KCC, "PCA & KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PCA & KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)
dr %>% select(Root, tSNE_KCC) %>% mutate(KC = map2_plot(tSNE_KCC, "tSNE & KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "tSNE & KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)
dr %>% select(Root, SVD_KCC) %>% mutate(KC = map2_plot(SVD_KCC, "SVD & KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "SVD & KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/DR/", thumb = TRUE)

################
## BLUR STUDY ##
################

# Read blur images 
toBuild <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  filter(Blur == "X") %>%
  rename(Root = Path) %>%
  group_by(Root) %>%
  summarise(`Number of Clusters` = n()) %>%
  mutate(
    Root = gsub("_Annotations", "", Root, fixed = T),
    Original = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Root, ".png"),
    Target = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/",
                    Root, "_Annotations.png"),
    Clara = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/", Root,
                   "_Clara.png"),
    Clara_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_Blur_PNG/", Root,
                        "_Clara.png"),
    KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", Root, "_KCC.png"),
    KCC_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", Root, "_KCC.png"),
    KMeans = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG/", Root, "_KMeans.png"),
    KMeans_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_Blur_PNG/", Root, "_KMeans.png"),
    pyImSegm = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/", Root, ".png"),
    pyImSegm_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_Blur_PNG/", Root, ".png"),
    `pytorch-tip` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/", Root, ".png"),
    `pytorch-tip_Blur` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_Blur_PNG/", Root, ".png"),
    Recolorize = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", Root, "_recolorize.png"),
    Recolorize_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_PNG/", Root, "_recolorize.png"),
    Supercells = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", Root, "_supercells.png"),
    Supercells_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_PNG/", Root, "_supercells.png")
  )
  
  
# Build display one plot at a time  
toBuild %>% select(Root, Original) %>% mutate(Original = map2_plot(Original, "Original Image", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Target) %>% mutate(Target = map2_plot(Target, "Target", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Clara)  %>% mutate(Clara = map2_plot(Clara, "Clara", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Clara_Blur) %>% mutate(Clara_Blur = map2_plot(Clara_Blur, "Clara Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, KCC) %>% mutate(KC = map2_plot(KCC, "KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, KCC_Blur) %>% mutate(KCC_Blur = map2_plot(KCC_Blur, "KCC Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, KMeans) %>% mutate(KMeans = map2_plot(KMeans, "KMeans", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, KMeans_Blur) %>% mutate(KMeans_Blur = map2_plot(KMeans_Blur, "KMeans Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, pyImSegm) %>% mutate(pyImSegm = map2_plot(pyImSegm, "pyImSegm", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pyImgSegm", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, pyImSegm_Blur) %>% mutate(pyImSegm_Blur = map2_plot(pyImSegm_Blur, "pyImSegm Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pyImgSegm_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, `pytorch-tip`) %>% mutate(`pytorch-tip`= map2_plot(`pytorch-tip`, "pytorch-tip", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pytorch-tip", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, `pytorch-tip_Blur`) %>% mutate(`pytorch-tip_Blur` = map2_plot(`pytorch-tip_Blur`, "pytorch-tip Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pytorch-tip_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Recolorize) %>% mutate(Recolorize = map2_plot(Recolorize, "Recolorize", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Recolorize_Blur) %>% mutate(Recolorize_Blur = map2_plot(Recolorize_Blur, "Recolorize Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Supercells) %>% mutate(Supercells = map2_plot(Supercells, "Supercells", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)
toBuild %>% select(Root, Supercells_Blur) %>% mutate(Supercells_Blur = map2_plot(Supercells_Blur, "Supercells Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Blur/", thumb = TRUE)

################
## FULL STUDY ##
################

# Read blur images 
toBuild2 <- fread("~/Git_Repos/UnsupervisedSegmentation/Metadata/Kidney_Annotations_Summary.csv") %>%
  rename(Root = Path) %>%
  group_by(Root) %>%
  summarise(`Number of Clusters` = n()) %>%
  mutate(
    Root = gsub("_Annotations", "", Root, fixed = T),
    Original = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Original/", Root, ".png"),
    Target = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Manual_Segmentation_Masks_PNG/",
                    Root, "_Annotations.png"),
    Binning = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Binning_PNG/", Root, 
                     "_binning.png"),
    Clara = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/", Root,
                   "_Clara.png"),
    KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_PNG/", Root, "_KCC.png"),
    KCC_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", Root, "_KCC.png"),
    KMeans = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG/", Root, "_KMeans.png"),
    `Multi-Otsu` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Multiotsu_PNG/", Root, 
                          "_multiotsu.png"),
    pyImSegm = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/", Root, ".png"),
    `pytorch-tip` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/", Root, ".png"),
    Recolorize = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", Root, "_recolorize.png"),
    Supercells = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", Root, "_supercells.png")
  )

# Build display one plot at a time  
toBuild2 %>% select(Root, Original) %>% mutate(Original = map2_plot(Original, "Original Image", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Target) %>% mutate(Target = map2_plot(Target, "Target", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Clara)  %>% mutate(Clara = map2_plot(Clara, "Clara", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, KCC_Blur) %>% mutate(KCC_Blur = map2_plot(KCC_Blur, "KCC Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC_Blur", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, KMeans) %>% mutate(KMeans = map2_plot(KMeans, "KMeans", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, pyImSegm) %>% mutate(PyImSeg = map2_plot(pyImSegm, "pyImSegm", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pyImgSegm", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, `pytorch-tip`) %>% mutate(`pytorch-tip` = map2_plot(`pytorch-tip`, "pytorch-tip", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pytorch-tip", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Recolorize) %>% mutate(Recolorize = map2_plot(Recolorize, "Recolorize", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Supercells) %>% mutate(Supercells = map2_plot(Supercells, "Supercells", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Binning) %>% mutate(Binning = map2_plot(Binning, "Binning", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Binning", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, `Multi-Otsu`) %>% mutate(`Multi-Otsu` = map2_plot(`Multi-Otsu`, "Multi-Otsu", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Multi-Otsu", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)
toBuild2 %>% select(Root, KCC) %>% mutate(KCC = map2_plot(KCC, "KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/Full/", thumb = TRUE)

################
## ROOT STUDY ##
################

toBuild3 <- fread("../Metadata/Root.csv") %>%
  select(Image) %>%
  group_by(Image) %>%
  summarize(`Number of Clusters` = n()) %>%
  mutate(
    Original = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Original/", Image, ".png"),
    Target = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Manual_Segmentation_Masks_PNG/", Image, ".png"),
    Binning = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Binning_PNG/", Image, "_binning.png"),
    Clara = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Clara_PNG/", Image, "_CLARA.png"),
    KCC = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KCC_PNG/", Image, "_KCC.png"),
    KMeans = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/KMeans_PNG/", Image, "_KMeans.png"),
    `Multi-Otsu` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Multiotsu_PNG/", Image, "_multiotsu.png"),
    `pytorch-tip` = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/PyTorch_PNG/", Image, ".png"),
    Recolorize = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Recolorize_PNG/", Image, "_recolorize.png"),
    Supercells = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Root/Supercells_PNG/", Image, "_supercells.png")
  )

toBuild3 %>% select(Image, Original) %>% mutate(Original = map2_plot(Original, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, Target) %>% mutate(Target = map2_plot(Target, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, Binning) %>% mutate(Binning = map2_plot(Binning, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Binning", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, Clara) %>% mutate(Clara = map2_plot(Clara, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, KCC) %>% mutate(KCC = map2_plot(KCC, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, KMeans) %>% mutate(KMeans = map2_plot(KMeans, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, `Multi-Otsu`) %>% mutate(`Multi-Otsu` = map2_plot(`Multi-Otsu`, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Multi-Otsu", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, `pytorch-tip`) %>% mutate(`pytorch-tip` = map2_plot(`pytorch-tip`, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "pytorch-tip", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, Recolorize) %>% mutate(Recolorize = map2_plot(Recolorize, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)
toBuild3 %>% select(Image, Supercells) %>% mutate(Supercells = map2_plot(Supercells, "", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells", path = "~/Git_Repos/unsupervisedsegmentation.io/Root/", thumb = TRUE)


