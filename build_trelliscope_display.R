library(tidyverse)
library(data.table)
library(trelliscopejs)
library(cowplot)
library(magick)

draw_fun <- function(x, y) {ggdraw(clip = "on") + draw_image(x) + draw_label(y, y = 0.9, size = 16)}

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
    PyImSeg = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/", Root, ".png"),
    PyImSeg_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_Blur_PNG/", Root, ".png"),
    PyTorchTip = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/", Root, ".png"),
    PyTorchTip_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_Blur_PNG/", Root, ".png"),
    Recolorize = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", Root, "_recolorize.png"),
    Recolorize_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_Blur_PNG/", Root, "_recolorize.png"),
    Supercells = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", Root, "_supercells.png"),
    Supercells_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_Blur_PNG/", Root, "_supercells.png")
  )
  
  
# Build display one plot at a time  
toBuild %>% select(Root, Original) %>% mutate(Original = map2_plot(Original, "Original Image", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Target) %>% mutate(Target = map2_plot(Target, "Target", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Clara)  %>% mutate(Clara = map2_plot(Clara, "Clara", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Clara_Blur) %>% mutate(Clara_Blur = map2_plot(Clara_Blur, "Clara Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, KCC) %>% mutate(KC = map2_plot(KCC, "KCC", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, KCC_Blur) %>% mutate(KCC_Blur = map2_plot(KCC_Blur, "KCC Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, KMeans) %>% mutate(KMeans = map2_plot(KMeans, "KMeans", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, KMeans_Blur) %>% mutate(KMeans_Blur = map2_plot(KMeans_Blur, "KMeans Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, PyImSeg) %>% mutate(PyImSeg = map2_plot(PyImSeg, "PyImSeg", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyImgSeg", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, PyImSeg_Blur) %>% mutate(PyImSeg_Blur = map2_plot(PyImSeg_Blur, "PyImSeg Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyImgSeg_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, PyTorchTip) %>% mutate(PyTorchTip = map2_plot(PyTorchTip, "PyTorchTip", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyTorchTip", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, PyTorchTip_Blur) %>% mutate(PyTorchTip_Blur = map2_plot(PyTorchTip_Blur, "PyTorchTip Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyTorchTip_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Recolorize) %>% mutate(Recolorize = map2_plot(Recolorize, "Recolorize", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Recolorize_Blur) %>% mutate(Recolorize_Blur = map2_plot(Recolorize_Blur, "Recolorize Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Supercells) %>% mutate(Supercells = map2_plot(Supercells, "Supercells", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)
toBuild %>% select(Root, Supercells_Blur) %>% mutate(Supercells_Blur = map2_plot(Supercells_Blur, "Supercells Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Blur/", thumb = TRUE)

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
    Clara = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Clara_PNG/", Root,
                   "_Clara.png"),
    KCC_Blur = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KCC_Blur_PNG/", Root, "_KCC.png"),
    KMeans = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/KMeans_PNG/", Root, "_KMeans.png"),
    PyImSeg = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/pyImSeg_PNG/", Root, ".png"),
    PyTorchTip = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/PyTorch_PNG/", Root, ".png"),
    Recolorize = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Recolorize_PNG/", Root, "_recolorize.png"),
    Supercells = paste0("~/Git_Repos/UnsupervisedSegmentation/Images/Kidney_Tiles/Supercells_PNG/", Root, "_supercells.png")
  )

# Build display one plot at a time  
toBuild2 %>% select(Root, Original) %>% mutate(Original = map2_plot(Original, "Original Image", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Original", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Target) %>% mutate(Target = map2_plot(Target, "Target", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Target", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Clara)  %>% mutate(Clara = map2_plot(Clara, "Clara", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Clara", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, KCC_Blur) %>% mutate(KCC_Blur = map2_plot(KCC_Blur, "KCC Blur", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KCC_Blur", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, KMeans) %>% mutate(KMeans = map2_plot(KMeans, "KMeans", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "KMeans", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, PyImSeg) %>% mutate(PyImSeg = map2_plot(PyImSeg, "PyImSeg", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyImgSeg", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, PyTorchTip) %>% mutate(PyTorchTip = map2_plot(PyTorchTip, "PyTorchTip", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "PyTorchTip", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Recolorize) %>% mutate(Recolorize = map2_plot(Recolorize, "Recolorize", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Recolorize", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)
toBuild2 %>% select(Root, Supercells) %>% mutate(Supercells = map2_plot(Supercells, "Supercells", draw_fun)) %>% ungroup() %>%
  trelliscope(name = "Supercells", path = "~/Git_Repos/UnsupervisedSegmentation_Trelliscope/docs/Full/", thumb = TRUE)

