# Copyright © 2026 Andrew R. Crocker
# Lesson 1: "What's Wrong With You?" — Visual assets
# Notes from the Abstract — Medical Coding Fundamentals
# R code available on GitHub.

library(ggplot2)
library(dplyr)

theme_1950s <- function() {
  theme_minimal(base_family = "Arial", base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.background = element_rect(fill = "#FAF5E9", color = NA),
      panel.grid.major = element_line(color = "#E6DECA", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "#3A3A3A"),
      axis.text = element_text(color = "#4A4A4A"),
      plot.title = element_text(face = "bold", size = 20, color = "#3A3A3A"),
      plot.subtitle = element_text(size = 13, color = "#5A5A5A"),
      plot.margin = margin(15, 15, 15, 15),
      legend.position = "none"
    )
}

red <- "#C0392B"
blue <- "#2471A3"
bg <- "#FAF5E9"
dark <- "#3A3A3A"
mid <- "#5A5A5A"
w <- 1456/100
h <- w * 9/16

# CHART 1: Code Anatomy
chars <- data.frame(x = 1:8, char = c("E","1","1",".","3","2","1","1"), stringsAsFactors = FALSE)
p1 <- ggplot() +
  geom_rect(data = chars, aes(xmin = x-0.42, xmax = x+0.42, ymin = -0.5, ymax = 0.5),
            fill = "white", color = dark, linewidth = 1.3) +
  geom_text(data = chars, aes(x = x, y = 0, label = char),
            size = 12, fontface = "bold", color = dark, family = "Arial") +
  geom_segment(aes(x=1,xend=1,y=0.6,yend=1.0), color=blue, linewidth=0.9) +
  annotate("text",x=1,y=1.3,label="CHAPTER",fontface="bold",size=3.5,color=blue,family="Arial") +
  annotate("text",x=1,y=1.9,label="Endocrine, nutritional,\n& metabolic diseases",size=3,color=mid,family="Arial",lineheight=0.9) +
  geom_segment(aes(x=5,xend=5,y=0.6,yend=1.0), color=blue, linewidth=0.9) +
  annotate("text",x=5,y=1.3,label="COMPLICATION TYPE",fontface="bold",size=3.5,color=blue,family="Arial") +
  annotate("text",x=5,y=1.8,label="Ophthalmic",size=3,color=mid,family="Arial") +
  geom_segment(aes(x=7,xend=7,y=0.6,yend=0.8),color=blue,linewidth=0.9) +
  geom_segment(aes(x=8,xend=8,y=0.6,yend=0.8),color=blue,linewidth=0.9) +
  geom_segment(aes(x=7,xend=8,y=0.8,yend=0.8),color=blue,linewidth=0.9) +
  geom_segment(aes(x=7.5,xend=7.5,y=0.8,yend=1.0),color=blue,linewidth=0.9) +
  annotate("text",x=7.5,y=1.3,label="EXTENSION",fontface="bold",size=3.5,color=blue,family="Arial") +
  annotate("text",x=7.5,y=1.9,label="With macular edema,\nright eye",size=3,color=mid,family="Arial",lineheight=0.9) +
  geom_segment(aes(x=2,xend=2,y=-0.6,yend=-0.8),color=red,linewidth=0.9) +
  geom_segment(aes(x=3,xend=3,y=-0.6,yend=-0.8),color=red,linewidth=0.9) +
  geom_segment(aes(x=2,xend=3,y=-0.8,yend=-0.8),color=red,linewidth=0.9) +
  geom_segment(aes(x=2.5,xend=2.5,y=-0.8,yend=-1.0),color=red,linewidth=0.9) +
  annotate("text",x=2.5,y=-1.3,label="CATEGORY",fontface="bold",size=3.5,color=red,family="Arial") +
  annotate("text",x=2.5,y=-1.9,label="Type 2 diabetes\nmellitus",size=3,color=mid,family="Arial",lineheight=0.9) +
  geom_segment(aes(x=6,xend=6,y=-0.6,yend=-1.0),color=red,linewidth=0.9) +
  annotate("text",x=6,y=-1.3,label="COMPLICATION DETAIL",fontface="bold",size=3.5,color=red,family="Arial") +
  annotate("text",x=6,y=-1.9,label="Mild nonproliferative\nretinopathy",size=3,color=mid,family="Arial",lineheight=0.9) +
  labs(title="Anatomy of an ICD-10-CM Code",
       subtitle="E11.3211  \u2014  Type 2 diabetes with mild nonproliferative diabetic retinopathy,\nmacular edema, right eye") +
  coord_cartesian(xlim=c(-0.2,9.2),ylim=c(-2.6,2.6)) +
  theme_1950s() +
  theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),
        plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5,size=11))
ggsave("chart1_code_anatomy.png", p1, width=w, height=h, dpi=100, bg=bg)

# CHART 2: E11 Family Tree
tree_data <- data.frame(
  code=c("E11.0","E11.1","E11.2","E11.3","E11.4","E11.5","E11.6","E11.8","E11.9"),
  label=c("Hyperosmolarity","Ketoacidosis","Kidney","Eyes","Neurological",
          "Circulatory","Other specified","Unspecified\ncomplications","Without\ncomplications"),
  y=9:1, highlight=c(F,F,F,T,F,F,F,F,T), stringsAsFactors=FALSE)
p2 <- ggplot(tree_data) +
  geom_segment(aes(x=2,xend=2,y=0.5,yend=9.5),color=mid,linewidth=1) +
  geom_segment(aes(x=2,xend=3,y=y,yend=y),color=ifelse(tree_data$highlight,red,mid),linewidth=0.8) +
  annotate("label",x=2,y=10.3,label="E11\nType 2 Diabetes Mellitus",size=4,fontface="bold",
           color="white",fill=dark,label.size=0,label.r=unit(0.3,"lines"),lineheight=1.1,family="Arial") +
  geom_text(aes(x=3.2,y=y,label=code),hjust=0,size=4.5,fontface="bold",
            color=ifelse(tree_data$highlight,red,dark),family="Arial") +
  geom_text(aes(x=4.8,y=y,label=label),hjust=0,size=3.8,
            color=ifelse(tree_data$highlight,red,mid),lineheight=0.9,family="Arial") +
  annotate("label",x=7.5,y=1,label="Dale's code",size=3.5,fontface="italic",
           color="white",fill=red,label.size=0,label.r=unit(0.3,"lines"),family="Arial") +
  geom_segment(aes(x=7,xend=5.6,y=1,yend=1),color=red,linewidth=0.5,
               arrow=arrow(length=unit(0.12,"inches"),type="closed")) +
  annotate("label",x=7.5,y=6,label="Where the\nretinopathy lives",size=3.5,fontface="italic",
           color="white",fill=red,label.size=0,label.r=unit(0.3,"lines"),lineheight=0.9,family="Arial") +
  geom_segment(aes(x=7,xend=5.1,y=6,yend=6),color=red,linewidth=0.5,
               arrow=arrow(length=unit(0.12,"inches"),type="closed")) +
  labs(title="The E11 Family",subtitle="How the fourth character sorts Type 2 diabetes by complication") +
  coord_cartesian(xlim=c(0.5,9),ylim=c(0,11.2)) +
  theme_1950s() +
  theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),
        plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
ggsave("chart2_e11_tree.png", p2, width=w, height=h, dpi=100, bg=bg)

# CHART 3: Specificity Comparison
spec_data <- data.frame(
  code=rep(c("E11.9","E11.319","E11.3211"),each=6),
  dimension=rep(c("Disease","Complication\npresent","Complication\ntype","Severity","Macular\nedema","Laterality"),3),
  captured=c(1,0,0,0,0,0, 1,1,1,0,1,0, 1,1,1,1,1,1), stringsAsFactors=FALSE)
spec_data$code <- factor(spec_data$code, levels=c("E11.9","E11.319","E11.3211"))
spec_data$dimension <- factor(spec_data$dimension,
  levels=c("Disease","Complication\npresent","Complication\ntype","Severity","Macular\nedema","Laterality"))
p3 <- ggplot(spec_data, aes(x=dimension,y=code)) +
  geom_tile(aes(fill=factor(captured)),color="white",linewidth=2) +
  scale_fill_manual(values=c("0"="#E8E0D0","1"=blue)) +
  geom_text(aes(label=ifelse(captured==1,"\u2713","\u2717")),size=7,
            color=ifelse(spec_data$captured==1,"white","#B0A898"),family="Arial") +
  annotate("text",x=7.1,y=1,label="Type 2 diabetes\nwithout complications",
           hjust=0,size=3.2,color=mid,lineheight=0.9,family="Arial") +
  annotate("text",x=7.1,y=2,label="Type 2 diabetes with\nunspecified retinopathy",
           hjust=0,size=3.2,color=mid,lineheight=0.9,family="Arial") +
  annotate("text",x=7.1,y=3,label="Type 2 diabetes, mild NPDR\nwith macular edema, right eye",
           hjust=0,size=3.2,color=mid,lineheight=0.9,family="Arial") +
  labs(title="What the Code Captures",
       subtitle="Three codes for the same patient. Three levels of specificity.\nOnly one tells the full story.") +
  coord_cartesian(xlim=c(0.5,9.5),clip="off") +
  theme_1950s() +
  theme(axis.text.x=element_text(size=11,face="bold"),
        axis.text.y=element_text(size=13,face="bold",color=dark),
        axis.title=element_blank(),panel.grid.major=element_blank(),
        plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5),
        plot.margin=margin(15,120,15,15))
ggsave("chart3_specificity.png", p3, width=w, height=h, dpi=100, bg=bg)

# CHART 4: Coding Workflow
p4 <- ggplot() +
  # Step 1: Documentation
  geom_rect(aes(xmin=0.5,xmax=3.5,ymin=2.5,ymax=4.5),fill="white",color=dark,linewidth=1.2) +
  annotate("text",x=2,y=4.0,label="CLINICAL DOCUMENTATION",fontface="bold",size=3.8,color=dark,family="Arial") +
  annotate("text",x=2,y=3.3,label='"Patient presents with\nType 2 diabetes mellitus"',
           size=3.2,color=mid,family="Arial",lineheight=0.9,fontface="italic") +
  # Arrow 1-2
  geom_segment(aes(x=3.5,xend=4.5,y=3.5,yend=3.5),color=red,linewidth=1,
               arrow=arrow(length=unit(0.15,"inches"),type="closed")) +
  annotate("text",x=4.0,y=3.9,label="Look up",size=2.8,color=red,fontface="italic",family="Arial") +
  # Step 2: Alphabetic Index
  geom_rect(aes(xmin=4.5,xmax=7.8,ymin=2.5,ymax=4.5),fill="white",color=blue,linewidth=1.2) +
  annotate("text",x=6.15,y=4.0,label="ALPHABETIC INDEX",fontface="bold",size=3.8,color=blue,family="Arial") +
  annotate("text",x=6.15,y=3.3,label="Diabetes, diabetic (mellitus)\n   type 2 \u2014 see E11",
           size=3.2,color=mid,family="Arial",lineheight=0.9) +
  # Arrow 2-3
  geom_segment(aes(x=7.8,xend=8.8,y=3.5,yend=3.5),color=red,linewidth=1,
               arrow=arrow(length=unit(0.15,"inches"),type="closed")) +
  annotate("text",x=8.3,y=3.9,label="Verify",size=2.8,color=red,fontface="italic",family="Arial") +
  # Step 3: Tabular List
  geom_rect(aes(xmin=8.8,xmax=13,ymin=1.5,ymax=5.5),fill="white",color=blue,linewidth=1.2) +
  annotate("text",x=10.9,y=5.1,label="TABULAR LIST \u2014 Chapter 4",fontface="bold",size=3.8,color=blue,family="Arial") +
  annotate("text",x=9.2,y=4.5,label="E11",hjust=0,fontface="bold",size=3.4,color=dark,family="Arial") +
  annotate("text",x=10.2,y=4.5,label="Type 2 diabetes mellitus",hjust=0,size=3,color=mid,family="Arial") +
  annotate("text",x=9.5,y=4.0,label="E11.0",hjust=0,size=3,color="#999999",family="Arial") +
  annotate("text",x=10.6,y=4.0,label="...with hyperosmolarity",hjust=0,size=2.8,color="#999999",family="Arial") +
  annotate("text",x=9.5,y=3.6,label="E11.3",hjust=0,size=3,color="#999999",family="Arial") +
  annotate("text",x=10.6,y=3.6,label="...with ophthalmic complications",hjust=0,size=2.8,color="#999999",family="Arial") +
  annotate("text",x=9.5,y=3.2,label="E11.6",hjust=0,size=3,color="#999999",family="Arial") +
  annotate("text",x=10.6,y=3.2,label="...with other specified complications",hjust=0,size=2.8,color="#999999",family="Arial") +
  geom_rect(aes(xmin=9.3,xmax=12.8,ymin=2.55,ymax=3.0),fill=red,color=NA,alpha=0.12) +
  annotate("text",x=9.5,y=2.75,label="E11.9",hjust=0,fontface="bold",size=3.4,color=red,family="Arial") +
  annotate("text",x=10.6,y=2.75,label="...without complications",hjust=0,fontface="bold",size=3,color=red,family="Arial") +
  annotate("text",x=10.0,y=2.15,label="...",size=4,color="#CCCCCC") +
  # Output arrow
  geom_segment(aes(x=10.9,xend=10.9,y=1.5,yend=0.8),color=red,linewidth=1,
               arrow=arrow(length=unit(0.15,"inches"),type="closed")) +
  # Result
  geom_rect(aes(xmin=8.8,xmax=13,ymin=-0.1,ymax=0.8),fill=red,color=NA) +
  annotate("text",x=10.9,y=0.35,
           label="E11.9 \u2014 Type 2 diabetes mellitus without complications",
           fontface="bold",size=3.3,color="white",family="Arial") +
  labs(title="From Documentation to Code",
       subtitle="How a coder navigates the ICD-10-CM to assign Dale's diagnosis") +
  coord_cartesian(xlim=c(0,13.5),ylim=c(-0.5,6.2)) +
  theme_1950s() +
  theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),
        plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
ggsave("chart4_coding_workflow.png", p4, width=w, height=h, dpi=100, bg=bg)

cat("All four charts saved.\n")


# ============================================================
# CHART 5: Inside the Codebook — Simulated Tabular List page
# ============================================================

ls <- 0.48
y_start <- 18.0
ypos <- function(line) y_start - (line * ls)
page_bg <- "#FFFEF8"
light <- "#C8BFA8"
gray <- "#999999"

p5 <- ggplot() +
  geom_rect(aes(xmin = 1, xmax = 13, ymin = -1, ymax = 20),
            fill = page_bg, color = "#D5CDB8", linewidth = 0.8) +
  geom_segment(aes(x = 1.5, xend = 12.5, y = 19.2, yend = 19.2), color = light, linewidth = 0.5) +
  annotate("text", x = 1.5, y = 19.5,
           label = "Chapter 4: Endocrine, Nutritional and Metabolic Diseases (E00\u2013E89)",
           hjust = 0, size = 3, color = gray, family = "Arial", fontface = "italic") +
  annotate("text", x = 12.5, y = 19.5, label = "ICD-10-CM",
           hjust = 1, size = 3, color = gray, family = "Arial", fontface = "italic") +
  # Category header
  annotate("text", x = 1.8, y = ypos(0), label = "E11", hjust = 0, size = 5.5,
           fontface = "bold", color = dark, family = "Arial") +
  annotate("text", x = 3.2, y = ypos(0), label = "Type 2 diabetes mellitus", hjust = 0,
           size = 5, fontface = "bold", color = dark, family = "Arial") +
  # Includes
  annotate("text", x = 2.3, y = ypos(1.2), label = "Includes:", hjust = 0, size = 3.2,
           fontface = "bold.italic", color = mid, family = "Arial") +
  annotate("text", x = 3.5, y = ypos(1.2),
           label = "diabetes (mellitus) due to insulin secretory defect",
           hjust = 0, size = 3.2, color = mid, family = "Arial", fontface = "italic") +
  annotate("text", x = 3.5, y = ypos(2),
           label = "insulin resistant diabetes (mellitus)",
           hjust = 0, size = 3.2, color = mid, family = "Arial", fontface = "italic") +
  # Use additional code
  annotate("text", x = 2.3, y = ypos(3.2),
           label = "Use additional code to identify control using:",
           hjust = 0, size = 3.2, fontface = "bold.italic", color = blue, family = "Arial") +
  annotate("text", x = 3.5, y = ypos(4), label = "insulin (Z79.4)",
           hjust = 0, size = 3.2, color = blue, family = "Arial", fontface = "italic") +
  annotate("text", x = 3.5, y = ypos(4.8), label = "oral antidiabetic drugs (Z79.84)",
           hjust = 0, size = 3.2, color = blue, family = "Arial", fontface = "italic") +
  annotate("text", x = 3.5, y = ypos(5.6), label = "oral hypoglycemic drugs (Z79.84)",
           hjust = 0, size = 3.2, color = blue, family = "Arial", fontface = "italic") +
  # Excludes1
  annotate("text", x = 2.3, y = ypos(7), label = "Excludes1:", hjust = 0, size = 3.2,
           fontface = "bold.italic", color = red, family = "Arial") +
  annotate("text", x = 4.0, y = ypos(7),
           label = "diabetes mellitus due to underlying condition (E08.-)",
           hjust = 0, size = 3.2, color = red, family = "Arial", fontface = "italic") +
  annotate("text", x = 4.0, y = ypos(7.8),
           label = "drug or chemical induced diabetes mellitus (E09.-)",
           hjust = 0, size = 3.2, color = red, family = "Arial", fontface = "italic") +
  annotate("text", x = 4.0, y = ypos(8.6), label = "gestational diabetes (O24.4-)",
           hjust = 0, size = 3.2, color = red, family = "Arial", fontface = "italic") +
  annotate("text", x = 4.0, y = ypos(9.4), label = "neonatal diabetes mellitus (P70.2)",
           hjust = 0, size = 3.2, color = red, family = "Arial", fontface = "italic") +
  annotate("text", x = 4.0, y = ypos(10.2), label = "type 1 diabetes mellitus (E10.-)",
           hjust = 0, size = 3.2, color = red, family = "Arial", fontface = "italic") +
  # Separator
  geom_segment(aes(x = 1.8, xend = 12.2, y = ypos(11.2), yend = ypos(11.2)),
               color = light, linewidth = 0.3) +
  # Subcategories
  annotate("text",x=2.3,y=ypos(12),label="E11.0",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(12),label="Type 2 diabetes mellitus with hyperosmolarity",hjust=0,size=3.6,color=dark,family="Arial") +
  annotate("text",x=2.3,y=ypos(13),label="E11.1",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(13),label="Type 2 diabetes mellitus with ketoacidosis",hjust=0,size=3.6,color=dark,family="Arial") +
  annotate("text",x=2.3,y=ypos(14),label="E11.2",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(14),label="Type 2 diabetes mellitus with kidney complications",hjust=0,size=3.6,color=dark,family="Arial") +
  # E11.3 highlighted
  geom_rect(aes(xmin=1.8,xmax=12.2,ymin=ypos(15)-0.22,ymax=ypos(15)+0.22),fill=blue,alpha=0.08,color=NA) +
  annotate("text",x=2.3,y=ypos(15),label="E11.3",hjust=0,size=3.8,fontface="bold",color=blue,family="Arial") +
  annotate("text",x=3.8,y=ypos(15),label="Type 2 diabetes mellitus with ophthalmic complications",hjust=0,size=3.6,color=blue,family="Arial") +
  # Drill-down
  annotate("text",x=3.0,y=ypos(16),label="E11.31",hjust=0,size=3.4,color=gray,family="Arial") +
  annotate("text",x=4.5,y=ypos(16),label="...with unspecified diabetic retinopathy",hjust=0,size=3.2,color=gray,family="Arial") +
  annotate("text",x=3.5,y=ypos(16.8),label="E11.319",hjust=0,size=3.4,fontface="bold",color=red,family="Arial") +
  annotate("text",x=5.0,y=ypos(16.8),label="...without macular edema",hjust=0,size=3.2,fontface="bold",color=red,family="Arial") +
  annotate("text",x=3.0,y=ypos(17.8),label="E11.32",hjust=0,size=3.4,color=gray,family="Arial") +
  annotate("text",x=4.5,y=ypos(17.8),label="...with mild nonproliferative diabetic retinopathy",hjust=0,size=3.2,color=gray,family="Arial") +
  annotate("text",x=3.5,y=ypos(18.6),label="E11.3211",hjust=0,size=3.4,fontface="bold",color=red,family="Arial") +
  annotate("text",x=5.3,y=ypos(18.6),label="...with macular edema, right eye",hjust=0,size=3.2,fontface="bold",color=red,family="Arial") +
  # Remaining subcategories
  annotate("text",x=2.3,y=ypos(20),label="E11.4",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(20),label="Type 2 diabetes mellitus with neurological complications",hjust=0,size=3.6,color=dark,family="Arial") +
  annotate("text",x=2.3,y=ypos(21),label="E11.5",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(21),label="Type 2 diabetes mellitus with circulatory complications",hjust=0,size=3.6,color=dark,family="Arial") +
  annotate("text",x=2.3,y=ypos(22),label="E11.6",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(22),label="Type 2 diabetes mellitus with other specified complications",hjust=0,size=3.6,color=dark,family="Arial") +
  annotate("text",x=2.3,y=ypos(23),label="E11.8",hjust=0,size=3.8,fontface="bold",color=dark,family="Arial") +
  annotate("text",x=3.8,y=ypos(23),label="Type 2 diabetes mellitus with unspecified complications",hjust=0,size=3.6,color=dark,family="Arial") +
  # E11.9 highlighted
  geom_rect(aes(xmin=1.8,xmax=12.2,ymin=ypos(24.5)-0.22,ymax=ypos(24.5)+0.22),fill=red,alpha=0.08,color=NA) +
  annotate("text",x=2.3,y=ypos(24.5),label="E11.9",hjust=0,size=3.8,fontface="bold",color=red,family="Arial") +
  annotate("text",x=3.8,y=ypos(24.5),label="Type 2 diabetes mellitus without complications",hjust=0,size=3.6,fontface="bold",color=red,family="Arial") +
  annotate("label",x=10.5,y=ypos(24.5),label="  Dale's code  ",size=3.3,fontface="italic",
           color="white",fill=red,label.size=0,label.r=unit(0.3,"lines"),family="Arial") +
  geom_segment(aes(x=9.8,xend=8.8,y=ypos(24.5),yend=ypos(24.5)),color=red,linewidth=0.5,
               arrow=arrow(length=unit(0.1,"inches"),type="closed")) +
  labs(title="Inside the Codebook",
       subtitle="The E11 category as it appears in the ICD-10-CM Tabular List") +
  coord_cartesian(xlim=c(0.5,13.5),ylim=c(4.5,20)) +
  theme_1950s() +
  theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),
        plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5,size=11))

ggsave("chart5_codebook_page.png", p5, width=w, height=h, dpi=100, bg=bg)

cat("Chart 5 (codebook page) saved.\n")
