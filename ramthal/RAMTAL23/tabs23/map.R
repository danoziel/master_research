library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)

dev.off()

library(readxl)
Bhoomi_20160610 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_data/baseline_survey_2016/20160610_irrigation_Bhoomi_entire_sample_YR_5.xlsx", sheet = "sample_2")

Bhoomi <- readxl::read_excel("C:/Users/Dan/OneDrive/Documents/ramthal_database/Bhoomi.xlsx")

df_smpl <- Bhoomi |> 
  filter(!is.na(in1_out0)) |> 
  mutate(plot_match=ifelse(plot_match >= 0,1,0))|> 
  mutate(site_area = case_when(
    south1_north0 == 1  ~ "South",
    south1_north0 == 0  ~ "North", .default = "Center"
  )) |> filter(village != "Kamaladinni") |> 
  mutate(site_area=ifelse( site_area == "Center" & in1_out0==0,"North",site_area ) )

hh_baseline <- df_smpl |> 
  filter(!is.na(plot_match)) |> 
  select(Id,village,in1_out0) |> rename(hh_id=Id)


bhoomi_village_In <- 
  df_smpl |> filter(in1_out0 ==1 )|>
  select(village,survey,site_area) |>  distinct() |> 
  count(village,site_area)  |> arrange(village) |> 
  rename(n_surveys_bhoomi=n)

bhoomi_frame_sample <- 
  df_smpl |> 
  select(village,survey,in1_out0,site_area)|> distinct() |> 
  count(village,in1_out0,site_area)  |> arrange(village) |> 
  mutate(village_in_out_bhoomi=mean(in1_out0),.by = village,
         village_in_out_bhoomi=ifelse(village_in_out_bhoomi==1,"all_in",ifelse(village_in_out_bhoomi==0,"all_out","in_out"))
  ) 

polygons_n_village <- as_tibble(Ramthal_shp_11) |> count(layer_update) |> rename(village=layer_update, n_polygons=n)

bhoomi_frame_sample |> 
  summarise(n_bhoomi=sum(n),
            .by=c(village, village_in_out_bhoomi) )|> 
  left_join(polygons_n_village) |> 
  mutate(pct = n_bhoomi/n_polygons) |> 
  arrange(village_in_out_bhoomi) |> 
  as.data.frame()


bhoomi_frame_sample |> 
  summarise(n_bhoomi=sum(n),
            .by=c(village,in1_out0, village_in_out_bhoomi) )|> 
  left_join(polygons_n_village) |> 
  mutate(pct = n_bhoomi/n_polygons) |> 
  arrange(village_in_out_bhoomi) |> 
  as.data.frame()




bhoomi_village_in_out <- 
  df_smpl |> select(village,survey,in1_out0) |> 
  filter(!is.na(in1_out0 ))|> distinct() |> 
  count(village,in1_out0)  |> arrange(village) |> 
  mutate(village_in_out_bhoomi=mean(in1_out0),.by = village,
         village_in_out_bhoomi=ifelse(village_in_out_bhoomi==1,"all_in",ifelse(village_in_out_bhoomi==0,"all_out","in_out"))
  ) 

list_1 = bhoomi_village_in_out|> 
  filter(village_in_out_bhoomi !="all_out" ) |> 
  select(village) |> distinct() 
  
# village_in_out_bhoomi     n
# <chr>                 <int>
# 1 all_in                  9
# 2 all_out                 9
# 3 in_out                 23


Jain_crop_17_22 <- readr::read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_data/Jain_data/Jain_crop_17_22.csv")
Jain_crop_17_22 |> select(survey_number ) |> count(survey_number )

library(stringr)

# Create a vector of only the standard names from the list
clean_villages <- list_1$village

Jain_crop_clean <- 
  Jain_crop_17_22 |> 
  mutate(
    # Step 1: Basic cleaning of spaces and lowercase letters to improve accuracy
    village_clean = trimws(tolower(village)),
    
    # Step 2: Find the index of the closest name from the standard list
    closest_match_idx = amatch(village_clean, tolower(clean_villages), maxDist = 4, method = "jw"),
    
    # Step 3: Replace the original name with the official name from your list
    village = ifelse(!is.na(closest_match_idx), clean_villages[closest_match_idx], village)
  )|> 
  select("farmer_name","zone","block","village","survey_number", "area_ha","crop","season" 
         ) |> 
  mutate(
    survey_number = str_extract(survey_number, "^\\d+"),
    survey_number = as.numeric(survey_number) ) 

  
Jain_crop_1 =  Jain_crop_clean |> 
  drop_na() |> 
  filter(survey_number < 1000, village !="Blank") |>   
  select(village , survey_number) |> distinct() |> 
  group_by(village) |> 
  summarise(n_surveys_Jain = n() )


bhoomi_village_In  |> 
  left_join(Jain_crop_1) |> 
  mutate(pct = n_surveys_bhoomi/n_surveys_Jain)

bhoomi_Sampling_frame_IN <- 
  bhoomi_village_In  |> 
  mutate(site_area= ifelse(site_area=="Center","Inner","boundery")) |> 
  summarise(n_surveys_bhoomi=sum(n_surveys_bhoomi),.by = c(village,site_area)) |> 
  left_join(Jain_crop_1) |> 
  mutate(pct = n_surveys_bhoomi/n_surveys_Jain) |> 
  
  mutate(village_in_out_bhoomi=n(),.by = village,
       village_in_out_bhoomi=
         ifelse(village_in_out_bhoomi==1,"all_in","in_out"))

bhoomi_Sampling_frame_IN |> 
  summarise(survy_num_covered = sum(pct),.by = village) |> 
  mutate(survy_num_NOcovered= 1-survy_num_covered) |> 
  arrange(survy_num_NOcovered)


############  Clean Ramthal_East.shp       [Ramthal_shp_1]              ####
library(sf)

sf::sf_use_s2(FALSE)  # Disable S2 to avoid geometry engine issues

# Adjust this path if your file is stored elsewhere
Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
v_Hachanur_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Hachanur.shp")

dev.new()  # opens a new plotting window
plot(st_geometry(Ramthal_East_shp))
dev.new()  # opens a new plotting window

st_crs(Ramthal_East_shp)
# EPSG:4326 →  lat/lon
# EPSG:32643 →  UTM (meters)
# Ramthal_East_shp - mismach - EPSG:4326 AND  UTM (meters)
# Ramthal_East_shp has mismach of Bounding box UTM  and Geodetic CRS:WGS 84

st_crs(Ramthal_East_shp) <- 32643  # UTM zone 43N — coordinates are in meters
st_crs(Ramthal_East_shp)

# DF= 
as.data.frame(Ramthal_East_shp) %>%select(-path) %>% filter(!is.na(fid)) |>  filter(str_detect(id, "^129") )
# id, "^105" Chinnapur AND Chikkabadawadagi
##    104 = Chikkabadawadagi  [] all HH inside project
##    105 = Chinnapur []

# id, "^129" Virapura AND Banihatti
##    29 = Virapura  [Veerapur]
##    31 = Banihatti [Bannihatti, bannihatti] all HH inside project


133001 - 133097 Chatnihal 
136001 - 136059 Hirehunakunti 

133001 - 133059 Chatnihal 33
133060 - 133097 Hirehunakunti 36  /Bhoomi=Hirehunkunti
as_tibble(Ramthal_East_shp) %>%select(-path) %>% 
  filter(!is.na(fid)) |>  filter(str_detect(id, "^133") )    # 97 id

as_tibble(Ramthal_East_shp) %>%select(-path) %>% 
  filter(!is.na(fid)) |>  filter(str_detect(id, "^136") )    # 59 id

Bhoomi_20160610 |> filter(village =="Chatnihal" ) |> count(survey)    # 61 survey
Bhoomi_20160610 |> filter(village =="Hirehunkunti" ) |> count(survey) # 60 survey





library(stringr)
library(sf)

Ramthal_shp_1 <- st_read("C:/Users/Dan/OneDrive/Documents/ramthal_database/Ramthal_shp_1.shp")
st_crs(Ramthal_shp_1)

Ramthal_shp_1 <- Ramthal_East_shp %>%
  mutate(id = case_when(
    str_starts(as.character(id), "129") & layer == "Banihatti" ~ {
      paste0("131", substr(as.character(id), 4, 6))
    },
    str_starts(as.character(id), "105") & layer == "Chikkabadawadagi" ~ {
      paste0("104", substr(as.character(id), 4, 6))
    },
    T ~ as.character(id))) %>% mutate(id = as.integer(id)) |> 
  # mutate(
  #   layer = case_when(
  #     id >= 133064 & id <= 133097 ~ "NEW_NAME_HERE", # Replace with your target name
  #     TRUE ~ layer )) |> 
  select(-path) %>% filter(!is.na(fid) )

# sf::st_write(Ramthal_shp_1, "C:/Users/Dan/OneDrive/Documents/ramthal_database/Ramthal_shp_1.shp")

library(dplyr)

Ramthal_shp_11 <- Ramthal_shp_1 |>
  mutate(layer_update = case_match(
    layer,
    "Banihatti"        ~ "Bannihatti",
    "Binjawadgi"       ~ "Binjawadagi",
    "Chaitawadagi"     ~ "Chittavadagi",
    "Gattiganur"       ~ "Ghattiganur",
    "Hemavadgi"        ~ "Hemavadagi",
    "Hirehunakunti"    ~ "Hirehunkunti",
    "Jalakamaladinni"  ~ "Jalakamaladini",
    "Jambladinni"      ~ "Jambaladinni",
    "Kesarbhavi"       ~ "Kesarabhavi",
    "Malligihal"       ~ "Malagihal",
    "Nindasanur"       ~ "Nidasanur",
    "Ramawadgi"        ~ "Ramawadagi",
    "Virapura"         ~ "Virapur",
    "herur"            ~ "Herur",
    .default = layer
  ))

setdiff(polygons_n_village$village, bhoomi_frame_sample$village)

# BDIKA
Ramthal_shp_1 %>% filter(id >= 131000 & id <= 131999 ) %>%select(id, layer) %>%head(10)

# Quick verification check
Ramthal_shp_1 |> filter(id >= 133064 & id <= 133097) |> select(id, layer)

as.data.frame(Ramthal_shp_1) |>  filter(str_detect(id, "^129") )

as.data.frame(Ramthal_shp_1) %>% select(geometry) %>% distinct() %>% count()
as.data.frame(Ramthal_shp_1) %>% select(id) %>% distinct() %>% count()
# 24 id/shp_code have 2 POLYGONs each

# list_shape_code |> mutate(n=n(),.by = shp_code ) |> arrange(desc(n))


# Bhoomi_entire_sample ........................................................----


Bhoomi = 
  Bhoomi_20160610 |> select(Id, village, village_code,survey ,in1_out0,plot_match,south1_north0 )|>
  mutate(village = stringr::str_to_title(village) )|>  
  extract(  # Extract survey and hissa safely (including negative codes)
    col = survey, into = c("survey", "hissa"),
    regex = "^(-?[0-9]+)(.*)$", remove = T
  ) |> 
  filter(!is.na(survey))|>
  mutate( survey=as.numeric(survey),
          id = sprintf("1%02d%03d", as.numeric(village_code), survey),
          id=as.numeric(id))  |> 
  mutate(site_area = case_when(
    south1_north0 == 1  ~ "South",
    south1_north0 == 0  ~ "North", .default = "Center"
  )) |> select(-south1_north0)
# writexl::write_xlsx(Bhoomi, "C:/Users/Dan/OneDrive/Documents/ramthal_database/Bhoomi.xlsx")

id = POLYGON
Id = hh_id

unique(Bhoomi$village)

# Sampling Frame
n_distinct(Bhoomi$Id) # [] 11375

# Sampling Frame: polygons IDs
n_distinct(Bhoomi$id) # [] 2654

# Map polygons
length(unique(Ramthal_shp_1$id))# [] 3613

# OVERLAP S 
# what the overlap of | n_distinct(Bhoomi$id)
#                     | length(unique(Ramthal_shp_1$id))
df_overlap <- data.frame(id = intersect(Bhoomi$id, Ramthal_shp_1$id))
nrow(df_overlap)

# out of OVERLAP 
df_bhoomi_only <- data.frame(id = setdiff(Bhoomi$id, Ramthal_shp_1$id))
df_shp_only    <- data.frame(id = setdiff(Ramthal_shp_1$id, Bhoomi$id))
nrow(df_bhoomi_only)
nrow(df_shp_only)

# length(unique(na.omit(Ramthal_shp_1$geometry)))# [] 3637

Bhoomi |> select(Id,id,plot_match)|> 
  filter(!is.na(plot_match)) |> count(id)|> count(n)
# n    nn
# <int> <int>
# 1  1521
# 2   122
# 3    11
# 4     3

pre_sample <- readxl::read_excel("C:/Users/Dan/OneDrive/Documents/ramthal_database/pre_sample.xlsx")
pre_sample <- 
  Bhoomi |> 
  select(id, in1_out0, Id, plot_match,south1_north0) |> 
  mutate(site_area = case_when(
    south1_north0 == 1  ~ "South",
    south1_north0 == 0  ~ "North", .default = "Center"
  )) |> select(-south1_north0) |> 
  rename(hh_id = Id) |> 
  mutate(plot_match = +(!is.na(plot_match))) |> 
  group_by(id) |> 
  arrange(desc(plot_match), .by_group = TRUE) |> 
  slice(1) |> 
  ungroup() |> # filter(is.na(in1_out0))
  mutate(
    in1_out0 = case_when(
      hh_id == 100360 ~0, # id115039 fid3425 layerherur 
      hh_id == 103520 ~1, hh_id == 105686 ~ 0, .default = in1_out0
      ) )|> 
  mutate(category = case_when(
    in1_out0 == 1 & plot_match == 1 ~ "In-Sampled",
    in1_out0 == 1 & plot_match == 0 ~ "In",
    in1_out0 == 0 & plot_match == 1 ~ "Out-Sampled",
    in1_out0 == 0 & plot_match == 0 ~ "Out"
  )) |> 
  mutate(site_area_sample = case_when(
    site_area == "South" & plot_match == 1 ~ "South-Sampled",
    site_area == "North" & plot_match == 1 ~ "North-Sampled",
    site_area == "Center" & plot_match == 1 ~ "Center-Sampled", 
    .default = site_area
  )) 

writexl::write_xlsx(pre_sample, "C:/Users/Dan/OneDrive/Documents/ramthal_database/pre_sample.xlsx")

pre_sample |> filter(is.na(category))
pre_sample |> count(plot_match)
pre_sample |> count(category)

library(sf)
library(ggplot2)

# 
map_sf <- Ramthal_shp_1 |> 
  left_join(pre_sample, by = "id") #|>
  # mutate(village_code = as.numeric(substr(id, start = 2, stop = 3))) |> 
  # mutate(Village = paste0(layer, " (#", village_code, ")")) 
  
#
ggplot() +
  # שכבה 1: רק הפוליגונים שהם NA (צבע קונטור שונה, ללא צבע מילוי)
  geom_sf(data = filter(map_sf, is.na(site_area_sample)), 
          fill = "transparent", color = "gray70", size = 0.2) +
  
  # שכבה 2: רק הפוליגונים עם הערכים (קונטור gray85 רגיל)
  geom_sf(data = filter(map_sf, !is.na(site_area_sample)), 
          aes(fill = site_area_sample), color = "gray45", size = 0.1) +
  
  scale_fill_manual(
    values = c(
      "Center"  = "bisque2", "Center-Sampled"  = "bisque4",
      "North"  = "#8ECDB7",  "North-Sampled"  = "#52A58A",
      "South" = "#EBABCE", "South-Sampled" = "#C66597"
    )
  ) + 
  theme_minimal()

#

ggplot(map_sf) +
  geom_sf(aes(fill = category), color = "gray90", size = 0.1) +
  geom_sf_text(aes(label = id), size = 2.5, check_overlap = TRUE) +
  scale_fill_manual(
    values = c(
      "In-Sampled"= "dodgerblue4", # steelblue
      "In"  =       "dodgerblue",
      "Out-Sampled" = "gray70", #525252
      "Out" =         "gray85"
    ),
    na.value = "transparent"
  ) +
  theme_minimal()
#

library(sf)
library(dplyr)

# 1. ניצור תיבה חוסמת (Bounding Box) מסביב לכל המפה שלך
bbox_polygon <- st_as_sfc(st_bbox(map_sf))

library(sf)
library(dplyr)
library(ggplot2)

# --- שלב 1: תיקון הגיאומטריות הבעייתיות ב-Shapefile ---
map_sf_fixed <- map_sf %>%
  st_make_valid() %>%         # מתקן פוליגונים שחוצים את עצמם
  st_buffer(dist = 0)        # מנקה בעיות קצה וטופולוגיה עדינה

# --- שלב 2: יצירת הנגטיב (החלל הריק) ---

# 1. יצירת תיבה חוסמת (Bounding Box) מסביב לכל המפה שלך
bbox_polygon <- st_as_sfc(st_bbox(map_sf_fixed))

# 2. חיבור כל הפוליגונים הקיימים לגוף אחד רציף (כעת יעבוד ללא השגיאה)
combined_map <- st_union(map_sf_fixed)

# 3. פעולת הנגטיב: החסרת המפה מהקופסה הגדולה (משאיר רק את ה"קרחות" והרקע)
negative_space <- st_difference(bbox_polygon, combined_map)

# --- שלב 3: ציור המפה עם הגוון הניטרלי שבחרת ---
ggplot() +
  # צביעת ה"אין" (הרקע/חורים) בגוון הניטרלי-חם שבחרת (חול-אפרפר)
  geom_sf(data = negative_space, fill = "#C2B8B2", color = NA) + 
  
  # ציור קווי המתאר של הפוליגונים המקוריים בלבד (כדי שיראו את המבנה שלהם בתוך הריק)
  geom_sf(data = map_sf_fixed, fill = "transparent", color = "gray85", size = 0.1) +
  
  theme_minimal()

ggplot() +
  geom_sf(data = negative_space, fill = "#C2B8B2", color = NA) + 
    geom_sf(data = map_sf_fixed, fill = "transparent", color = NA) + 
  theme_minimal() + theme(panel.grid.major = element_blank())

library(ggplot2)

# 1. נשמור את המפה לתוך משתנה (למשל בשם p)
p <- ggplot() +
  geom_sf(data = negative_space, fill = "#C2B8B2", color = NA) + 
  geom_sf(data = map_sf_fixed, fill = "transparent", color = NA) + 
  theme_minimal() +
  # הסרת הרקע הלבן והקווים הלבנים של ה-Theme כדי להבטיח שקיפות מוחלטת
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# 2. שמירת הקובץ במחשב כ-PNG שקוף
ggsave(
  filename = "C:/Users/Dan/Desktop/my_transparent_map.png",
  plot = p,                             # המשתנה של המפה שיצרנו
  bg = "transparent",                   # הגדרת הרקע כשקוף
  width = 10,                           # רוחב התמונה באינצ'ים
  height = 8,                           # גובה התמונה באינצ'ים
  dpi = 300                             # איכות תמונה גבוהה להדפסה או תצוגה
)



library(ggplot2)
library(RColorBrewer)

# 1. פונקציה לייצור מפה לפי רשימת כפרים
plot_village_group <- function(data, selected_villages) {
  # ספירת כמות הכפרים בקבוצה (10 או 12)
  n_villages <- length(selected_villages)
  
  # יצירת הוקטור הצבעוני: n צבעים מובחנים + אפור לכל השאר
  map_colors <- c(brewer.pal(n_villages, "Paired"), "lightgray")
  names(map_colors) <- c(selected_villages, "Other")
  
  # הפקת המפה
  ggplot(data) +
    geom_sf(aes(fill = ifelse(Village %in% selected_villages, Village, "Other")), color = "white") +
    scale_fill_manual(
      values = map_colors, breaks = selected_villages,
      name = "Villages_________________") +
    theme_minimal()
}




# 2. חילוץ רשימת כל 42 הכפרים הייחודיים מהדאטה שלך (לפי סדר הופעתם)
all_villages <- unique(map_sf$Village)

# 3. חלוקת הכפרים ל-4 קבוצות (3 קבוצות של 10, וקבוצה אחרונה של 12)
group1 <- all_villages[1:10]
group2 <- all_villages[11:20]
group3 <- all_villages[21:30]
group4 <- all_villages[31:41]

# מפה 1 (כפרים 1-10)
plot_village_group(map_sf, group1)
plot_village_group(map_sf, group2)
plot_village_group(map_sf, group3)
plot_village_group(map_sf, group4)





library(ggplot2)
library(sf)

# 1. יצירת האובייקט (אם לא הרצת עדיין - יש להריץ פעם אחת)
lines_list <- list(
  st_linestring(coords_south1),
  st_linestring(coords_north2),
  st_linestring(coords_innerA),
  st_linestring(coords_innerB),
  st_linestring(coords_innerC)
)

ramthal_edge_sf <- st_sf(
  geometry = st_sfc(lines_list, crs = st_crs(south1_edge_sf)),
  type = c("South", "Other", "Other", "Other", "Other"))

# 2. הפקת המפה המשולבת עם קווי אורך ורוחב
ggplot() +
  geom_sf(data = map_sf, aes(fill = category), color = "gray90", size = 0.1) +
  geom_sf(data = ramthal_edge_sf_2, aes(color = type, size = type)) +
  scale_color_manual(values = c("South" = "gray20", "Other" = "gray20"), guide = "none") +
  scale_size_manual(values = c("South" = 0.5, "Other" = 0.5), guide = "none") +
  scale_fill_manual(
    values = c(
      "In-Sampled"= "dodgerblue4","In"= "dodgerblue",
      "Out-Sampled" = "gray70","Out"= "gray85"
    ),na.value = "transparent") +
  theme_minimal()
#


############  Clean Ramthal_East.shp       [Ramthal_shp_1]              ####
library(sf)

sf::sf_use_s2(FALSE)  # Disable S2 to avoid geometry engine issues

# Adjust this path if your file is stored elsewhere
Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
v_Hachanur_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Hachanur.shp")

dev.new()  # opens a new plotting window
plot(st_geometry(Ramthal_East_shp))
dev.new()  # opens a new plotting window

st_crs(Ramthal_East_shp)
# EPSG:4326 →  lat/lon
# EPSG:32643 →  UTM (meters)
# Ramthal_East_shp - mismach - EPSG:4326 AND  UTM (meters)
# Ramthal_East_shp has mismach of Bounding box UTM  and Geodetic CRS:WGS 84

st_crs(Ramthal_East_shp) <- 32643  # UTM zone 43N — coordinates are in meters
st_crs(Ramthal_East_shp)

# DF= 
  as.data.frame(Ramthal_East_shp) %>%select(-path) %>% filter(!is.na(fid)) |>  filter(str_detect(id, "^129") )
# id, "^105" Chinnapur AND Chikkabadawadagi
##    104 = Chikkabadawadagi  [] all HH inside project
##    105 = Chinnapur []

# id, "^129" Virapura AND Banihatti
##    29 = Virapura  [Veerapur]
##    31 = Banihatti [Bannihatti, bannihatti] all HH inside project

  
  133001 - 133097 Chatnihal 
  136001 - 136059 Hirehunakunti 
  
  133001 - 133059 Chatnihal 33
  133060 - 133097 Hirehunakunti 36
  
  
library(stringr)

Ramthal_shp_1 <- Ramthal_East_shp %>%
  mutate(id = case_when(
    str_starts(as.character(id), "129") & layer == "Banihatti" ~ {
      paste0("131", substr(as.character(id), 4, 6))
    },
    str_starts(as.character(id), "105") & layer == "Chikkabadawadagi" ~ {
      paste0("104", substr(as.character(id), 4, 6))
    },
    T ~ as.character(id))) %>% mutate(id = as.integer(id)) |> 
  mutate(
    layer = case_when(
      id >= 133064 & id <= 133097 ~ "NEW_NAME_HERE", # Replace with your target name
      TRUE ~ layer )
  ) |> select(-path) %>% filter(!is.na(fid) )



# BDIKA
Ramthal_shp_1 %>% filter(id >= 131000 & id <= 131999 ) %>%select(id, layer) %>%head(10)

# Update the layer name strictly for IDs in the 133064-133097 range
Ramthal_shp_1 <- Ramthal_shp_1 |> 
  mutate(
    layer = case_when(
      id >= 133064 & id <= 133097 ~ "NEW_NAME_HERE", # Replace with your target name
      TRUE ~ layer
    )
  )

# Quick verification check
Ramthal_shp_1 |> 
  filter(id >= 133064 & id <= 133097) |> 
  select(id, layer)


as.data.frame(Ramthal_shp_1) %>% select(geometry) %>% distinct() %>% count()
as.data.frame(Ramthal_shp_1) %>% select(id) %>% distinct() %>% count()


as_tibble(Ramthal_shp_1) %>% select(id) %>% count(id) |> arrange(desc(n))
as_tibble(Ramthal_shp_1) %>% select( geometry) %>% distinct()
# 24 id/shp_code have 2 POLYGONs each

list_shape_code |> mutate(n=n(),.by = shp_code ) |> arrange(desc(n))

# DFs ------------------------------------------------------------------
shpfile_3613_shp_code <- as_tibble(Ramthal_shp_1) |>  select(id) |> 
  distinct() |> rename(shp_code=id)

bl_1811_hh_id <- rmtl_baseline2016 |> select(hh_id)

df_1702_shp_code <-  list_shape_code |> rename(hh_id=id)

# DF להתאמות של 1:1
df_match_ids_I <- # A tibble: 1,317 [df_one_ids]
  df_1702_shp_code |> filter(!is.na(survey)) |> 
  mutate(n=n(),.by = shp_code ) |> filter(n==1) |> select(-n)|> 
  inner_join(shpfile_3613_shp_code) # Save polyID only if there is a match in the shapefile


# בחירת תצפית ראשונה מפוליגונים שיש בהם 2-3 משקי בית
df_match_ids_II <- # A tibble: 105
  df_1702_shp_code  |> # [df_multi_ids]
  filter(!is.na(survey)) |> 
  mutate(n = n(), .by = shp_code) |> filter(n > 1) |> select(-n)|> 
  inner_join(shpfile_3613_shp_code) |>  # Save polyID only if there is a match in the shapefile
  slice_head(n = 1, by = shp_code)


### 1317 + 105

#### DFs [df_remaining_hh_I] [df_used_shpcode_I]

# #  שאר התצפיות 
df_remaining_hh_I <- rmtl_baseline2016 |> # [df_remaining_hhids_bl]
  select(hh_id) |> anti_join( df_match_ids_I)|> anti_join( df_match_ids_II)

df_used_shpcode_I <- rbind(df_match_ids_I,df_match_ids_II) |> select(shp_code)


# CREAT SHP_CODE FOR df_remaining_hh_I
library(dplyr)
library(tidyr)

#### DF [df_remaining_hh]
df_remaining_hh <- # 389HH MASTER  df_creat_shp_code_I
  df_remaining_hh_I |> left_join(rmtl_baseline2016) |> 
  select(hh_id,  A10, # A9, 
         num_range("D4_", 1:17), num_range("D5_", 1:17)) |> 
  mutate(across(c(starts_with("D4_"), starts_with("D5_")), as.character)) |> 
  pivot_longer(
    cols = c(starts_with("D4_"), starts_with("D5_")),
    names_to = c(".value", "plot_number"),
    names_pattern = "(D4|D5)_(.*)" ) |> 
  rename(survey = D4, plot_village_raw = D5) |> 
  extract(  # Extract survey and hissa safely (including negative codes)
    col = survey, into = c("survey", "hissa"),
    regex = "^(-?[0-9]+)(.*)$", remove = T
  )

df_remaining_hh |> count(hh_id)  

#### DF [index_code_village_II]
index_code_village_I <- df_remaining_hh |> filter(!is.na(survey )) |> mutate( plot_village_raw = stringr::str_to_title(plot_village_raw)) |>  rename(HH_village=A10) |>  left_join(vill_cod |> rename(plot_village_raw=A9)) |> arrange(plot_village_raw)
# library(writexl)
# write_xlsx(index_code_village_I, "C:/Users/Dan/OneDrive/index_code_village_I.xlsx")
# עדכון ידני לקוד של הכפרים
# library(readxl)
# index_code_village_II <- read_excel("C:/Users/Dan/OneDrive/index_code_village_I.xlsx")

index_code_village_II[,1] |> distinct() # A tibble: 389

#### DF [df_389] 
# תצפיות נותרות אחרי סינון פוליגונים תפוסים וכלל חוות חסרות סורביינאמבר
df_389 <- 
  index_code_village_II |> select(hh_id,HH_village,survey,A10) |>  
  mutate( survey=as.numeric(survey),
          shp_code = sprintf("1%02d%03d", as.numeric(A10), survey)
          ) |>  
  left_join( # add used polygon
    rbind(df_match_ids_I,df_match_ids_II) |> 
      select(shp_code) |> mutate(shp_code=as.character(shp_code),polygon_used = 1) ) |> 
  # chack avilable polygon from shapfil
  left_join(
    shpfile_3613_shp_code |>  mutate(shp_code=as.character(shp_code)) |> mutate(polygon_exists=1)
    ) 

df_389[,1] |> distinct() 
# df_191|> filter(hh_id == 103687) 106975 |> filter(shp_code == 123014)

#### DF []
df_191 <-
  df_389 |>  filter(survey!= -999,is.na(polygon_used), polygon_exists==1  ) |> 
  select(hh_id,shp_code) |> distinct() |> 
  mutate(n_hh_id=n(),.by = hh_id ) |> 
  mutate(n_shp_code=n(),.by = shp_code ) 
  
DF126=df_191 |> filter(n_hh_id==1, n_shp_code==1) |> distinct() |> select(hh_id,shp_code)

DF54=df_191 |> anti_join(DF126)|> 
  distinct(hh_id, .keep_all = TRUE) |> 
  distinct(shp_code, .keep_all = TRUE)

DF02 <- df_191 |> anti_join(DF126)|> anti_join(DF54 |> select(hh_id) ) |> 
  anti_join(DF54 |> select(shp_code)) |> 
  slice_max(as.numeric(shp_code), n = 1, by = hh_id)
  
df_match_ids_III <- bind_rows(DF126,DF54,DF02) |> select(hh_id, shp_code)

df_match_ids_III[,1] |> distinct()
df_match_ids_III[,2] |> distinct()

### [1317 + 105] + 182
  
bind_rows(df_match_ids_I,df_match_ids_II) |> 
  bind_rows(df_match_ids_III |> mutate(shp_code=as.numeric(shp_code)) )


df_389 |> arrange(hh_id) |> 
  left_join( df_match_ids_III |> select(hh_id ) |> mutate(matchedHH=1)) |> 
  left_join( df_match_ids_III |> select(shp_code  ) |> mutate(polygon_usedIII=1)  ) |> 
  mutate(polygon_1 = if_else(polygon_used == 1 | polygon_usedIII == 1, 1, NA_real_)) |> 
  select(-polygon_used, -polygon_usedIII) # |> filter(is.na(polygon_1),is.na(matchedHH))


### אילתור פוליגונים ל 207 הנותרים
# נוהל שכן
df_179 <- df_389 |> arrange(hh_id) |> 
    anti_join( df_match_ids_III |> select(hh_id)) |> 
  filter(survey != -999) |> 
  select(hh_id, shp_code) |>  mutate(shp_code=as.numeric(shp_code))
  
df_28 <- df_389 |> arrange(hh_id) |> 
  anti_join( df_match_ids_III |> select(hh_id)) |> 
  anti_join( df_179 |> select(hh_id)) |>  
  select(hh_id, survey, A10,shp_code ) |> distinct()



# נוהל שכן
df_179
  
df_UNused_shpcode <- 
  shpfile_3613_shp_code |> 
  anti_join( df_match_ids_I)|> 
  anti_join( df_match_ids_II)|> 
  anti_join(df_match_ids_III |> mutate(shp_code=as.numeric(shp_code)) )



library(dplyr)

# 1. הכנת הדאטה
hh_targets <- df_179 |> mutate(shp_code_num = as.numeric(shp_code))
available_codes <- as.numeric(df_UNused_shpcode$shp_code)

# 2. הקצאה ייחודית ללא חזרות
results <- vector("numeric", length = nrow(hh_targets))

for (i in seq_len(nrow(hh_targets))) {
  current_code <- hh_targets$shp_code_num[i]
  
  # מציאת האינדקס של הקוד הפנוי הקרוב ביותר מתוך היתרה
  best_idx <- which.min(abs(available_codes - current_code))
  
  # שמירת הקוד שנבחר
  results[i] <- available_codes[best_idx]
  
  # הוסרת הקוד שנבחר ממלאי הפנויים!
  available_codes <- available_codes[-best_idx]
}

# 3. חיבור התוצאה לטבלה הסופית
df_179_unique <- hh_targets |> 
  mutate(new_shp_code = results) |> 
  select(hh_id, old_shp_code = shp_code, new_shp_code)


# בדיקה 1: האם מספר ה-shp_code הראשיים שווה למספר השורות? (חייב להחזיר TRUE)
n_distinct(df_179_unique$new_shp_code) == nrow(df_179_unique)

# בדיקה 2: האם קיבלנו בדיוק 179 משקי בית ייחודיים?
n_distinct(df_179_unique$hh_id)




df_179_best <- df_179_unique |> 
  # 1. חישוב המרחק המוחלט בין הקוד הישן לחדש
  mutate(dist = abs(old_shp_code - new_shp_code)) |> 
  # 2. בחירת השורה עם המרחק הקטן ביותר לכל hh_id
  slice_min(dist, n = 1, by = hh_id, with_ties = FALSE) |> 
  # 3. ניקוי עמודת המרחק הזמנית
  select(-dist)

# בדיקה 1: כמה משקי בית ייחודיים נשארו בטבלה הסופית?
nrow(df_179_best)

# בדיקה 2: לוודא שאין אף hh_id שמופיע פעמיים (חייב להחזיר TRUE)
n_distinct(df_179_best$hh_id) == nrow(df_179_best)

df_match_ids_VI <- df_179_best |> select(hh_id, new_shp_code) |> rename(shp_code=new_shp_code)


### DF [df_28]


df_UNused_shpcode_I <- 
  shpfile_3613_shp_code |> 
  anti_join( df_match_ids_I)|> 
  anti_join( df_match_ids_II)|> 
  anti_join(df_match_ids_III |> mutate(shp_code=as.numeric(shp_code)) ) |> 
  anti_join( df_match_ids_VI)
  

library(dplyr)
library(stringr)

# 1. הכנת הדאטה
hh_targets <- df_28 |> 
  # חילוץ קוד הכפר (הספרות שמופיעות לפני המקף, למשל 115)
  mutate(village_prefix = str_extract(shp_code, "^[0-9]+"))

# מלאי פוליגונים פנויים כמחרוזות
available_codes <- as.character(df_UNused_shpcode_I$shp_code)

# 2. הקצאה אקראית ייחודית בתוך הכפר
set.seed(42) # מבטיח תוצאות אקראיות אך שחיזירות (Reproducible)
results <- character(nrow(hh_targets))

for (i in seq_len(nrow(hh_targets))) {
  prefix <- hh_targets$village_prefix[i]
  
  # מציאת כל הפוליגונים הפנויים שמתחילים בקוד הכפר הזה
  matching_indices <- which(str_starts(available_codes, prefix))
  
  if (length(matching_indices) > 0) {
    # הכללה: אם יש פוליגונים תואמים בכפר - בוחרים אחד באקראי
    chosen_idx <- sample(matching_indices, 1)
  } else {
    # גיבוי: אם נגמרו הפוליגונים בכפר הספציפי הזה - בוחרים פוליגון אקראי מכלל המלאי
    chosen_idx <- sample(seq_along(available_codes), 1)
  }
  
  # שמירת הקוד שנבחר
  results[i] <- available_codes[chosen_idx]
  
  # הסרת הקוד שנבחר ממלאי הפנויים כדי למנוע כפילויות!
  available_codes <- available_codes[-chosen_idx]
}

# 3. חיבור התוצאה לטבלה
df_28_matched <- hh_targets |> 
  mutate(new_shp_code = as.numeric(results)) |> 
  select(hh_id, old_shp_code = shp_code, new_shp_code)

# 4. במידה ויש משקי בית שהיו להם כמה שורות ב-df_28, בוחרים שורה אחת לכל hh_id
df_match_ids_V <- df_28_matched |> 
  slice_head(n = 1, by = hh_id) |> 
  select(hh_id, new_shp_code) |> rename(shp_code=new_shp_code)
  


# בדיקה 1: האם קיבלנו בדיוק 28 משקי בית ייחודיים?
n_distinct(df_match_ids_V$hh_id)

# בדיקה 2: האם כל הפוליגונים שהוקצו הם ייחודיים (ללא כפילויות)? (חייב להיות TRUE)
n_distinct(df_match_ids_V$shp_code) == nrow(df_match_ids_V)


list_match_shp_hh <- 
  bind_rows( df_match_ids_I,
           df_match_ids_II)|> 
  bind_rows(df_match_ids_III |> mutate(shp_code=as.numeric(shp_code)) ) |> 
  bind_rows( df_match_ids_VI,df_match_ids_V)|> select(hh_id, shp_code) |> 
  left_join(rmtl_16_18_22_sample |> select(hh_id,sample) )


n_distinct(list_match_shp_hh$hh_id)
n_distinct(list_match_shp_hh$shp_code)
list_match_shp_hh |> select(hh_id, shp_code) |> distinct()











# ___________________________________________________________________________________
# ___________________________________________________________________________________
# ___________________________________________________________________________________



df_creat_shp_code_I <- df_remaining_hh |> 
  filter(survey > 0 )  |> 
  mutate(   # Clean village names and standardize village codes to remove leading zeros
    plot_village_raw = stringr::str_to_title(plot_village_raw), 
    # A9 = stringr::str_to_title(A9), 
    A10 = as.character(as.numeric(A10))
    ) |> 
  extract(  # Extract survey and hissa safely (including negative codes)
    col = survey, into = c("survey", "hissa"),
    regex = "^(-?[0-9]+)(.*)$", remove = T
  ) |> 
  mutate( survey=as.numeric(survey),
           shp_code = sprintf("1%02d%03d", as.numeric(A10), survey),
           shp_code = as.numeric(shp_code)) |> 
  select(hh_id,shp_code) |> 
  anti_join(df_match_ids_I |> select(shp_code)) |> 
  anti_join(df_match_ids_II |> select(shp_code)) |> 
  distinct() |> 
  mutate(n=n(),.by = hh_id ) 
  
  

df_creat_shp_code_III <- 
  df_creat_shp_code_I |> select(hh_id, A10 ,survey)|> distinct() |> 
  anti_join(df_creat_shp_code_II[,1] |> distinct()) |> mutate(survey="000") |> 
  bind_rows(
    df_creat_shp_code_II
  )
df_creat_shp_code_III |> count(hh_id)  






vill_cod <- rmtl_baseline2016 |> select( A9, A10) |> 
  mutate( A9 = stringr::str_to_title(A9), 
          A10 = as.character(as.numeric(A10))) |> distinct()
  
index_code_village_I <-  # 361
  df_creat_shp_code_III |> rename(HH_village=A10) |>  
  left_join(vill_cod |> rename(plot_village_raw=A9)) |> 
  arrange(plot_village_raw)

# library(writexl)
# write_xlsx(index_code_village_I, "C:/Users/Dan/OneDrive/index_code_village_I.xlsx")
# עדכון ידני לקוד של הכפרים
# library(readxl)
# index_code_village_II <- read_excel("C:/Users/Dan/OneDrive/index_code_village_I.xlsx")

df_creat_shp_code_III <- index_code_village_II |>
  mutate( survey=as.numeric(survey),
          shp_code = sprintf("1%02d%03d", as.numeric(A10), survey),
          shp_code = as.numeric(shp_code)) |> 
  select(hh_id,shp_code) |> distinct() |> 
  left_join(
    df_used_shpcode_I |> mutate(used=1)) |> # mark used polygons
    mutate(n=n(),.by = hh_id ) # count hh_id

df_match_ids_III <- 
  df_creat_shp_code_III |> 
  filter(is.na(used),n==1 )|> 
  distinct(hh_id, .keep_all = TRUE) |> 
  distinct(shp_code, .keep_all = TRUE)

df_used_shpcode_II <- 
  df_match_ids_I |> 
  bind_rows (df_match_ids_II) |> 
  bind_rows(df_match_ids_III) |> 
  select(shp_code) |> distinct()

df_553 <- 
  df_creat_shp_code_III |> select(hh_id,shp_code) |> 
  anti_join(df_match_ids_III |> select(hh_id)) |> distinct() |> 
  left_join(
    df_used_shpcode_II |> mutate(used=1)) # mark used polygons
  

# 1. הוספת דירוג עדיפות לכל שורה ב-df_553
df_ranked <- df_553 |> 
  mutate(
    score = case_when(
      used == 1             ~ 3,  # תפוס - מוצא אחרון/פסול
      shp_code %% 1000 == 0 ~ 2,  # פנוי אך מסתיים ב-000 - עדיפות שנייה
      TRUE                  ~ 1   # פנוי ורגיל - עדיפות ראשונה
    )
  )

# 2. מיון ובחירת האפשרות הטובה ביותר לכל חקלאי
df_best_per_hh <- df_ranked |> 
  arrange(hh_id, score) |> 
  slice_head(n = 1, by = hh_id)

# 3. מניעת כפילויות בפוליגונים (התאמה ייחודית 1 ל-1)
df_final_matched <- df_best_per_hh |> 
  arrange(score) |> 
  slice_head(n = 1, by = shp_code)

df_best_per_hh |> filter(score==1)





#

# נבדוק התאמה ל פוליגונים הפנויים

df_remain_match_177_shp <-  #177 matches
  df_remaining_hhids_shp |> 
  select(hh_id,shp_code) |> 
  left_join(
    df_UNused_shp |> mutate(shp_code=id) 
  ) |> filter(!is.na(id)) |> select(-id) |> 
  distinct(hh_id, .keep_all = TRUE) |> 
  distinct(shp_code, .keep_all = TRUE)


# לבדוק שום כמה נשאר

# UNused shp ids
df_UNused_shp_II <- 
  as_tibble(Ramthal_shp_1) %>% select(id) %>% distinct() |> 
  anti_join(df_used_shp_1422) |> 
  anti_join(df_remain_match_177_shp |> rename(id=shp_code) )
  
####
df_remain_match_2_shp <- df_remaining_hhids_shp |> 
  anti_join(df_remain_match_177_shp |> select(hh_id ) ) |> 
  left_join(
    df_UNused_shp_II |> mutate(shp_code=id) 
  )|> filter(!is.na(id)) |> select(-id) |> 
  slice_head(n = 1, by = hh_id) |> 
  select(hh_id , shp_code)
  

# combin matched hh_id shp_code
df_shp_1601  <- 
  bind_rows(
  df_match_ids_I, df_first_ids) |> 
  select(id, shp_code) |> rename(hh_id=id) |> 
  bind_rows(df_remain_match_177_shp) |> 
  bind_rows(df_remain_match_2_shp)

# UNused shp ids
df_UNused_shp_III <- 
  as_tibble(Ramthal_shp_1) %>% select(id) %>% distinct() |> 
  anti_join(df_shp_1601 |> rename(id= shp_code) ) 
  


rmtl_baseline2016 |> select( hh_id) |> 
  anti_join(df_shp_1601 ) 









# DF use in this script
rmtl_InOut
cultivated_land_2022

Ramthal_East_shp # 7274 obs sf         # original file # 
Ramthal_clean    # 3548 obs sf         # cols: id,geometry=POLYGON [m]
centroids_coords # 3548 obs data.frame # cols: X, Y, id


centroids_coords <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_coords.csv")

library(sf)
centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)

rmtl_cntrl_vars %>% count(village,farmers_hh) %>% as.data.frame()
list_shape_code %>% filter(str_detect(shp_code, "^129") )%>% as.data.frame()
rmtl_cntrl_vars %>% select(village, dist_Km_boundary) %>% 
  filter(is.na(dist_Km_boundary)) %>% as.data.frame() %>% arrange(village)











# Create centroids for each polygon (in meters)
Ramthal_shp_2 <- Ramthal_shp_1

Ramthal_centroids_utm <- 
  Ramthal_shp_2 %>% mutate(geometry = st_centroid(geometry))
# class(Ramthal_centroids_utm) # sf , tbl_df , tbl , data.frame


Ramthal_shp_3 <- Ramthal_shp_2 %>%
  mutate(
    X = st_coordinates(st_centroid(geometry))[, 1],
    Y = st_coordinates(st_centroid(geometry))[, 2]
  ) %>% as.data.frame() %>% select(id,X,Y)










# STEP 1: Fix geometries with st_buffer(0)
Ramthal_fixed <- Ramthal_East_shp %>%
  mutate(geometry = st_buffer(geometry, 0))
# IGNOR THE WARNING


# STEP 2: Remove broken, empty, or NA-boundary geometries
Ramthal_fixed_clean <- Ramthal_fixed %>%
  filter(!st_is_empty(geometry), st_is_valid(geometry)) %>%
  filter(!sapply(st_geometry(.), function(g) any(is.na(st_bbox(g)))))


# STEP 3: Group polygons, union them
Ramthal_clean <- Ramthal_fixed_clean %>%
  group_by(id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
# IGNOR THE WARNING AND THE ENDLESS LIST


# STEP 4: Reassign CRS to UTM
st_crs(Ramthal_clean) <- 32643  # UTM zone 43N — coordinates are in meters


### df status
st_crs(Ramthal_clean)
class(Ramthal_clean)
rm(Ramthal_fixed,Ramthal_fixed_clean)

############  Convert POLYGON to POINT     [centroids_coords]           ####

# Create centroids for each polygon (in meters)
Ramthal_centroids_utm <- 
  Ramthal_clean %>% mutate(geometry = st_centroid(geometry))
  # class(Ramthal_centroids_utm) # sf , tbl_df , tbl , data.frame


# Extract X/Y coordinates from the centroids
coords <- st_coordinates(Ramthal_centroids_utm) 
       ## class(coords) # matrix , array

# Combine coordinates with polygon id
centroids_coords <- as.data.frame(coords)
centroids_coords$id <- Ramthal_centroids_utm$id

class(centroids_coords)
rm(coords)

############  Allocate more coordinates    [Ramthal_points_XY]                ----


# Missing coordinates
Ramthal_points_XY = 
  list_shape_code  %>% rename(hh_id=id,id=shp_code) %>% right_join(hh_2022) %>% 
  select(hh_id, id) %>%
  left_join(centroids_coords)

# There are polygon ids' in list_shape_code that dontt exist in Ramthal_East_shp

Ramthal_points_XY %>% filter(is.na(X))   
Ramthal_points_XY %>% filter(is.na(X), !is.na(id)) 
Ramthal_points_XY %>% filter(is.na(id))   
# 203 hh_id dont have matched polygon 
# Of those:
#          200 hh_id polygons' dont matched polygon
#            3 hh_id dont have polygon at all

Ramthal_points_XY %>% filter(!is.na(X))   

Ramthal_points_XY %>% filter(!is.na(X))  %>% group_by(id) %>% mutate(n=n()) %>% ungroup() %>% filter(n==1) %>% count(n)
Ramthal_points_XY %>% filter(!is.na(X))  %>% group_by(id) %>% mutate(n=n()) %>% ungroup() %>% filter(n>1) %>% count(n)
#  1,409 hh_id have matched polygon
# Of those:
#          1233 obs :  One "hh_id" per ONE "id" (polygon)
#           164 obs :  Two "hh_id" per ONE id (polygon)
#            12 obs :  Three "hh_id" per ONE id (polygon)


# # What to do with cases of 
#  A. hh_id with "no polygon" (203)
#  B. 2-3 hh_id in ONE polygon (164+12)


# For A : Generate a new  polygon id for the HH 
#         based on the HH's survey/village plots
# For B : Will add 25 meters from the point of one HH to second HH 
#              and 50 to third HH



# For A : Generate a new  polygon
# Generate a new  polygon


miss_xy_A <- Ramthal_points_XY %>% filter(is.na(X)) %>% select(hh_id) %>% 
  left_join(a_plots_size %>% select(hh_id,plotSrvy ,plotVillage,plotID,acres ) ) %>% 
  mutate(plotVillage = tools::toTitleCase(plotVillage))

# Replace variants with correct spellings
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Amaravati"]        <- "Amaravathi"                    
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Binjawadagi"]      <-  "Binjawadgi" # bannihatti
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Chikkabadawadagi"] <- "Chikkabadwadgi" # chikkabadawadagi
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Chittavadagi"]     <- "Chittawadagi"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Ghattiganur"]     <- "Ghattignur"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Hegedal"]     <- "Hagedal"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Hirebadawadagi"]     <- "Hirebadawadgi"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Hirehunkunti"] <- "Hirehunakunti"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Kesarabhavi"]     <- "Kesarbhavi"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Thumba"]     <- "Tumba"
miss_xy_A$plotVillage[miss_xy_A$plotVillage == "Virapur"]     <- "Veerapur"

list_villages <- read.csv("~/master_research/DATAs/list_villages.csv")

miss_xy_B <-  miss_xy_A %>% left_join(list_villages %>% rename(plotVillage = village) )

# library(stringer)
# Remove anything after '-' or '/' AND trailing letters
miss_xy_B$plotSrvy_clean <- sub("[-/_].*$", "", miss_xy_B$plotSrvy)
miss_xy_B$plotSrvy_clean <- sub("[A-Za-z]+$", "", miss_xy_B$plotSrvy_clean)
miss_xy_B$plotSrvy_clean <- as.numeric(miss_xy_B$plotSrvy_clean)

miss_XY$id_new <-  floor(miss_XY$id / 1000) * 1000 # down to the nearest thousand

miss_point_A <- miss_xy_B %>% select(hh_id, village_code, plotSrvy_clean,plotID) %>% 
  mutate(plotSrvy_clean = ifelse(plotSrvy_clean>1000,NA,plotSrvy_clean)) %>% 
  mutate(id_new=100000+(village_code*1000)+ plotSrvy_clean ) %>% 
  mutate(id=id_new) %>% 
  left_join(centroids_coords) %>% 
  filter(!is.na(X)) %>% 
  arrange(plotID) %>%  group_by(hh_id) %>%  slice(1) %>% ungroup() %>% 
  select(hh_id,id_new,X,Y)


# combin [Ramthal_points]                                  ----

rr=hh_2022 %>% select(hh_id) %>% 
  left_join(
    Ramthal_points_XY %>% filter(!is.na(X)) %>%  # 1452
      rbind(miss_point_A %>% rename(id = id_new) )  # 117
  )


  
  
  
Ramthal_pointsA <- 
  Ramthal_points_XY %>% 
  left_join(miss_point_A, by = join_by(hh_id)) %>% 
  mutate(X=ifelse(is.na(X.x),X.y,X.x),
         Y=ifelse(is.na(Y.x),Y.y,Y.x),
         id=ifelse(is.na(id_new),id,id_new)) %>%
  select(hh_id,id,X,Y) %>% arrange(id) %>% 
  mutate(
    village = substr(as.character(id), 1, 3),
    survey  = substr(as.character(id), 4, 6)
  ) %>% 
  group_by(village) %>%
  mutate(
    X_temp = ifelse(survey == "000", NA, X),
    Y_temp = ifelse(survey == "000", NA, Y)
  ) %>% fill(X_temp, Y_temp, .direction = "up") %>% 
  mutate(
    v_X = median(X, na.rm = TRUE),
    v_Y = median(Y, na.rm = TRUE)
  ) %>% 
  mutate(
    X = case_when(!is.na(X) ~ X, survey == "000" ~ v_X, TRUE ~ X_temp),
    Y = case_when(!is.na(Y) ~ Y, survey == "000" ~ v_Y, TRUE ~ Y_temp)
  ) %>%
  ungroup() %>% select(hh_id,id,X,Y )

Ramthal_pointsA %>%  filter(is.na(X))

Ramthal_points <- Ramthal_pointsA %>% 
  mutate(
    X = case_when(
      id %in% c(125058, 125064, 125067) ~ 608363.5,
      id %in% c(125189, 125192, 125196, 125199, 125201) ~ 607797.1,
      id == 139087 ~ 618306.2,
      T ~ X),
    Y = case_when(
      id %in% c(125058, 125064, 125067) ~ 1769711.1,
      id %in% c(125189, 125192, 125196, 125199, 125201) ~ 1770400.1,
      id == 139087 ~ 1784913.0,
      T ~ Y ))


# Ramthal_points %>% filter(id %in% c(110000,111049 ,111050))











# df to Ramthal boundary       [ramthal_border_utm]         ----
#|==============================================================================
library(sf)
ramthal_border <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/ramthal_border.shp")

# Fix the ramthal_border (project boundary lines)
# Drop Z dimension (from LINESTRING Z to LINESTRING)
st_crs(ramthal_border)
ramthal_border_utm <- st_zm(ramthal_border, drop = TRUE)
st_crs(ramthal_border_utm)

dev.new()
plot(st_geometry(ramthal_border))
st_crs(ramthal_border)

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(sf)
south1_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/1 south-in.shp")
north2_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/2 north-in.shp")
# south3_out <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/3 south-out.shp")
# north4_out <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/4 north-out.shp")
inner4_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/5 innner.shp")

st_crs(south1_in)
dev.new()
plot(st_geometry(south1_in))

south1_utm <- st_transform(south1_in, crs = 32643)
north2_utm <- st_transform(north2_in, crs = 32643)
inner4_utm <- st_transform(inner4_in, crs = 32643)

# Step 1: Extract boundary coordinates from polygon
boundary_coords1 <- st_coordinates(south1_utm)
boundary_coords2 <- st_coordinates(north2_utm)
boundary_coords4 <- st_coordinates(inner4_utm)

# Step 2: Visualize coordinates to identify problematic points
dev.new()  
plot(boundary_coords[, "X"], boundary_coords[, "Y"],type = "l", col = "gray", main = "Polygon Coordinates")
points(boundary_coords[, "X"], boundary_coords[, "Y"], col = "yellow", pch = 19)
text(boundary_coords[, "X"], boundary_coords[, "Y"],labels = seq_len(nrow(boundary_coords)), cex = 0.5, pos = 3)

boundary_coords_clean <- boundary_coords1 [75:nrow(boundary_coords1), ]
edge_line  <- st_linestring(as.matrix(boundary_coords_clean[, c("X", "Y")]))
edge_sf      <- st_sfc(edge_line, crs = st_crs(south1_utm))
plot(st_geometry(edge_sf), col = "blue", lwd = 2,)


# Step 3: Remove bad points
boundary_coords_clean1 <- boundary_coords1 [75:nrow(boundary_coords1), ]
boundary_coords_clean2 <- boundary_coords2 [94:189, ]
boundary_coords_clean4a <- boundary_coords4 [126:141, ]
boundary_coords_clean4b <- boundary_coords4 [142:164, ]
boundary_coords_clean4c <- boundary_coords4 [181:nrow(boundary_coords4), ]


# Step 4: Create a LINESTRING from cleaned coordinates
south1_edge_line  <- st_linestring(as.matrix(boundary_coords_clean1[, c("X", "Y")]))
north2_edge_line  <- st_linestring(as.matrix(boundary_coords_clean2[, c("X", "Y")]))
innerA_edge_line  <- st_linestring(as.matrix(boundary_coords_clean4a[, c("X", "Y")]))
innerB_edge_line  <- st_linestring(as.matrix(boundary_coords_clean4b[, c("X", "Y")]))
innerC_edge_line  <- st_linestring(as.matrix(boundary_coords_clean4c[, c("X", "Y")]))

# Step 5: Wrap into an sf object using the original CRS (UTM zone 43N)
south1_edge_sf <- st_sfc(south1_edge_line, crs = st_crs(south1_utm))
north2_edge_sf <- st_sfc(north2_edge_line, crs = st_crs(north2_utm))
innerA_edge_sf <- st_sfc(innerA_edge_line, crs = st_crs(inner4_utm))
innerB_edge_sf <- st_sfc(innerB_edge_line, crs = st_crs(inner4_utm))
innerC_edge_sf <- st_sfc(innerC_edge_line, crs = st_crs(inner4_utm))

dev.new()
plot(st_geometry(south1_edge_sf), col = "blue", lwd = 2,)
plot(st_geometry(north2_edge_sf), col = "blue", lwd = 2,)


###### Combined  EdgeS

# Step 1: Extract coordinates as separate matrices
coords_south1 <- st_coordinates(south1_edge_sf)[, c("X", "Y")]
coords_north2 <- st_coordinates(north2_edge_sf)[, c("X", "Y")]
coords_innerA <- st_coordinates(innerA_edge_sf)[, c("X", "Y")]
coords_innerB <- st_coordinates(innerB_edge_sf)[, c("X", "Y")]
coords_innerC <- st_coordinates(innerC_edge_sf)[, c("X", "Y")]


# Step 2: Wrap both in a list
edge_segments <- list(coords_south1, coords_north2,
                      coords_innerA,coords_innerB,coords_innerC)

# Step 3: Create MULTILINESTRING
ramthal_multiline <- st_multilinestring(edge_segments)

# Step 4: Convert to sf object with same CRS
ramthal_edge_sf <- st_sfc(ramthal_multiline, crs = st_crs(south1_edge_sf))

# Step 4B: יצירת אובייקט sf עם עמודת קטגוריה לצביעה
ramthal_edge_sf_2 <- st_sf(
  geometry = st_sfc(lines_list, crs = st_crs(south1_edge_sf)),
  type = c("South", "Other", "Other", "Other", "Other") )


sf::st_write(ramthal_edge_sf, "C:/Users/Dan/OneDrive/Documents/ramthal_database/ramthal_edge_sf.shp")
sf::st_write(ramthal_edge_sf_2, "C:/Users/Dan/OneDrive/Documents/ramthal_database/ramthal_edge_sf_2.shp")

library(sf)
ramthal_edge_sf   <- sf::st_read("C:/Users/Dan/OneDrive/Documents/ramthal_database/ramthal_edge_sf.shp")
ramthal_edge_sf_2 <- sf::st_read("C:/Users/Dan/OneDrive/Documents/ramthal_database/ramthal_edge_sf_2.shp")


ggplot(ramthal_edge_sf) + geom_sf(color = "gray20", size = 0.5) + theme_void()
ggplot(ramthal_edge_sf) + geom_sf(color = "gray20", size = 0.5) + theme_minimal()




# 1.
p <- ggplot(ramthal_edge_sf) + geom_sf(color = "gray20", size = 1) + theme_void()

getwd()
setwd("C:/Users/Dan/OneDrive/Documents/ramthal_database")
# 2.
ggsave("ramthal_edge.png", plot = p, bg = "transparent", width = 8, height = 8)




# 3. הפקת המפה ב-ggplot2 עם צבעים מותאמים
ggplot(data = ramthal_edge_sf_2) +
  geom_sf(aes(color = type), size = 0.5) +
  scale_color_manual(values = c("South" = "purple", "Other" = "gray20")) +
  theme_void() +
  theme(legend.position = "none")
#

# # Calculate shortest distance to the boundary  ----

library(dplyr)
library(sf)
library(elevatr)

# 1) Rows with coordinates
pts_with_coords <- Ramthal_points %>%
  filter(!is.na(X) & !is.na(Y))

# 2) Rows without coordinates
pts_no_coords <- Ramthal_points %>%
  filter(is.na(X) | is.na(Y)) %>%
  mutate(dist_to_boundary_m = NA_real_,
         elevation_m = NA_real_)

# 3) Convert to sf
pts_sf <- st_as_sf(pts_with_coords, coords = c("X", "Y"), crs = 32643, remove = FALSE)

# 4) Boundary sf
edge <- st_sf(geometry = ramthal_edge_sf)

# 5) Distance to boundary
pts_sf$dist_to_boundary_m <- as.numeric(st_distance(pts_sf, edge)[, 1])

# 6) Elevation (convert to lat/lon first)
pts_wgs <- st_transform(pts_sf, 4326)
elev_data <- get_elev_point(pts_wgs, src = "aws", z = 10, units = "meters")
pts_sf$elevation_m <- elev_data$elevation

# 7) Drop geometry for regression
pts_with_vals <- st_drop_geometry(pts_sf)

# 8) Combine back with NA rows
Ramthal_dist_elev <- bind_rows(pts_with_vals, pts_no_coords) %>%
  arrange(hh_id)

# final_df now has all rows with dist_to_boundary_m and elevation_m
library(writexl)

write_xlsx(Ramthal_dist_elev,
           "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/Ramthal_dist_elev.xlsx")




centroids_coords <- 
  read_csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_coords.csv")

library(sf)
centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)






centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)

sf::st_write(
  ramthal_border_sf, 
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_border_sf.shp"
)

sf::st_write(
  ramthal_border_sf, 
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_border_sf.gpkg"
)

https://rcn.montana.edu/resources/Converter.aspx





# # 1. Bind the df's
# rd_water_with_coords <- 
#   multi_hh_df %>% select(hh_id, X, Y) %>%
#   inner_join(df_for_water_map, by = "hh_id")
# 
# # 2. Conver to sf format
# rd_water_with_coords <- rd_water_with_coords %>%
#   st_as_sf(coords = c("X", "Y"), crs = 32643) # UTM
# 
# # 3. Compute distance to the southern boundary line  (south_edge_sf) (combined_edge_sf)
# rd_water_with_coords$dist_to_south_m <- st_distance(
#   rd_water_with_coords,
#   ramthal_b)
# 
# # 3. Convert from units to numeric meters (optional but clearer)
# rd_water_with_coords$dist_to_south_m <- as.numeric(rd_water_with_coords$dist_to_south_m)
# rd_water_with_coords$elevation <- as.factor(rd_water_with_coords$elevation)









############  elevation data               [elev_data_sf] [elev_data_tbl]    ----
# extract elevation data from coordinates
#|=============================================================================

library(elevatr)
library(sf)

## FOR SHAPE FILE centroids_sf
# Make sure the points are in WGS84 (EPSG:4326) # "centroids_sf" current CRS is UTM zone 43N
centroids_wgs <- st_transform(centroids_sf, crs = 4326)

# Extract elevation
elevation_points_sf <- get_elev_point(locations = centroids_wgs, src = "aws", units = "meters", z = 10)
st_write(elevation_points_sf, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_sf.shp")


## FOR tibble df "centroids_coords"
centroids_sf2 <- st_as_sf(centroids_coords, coords = c("X", "Y"), crs = 32643)  # UTM zone 43N
centroids_wgs2 <- st_transform(centroids_sf2, crs = 4326)
elev_data2 <- get_elev_point(locations = centroids_wgs2, src = "aws", units = "meters", z = 10)

elevation_points_tbl <- st_drop_geometry(elev_data2)
elevation_points_tbl <- elevation_points_tbl %>% 
  select(id ,elevation) %>% rename(elevation_m=elevation)

library(writexl)
write_xlsx(elevation_points_tbl, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/elevation_points_tbl.xlsx")

elev_df <- cbind(st_drop_geometry(elev_data2),st_coordinates(elev_data2))




# __________________________________________________________________________ ====

########  DF 2022    [df_for_Land_map] [df_for_water_map]     OLD

# [df_for_water_map] 
df_for_water_map <- 
  rmtl_InOut %>% select(
    hh_id,in1_out0, mm4 ,hh_6m_2021_22 , elevation,distance_km,
    south1_north0,a5,drip_use, ir_use) %>% 
  rename(in_project  = in1_out0, drip_installed=mm4) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) #  %>% 
# mutate(Elevation=
#          case_when( elevation==0~ 519,elevation==1~ 522,
#                     elevation==2~ 525,elevation==3~ 528,
#                     elevation==4~ 531,elevation==5~ 534,
#                     elevation==6~ 537,elevation==7~ 540,
#                     TRUE ~ 1000))


# [df_for_Land_map]
df_for_Land_map <- 
  cultivated_land_2022 %>% # df from impact2.R
  group_by(hh_id ) %>% 
  summarise(acre_drip=sum(acre_drip),
            acre_ir=sum(acre_ir),
            acre_cult=sum(acre_cult),# Sum
            land_holding=mean(land_holding), # Mean
            pct_cult_land=sum(pct_cult_land),
            pct_drip_land=sum(pct_drip_land),
            pct_ir_land=sum(pct_ir_land) ) %>% 
  left_join(
    rmtl_InOut %>% select(hh_id,in1_out0,drip_use, ir_use,elevation,distance_km, south1_north0  )
  ) %>% rename(in_project=in1_out0) %>% 
  mutate(elevation = ifelse(elevation == "7+",7,elevation)) %>% 
  mutate(elevation = ifelse(is.na(elevation),7,elevation),
         elevation=as.numeric(elevation)) 

rd_water_with_coords
rd_land_with_coords


# ##      [rd_water_with_coords]  OLD

# 1. Bind the df's
rd_water_with_coords <- 
  multi_hh_df %>% select(hh_id, X, Y) %>%
  inner_join(df_for_water_map, by = "hh_id")

# 2. Conver to sf format
rd_water_with_coords <- rd_water_with_coords %>%
  st_as_sf(coords = c("X", "Y"), crs = 32643) # UTM

# 3. Compute distance to the southern boundary line  (south_edge_sf) (combined_edge_sf)
rd_water_with_coords$dist_to_south_m <- st_distance(
  rd_water_with_coords,
  combined_edge_sf)
  # southern_edge_sf)

# 3. Convert from units to numeric meters (optional but clearer)
rd_water_with_coords$dist_to_south_m <- as.numeric(rd_water_with_coords$dist_to_south_m)
rd_water_with_coords$elevation <- as.factor(rd_water_with_coords$elevation)


# ##      [rd_land_with_coords]        ----
# A tibble: 1,378

# 1. Bind the df's
rd_land_with_coords <- 
  multi_hh_df %>% select(hh_id, X, Y) %>%
  inner_join(df_for_Land_map, by = "hh_id") 

# 2. Conver to sf format
rd_land_with_coords <- 
  st_as_sf(rd_land_with_coords,
  coords = c("X", "Y"), crs = 32643)  # UTM

# 3. Compute distance to the southern boundary line
rd_land_with_coords$dist_to_south_m <- st_distance(
  rd_land_with_coords,
  combined_edge_sf)
  # south_edge_sf) # df south_edge_sf creating is below

# 4. Convert from units to numeric meters (optional but clearer)
rd_land_with_coords$dist_to_south_m <- as.numeric(rd_land_with_coords$dist_to_south_m)



# summary stats for obs with coords ------------------------------

rd_water_with_coords %>% # df below
  st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup() %>% 
  mutate(Sample=sum(N)/2 ) %>% 
  kable() %>% kable_minimal()

rd_land_with_coords %>% # df below
  st_drop_geometry() %>%
  count(in_project,drip_use) %>% 
  group_by(in_project) %>% 
  mutate(N=sum(n),Percent=n/N) %>% ungroup()%>% 
  mutate(Sample=sum(N)/2 )%>% 
  kable() %>% kable_minimal()


rd_land_with_coords %>% st_drop_geometry() %>%
  group_by(in_project) %>%
  summarise(
    n = n(),
    acre_drip  = mean(acre_drip , na.rm = TRUE),
    acre_ir = mean(acre_ir , na.rm = TRUE),
    acre_cult = mean(acre_cult , na.rm = TRUE),
    pct_drip_land = mean(pct_drip_land , na.rm = TRUE),
    land_holding = mean(land_holding, na.rm = TRUE)
  ) %>% arrange(desc(in_project))%>% 
  kable() %>% kable_minimal()


# __________________________________________________________________________ ====

#  ZONEs VAR ----

library(readxl)
village_list4_S1 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_data/village_list4.xlsx", 
                               sheet = "Sheet1")

village_list4_S2 <- read_excel("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/ramthal_data/village_list4.xlsx", 
                               sheet = "Sheet2")

# [1] Chaking the data
village_list4_S2[,c(1,3)] %>% distinct() %>% count(village) %>% arrange(desc(n))
# 5 villges have 2-4 zones
# the rest have ONE

# [2] Adding "zone" to villages with MORE than one zone
# prepering df to work on Excel manualy
list_shape_code %>% dplyr::select(id, a6, survey) %>% rename(hh_id=id) %>% 
  left_join(village_list4_S1 %>% dplyr::select(a5,a6, village) ) %>% 
  filter(village %in% c("Hungund", "Amaravati", "Ramawadagi",
                        "Binjawadagi","Binjawadgi",
                        "Hirehunakunti") 
  ) %>% arrange(a6) %>% 
  kbl() %>% kable_styling()

# [3] copy paste it to work on Excel manualy    
#    Locate the "ZONE" of the HH in these villages.
#    [rmtl_block_zone_A]

# [4] Adding "zone" to villages with ONE zone only
#df 1
village_list_of_ONE_zone <- village_list4_S2 %>% filter(!village %in% c("Hungund", "Amaravati", "Ramawadagi", "Binjawadagi", "Hirehunakunti") 
) %>% dplyr::select(village,zone) %>% distinct()

#df 2  
vill_ONE_zone <- list_shape_code %>% dplyr::select(id, a6) %>% rename(hh_id=id) %>% 
  left_join(village_list4_S1 %>% dplyr::select(a5,a6, village) ) %>% 
  filter( ! village %in% c("Hungund", "Amaravati", "Ramawadagi",
                           "Binjawadagi","Binjawadgi",
                           "Hirehunakunti") 
  ) %>% left_join(village_list_of_ONE_zone)

### some villages didnt meet the Zone - Bcoz villages wording

# [5] locate villages without zone
vill_ONE_zone %>% 
  filter(is.na(zone)) %>% 
  dplyr:: select(-hh_id) %>% distinct()

# [6] complit missing zone data
vill_ONE_zone$zone[vill_ONE_zone$a6 == 19] <- "III"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 11] <- "iv"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 22] <- "III"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 25] <- "iv"
vill_ONE_zone$zone[vill_ONE_zone$a6 ==  5] <- "III"

vill_ONE_zone$zone[vill_ONE_zone$a6 == 10] <- "iii"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 20] <- "I"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 16] <- "iv"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 35] <- "IV"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 32] <- "ii"

vill_ONE_zone$zone[vill_ONE_zone$a6 == 4] <- "IV"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 37] <- "i"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 34] <- "iii"
vill_ONE_zone$zone[vill_ONE_zone$a6 == 23] <- "i"

vill_ONE_zone_b <- 
  vill_ONE_zone %>% mutate(block="") %>% 
  dplyr::select(hh_id:village,block,zone)

# [7] import df {rmtl_block_zone_A} of 5 villages with 2-4 zones
library(readr)
rmtl_block_zone_A <- read_csv(
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_block_zone_A.csv")

# [8] Merging the df's {rmtl_block_zone} & {vill_ONE_zone}
rmtl_block_zone <- 
  rmtl_block_zone_A %>%  dplyr::select(-survey) %>% 
  rbind(vill_ONE_zone_b )

library(writexl)
write_xlsx(rmtl_block_zone, "C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/rmtl_block_zone.xlsx")




# __________________________________________________________________________ ====

# MAPS ----
library(ggplot2)
library(sf)

# Map | elevation                        ----
rd_water_with_coords
ramthal_border_utm

# Create custom labels
elevation_labels <-
  c("0" = "~519 m", "1" = "~521 m", "2" = "~524 m", "3" = "~527 m",
    "4" = "~531 m", "5" = "~534 m", "6" = "~537 m", "7" = "~540 m" )

library(RColorBrewer)
dev.new()
ggplot() + # Export 650 X 490
  geom_sf(data = rd_water_with_coords, aes(fill = as.factor(elevation)),
          color = "gray70", shape = 21, size = 2, alpha = 0.7) +
  scale_fill_brewer(palette = "Reds", name = "Elevation",
                    labels = elevation_labels) +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  labs(title = "Farmer Locations by Elevation",x = NULL, y = NULL) +
  theme_minimal(base_family = "serif")

# Map | drip_use                         ----
rd_water_with_coords
ramthal_border_utm

ggplot() +  # Export 650 X 490
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  geom_sf(data = rd_water_with_coords, aes(color = factor(drip_use)), size = 1.5) +
  scale_color_manual(
    name = "Farmers who used \nat least once",
    values = c("0" = "gray70", "1" = "dodgerblue4"),
    labels = c("0" = "Use", "1" = "Didn't Use")) +
  labs(title = "Drip Irrigation Users In & Out Ramthal Project (2016-2023)",x = NULL,y = NULL
  ) +theme_minimal(base_family = "serif")

# Map | drip_use in southern boundary"   ----

rd_water_south1 <- rd_water_with_coords %>% filter(south1_north0 ==1)
# rd_water_south1 <- rd_water_with_coords %>% filter(dist_to_south_m < 1500)

ggplot() +
  geom_sf(data = rd_water_south1 %>% filter(drip_use == 1), 
          color = "dodgerblue4", size = 1.5) +
  geom_sf(data = rd_water_south1 %>% filter(drip_use == 0), 
          color = "gray60", size = 1.5) +
  geom_sf(data = south_edge_sf,color = "red4", linewidth= 1) +
  labs( title= "Drip Adoption Around the Southern Boundary",
  ) + theme_minimal(base_family = "serif")


# Map | acre_drip   ----

# Create color group
rd_land_with_coords2 <- rd_land_with_coords%>% 
  mutate(acre_drip2= # To distinguish between 0 acre_drip of drip_use==1 and 0 acre_drip of drip_use==0
           ifelse(drip_use==1 & acre_drip==0,0.0001,acre_drip)) %>% 
  mutate(color_group = case_when(
    acre_drip2  == 0.0001 ~ "gray70",
    acre_drip2  == 0      ~ "gray90",
    acre_drip  <= 2       ~ "dodgerblue",
    acre_drip  > 2        ~ "dodgerblue4",
    TRUE          ~ "gray"    ))%>% arrange(desc(color_group))

# acre_drip
rd_land_with_coords2 %>% # Export 650 X 490
  ggplot() +
  geom_sf(aes(color = factor(color_group)), size = 2) +  # map color_group as a factor
  scale_color_manual(name = "Drip-Irrigated Acres",
                     values = c("gray70" = "gray70",          # 0 acre for drip_use==1
                                "gray90" = "gray90",          # 0 acre for drip_use==0
                                "dodgerblue" = "dodgerblue",  # <= 2 acres
                                "dodgerblue4" = "dodgerblue4" ),# > 2 acres
                     labels = c("gray70" = "0 acres | Use before 2022",
                                "gray90" = "Non-users",
                                "dodgerblue" = "≤ 2 acres",
                                "dodgerblue4" = "> 2 acres") ) +
  geom_sf(data = ramthal_border_utm, color = "black", linewidth = 1) +
  labs(title = "Acre Land Under Drip Irrigation") +
  theme_minimal(base_family = "serif")

# Map | acre_drip in southern boundary  ----
rd_land_with_coords2%>% # Export 650 X 490
  filter(south1_north0 ==1) %>%
  ggplot() +
  geom_sf(aes(color = factor(color_group)), size = 2) +  # map color_group as a factor
  scale_color_manual(name = "Drip-Irrigated Acres",
                     values = c("gray70" = "gray70",          # 0 acre for drip_use==1
                                "gray90" = "gray90",          # 0 acre for drip_use==0
                                "dodgerblue" = "dodgerblue",  # <= 2 acres
                                "dodgerblue4" = "dodgerblue4" ),# > 2 acres
                     labels = c("gray70" = "0 acres | Use before 2022",
                                "gray90" = "Non-users",
                                "dodgerblue" = "≤ 2 acres",
                                "dodgerblue4" = "> 2 acres") ) +
  geom_sf(data = south_edge_sf, color = "red3", linewidth = 1) +
  labs(title = "Acre Land Under Drip Irrigation | southern boundary") +
  theme_minimal(base_family = "serif")





# library(RColorBrewer)  ----
library(RColorBrewer)
display.brewer.all()
graphics.off()  # shuts down all graphics devices


YIOrRd
YIOrBr
YIGnBu
YIGn
Reds
RdPu
Purples
PuRd
PuBuGn
PuBu
OrRd
Oranges
Greys
"Greens"
"GnBu"
BuPu
"BuGn"
Blues

Set3
Set2
Set1
Pastel2
Pastel1
Paired
Dark2
Accent

Spectral
RdYIGn
RdYIBu
RdGy
RdBu
PuOr
PRGn
PİYG
BrBG

















# __________________________________________________________________________ ====



# OLD CODE ............................................................. ----

############  df to south_ boundary         [south_edge_sf]              =====================================
library(sf)

south1_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/1 south-in.shp")
north2_in <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/New folder/2 north-in.shp")

plot(st_geometry(south1_in ),main = "Project's South Boundary")
dev.new()

# Southern Border
#|===============
st_crs(south1_in)
south1_utm <- st_transform(south1_in, crs = 32643)
st_crs(south1_utm)

boundary_coords <- st_coordinates(south1_utm)
#  Visualize coordinates to identify problematic points
boundary_coords_clean <- boundary_coords[76:nrow(boundary_coords), ]
southern_edge_line <- st_linestring(as.matrix(boundary_coords_clean[, c("X", "Y")]))
southern_edge_sf <- st_sfc(southern_edge_line, crs = st_crs(south1_utm))

dev.new()
plot(st_geometry(southern_edge_sf), col = "red3", lwd = 2,
     main = "Southern Border")

# Step 7 😄:  Ensure centroids are in sf format
centroids_sf <- st_as_sf(centroids_coords, coords = c("X", "Y"), crs = 32643)
library(sf)

# Read the shapefile back into R
centroids_sf <- st_read("C:/Users/Dan/OneDrive - mail.tau.ac.il/Ramthal Data/centroids_sf.shp", stringsAsFactors = F, quiet=F)

#|==============================
# Completion to Southern Border 
#|==============================
st_crs(north2_in)
north2_utm <- st_transform(north2_in, crs = 32643)
st_crs(north2_utm)

boundary_north2_coords <- st_coordinates(north2_utm)

boundary_north2_clean <- boundary_north2_coords[174:nrow(boundary_north2_coords), ]
north2_edge_line <- st_linestring(as.matrix(boundary_north2_clean[, c("X", "Y")]))
north2_edge_sf <- st_sfc(north2_edge_line, crs = st_crs(north2_utm))

dev.new()
plot(st_geometry(north2_edge_sf), col = "red3", lwd = 2)

#|====================================================================== 
#  combine [southern_edge_sf] & [north2_edge_sf] into a single LINESTRING
#|======================================================================
coords_south <- st_coordinates(southern_edge_sf)[, c("X", "Y")]
coords_north <- st_coordinates(north2_edge_sf)[, c("X", "Y")]
edge_segments <- list(coords_south, coords_north)
combined_multiline <- st_multilinestring(edge_segments)
combined_edge_sf <- st_sfc(combined_multiline, crs = st_crs(southern_edge_sf))
plot(combined_edge_sf, col = "red3", lwd = 2, main = "Combined Project Edge (Unconnected)")
rm(south1_in,south1_utm,boundary_coords,boundary_coords_clean, 
   southern_edge_line,southern_edge_sf )
rm(north2_in,north2_utm, boundary_north2_coords, boundary_north2_clean,
   north2_edge_line,north2_edge_sf)
rm(coords_south, coords_north, edge_segments)






# library(raster) ----
# shp <- shapefile("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
library(dplyr)
library(ggplot2)
library(sf)

# Ramthal_East_shp <- st_read("C:/Users/Dan/Documents/master_research/DATAs/ramthal_data/project_map/villages/Ramthal_East.shp")
# Ramthal_East_shp %>% ggplot() +geom_sf()

shapefile_wgs84 <- st_transform(shapefile, crs = st_crs(4326))  # 4326 is the EPSG code for WGS84 (longitude and latitude)


#  shapfile_rmtl_2022----
shp_index22_NA <-  # 118 hh NAs
  list_shape_code %>% rename(hh_id=id) %>%  inner_join(hh_2022) %>% filter(is.na(survey))

shp_rmtl_id <- # id polygon
  Ramthal_East_shp %>% as.data.frame() %>% dplyr::select(id) %>% distinct() %>% mutate(is=1)

library(tidyr)
survey_nu_22 <- # generating 118 new id's 
  a_plots_size %>% dplyr::select(hh_id,plotSrvy) %>% right_join(shp_index22_NA %>% dplyr::select(hh_id) ) %>% 
  group_by(hh_id) %>% slice(1) %>% 
  separate(plotSrvy, into = c("survey", "hiss"), sep = "-") %>% 
  separate(survey, into = c("survey", "hiss"), sep = "/") %>% 
  mutate(survey=ifelse(survey%in% c("_999","12A"),"",survey)) %>% 
  mutate(survey=ifelse(survey=="12A","12",survey)) %>% 
  mutate(survey=ifelse(survey%in% c("44744","44714","44573"),"44",survey)) %>% 
  mutate(survey= as.numeric(survey)) %>% dplyr::select(-hiss)  %>% 
  left_join(shp_index22_NA[,c(1,5)]) %>% 
  mutate(idOLD=shp_code, 
         shp_code= ifelse(!is.na(survey),survey+shp_code,shp_code )) %>%   
  left_join(shp_rmtl_id %>% rename(shp_code=id)) %>% 
  mutate(idNA=ifelse(is.na(is),idOLD,shp_code))  %>% 
  dplyr::select(idNA,hh_id)
  
shp_index22 <- # implimanting 79 of the 118 new id's into the index =
  list_shape_code %>%  rename(hh_id=id) %>% inner_join(hh_2022) %>% 
  left_join(survey_nu_22) %>% 
  mutate(shp_code=ifelse(is.na(idNA),shp_code,idNA)) %>% 
  # mutate(id=ifelse(is.na(idNA),id,idNA)) %>% select(id, hh_id, farmers_hh)
  dplyr::select(shp_code, hh_id)

rm(shp_index22_NA,shp_rmtl_id, survey_nu_22)

#### shapfile_rmtl_2022 ###
shapfile_rmtl_2022 <- Ramthal_East_shp %>% left_join(shp_index22)

# MAPS -----
       -----------------------------
#|     |  R map for presentaition  |
#|     |  W 8.91   H  6.5          |
       -----------------------------

# polygons ----

shapfile_rmtl_2022 <- Ramthal_shp_1 |> select(-path) |> 
  arrange( fid) |> filter(!is.na(fid) ) |> 
  left_join(list_shape_code |> rename(hh_id=id, id = shp_code)) |> left_join(hh_2022)
ggplot(shapfile_rmtl_2022) +geom_sf()


# villages map ----

shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scale_fill_distiller(palette = "Greys")
scale_fill_distiller(palette = "YlOrBr")

# for the villages names  
shapfile_rmtl_2022 %>% ggplot()+ geom_sf(aes(fill = layer))+ theme(legend.position = "bottom")

# ZONE ----


zone_village_survey <- # A tibble: 1,700
  jain_rabbi_2019 %>% 
  select(zone,block ,village,survey_number) %>% distinct() %>%
  mutate(survey_number=as.numeric(survey_number)) 

zone_survey <- zone_village_survey %>% select(zone,block, survey_number) %>% distinct()%>% drop_na()
  
index4zone <- 
  shp_index22 %>% 
  right_join(list_shape_code) %>% # A tibble: 1,702
  rename(survey_number=survey) %>% 
  inner_join(zone_survey) %>% 

list_shape_code
  
# project/ non project ----

shapfile_rmtl_2022 %>% mutate(farmers_hh=ifelse(is.na(farmers_hh),"not_sampled",farmers_hh)) %>% 
  ggplot()+ geom_sf(aes(fill = farmers_hh), color="gray25")+
  scale_fill_manual(
    values = c("inside_ramthal" = "dodgerblue4", "outside_ramthal" = "gray65", "not_sampled" = "white"))+
  theme_minimal() +theme(legend.position = "none")

# share hh irrigation ----

shp_rmtl_id <- Ramthal_East_shp %>% as.data.frame() %>% select(id) %>% distinct() %>% mutate(is=1)

# irrigation from Gov supply group_by( id )
A.method <- a_irri_rain_method %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct()
  
A.gov <- 
  a_source_irri[,c(2,7)] %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(irri_source_num= ifelse(5 %in% irri_source_num, "5", ifelse(any(irri_source_num== 2), "2",ifelse(any(irri_source_num== 3), "3",ifelse(any(irri_source_num== 4), "4",ifelse(any(irri_source_num== 7), "7","0"))))) ) %>% ungroup() %>% 
  select(id,irri_source_num ) %>% distinct() 

A.mg=left_join(A.method,A.gov) %>% right_join(shp_rmtl_id) %>% 
  mutate(hh_6methods=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  mutate(irri_source_num=ifelse(is.na(irri_source_num),"not_sampled",irri_source_num ) ) %>% 
  mutate(mg=ifelse(hh_6methods!="rain" & irri_source_num==5,"irri_gov",hh_6methods ) )

# map
shapfile_rmtl_2022 %>% 
  left_join(A.mg) %>% 
  ggplot()+ geom_sf(aes(fill = mg), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("irri_gov" = "aquamarine2","drip" = "deepskyblue",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+
  theme_minimal() 


# irrigation group_by( id )
irrigation22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% ungroup() %>%   
  select(id,hh_6methods ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"not_sampled",hh_6methods ) ) %>% 
  select(id ,irrigation)

# irrigation group_by(hh_id )
irrigation.HH.22 <- 
  a_irri_rain_method %>% left_join(shp_index22) %>% group_by(hh_id )  %>%
  mutate(hh_6methods=ifelse("drip" %in% irri_method , "drip", ifelse(any(irri_method  == "furrows"), "furrows",ifelse(any(irri_method  == "flood"), "flood",ifelse(any(irri_method  == "sprinkler"), "sprinkler",ifelse(any(irri_method  == "hose"), "hose","rain"))))) ) %>% 
  ungroup() %>% select(hh_id,hh_6methods ) %>% distinct() %>% right_join(hh_2022) %>% 
  mutate(irrigation=ifelse(is.na(hh_6methods),"rain",hh_6methods ) ) %>% 
  select(hh_id ,irrigation)




# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80")+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
      "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
      "rain"="gray70" ,"not_sampled" = "white"))+theme(legend.position = "none")+
  theme_minimal() 

# map irrigation only
# map irrigation or rain
shapfile_rmtl_2022 %>% 
  left_join(irrigation22) %>% 
  ggplot()+ geom_sf(aes(fill = irrigation), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("drip" = "deepskyblue4",
                               "furrows" = "deepskyblue","flood" = "deepskyblue","sprinkler" = "deepskyblue","hose"="deepskyblue" ,
                               "rain"="gray70" ,"not_sampled" = "white"))+theme_minimal() 

# location_on_pipe ----

location_on_pipe <-
  rmtl_srvy22 %>% select(hh_id,mm9,mm10,farmers_hh) %>% mutate(mm10=as.factor(mm10)) %>% 
  mutate(on_pipe1 = case_when(mm9>=0 & mm9<6~"0-5",mm9>=6 & mm9<51~"5+",TRUE~"NA")) %>% 
  left_join(shp_index22) %>% group_by(id )  %>%
  mutate(on_pipe2=ifelse("0-5" %in% on_pipe1 , "0-5",
                        ifelse(any(on_pipe1  == "5+"), "5+",
                               "no")))  %>% ungroup() %>%   
  select(id,on_pipe2 ) %>% distinct() %>% right_join(shp_rmtl_id) %>% 
  mutate(on_pipe=ifelse(is.na(on_pipe2),"not_sampled",on_pipe2 ) ) %>% 
  select(id ,on_pipe)



# mm9
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>% 
  ggplot()+ geom_sf(aes(fill = on_pipe), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("0-5" = "maroon","5+" = "maroon4" ,"no" ="gray70" ,"not_sampled" = "white"))+
  theme_minimal()

#mm10
shapfile_rmtl_2022 %>% left_join(location_on_pipe) %>%
  ggplot()+ geom_sf(aes(fill = mm10), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()



# irrigation source 2022  -----

# rank1
source_rank_one <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank1= # former rank_1
           ifelse(5 %in% l7_rank_1 , "5", 
           ifelse(any(l7_rank_1  == 2), "2",
           ifelse(any(l7_rank_1  == 3), "3",
           ifelse(any(l7_rank_1  == 4), "4",
           ifelse(any(l7_rank_1  == 7), "7",
                            "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank1 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank1=ifelse(is.na(source_rank1),"not_sampled",source_rank1)) %>% 
  select(id ,source_rank1) %>%  #irrigation_source
  mutate(source_rank1_1yes0no =
           ifelse(source_rank1==0,0,
           ifelse(source_rank1=="not_sampled",
                                     "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_one) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank1), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()


# rank2
source_rank_two <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_rank2=  # source_rank1
           ifelse(5 %in% l7_rank_2, "5", 
                  ifelse(any(l7_rank_2== 2), "2",
                         ifelse(any(l7_rank_2== 3), "3",
                                ifelse(any(l7_rank_2== 4), "4",
                                       ifelse(any(l7_rank_2== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_rank2 ) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_rank2=ifelse(is.na(source_rank2),"not_sampled",source_rank2)) %>% 
  select(id ,source_rank2) %>%
  mutate(source_rank2_1yes0no =
           ifelse(source_rank2==0,0,
                  ifelse(source_rank2=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join(source_rank_two ) %>% 
  ggplot()+ geom_sf(aes(fill = source_rank2), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()



# rank1+rank2+rank3 GOV SUPPLY
source_gov_supply <-
  a_source_irri %>% left_join(shp_index22) %>% 
  group_by(id) %>% 
  mutate(source_Gov=  # source_rank1
           ifelse(5 %in% irri_source_num, "5", 
                  ifelse(any(irri_source_num== 2), "2",
                         ifelse(any(irri_source_num== 3), "3",
                                ifelse(any(irri_source_num== 4), "4",
                                       ifelse(any(irri_source_num== 7), "7",
                                              "0"))))) ) %>% ungroup() %>% 
  select(id,source_Gov) %>% distinct() %>% 
  right_join(shp_rmtl_id) %>% 
  mutate(source_Gov=ifelse(is.na(source_Gov),"not_sampled",source_Gov)) %>% 
  select(id ,source_Gov) %>%
  mutate(source_Gov_1yes0no =
           ifelse(source_Gov==0,0,
                  ifelse(source_Gov=="not_sampled",
                         "not_sampled", 1)))

shapfile_rmtl_2022 %>% left_join( source_gov_supply ) %>% 
  ggplot()+ geom_sf(aes(fill = source_Gov), color="gray80",show.legend = FALSE)+
  scale_fill_manual(values = c("5" = "aquamarine4",
                               "2"= "aquamarine2", "3"= "aquamarine2", "4"= "aquamarine2", "7"="aquamarine2" ,
                               "0"="gray70" ,"not_sampled" = "white"))+theme_minimal()







# first make use ----
first_yr <- rmtl_srvy22 %>% select(hh_id,contains("mw1a")) %>% mutate(first_yr_use=ifelse(is.na(mw1a),2000,mw1a))%>% mutate(mw1a=as.integer(mw1a) )
  

shapfile_rmtl_2022 %>% left_join(first_yr) %>%
  ggplot()+ geom_sf(aes(fill = first_yr_use), color="gray40") +
  scale_fill_distiller(palette = "RdPu") + theme_bw()


cor.test(irrigation.HH.22$irri01,first_yr$mw1a) # 👎🏽






# cropping pattern ----

a_plots_crop %>% filter(season == "kha") %>% count(crop_common)
# Chillies Greengram Toor Maize Onions   Sunflower Oilseeds Pearl.millet_bajra                       

a_plots_crop %>% filter(season == "rabi") %>% count(crop_common)
# Bengal_gram Oilseeds Sorghum_jowar Sugarcane Wheat                         

#| OUT: Cereals Groundnut            ;Horticulture   



map_crop %>% filter(season == "kha") %>% count(crop_common)
map_crop %>% filter(season == "rabi") %>% count(crop_common)

map_crop <- 
  a_plots_crop %>% left_join(shp_index22) %>% 
  filter(season !="KHA22", plot_crop =="01_1"  ) %>% 
  select(season ,id,plot_crop,crop_common) %>% distinct() %>% 
  group_by(id, season) %>% slice(1)

map_kha_crop <- map_crop %>% filter(season =="kha")
map_rabi_crop <- map_crop %>% filter(season =="rabi")


shapfile_rmtl_2022 %>% left_join(map_kha_crop ) %>% 
  ggplot()+ 
  geom_sf(aes(fill = crop_common  ), color="gray40",show.legend = FALSE)+
  theme_minimal() +
  scico::scale_fill_scico(palette = "bilbao")

  
# https://ggplot2-book.org/scales-colour ----

erupt =
  shapfile_rmtl_2022 %>% 
  ggplot()+ 
  geom_sf(aes(fill = fid), color="gray40")+
  theme_minimal() +
  theme(legend.position = "none")

library(viridis)
erupt + scale_fill_viridis_c()
erupt + scale_fill_viridis_c(option = "H") #options A-H

library(RColorBrewer)
display.brewer.all()

erupt + scale_fill_distiller()
erupt + scale_fill_distiller(palette = "RdPu")
erupt + scale_fill_distiller(palette = "YlOrBr")


library(scico)
erupt + scico::scale_fill_scico(palette = "bilbao") # the default
erupt + scico::scale_fill_scico(palette = "vik")
erupt + scico::scale_fill_scico(palette = "lajolla")





# terror_11012024 ----
library(readr)
dt_peace <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/dt_peace.csv", header=T)
View(dt_peace)


dt_peace2=dt_peace %>% filter(survey_year >2013, survey_year<2015)


dt_terror <- read.csv("C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/dt_terror.csv", header=T)
dt_terror2=dt_terror %>% filter(date  < "2014-08-01" )


panel_jews_6_94_till_4_17_273_panels <- read.csv(
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_df_16102023/panel_jews_6_94_till_4_17_273_panels.csv", header=T)

terror_11012024 <- read.csv(
  "C:/Users/Dan/OneDrive - mail.tau.ac.il/terror_and_peace/terror_11012024.csv", header=T)

