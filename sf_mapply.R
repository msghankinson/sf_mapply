library(sf)
library(dplyr)

setwd("~/Documents/GitHub/sf_mapply")

# load data
circles <- st_read("circles.shp")
blocks <- st_read("ca_blocks.shp") # only a sample, because file is massive
points <- st_read("points.shp")

# create function
county_func <- function(x, y) { # inputs are circles and points
  y_id <- y %>% # keep points only as an index
    st_drop_geometry()
  x_int <- st_intersects(x, blocks) # intersect circles with blocks to build index of overlap
  ints_holder <- data.frame() 
  for(i in 1:nrow(x_int)){ # for each circle
    blocks_int <- subset(blocks, as.numeric(rownames(blocks)) %in% x_int[[i]]) # subset blocks that intersect
    blocks_int$hud_id <- y_id[i, 1] # add point id to these blocks for merge later
    ints_holder <- rbind(ints_holder, blocks_int)
  }
  x_blocks <- st_intersection(x, ints_holder) %>% # intersection between circles and blocks they intersect
                mutate(intersect_area = st_area(.)) %>% # calculate intersect area
                dplyr::select(GEOID10, intersect_area) # drop irrelevant data
  return(x_blocks)
}

# run commands removed from function on single county (San Francisco County, FIPS 075)
circles_75 <- subset(circles, cnty2010 == 75)
points_75 <- subset(points, cnty2010 == 75)

y_id <- points_75 %>%
    st_drop_geometry()
x_int <- st_intersects(circles_75, blocks)
ints_holder <- data.frame()
for(i in 1:nrow(x_int)){
  blocks_int <- subset(blocks, as.numeric(rownames(blocks)) %in% x_int[[i]])
  blocks_int$hud_id <- y_id[i, 1]
  ints_holder <- rbind(ints_holder, blocks_int)
}
x_blocks <- st_intersection(circles_75, ints_holder) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(GEOID10, intersect_area) 

plot(x_blocks) # works

### test function on single county structured as part of list
# split circles into list by their county
test <- circles %>%
  filter(!is.na(cnty2010)) %>%
  group_by(cnty2010)
test_df <- group_keys(test) %>%
  dplyr::mutate(group_name = stringr::str_c("county_", cnty2010))
group_name <- test_df$group_name
circles_list <- group_split(test) %>%
  setNames(group_name)

county_test <- county_func(circles_list$county_75, points_75)

plot(county_test) # works

mapply(county_func, x = circles_list$county_75, y = points_75) # fails

# Error in UseMethod("st_drop_geometry") : 
#   no applicable method for 'st_drop_geometry' applied to an object of class "character"

### keep only numeric data
test <- circles %>%
  filter(!is.na(cnty2010)) %>%
  group_by(cnty2010)
test_df <- group_keys(test) %>%
  dplyr::mutate(group_name = stringr::str_c("county_", cnty2010))
test <- test %>%
  mutate(hud_id_num = as.numeric(substring(hud_id, 4, 11))) %>%
  dplyr::select(hud_id_num, cnty2010, geometry)
group_name <- test_df$group_name
circles_list_num <- group_split(test) %>%
  setNames(group_name)

points_75_num <- points_75 %>%
  dplyr::select(geometry)

# test mapply
mapply(county_func, x = circles_list_num$county_75, y = points_75_num)

# Error in UseMethod("st_drop_geometry") : 
#   no applicable method for 'st_drop_geometry' applied to an object of class "c('sfc_POINT', 'sfc')"
class(points_75)
sessionInfo()
sf::sf_extSoftVersion()


