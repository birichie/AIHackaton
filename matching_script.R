
## Looking for the best matches

## abrev data: https://www.medindia.net/nutrition-data/game-meat-buffalo-water-raw.htm

## NOTE: Import data as agric_tbl,abbrev, and daily_nutrients

## Step 1
## Extract the product names
step_1 <- agric_tbl %>% 
  select(-Area, -Year) %>% 
  colnames() %>% 
  str_to_lower() %>% 
  as_tibble() %>% 
  separate(value, c("product","metric"), sep = "-", extra = "merge") %>% 
  distinct(product)

abbrev_tidy <- abbrev %>% 
  janitor::clean_names() %>% 
  mutate(shrt_desc = str_to_lower(shrt_desc))

## Step 2
## IDs for the 14 products -- only 11 were discovered
products <- c("20038","","17160","13047",
              "05062","17169","10007","17285",
              "01108","","","01106",
              "01109","20047")

## Step 2 result
step_2 <- abbrev_tidy %>% 
  filter(ndb_no %in% products)

# Take the remaining nutritional columns in ABBREV.xlsx
# ( from Water_(g) onwards) and map them to the daily nutritional
# requirements data in the attached file daily_nutrients.xlsx.
# Again this can be done by hand or by code.
# Not everything will have an equivalent.

## Clean nutrients data
daily_nutrients_tidy <- daily_nutrients %>% 
  janitor::clean_names() %>% 
  pivot_wider(names_from = nutrient, values_from = daily_value) %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = everything(), names_to = "nutrient", values_to = "amount") %>% 
  mutate(unit = amount,
         amount = parse_number(amount),
         unit = str_remove_all(unit,"\\d|\\."))

## Rename the columns in the abbrev file
cc <- c("alpha_carot", "ash", "beta_carot", "beta_crypt",
        "calcium", "carbohydrt", "cholestrl", "choline_tot",
        "copper", "energ", "fa_mono", "fa_poly", "fa_sat",
        "fiber_td", "folate_dfe", "folate_tot", "folic_acid",
        "food_folate", "gm_wt_1", "gm_wt_2", "iron", "lipid_tot",
        "lut_zea", "lycopene", "magnesium", "manganese", "niacin",
        "panto_acid", "phosphorus", "potassium", "protein", "refuse",
        "retinol", "riboflavin", "selenium", "sodium", "sugar_tot",
        "thiamin", "vit_a_iu", "vit_a_rae", "vit_b12", "vit_b6", "vit_c",
        "vit_d_iu", "vit_d_mg", "vit_e", "vit_k", "water", "zinc")

## To match with the nutrient name in the tidied nutrients table
cc_new <- c("alpha_carot", "ash", "beta_carot", "beta_crypt",
            "calcium", "total_carbohydrate", "cholesterol", "choline",
            "copper", "energ", "fa_mono", "fa_poly", "saturated_fat",
            "dietary_fiber", "folate_dfe", "folate_tot", "folic_acid",
            "food_folate", "gm_wt_1", "gm_wt_2", "iron", "lipid_tot",
            "lut_zea", "lycopene", "magnesium", "manganese", "niacin",
            "pantothenic_acid", "phosphorus", "potassium", "protein", "refuse",
            "retinol", "riboflavin", "selenium", "sodium", "sugar_tot",
            "thiamin", "vitamin_a", "vit_a_rae", "vitamin_b12", "vitamin_b6", "vitamin_c",
            "vitamin_d", "vit_d_mg", "vitamin_e", "vitamin_k", "water", "zinc")

## Arrange data for matching
clean_names <- step_2 %>% 
  select_if(is.numeric) %>% 
  gather() %>% 
  distinct(key) %>% 
  arrange(key) %>% 
  mutate(new_name = cc,
         nutrient_name = cc_new) 

## Match nutrients
final_tbl <- abbrev_tidy %>% 
  filter(ndb_no %in% products) %>% 
  select(shrt_desc, where(is.numeric)) %>% 
  pivot_longer(cols = 2:last_col()) %>% 
  rename("key" = "name") %>% 
  left_join(clean_names)  %>% 
  left_join(daily_nutrients_tidy,
            by = c("nutrient_name" = "nutrient")) %>% 
  select(-new_name) 

## Properly format data
final_tbl %>% 
  separate(key, c("first","second","third"), sep = "_", remove = F) %>% 
  mutate(
    product_nutrient = ifelse(is.na(third), first, glue::glue("{first}_{second}")),
    product_nutrient_unit = ifelse(is.na(third), second, third)
  ) %>% 
  select(-c(key:third)) %>% 
  select(
    product = shrt_desc,
    product_nutrient,
    product_nutrient_amount = value,
    product_nutrient_unit,
    daily_nutrient = nutrient_name,
    daily_nutrient_amount = amount,
    daily_nutrient_unit = unit
  ) -> final_tbl_2
