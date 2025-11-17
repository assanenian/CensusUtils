# library(readr)
# library(reshape2)
# library(tidyverse)
# library(ggplot2)
# library(lemon)

#' age_sex_structure
#'
#' @param df a dataframe containing count of people by agegroup, sex and distributed across administrative units
#' @param choice specify whether a national pyramid or regional pyramids are created
#' @param output_dir folder where the pyramids will be saved
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
age_sex_structure <- function(df,choice="national",output_dir,...){

  if(!dir.exists(output_dir)){
    dir.create(output_dir, recursive = TRUE)
    message(paste("Directory", output_dir, "created successfully."))
  } else {
    message(paste("Directory", output_dir, "already exists."))
  }

  df_wide <- melt(df, id.vars = c("Name", "id"),
                  variable.name = "agesex",
                  value.name = "p")
  df_wide$sex <- ifelse(grepl("m", df_wide$agesex), "m", "f")
  df_wide$age <- substr(as.character(df_wide$agesex), 3, 4)
  df_wide <- df_wide[order(df_wide$Name,df_wide$age,df_wide$sex),]

  sexage_table <- df_wide %>%
    select(Name, age, sex, p) %>%
    mutate(
      age = factor(age),
      sex = factor(sex)
    )
  x <- sapply(sexage_table, is.factor)
  sexage_table[x] <- lapply(sexage_table[x], as.character)

  sexage_table$age[sexage_table$age == '0'] = '< 1'
  sexage_table$age[sexage_table$age == '1'] = '01 to 04'
  sexage_table$age[sexage_table$age == '5'] = '05 to 09'
  sexage_table$age[sexage_table$age == '10'] = '10 to 14'
  sexage_table$age[sexage_table$age == '15'] = '15 to 19'
  sexage_table$age[sexage_table$age == '20'] = '20 to 24'
  sexage_table$age[sexage_table$age == '25'] = '25 to 29'
  sexage_table$age[sexage_table$age == '30'] = '30 to 34'
  sexage_table$age[sexage_table$age == '35'] = '35 to 39'
  sexage_table$age[sexage_table$age == '40'] = '40 to 44'
  sexage_table$age[sexage_table$age == '45'] = '45 to 49'
  sexage_table$age[sexage_table$age == '50'] = '50 to 54'
  sexage_table$age[sexage_table$age == '55'] = '55 to 59'
  sexage_table$age[sexage_table$age == '60'] = '60 to 64'
  sexage_table$age[sexage_table$age == '65'] = '65 to 69'
  sexage_table$age[sexage_table$age == '70'] = '70 to 74'
  sexage_table$age[sexage_table$age == '75'] = '75 to 79'
  sexage_table$age[sexage_table$age == '80'] = '80 +'
  sexage_table$sex[sexage_table$sex == 'm'] = 'male'
  sexage_table$sex[sexage_table$sex == 'f'] = 'female'

  if(choice=="national"){
    message("Creating national pyramid\n")
    sexage_national <- sexage_table %>%
      select(age, sex, p) %>%
      group_by(age, sex) %>%
      summarise(total=sum(p))

    national_pyramid <- ggplot(data = sexage_national,
                               mapping = aes(x = ifelse(sex == "male", -total, total),
                                             y = age, fill = sex)) +
      geom_col() +
      scale_x_symmetric(labels = abs)+
      labs(x = "Population")
    ggsave(national_pyramid, filename = paste0(output_dir, "National pyramid.png"), dpi = 300)
    print(national_pyramid)


  } else {
    message("Creating regional-level pyramids\n")

    for (i in unique(sexage_table$Name)){
      regional_pyramid <- sexage_table %>%
        filter(Name == i) %>%
        ggplot(mapping = aes(x = ifelse(sex == "male", -p, p),
                             y = age, fill = sex)) +
        geom_col() +
        scale_x_symmetric(labels = abs) +
        labs(x = "Population")
      theme(text = element_text(size = 20))
      print(regional_pyramid)
      ggsave(regional_pyramid, filename = paste0(output_dir, i, ".png"), dpi = 300)
    }



  }

}
