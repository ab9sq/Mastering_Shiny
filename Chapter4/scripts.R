download <- function(name){
     url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
     download.file(paste0(url,name),
                   paste0("neiss/",name),
                   quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries
products <- vroom::vroom("neiss/products.tsv")
products
population <- vroom::vroom("neiss/population.tsv")
population


selected <- injuries %>%
     filter(prod_code == 649)
nrow(selected)

selected %>%
     count(location, wt = weight, sort = TRUE)
selected %>% 
     count(body_part, wt = weight, sort = TRUE)
selected %>%
     count(diag, wt = weight, sort = TRUE)
summary <- selected %>% 
     count(age, sex, wt = weight)
summary

summary %>%
     ggplot(aes(age, n, color = sex)) +
     geom_line() +
     labs(y = "Estimated number of injuries")

summary <- selected %>%
     count(age, sex, wt = weight) %>%
     left_join(population, by = c("age", "sex")) %>%
     mutate(rate = n / population *1e4)
summary

summary %>%
     ggplot(aes(age, rate, color = sex)) +
     geom_line(na.rm = TRUE) + 
     labs(y = "Injuries per 10,000 people")
selected %>%
     sample_n(10) %>%
     pull(narrative)

injuries%>%
     mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
     group_by(diag) %>%
     summarise(n = as.integer(sum(weight)))
count_top <- function(df, var, n = 5) {
     df %>%
          mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
          group_by({{ var }}) %>%
          summarise(n = as.integer(sum(weight)))
}




