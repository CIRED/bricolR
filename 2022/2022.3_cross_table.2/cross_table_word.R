### inspiré de la page ::
# https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable-report.html


library(officer)

### -----
ct1 = crosstable(iris, by = Species, test = TRUE)
ct2 = crosstable(mtcars2,
                 c(mpg,cyl,disp),
                 by = am,
                 effect = TRUE, 
                 total = "both",
                 showNA = "always")
ct3 = crosstable(esoph)

### options globables de crosstable
crosstable_options(
  crosstable_fontsize_body = 8,
  crosstable_padding_v = 0,
  crosstable_units = "cm"
)

my_plot = ggplot(data = iris ) +
  geom_point(mapping = aes(Sepal.Length, Petal.Length))

doc = read_docx() %>% #default template
  body_add_title("Dataset iris (nrow={nrow(iris)})", 1) %>%
  body_add_title("Not compacted", 2) %>%
  body_add_normal("Table \\@ref(table_autotest) is an example. However, automatic testing is bad and I should feel bad.") %>%
  body_add_crosstable(ct1) %>%
  body_add_table_legend("Automatic testing is bad", bookmark="table_autotest") %>%
  body_add_normal("Let's add a figure as well. You can see in Figure \\@ref(fig_iris) 
                  that sepal length is somehow correlated with petal length.") %>%
  body_add_figure_legend("Relation between Petal length and Sepal length", 
                         bookmark = "fig_iris") %>% 
  body_add_gg2(my_plot, w = 14, h = 10, scale = 1.5) %>% 
  body_add_break() %>%
  
  body_add_title("Compacted", 2) %>%
  body_add_normal("When compacting, you might want to remove the test names.") %>%
  body_add_crosstable(ct1, compact = TRUE, show_test_name = FALSE) %>%
  body_add_break() %>%
  
  body_add_title("Dataset mtcars2", 1) %>%
  body_add_normal("This dataset has {nrow(ct3)} rows and {x} columns.", 
                  x = ncol(ct3)) %>%
  body_add_normal("Look, there are labels!") %>%
  body_add_crosstable(ct2, compact = TRUE)