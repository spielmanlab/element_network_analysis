
# Obtain model formula and R^2 as HTML for plot subtitle
get_model_info <- function(fitted_model, x_term)
{
  broom::tidy(fitted_model) -> coefficients
  # Get coefficients into their own variables
  coefficients %>%
    filter(term == x_term) %>%
    pull(estimate) -> slope
  coefficients %>%
    filter(term == "(Intercept)") %>%
    pull(estimate) -> yint
  # Are we subtracting or adding the y intercept?
  if (yint >= 0)
  {
    sign <- " + "
  } else {
    sign <- " - "
  }
  
  # Get the formula as a string
  formula <- glue::glue(
    "Y = ",
    {round(slope, 3)}, "X", 
    {sign}, {abs(round(yint, 3))}
  )
  
  # Get the R^2
  broom::glance(fitted_model) %>%
    pull(r.squared) %>%
    round(3) -> r2
  
  list(
    formula =  glue::glue(
      "{formula} (*R<sup>2</sup> = {r2}*)"
      ),
    pvalue = broom::glance(fitted_model) %>% pull(p.value)
  )
}



# Make a scatterplot
make_plot <- function(df, xvar, yvar, model_formula, xlab, ylab) {
  df %>% 
    ggplot() +
    aes(x = {{xvar}},
        y = {{yvar}}) +
    geom_point() +
    labs(x = xlab,
         y = ylab, 
         subtitle = model_formula) +
    geom_smooth(method = "lm", color = "grey20") 
}

# function to specifically add element labels to a plot with ggrepel
add_element_labels <- function(plot, label.size = 4.5, repel_seed = 11, ...) {
  plot +
    geom_text_repel(
      aes(
        label = element,
        color = label_color
      ),
      size = label.size,
      ...,
      fontface = "bold",
      min.segment.length = 0.0001,
      seed = repel_seed
    ) +
    scale_color_identity()
}



# Function to build a network for a given element, considering only < 4.34 Ga
parse_element_network <- function(element)
{
  # Build the full network. Returns a LIST with various important stuff.
  dragon::initialize_network(elements_of_interest = element, 
                             # age_range is inclusive and we don't want =4.34, so this is hopefully good enough.
                             age_range = c(4.33, 0)) -> full_network
  
  # The important stuff is in `$edges` and `$locality_info` names
  edges <- full_network$edges 
  locality <- full_network$locality_info 
  
  # How many elements does element form minerals with?
  edges %>%
    dplyr::select(to) %>%
    dplyr::distinct() %>%
    dplyr::filter(to != element) %>% # not self
    nrow() -> n_elements
  
  # How many minerals does element form?
  edges %>%
    dplyr::select(from) %>%
    dplyr::distinct() %>%
    nrow() -> n_minerals
  
  # At how many localities? 
  locality %>%
    # mindat_id column represents unique ID for localities
    dplyr::select(mindat_id) %>%
    dplyr::distinct() %>%
    nrow() -> n_localities
  
  # whatever is written last in the function body gets returned
  c("element" = element,
    "n_elements" = n_elements,
    "n_minerals" = n_minerals,
    "n_localities" = n_localities)
}

