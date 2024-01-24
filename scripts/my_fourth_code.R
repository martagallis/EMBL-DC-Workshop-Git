plt <- ggplot(
  data = surveys_complete,
  mapping = aes(x = weight, y = hindfoot_length)
)
plt
str(plt)

plt+
  geom_point()

plt+
  geom_point() +
  ggtitle("My first plot!")

#1. define ggplot object
# plt <- ggplot(data = <data.frame>, mapping = <aestethics>)
# x aestetics
# y aestetics
# color aestetics
# shape aestetics
#...
#2. add geometry layer(s)
# geometry functions have predictable names
# geom_{point, line, bar, histogram, violin, hex, ...}
#
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

plt +
  ggtitle("Weight vs Hindfoot Length")

install.packages("hexbin")
library(hexbin)

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1)

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, colour = "blue") 

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.25, aes(colour = species_id))

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = weight,
    y = hindfoot_length,
    colour = species_id
  )
) +
  geom_point(alpha = 0.25)

# Challenge: scatterplot weight vs species_id, color by plot_type

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
    colour = plot_type
  )
) +
  geom_point()

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_boxplot()

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, colour = "salmon") #adding a little value for each x coord



ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_boxplot(outlier.shape = NA) + #excluding dots
  geom_jitter(alpha = 0.3, colour = "salmon")


ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_jitter(alpha = 0.3, colour = "salmon") +
  geom_boxplot(outlier.shape = NA, fill = NA)

# Challenge: produce a violin plot of the weight by species_id

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_violin()

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_violin() +
  scale_y_log10()

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = weight,
  )
) +
  geom_violin() +
  scale_y_log10() +
  ylab ("Weight (log10)")

# Challenge: make a boxplot + jittered scatterplot of 
#hindfoot_length by species_id. Boxplot in front of the dots
#and filled with white

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length,
  )
) +
  geom_jitter(alpha = 0.3, colour = "firebrick") +
  geom_boxplot(colour = "white")

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length,
  )
) +
  geom_jitter(alpha = 0.3, aes(colour = plot_id)) +
  geom_boxplot(outlier.shape = NA)  

ggplot(
  data = surveys_complete,
  mapping = aes(
    x = species_id,
    y = hindfoot_length,
  )
) +
  geom_jitter(alpha = 0.3, aes(colour = factor(plot_id))) +
  geom_boxplot(outlier.shape = NA)



yearly_count <- surveys_complete %>% 
  count(year, genus)
View(yearly_count)

ggplot(
  data = yearly_count, 
  mapping = aes(
    x = year, 
    y = n, 
    group = genus)) +
  geom_line()

ggplot(
  data = yearly_count, 
  mapping = aes(
    x = year, 
    y = n, 
    colour = genus)) +
  geom_line()

#Another way of writing same code
yearly_count %>% 
  ggplot(mapping = aes(x= year, y = n, colour = genus)) +
  geom_line()

#Yet another way
surveys_complete %>% 
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year, y = n, colour = genus)) +
  geom_line()



ggplot(
  data = yearly_count,
  mapping = aes(
    x = year,
    y = n
  )
) +
  geom_line() +
  facet_wrap(facets = vars(genus))



surveys_complete %>% 
  count(year, genus, sex) %>%
  ggplot(
  mapping = aes(
    x = year,
    y = n,
    color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))


surveys_complete %>% 
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
  )

surveys_complete %>% 
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    rows = vars(genus),
  )

surveys_complete %>% 
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_grid(
    cols = vars(genus),
  )

plt <- surveys_complete %>% 
  count(year, genus, sex) %>%
  ggplot(
    mapping = aes(
      x = year,
      y = n,
      color = sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  scale_colour_manual(
    values = c("tomato", "dodgerblue"),
    labels = c("female", "male"),
    name = "Sex") +
  xlab("Year of observation") +
  ylab("NUmber of individuals") +
  ggtitle("Observed genera over time") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", #"none"
    aspect.ratio = 0.5,
    axis.text.x.bottom = element_text(
      angle = 45, 
      hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    strip.background = element_blank()
  )
plt
ggsave(filename = "data/plot.pdf",
       plot = plt,
       width = 20,
       height = 20)

 #Annotate function will get you some text within a graph lika a scatter plot for example