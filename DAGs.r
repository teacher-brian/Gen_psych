library(dagitty)
library(tidyverse)
library(ggdag)
library(tidygraph)
library(ggraph)

#
#
# Introduce the concept of causality: Begin by explaining the difference between correlation and causation, and how DAGs can help us to understand causal relationships between variables.
#
# Define DAGs: Define what a DAG is and how it can be used to represent causal relationships between variables. You can use examples such as a DAG showing the relationship between smoking and lung cancer, or the relationship between exercise and heart disease.
#
#
#
#
#
# Basic structure of DAGs: Teach the basic structure of a DAG, including nodes (representing variables) and edges (representing causal relationships).
#
#
#
#
# Explain the importance of directionality: Highlight the importance of directionality in a DAG, and how it represents the direction of causation.
#
#
#
#
# Practice drawing DAGs: Provide opportunities for students to practice drawing DAGs, using examples from their own lives or from current events.
#
# Discuss common misconceptions: Discuss common misconceptions about DAGs, such as assuming that all relationships are causal or assuming that all causal relationships are direct.
#
# Analyze DAGs: Once students have a basic understanding of DAGs, you can move on to analyzing them. You can use R code to illustrate how to analyze DAGs, such as using the tidyverse package to create a DAG and ggdag to visualize it.




#DAGs in psychology

#
# The effect of diet and exercise on weight: This DAG could include variables like diet, exercise, genetics, and weight. The arrows would go from diet and exercise to weight, since these are believed to directly impact weight, and from genetics to weight, since genetics can indirectly affect weight through their influence on metabolism.
#
# The impact of social media on mental health: This DAG could include variables like social media use, self-esteem, depression, and anxiety. The arrows would likely go from social media use to self-esteem, since social media has been linked to lower self-esteem in some studies, and from self-esteem to depression and anxiety, since low self-esteem is associated with these conditions.
#
# The relationship between education and income: This DAG could include variables like education level, job skills, work experience, and income. The arrows would likely go from education level to job skills and work experience, since education is believed to directly impact these variables, and from job skills and work experience to income, since these are believed to directly impact income.



# simple dagitty

# Create a new DAG object
depression <- dagitty('dag{
  Genetics -> DepressionSymptoms
  LifeStressors -> DepressionSymptoms
  SocialSupport -> DepressionSymptoms
  }')

# Manually set the node coordinates
coordinates(depression) <- list(
  Genetics = c(0, 1),
  LifeStressors = c(0, 3),
  SocialSupport = c(1, 5),
  DepressionSymptoms = c(5, 3)
)

# View the DAG
plot(depression)



# using tidyverse and ggdag

dagified <- dagify(DepressionSymptoms ~ Genetics,
                   DepressionSymptoms ~ LifeStressors,
                   DepressionSymptoms ~ SocialSupport,
                   exposure = c("LifeStressors",
                                "SocialSupport",
                                "Genetics"),
                   outcome = "DepressionSymptoms",
                   coords = list(
                     x = c(DepressionSymptoms = 5,
                           Genetics = 1,
                           LifeStressors = 1.5,
                           SocialSupport = 2.5),
                     y = c(DepressionSymptoms = 5,
                           Genetics = 1,
                           LifeStressors = 3,
                           SocialSupport=4.2)
                   ))



dagified %>% tidy_dagitty() %>%
  ggplot(aes(x=x,y=y,xend = xend,
             yend = yend))+
  geom_dag_point(color='lightgreen') +
  geom_dag_edges() +
  geom_dag_text(color='blue',nudge_x = .5) +
  theme_dag()

ggdag( layout = "sugiyama") +
  #  theme_dag() +
  #  scale_size_continuous(range = c(10, 30)) +
  #geom_node_text(aes(label = name), size = 4, hjust = 1, vjust = 1) +
  #  coord_flip() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )





