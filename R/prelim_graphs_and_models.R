## Preliminary modeling

ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point(aes(color = State)) +
  ylab("Efficiency Gap") +
  ggtitle("Scatterplot of state house efficiency gap over time")


ggplot(egs, aes(x=Year, y=Efficiency_gap)) +
  geom_point() +
  facet_wrap(~State)


ggplot(egs, aes(x=Efficiency_gap, y=Efficiency_gap_contested)) +
  geom_point(aes(color=State))

