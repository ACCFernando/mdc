# install.packages("ggplot2")
library(ggplot2)

# print

?pressure
cor(pressure)

pdf("grafico.pdf")
p <- ggplot(pressure, aes(x = temperature, y = pressure))
p <- p + geom_point()
p <- p + geom_line()
print(p)
dev.off()

# ggsave

p <- ggplot(pressure, aes(x = temperature, y = pressure))
p <- p + geom_point()
ggsave("grafico.png", width = 6, height = 4)
p + geom_line()
ggsave("grafico.jpg", units = "cm", width = 20, height = 20)

# layers

p <- ggplot(pressure, aes(x = temperature, y = pressure))
p$layers; p
p <- p + geom_point() + geom_line()
p$layers; p
p$layers[1] <- NULL
p$layers; p

# ggplot

a <- -10:10
f1 <- a + 5
f2 <- (a^2)/10 - a - 5
df <- data.frame(a, f1, f2)

head(df)

p <- ggplot(df, aes(x = a)); p
p <- p + geom_point(aes(y = f1, colour = "f1")); p
p <- p + geom_line(aes(y = f2, colour = "f2")); p
p <- p + labs(x = "x", y = "y", colour = "Legenda:", title = "Meu Teste"); p

(p <- p + theme_classic())
(p <- p + theme_linedraw())
(p <- p + theme_light())
(p <- p + theme_gray())
(p <- p + theme_dark())
(p <- p + theme_void())
(p <- p + theme_minimal())

p <- p + theme(plot.title = element_text(hjust = 0.5)); p
p <- p + theme(axis.text = element_text(color = "blue")); p
p <- p + theme(legend.background = element_rect(linetype = "solid")); p
p <- p + theme(legend.position = "bottom"); p
p <- p + theme(legend.position = c(0.85, 0.5)); p

p <- ggplot(airquality[airquality$Month == 9, ], 
            aes(x = Day))
p <- p + geom_point(aes(y = Wind, colour = "Vento (MPH)"))
p <- p + geom_line(aes(y = Temp, colour = "Temperatura (F)"))
p <- p + labs(x = "Dia", y = "Valor", 
              colour = "Legenda:", 
              title = "Setembro de 1973")
p <- p + theme_minimal()
p <- p + theme(legend.position = "bottom"); p

# geom_point + geom_smooth

ggplot(faithful, aes(x = waiting, y = eruptions)) + 
  geom_point()

?faithful

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() + geom_smooth()

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x)

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x))

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point(color = "DarkGreen") +
  geom_smooth(color = "Red")

ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point(color = rgb(100, 200, 150, maxColorValue = 255)) +
  geom_smooth(color = rgb(0.5, 0.1, 0.4), se = FALSE)

length(colors())
sample(colors(), 12)
rgb
col2rgb(c("orchid3", "sienna4", "seashell1"))

ggplot(mtcars, aes(x = qsec, y = mpg)) +
  geom_point()

?mtcars

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$liters <- mtcars$disp*(2.54^3)/1000

ggplot(mtcars, aes(x = qsec, y = mpg,
                   size = liters, 
                   colour = cyl)) +
  geom_point(alpha = 0.5)

ggplot(mtcars, aes(x = qsec, y = mpg,
                   size = liters, 
                   colour = hp)) +
  geom_point(alpha = 0.5) +
  scale_color_continuous(low = "blue", high = "red")

ggplot(airquality, aes(x = Wind, y = Month)) +
  geom_point(aes(colour = Temp), 
             alpha = 0.5) +
  scale_color_continuous(low = "blue", high = "red")

?airquality

# geom_line

ggplot(economics, aes(x = date, y = pop)) + 
  geom_line()

ggplot(economics, aes(x = date, y = unemploy)) + 
  geom_line()

ggplot(economics, aes(x = date, 
                      y = unemploy / pop)) + 
  geom_line()

ggplot(Orange, aes(x = age, 
                   y = circumference)) + 
  geom_line(aes(color = Tree))

ggplot(Orange, aes(x = age, 
                   y = circumference)) + 
  geom_line(aes(linetype = Tree))

ggplot(Orange, aes(x = age, 
                   y = circumference)) + 
  geom_line(aes(color = Tree,
                linetype = Tree))

# geom_boxplot & geom_violin

ggplot(airquality, aes(x = Month, y = Temp, group = Month)) +
  geom_boxplot() 

ggplot(airquality, aes(x = Month, y = Temp, group = Month)) +
  stat_boxplot(geom ="errorbar", width = 0.25) + 
  geom_boxplot() 

ggplot(airquality, aes(x = Month, y = Temp, group = Month)) +
  geom_violin()

ggplot(airquality, aes(x = Month, y = Temp, 
                       group = Month, fill = Month)) +
  geom_violin()

ny <- airquality
ny$Month <- factor(month.abb[ny$Month],
                   levels = month.abb,
                   ordered = TRUE)

ggplot(ny, aes(x = Month, y = Temp, 
               group = Month, fill = Month)) +
  geom_boxplot() + scale_fill_brewer(palette = "Pastel1")

ny$Windy <- ifelse(ny$Wind > 10, "Windy", "Not Windy")

ggplot(ny, aes(x = Month, y = Temp, group = Month, fill = Month)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ Windy)

ggplot(ny, aes(x = Month, y = Temp, group = Month, fill = Month)) +
  geom_violin() +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ Windy)

ny$Windy <- factor(ifelse(ny$Wind > 8, 
                          ifelse(ny$Wind > 12, 
                                "Super Windy",
                                "Windy"),
                          "Not Windy"),
                   levels = c("Not Windy", 
                              "Windy",
                              "Super Windy"),
                   ordered = TRUE)

ggplot(ny, aes(x = Month, y = Temp, group = Month, fill = Month)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ Windy)

ggplot(ny, aes(x = Month, y = Temp, group = Month, fill = Month)) +
  geom_violin() +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ Windy)

# geom_histogram

ggplot(ny, aes(x = Temp)) +
  geom_histogram(color = "White")
ggplot(ny, aes(x = Temp)) +
  geom_histogram(color = "White", bins = 10)
ggplot(ny, aes(x = Temp)) +
  geom_histogram(color = "White", binwidth = 5, boundary = 0)
ggplot(ny, aes(x = Temp, fill = Windy)) +
  geom_histogram(color = "White", binwidth = 5, boundary = 0)

# geom_density

ggplot(ny, aes(x = Temp)) +
  geom_density()
ggplot(ny, aes(x = Temp, colour = Windy)) +
  geom_density()
ggplot(ny, aes(x = Temp, colour = Windy, fill = Windy)) +
  geom_density(alpha = 0.25)

# geom_histogram + geom_density

ggplot(ny, aes(x = Temp, y = ..density..)) +
  geom_histogram(color = "White", binwidth = 5, boundary = 0, alpha = 0.5) +
  geom_density()

# geom_bar

tdf <- as.data.frame(Titanic)

ggplot(tdf, aes(x = Class, weight = Freq)) +
  geom_bar()
?Titanic
ggplot(tdf, aes(x = Class, weight = Freq, fill = Sex)) +
  geom_bar()
ggplot(tdf, aes(x = Class, weight = Freq, fill = Sex)) +
  geom_bar(position = position_dodge())
ggplot(tdf, aes(x = Class, weight = Freq, fill = Sex)) +
  geom_bar(position = position_fill())

# pie chart

pie <- ggplot(tdf[tdf$Sex == "Male", ],
              aes(x = "Male", weight = Freq,
                  fill = Class)) +
  geom_bar(); pie

pie <- pie + coord_polar(theta = "y"); pie
pie <- pie + theme(axis.text = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks = element_blank(),
                   panel.border = element_blank(),
                   panel.grid = element_blank()); pie

# geom_line (extra)

air <- AirPassengers
Year <- as.numeric(time(AirPassengers))
Passengers <- as.matrix(AirPassengers)
air <- data.frame(Year, Passengers)

airG <- ggplot(air, aes(x = Year, 
                        y = Passengers)) +
  scale_x_continuous(breaks = 
                     unique(ceiling(air$Year))) +
  geom_line(); airG

?AirPassengers

# aggregate

Year <- as.numeric(time(AirPassengers))
Year <- trunc(Year)
Passengers <- as.matrix(AirPassengers)
air <- data.frame(Year, Passengers)

airMean <- aggregate(air$Passengers, list(air$Year), mean)
colnames(airMean) <- c("Year", "Passengers")

ggplot(airMean, aes(x = Year, y = Passengers)) +
  geom_line() +
  scale_x_continuous(breaks = airMean$Year)

airG + geom_line(data = airMean, colour = "red",
                 aes(x = Year + 0.5)) +
  geom_point(data = airMean, colour = "red",
             aes(x = Year + 0.5))

# aggregate (extra)

ny <- airquality
ny$Month <- factor(month.abb[ny$Month],
                   levels = month.abb,
                   ordered = TRUE)

ny$Windy <- ifelse(ny$Wind > 10, "Windy", "Not Windy")

nyMedian <- aggregate(ny[c("Ozone", "Solar.R", "Temp")],
                      list(ny$Month, ny$Windy),
                      median, na.rm = TRUE)
colnames(nyMedian) <- c("Month", "Windy", "Ozone", "Solar.R", "Temp")

nyMedian

ggplot(nyMedian, aes(x = Month, y = Temp,
                     group = Windy,
                     colour = Windy)) +
  geom_line() + geom_point() +
  ylab("Median Temperature")

ggplot(nyMedian, aes(x = Month, y = Temp,
                     group = Windy,
                     colour = Windy)) +
  geom_line() + geom_point() +
  ylab("Median Temperature") +
  facet_wrap(~ Windy) +
  theme(legend.position = "none")
