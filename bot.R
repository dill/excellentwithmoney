# #botALLY
library(ggplot2)
library(stringr)
library(ggrepel)
set.seed(1)
# plotto
make_pie <- function(df, start){
  bp <- ggplot(df, aes(x="", y=value, fill=blarg)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=pi/2) +
    scale_fill_manual(values=col) +
    geom_text_repel(aes(label = str_wrap(paste0(group,": ","$",value), 12),
                  x=1.7),vjust=0,
              position = position_stack(vjust = 0.5)) +
    theme_void() +
    theme(legend.position="none",
          plot.title=element_text(hjust=0.5, size=rel(3)))+
    ggtitle(paste0("Budget = $", attr(df, "total")))
  bp
}


dat <- lapply(list.files("data", full.names=TRUE),
                   function(x) unname(unlist(read_json(x)))[-1])

thing_names <- function(n=10, data_boi){

  thingos <- c(sample(data_boi[[1]], 5),
               sample(data_boi[[2]], 2),
               paste(sample(data_boi[[3]], 1), "the", sample(data_boi[[4]], 1)),
               sample(data_boi[[5]], 1),
               sample(data_boi[[6]], 1))
  sample(thingos, 10)
}
thing_names(10, dat)


# prepare the palette
col <- list()
col[[7]] <- c(255, 206, 0)
col[[6]] <- c(117, 41, 254)
col[[5]] <- c(186, 74, 233)
col[[4]] <- c(109, 238, 83)
col[[3]] <- c(0, 97, 163)
col[[2]] <- c(247, 255, 0)
col[[1]] <- c(0, 189, 255)
col[[10]] <- c(255, 77, 79)
col[[9]] <- c(247, 255, 0)
col[[8]] <- c(0, 186, 255)
col <- unlist(lapply(col, function(x) do.call("rgb", as.list(x/255))))


# generate some data
gen_data <- function(){

  budget <- rnorm(1, 3000, 1000)

  # this is a very bad way of doing this
  props <- c(abs(rnorm(3, 3, 1)),
             abs(rnorm(4, 10, 2)),
             abs(rnorm(1, 20, 3)),
             abs(rnorm(1, 27, 10)))
  # ... therefore this sometimes goes wrong
  props <- c(props, 100-sum(props))
  props <- props/sum(props)

  cash_money <- props*budget

  df <- data.frame(group  = thing_names(10, dat),
                   value  = round(cash_money),
                   colour = col)

  df <- df[order(df$value), ]
  df$blarg <- factor(1:nrow(df))
  attr(df, "total") <- round(budget)
  df
}

df <- gen_data()
p <- make_pie(df)

print(p)


