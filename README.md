# cryptoData-package-r

This package was created to help import and visualize cryptocurrency data (like Bitcoin) in RStudio.

Using functions within the package, one can type in the cryptocurrency they want to analyze and pair it with their local currency. Then import the data into a dataframe for ease if use.

Using other functions in the package, one can plot the cryptocurrency on a line graph, plot the volume with a time scale, and obtain the 52-week highs and lows.



Below is all the functions and their uses:


pair <- format_pair("btc", "usd")

bitcoin <- get_coin_data(pair)

plot_coin_data(bitcoin)

plot_coin_volume_data(bitcoin)

low_52_week(bitcoin)

high_52_week(bitcoin)
