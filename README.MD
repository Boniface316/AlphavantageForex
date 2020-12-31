![GitHub repo size](https://img.shields.io/github/repo-size/Boniface316/AlphavantageForex)
![GitHub top language](https://img.shields.io/github/languages/top/Boniface316/AlphavantageForex)
![GitHub last commit](https://img.shields.io/github/last-commit/Boniface316/AlphavantageForex)


# AlphavantageForex

This library allows the user to download FX data from Alphavantage

## Getting Started

This library is built to allow user to quickly download FX data from Alphavantage. You can visit [Alphavantage](http://alphavantage.co) to access documentation.

### Prerequisites

API: Goto https://www.alphavantage.co/support/#api-key to claim your free API.

devtools package



### Installing

Install this package
```
devtools::install_github("Boniface316/AlphavantageForex")
```
## Downloading data

Please refer to ```interactive/download_FX_data.R``` to see intruction on downloading the data.


### Exploring data

Explain what these tests test and why

Exploring the currency frequency
```
> head(EURUSD)
             open   high    low  close
2001-10-31 0.9048 0.9078 0.8983 0.8999
2001-11-01 0.9000 0.9119 0.8990 0.9036
2001-11-02 0.9037 0.9069 0.9004 0.9023
2001-11-05 0.9030 0.9030 0.8940 0.8973
2001-11-06 0.8973 0.8993 0.8953 0.8972
2001-11-07 0.8976 0.9046 0.8935 0.8950
```

Start dates of major currency pairs downloaded
```
> lapply(masterData, head, n = 1)
$EURUSD
             open   high    low  close
2001-10-31 0.9048 0.9078 0.8983 0.8999

$USDJPY
             open   high    low  close
2001-10-31 121.98 122.62 121.61 122.31

$GBPUSD
             open  high  low close
2001-10-31 1.4525 1.457 1.45 1.455

$AUDUSD
             open   high   low  close
2001-10-31 0.5039 0.5064 0.502 0.5034

$USDCHF
             open   high    low  close
2001-10-30 1.6301 1.6307 1.6148 1.6234

```


## Author

* **[Boniface Yogendran](https://github.com/Boniface316)** 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

