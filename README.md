# Energyplus Output Parser

![Energyplus](https://energyplus.net/sites/all/themes/eplus_bootstrap/images/energyplus.jpg)

Python script developed to process output data from Energyplus application.

## Usage:

```
usage: python main.py [-h] [-t T]

Process output data from Energyplus.

optional arguments:
  -h, --help  show this help message and exit
  -t T        runs T threads
```

This python script crawls all folders beginning with an `_` and creates a `data_<folder>.csv` output file for each folder with some data interpretation.
