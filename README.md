Tables for the console
----------------------


Generate simple tables from stdin for example

    ┏━━━━━┳━━━━━━┓
    ┃First┃Second┃
    ┣━━━━━╋──────┫
    ┃a    ┃     1┃
    ┃a    ┃     2┃
    ┗━━━━━┻━━━━━━┛


Building
--------

### Dependencies

    opam install core
    opam install terminal_size
    opam install async

### Build

    corebuild tabulate.native


Examples
--------


    $ cat example.tsv | ./tabulate.native

    ┏━━━━━┳━━━━━━┓
    ┃First┃Second┃
    ┣━━━━━╋━━━━━━┫
    ┃b    ┃2     ┃
    ┃a    ┃1     ┃
    ┗━━━━━┻━━━━━━┛


    $ ./tabulate.native example.tsv

    ┏━━━━━┳━━━━━━┓
    ┃First┃Second┃
    ┣━━━━━╋━━━━━━┫
    ┃b    ┃2     ┃
    ┃a    ┃1     ┃
    ┗━━━━━┻━━━━━━┛


    $ ./tabulate.native -help

    Render input data as a table

      tabulate.native [FILENAME]

    Render input data as a table

    === flags ===

      [-buffer-size INT]  The number of lines the program can store at a time
      [-columns LIST]     Comma separated list of column names/ids to dislplay
      [-header LIST]      Column labels for csv/tsv file
      [-hide-header]      Hide the header
      [-no-header-row]    Don't use the first row as the header row, use numeric
                          id's instead unless -header is provided
      [-build-info]       print info about this build and exit
      [-version]          print the version of this build and exit
      [-help]             print this help text and exit
                          (alias: -?)


Expected Parameters
-------------------

 --key=[column,...]
   Comma separated list of columns names or indexes

 --buffer=[buffersize]
   Integer value of size of buffer to store table rows

 --header=[column_name,...]
    Only for tsv/csv input. Comma separated list of column names. By default the program will attempt to use the first row of data that comes in.

 --input_type=[csv/tsv/jsons]

 --columns=[column,...]
    The columns to display

Expected Features
-----------------

### Auto column type detection

 * Date
 * Datetime
 * Time
 * Integer
 * Currency (.00)
 * Float

Automatically chose the best type where all items in column have the same type

### Alignment

Automatically right align numeric values

### Key columns

Key columns allow two features

1) When streaming data into the table, if the a row matches the key columns replace that row. 
2) Support for simple aggregation operations on numeric rows

### Aggregation

The following aggretation operations need to be supported 

 * Percentiles / Median
 * Count
 * Mean
 * Max / Min