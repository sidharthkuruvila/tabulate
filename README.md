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

      [-buffer-size INT]    The number of lines the program can store at a time
      [-csv-header LIST]    Column labels for csv/tsv file
      [-csv-no-header-row]  Don't use the first row as the header row, use numeric
                            id's instead unless -header is provided
      [-csv-separator]      The column separater used by the csv parser, this
                            defaults to the tab character
      [-hide-header]        Hide the header
      [-build-info]         print info about this build and exit
      [-version]            print the version of this build and exit
      [-help]               print this help text and exit
                            (alias: -?)
