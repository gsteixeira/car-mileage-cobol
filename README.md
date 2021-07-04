# car-mileage-cobol
Calculate car mileage from csv file in COBOL.

You have supply a csv file as the input in the following format:

> odometer_read;gas_quantity;is_top_up?
> 990011;25.00;1

- compiles like this:

> cobc -x --free mileage.cob

- runs like this:

> ./mileage sample.csv

Why in COBOL?

Because it's so fun! :)
