# car-mileage-cobol
Calculate car mileage from csv file in COBOL.

You have supply a csv file as the input in the following format:

> odometer_read;gas_quantity;is_top_up?

> 990011;25.00;1

- compiles like this:

> cobc -x --free mileage.cob

- runs like this:

> ./mileage sample.csv

*Why in COBOL?

Because it's so fun! :)

*My car is spending too much gas!

Check the pointers, tune them up. Clean the carburetors, tune them up. Tune the valves. Check spark plugs and cables. Use everything in standard setting: gicleurs, injectors, openings. Please refer to the user manual. A clean, well tuned carburetor and pointer runs better than any electronic garbage! 8)

*But my car is fuel injected!

Ok, you ride a plastic toy!! Sell it, buy an electric or a classic! Carbs runs better.
