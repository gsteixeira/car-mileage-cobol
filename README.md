# car-mileage-cobol
Calculate car mileage from csv file in COBOL.
Also shows CO2 released, CO2 per km, and how many trees needs to plant.

You must supply a csv file as the input in the following format:

> odometer_read;gas_quantity;is_top_up?

> 990011;25.00;1

- compiles like this:

> cobc -x --free carmileage.cob

- runs like this:

> ./carmileage sample.csv


- Why in COBOL?

Because it's so fun! :)


- My car is spending too much gas!

Check the pointers, tune them up. Clean the carburetors, tune them up. Tune the valves. Check spark plugs and cables. Use everything in standard setting: gicleurs, injectors, openings. Please refer to the user manual. A clean, well tuned carburetor and pointer runs better than any electronic garbage! 8)


- But my car is fuel injected..

Ok, so you ride a plastic toy... Sell it, buy an electric or a classic! Carbs runs better.


- Carburetors and pointers?! This is something from the past..

So why are you messing around with COBOL?


- But fossil fuels are bad!!!

You are indeed RIGHT! And that's why I made this software. Now you know how much you spend on fuel. By the way... COBOL is so good in doing calculations, and so fun, I also added calculations on CO2 released to the atmosphere. So now there is a column for CO2 released and another for CO2 per km.


- The CO2 data makes me feel bad, what can I do to help?

If viable, buy an electric vehicle. If you have a classic like I do, consider converting it to electric as soon as possible. But while it's not viable for most of us, the best you can do now is to **PLANT TREES**! Plant as many as you can. By the way, COBOL is so fun, I also added calculations on how many trees you should plant to compensate for all this released CO2.


- How is the environmental impact calculated?

2.31 Kg of CO2 per Liter of Gasoline burned.
0.060 metric ton CO2 per urban tree planted.
source: https://www.epa.gov/energy/greenhouse-gases-equivalencies-calculator-calculations-and-references


Plant trees and have fun!
