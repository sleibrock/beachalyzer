Beachalyzer
===========

Analyze your Boom Beach costs and figure out what's the next logical, 
and most cheapest upgrade.

Calculates the cost differential between upgrading a building 
and upgrading a headquarters as well.

# What's the "Cost" of a building?

Cost is the total aggregate cost of a building upgrade. 
Here we add all resources (wood, stone and iron) into one number - 
this is what we will call the 'total cost' of the building.

```
cost = wood + stone + iron
```

The aim is to figure out the lowest costing building, so we will
use a functional model to find the lowest cost building.

```
findLowest :: List -> List -> Pair -> Pair
findLowest a b c
    | a = [] == c
    | otherwise == findLowest (tail a) b 
                        (if (cost (head a) b) < (cost c b)
                          then (head a) else c)
```

# Usage

First compile the package using the makefile
```
$ make
```

Binaries are placed in the bin/ folder once compiled.

Then, use FileGenerator to create a template file for you to use.
```
$ ./bin/FileGenerator my_base.txt
```

This will generate a template file containing building names and their levels.
Edit the zeroes so the numbers match the level of your *lowest* building.

Once that's done, you can parse the file with the Beachalyzer binary
```
$ ./bin/Beachalyzer my_base.txt
```

Some information will come out and it will give you some helpful information.

# Example Output

Output will typically look like this (included in this project is a test file you can try it out on).

![image](https://bytebucket.org/GodHand/beachalyzer/raw/418abf9f31f91752eb800db9b5899488bf21ac6f/doc/2015-09-27-230632_676x232_scrot.png)