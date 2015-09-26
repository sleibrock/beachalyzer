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
                        (if (cost (head a)) < (cost c)
                          then (head a) else c)
```

