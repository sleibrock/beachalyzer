#lang racket

; Tables.rkt
; Contains building upgrade information tables
(provide
  all-buildings
  get-levels
  total-cost
  interpret-file
  parse-string
  (struct-out cost)
  (struct-out building))

;; File IO code here
; Open up a file with the name file-name
; Return the list of pairs (name level)
(define (interpret-file file-name)
  (define (accum-file acc prt)
    (define val (read-line prt))
    (if (eof-object? val)
      acc
      (accum-file
        (cons val acc) prt)))
  (define infile (open-input-file file-name))
  (define data (accum-file '() infile))
  (close-input-port infile)
  data)

; Parse a string that's in the format of x:y into '(x y)
(define (parse-string input-str)
  (let
    ((new-data (string-split input-str ":")))
    (list
      (first new-data)
      (string->number (string-trim (second new-data))))))

;; Building structure data
; The abstract cost of a building
; lvl -> HQ level required to build
(struct cost (lvl wood stone iron))

; Building takes a list of costs in to form a table
(struct building (name table))

; Get the levels for a building by name
; (get-levels "headquarters" all-buildings) => '(#<cost>...)
; Error if Name not in table
(define (get-levels name table)
  (if (empty? table)
    (error (format "~a not in Database" name))
    (if (string=? name
                  (building-name (first table)))
      (building-table (first table))
      (get-levels name (rest table)))))

; Get the total cost for a building
; Just applies all struct methods to one cost
(define (total-cost item)
  (+ (cost-wood item)
     (cost-stone item)
     (cost-iron item)))

; Define the building structure tables
(define all-buildings
  (list
    (building
      "headquarters"
      (list
        (cost  0       0       0       0)
        (cost  1     600       0       0)
        (cost  2    1900       0       0)
        (cost  3    5900       0       0)
        (cost  4   13000       0       0)
        (cost  5   25000       0       0)
        (cost  6   33000    5800       0)
        (cost  7   49000    9800       0)
        (cost  8   81000   17800       0)
        (cost  9  126000   30000    8000)
        (cost 10  197000   50000   16000)
        (cost 11  268000   97000   28000)
        (cost 12  400000  189000   50000)
        (cost 13  570000  340000   97000)
        (cost 14  790000  690000  189000)
        (cost 15 1350000 1020000  370000)
        (cost 16 1900000 1700000  740000)
        (cost 17 2690000 2220000 1410000)
        (cost 18 3300000 3000000 1970000)
        (cost 19 3700000 3600000 2830000)
        (cost 20 4260000 4100000 3940000)))
    (building
      "sculptor"
      (list
        (cost  5    3000       0       0)
        (cost  7   29400    5900       0)
        (cost  9   88000   21000       0)
        (cost 12  320000  151000   40000)
        (cost 14  670000  580000  161000)
        (cost 16 1710000 1530000  670000)
        (cost 18 3060000 2790000 1830000)
        (cost 20 3900000 3900000 3900000)))
    (building
      "submarine"
      (list
        (cost  9   50000   12000   3200)
        (cost  9   57000   13500   3600)
        (cost 10   99000   24800   8000)
        (cost 11  134000   49000  14000)
        (cost 12  201000   95000  24800)
        (cost 13  284000  171000  49000)
        (cost 14  390000  340000  95000)
        (cost 15  670000  510000 187000)
        (cost 16  950000  850000 370000)
        (cost 17 1350000 1110000 700000)))
    (building
      "armory"
      (list
        (cost  4    2600       0       0)
        (cost  5   10000       0       0)
        (cost  6   19800    3500       0)
        (cost  7   29400    5900       0)
        (cost  8   49000   10700       0)
        (cost  9   76000   18000    4800)
        (cost 10  118000   29700    9600)
        (cost 11  161000   58000   16800)
        (cost 11  174000   63000   18200)
        (cost 12  261000  123000   32000)
        (cost 13  370000  222000   63000)
        (cost 14  550000  480000  132000)
        (cost 14  590000  510000  142000)
        (cost 15 1010000  770000  280000)
        (cost 16 1420000 1270000  560000)
        (cost 16 1520000 1360000  600000)
        (cost 17 2150000 1780000 1130000)
        (cost 18 2630000 2400000 1580000)
        (cost 19 2960000 2880000 2260000)
        (cost 20 3300000 3300000 3300000)
        (cost 21 4060000 3900000 3740000)))
    (building
      "radar"
      (list
        (cost  1     150       0       0)
        (cost  2     400       0       0)
        (cost  3    2360       0       0)
        (cost  4    5900       0       0)
        (cost  5   11300       0       0)
        (cost  6   16500    2900       0)
        (cost  7   25500    5100       0)
        (cost  8   44000    9600       0)
        (cost  9   71000   16800    4500)
        (cost 10  114000   28700    9300)
        (cost 11  161000   58000   16800)
        (cost 12  249000  117000   30700)
        (cost 13  360000  218000   62000)
        (cost 14  520000  450000  125000)
        (cost 15  910000  700000  254000)
        (cost 16 1330000 1190000  520000)
        (cost 17 1940000 1600000 1010000)
        (cost 18 2430000 2220000 1460000)
        (cost 19 2810000 2740000 2150000)
        (cost 20 3200000 3200000 3200000)))
    (building
      "gunboat"
      (list
        (cost  0       0       0       0)
        (cost  1     180       0       0)
        (cost  2    1240       0       0)
        (cost  3    4500       0       0)
        (cost  4   10000       0       0)
        (cost  5   19800       0       0)
        (cost  6   26400    4600       0)
        (cost  7   40000    7900       0)
        (cost  8   66000   14600       0)
        (cost  9  105000   24900    6600)
        (cost 10  166000   42000   13400)
        (cost 11  228000   82000   23800)
        (cost 12  350000  163000   43000)
        (cost 13  490000  297000   84000)
        (cost 14  690000  600000  166000)
        (cost 15 1200000  910000  330000)
        (cost 16 1710000 1530000  670000)
        (cost 17 2450000 2020000 1280000)
        (cost 18 3030000 2760000 1810000)
        (cost 19 3400000 3300000 2630000)
        (cost 21 4470000 4300000 4130000)))
    (building
      "weaponlab"
      (list
        (cost 15 1610000 1230000  450000)
        (cost 16 2280000 2040000  890000)
        (cost 17 3200000 2660000 1690000)
        (cost 18 3900000 3600000 2360000)
        (cost 19 4900000 4900000 4900000)))
    (building
      "landingcraft"
      (list
        (cost  1     150       0       0)
        (cost  1     250       0       0)
        (cost  3    3200       0       0)
        (cost  4    9400       0       0)
        (cost  5   18300       0       0)
        (cost  6   24400    4300       0)
        (cost  7   37000    7400       0)
        (cost  8   62000   13500       0)
        (cost  9   97000   23100    6200)
        (cost 10  154000   39000   12500)
        (cost 11  212000   77000   22100)
        (cost 12  320000  151000   40000)
        (cost 13  460000  276000   79000)
        (cost 14  640000  560000  155000)
        (cost 15 1120000  850000  310000)
        (cost 16 1590000 1430000  620000)
        (cost 17 2290000 1890000 1200000)
        (cost 18 2830000 2580000 1690000)
        (cost 19 3200000 3130000 2460000)
        (cost 20 3600000 3600000 3600000)
        (cost 21 3700000 4430000 4100000)))
    (building
      "residence"
      (list
        (cost  0     150       0       0)
        (cost  1     350       0       0)
        (cost  2    1140       0       0)
        (cost  3    3800       0       0)
        (cost  4    7800       0       0)
        (cost  6   19800    3500       0)
        (cost  9   57000   13500    3600)
        (cost 13  227000  136000   39000)
        (cost 16  760000  680000  298000)
        (cost 19 1480000 1440000 1130000)))
    (building
      "goldstorage"
      (list
        (cost  1     130      0      0)
        (cost  2     500      0      0)
        (cost  3    4100      0      0) 
        (cost  4   10400      0      0)
        (cost  5   20000      0      0)
        (cost  6   26400   4600      0)
        (cost  8   65000  14200      0)
        (cost 10  158000  40000  12800)
        (cost 13  450000 273000  78000)
        (cost 15 1080000 820000 298000)))
    (building
      "woodstorage"
      (list
        (cost  2    320      0      0)
        (cost  3   1300      0      0)
        (cost  4   3120      0      0)
        (cost  5   6300      0      0)
        (cost  7  12700   2550      0)
        (cost  9  34000   8100   2160)
        (cost 11  75000  27200   7800)
        (cost 13 164000  99000  28100)
        (cost 15 400000 307000 112000)
        (cost 16 570000 510000 223000)))
    (building
      "stonestorage"
      (list
        (cost  6   6600   1160      0)
        (cost  7  10300   2060      0)
        (cost  8  17800   3900      0)
        (cost  9  29000   6900   1840)
        (cost 11  64000  23300   6700)
        (cost 12 101000  47000  12400)
        (cost 13 147000  89000  25200)
        (cost 14 212000 185000  51000)
        (cost 15 380000 286000 104000)
        (cost 17 810000 670000 420000)))
    (building
      "ironstorage"
      (list
        (cost  9   18900    4500   1200)
        (cost  9   21400    5100   1360)
        (cost 10   37000    9400   3040)
        (cost 11   56000   20400   5900)
        (cost 12   93000   43000  11400)
        (cost 14  189000  164000  45000)
        (cost 15  340000  256000  93000)
        (cost 16  490000  440000 193000)
        (cost 17  750000  620000 390000)
        (cost 19 1110000 1080000 850000)))
    (building
      "vault"
      (list
        (cost  2     350       0       0)
        (cost  3    2070       0       0)
        (cost  4    4600       0       0)
        (cost  5    8800       0       0)
        (cost  6   11600    2030       0)
        (cost  7   17600    3500       0)
        (cost  8   30000    6600       0)
        (cost  9   48000   11400    3040)
        (cost 10   77000   19300    6200)
        (cost 11  107000   39000   11200)
        (cost 12  165000   77000   20300)
        (cost 13  238000  143000   41000)
        (cost 14  340000  295000   81000)
        (cost 15  590000  450000  164000)
        (cost 16  850000  760000  330000)
        (cost 17 1240000 1020000  650000)
        (cost 18 1550000 1410000  930000)
        (cost 19 1780000 1730000 1360000)
        (cost 19 1810000 1760000 1390000)
        (cost 20 2060000 2060000 2050000)))
    (building
      "sawmill"
      (list
        (cost  0     0      0      0)
        (cost  1   200      0      0)
        (cost  2   600      0      0)
        (cost  3  3500      0      0)
        (cost  5 10000      0      0)
        (cost  7     0   9300      0)
        (cost  9     0   2100   5600)
        (cost 12     0  95000  24800)
        (cost 15     0 410000 149000)
        (cost 17     0 670000 420000)))
    (building
      "quarry"
      (list
        (cost  7   9800 0      0)
        (cost  8  20300 0      0)
        (cost  9  31500 0   2000)
        (cost 10  59000 0   4800)
        (cost 11  80000 0   8400)
        (cost 12 121000 0  14900)
        (cost 13 170000 0  29100)
        (cost 14 236000 0  57000)
        (cost 16 570000 0 223000)
        (cost 18 990000 0 590000)))
    (building
      "ironmine"
      (list
        (cost 10   39000    9900 0)
        (cost 11  126000   33000 0)
        (cost 12  126000   63000 0)
        (cost 13  251000  126000 0)
        (cost 14  390000  261000 0)
        (cost 15  810000  540000 0)
        (cost 16 1680000  840000 0)
        (cost 17 1860000 1860000 0)
        (cost 18 1980000 1980000 0)
        (cost 19 2100000 2100000 0)))
    (building
      "sniper"
      (list
        (cost  1     100       0       0)
        (cost  1     200       0       0)
        (cost  2     760       0       0)
        (cost  3    2070       0       0)
        (cost  4    4700       0       0)
        (cost  5    9300       0       0)
        (cost  6   12500    2200       0)
        (cost  7   19100    3800       0)
        (cost  8   32000    7100       0)
        (cost  9   52000   12300    3300)
        (cost 10   83000   20800    6700)
        (cost 11  115000   42000   12000)
        (cost 12  177000   83000   21800)
        (cost 13  255000  153000   44000)
        (cost 14  360000  315000   87000)
        (cost 15  630000  480000  175000)
        (cost 16  910000  810000  360000)
        (cost 17 1320000 1090000  690000)
        (cost 18 1650000 1500000  990000)
        (cost 20 2100000 2100000 2090000)
        (cost 21 2760000 2530000 2300000)))
    (building
      "machinegun"
      (list
        (cost  4    3300       0       0)
        (cost  4    4600       0       0)
        (cost  5    8800       0       0)
        (cost  5   10000       0       0)
        (cost  6   13200    2320       0)
        (cost  6   14900    2610       0)
        (cost  7   22500    4500       0)
        (cost  8   38000    8400       0)
        (cost  9   60000   14400    3800)
        (cost 10   97000   24300    7800)
        (cost 11  134000   49000   14000)
        (cost 12  205000   96000   25200)
        (cost 13  295000  177000   50000)
        (cost 14  420000  360000  100000)
        (cost 15  730000  550000  201000)
        (cost 16 1040000  930000  410000)
        (cost 17 1510000 1240000  790000)
        (cost 18 1880000 1710000 1120000)
        (cost 19 2150000 2090000 1640000)
        (cost 20 2430000 2430000 2420000)
        (cost 21 2760000 2540000 2980000)))
    (building
      "mortar"
      (list
        (cost  3    1180       0       0)
        (cost  3    2360       0       0)
        (cost  4    5900       0       0)
        (cost  5   11500       0       0)
        (cost  6   15500    2730       0)
        (cost  7   23500    4700       0)
        (cost  8   40000    8700       0)
        (cost  9   63000   15000    4000)
        (cost 10  101000   25200    8200)
        (cost 11  140000   50000   14600)
        (cost 12  217000  102000   26700)
        (cost 13  320000  191000   54000)
        (cost 14  460000  400000  110000)
        (cost 15  810000  610000  224000)
        (cost 16 1180000 1050000  460000)
        (cost 17 1720000 1420000  900000)
        (cost 18 2170000 1980000 1300000)
        (cost 19 2520000 2450000 1920000)
        (cost 20 2880000 2880000 2870000)
        (cost 20 2960000 2970000 2950000)
        (cost 21 3420000 3100000 3070000)))
    (building
      "cannon"
      (list
        (cost  6   11600    2030      0)
        (cost  6   13200    2320      0)
        (cost  7   19600    3900      0)
        (cost  8   32000    7100      0)
        (cost  9   50000   12000   3200)
        (cost 10   79000   19800   6400)
        (cost 11  107000   39000  11200)
        (cost 12  161000   76000  19800)
        (cost 13  227000  136000  39000)
        (cost 14  314000  274000  76000)
        (cost 15  540000  410000  149000)
        (cost 15  610000  460000  168000)
        (cost 16  850000  760000  330000)
        (cost 16  950000  850000  370000)
        (cost 17 1350000 1110000  700000)
        (cost 17 1480000 1220000  770000)
        (cost 18 1810000 1650000 1080000)
        (cost 18 1970000 1800000 1180000)
        (cost 19 2220000 2160000 1700000)
        (cost 20 2470000 2470000 2470000)
        (cost 21 2750000 2990000 3230000)))
    (building
      "flamethrower"
      (list
        (cost  9   31500    7500    2000)
        (cost  9   44000   10500    2800)
        (cost 10   69000   17300    5600)
        (cost 11   94000   34000    9800)
        (cost 11  107000   39000   11200)
        (cost 12  161000   76000   19800)
        (cost 13  244000  147000   42000)
        (cost 14  340000  295000   81000)
        (cost 14  360000  315000   87000)
        (cost 15  620000  470000  172000)
        (cost 16  870000  780000  340000)
        (cost 17 1320000 1090000  690000)
        (cost 17 1320000 1090000  690000)
        (cost 18 1710000 1560000 1020000)
        (cost 19 1920000 1870000 1470000)
        (cost 20 2140000 2140000 2130000)
        (cost 20 2260000 2270000 2260000)
        (cost 21 2370000 2790000 2580000)))
    (building
      "boomcannon"
      (list
        (cost 12  121000   57000   14900)
        (cost 12  141000   66000   17300)
        (cost 13  198000  119000   34000)
        (cost 13  255000  153000   44000)
        (cost 14  350000  308000   85000)
        (cost 15  610000  460000  168000)
        (cost 16  850000  760000  330000)
        (cost 16  950000  850000  370000)
        (cost 17 1350000 1110000  700000)
        (cost 18 1650000 1500000  990000)
        (cost 18 1810000 1650000 1080000)
        (cost 19 2040000 1980000 1560000)
        (cost 20 2260000 2270000 2260000)
        (cost 20 2470000 2470000 2460000)
        (cost 21 3200000 3460000 2940000)))
    (building
      "rocketlauncher"
      (list
        (cost 13  142000   85000   24300)
        (cost 13  198000  119000   34000)
        (cost 14  275000  240000   66000)
        (cost 14  275000  240000   66000)
        (cost 15  540000  410000  149000)
        (cost 16  760000  680000  298000)
        (cost 16  850000  760000  330000)
        (cost 17 1210000 1000000  630000)
        (cost 18 1480000 1350000  890000)
        (cost 19 1850000 1800000 1420000)
        (cost 19 1850000 1800000 1420000)
        (cost 20 2060000 2060000 2050000)
        (cost 20 2470000 2470000 2460000)))
    (building
      "shocklauncher"
      (list
        (cost 17  810000  670000  420000)
        (cost 17  940000  780000  490000)
        (cost 18 1150000 1050000  690000)
        (cost 18 1480000 1350000  890000)
        (cost 19 1670000 1620000 1270000)
        (cost 19 2040000 1900000 1560000)
        (cost 20 2470000 2470000 2460000)
        (cost 20 2880000 2880000 2870000)
        (cost 20 3300000 3300000 3300000)))))

; end
