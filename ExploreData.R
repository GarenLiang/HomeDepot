# HomeDepot
In [1]:
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggplot2)
library(gridExtra)
train <- read.table("../input/train.csv", sep=",", header=TRUE)
test <- read.table("../input/test.csv", sep=",", header=TRUE)
In [2]:
#train set field 
names(train)
Out[2]:
'id' 'product_uid' 'product_title' 'search_term' 'relevance'
In [3]:
#test Set fields
names(test)
Out[3]:
'id' 'product_uid' 'product_title' 'search_term'
In [4]:
##Their are 5 column in train set
str(train)
'data.frame':	74067 obs. of  5 variables:
 $ id           : int  2 3 9 16 17 18 20 21 23 27 ...
 $ product_uid  : int  100001 100001 100002 100005 100005 100006 100006 100006 100007 100009 ...
 $ product_title: Factor w/ 53489 levels "0.140 in. x 1/4 in. x 1/4 in. Nylon Outer Diameter Spacer (2-Piece)",..: 44285 44285 5538 12396 12396 51760 51760 51760 30636 25377 ...
 $ search_term  : Factor w/ 11795 levels "019 070 vanity",..: 1949 6526 3748 8657 9526 3495 7145 7146 4413 7024 ...
 $ relevance    : num  3 2.5 3 2.33 2.67 3 2.67 3 2.67 3 ...
In [5]:
##Their are 4 column in test set
str(test)
'data.frame':	166693 obs. of  4 variables:
 $ id           : int  1 4 5 6 7 8 10 11 12 13 ...
 $ product_uid  : int  100001 100001 100001 100001 100001 100001 100003 100003 100003 100004 ...
 $ product_title: Factor w/ 94731 levels "0-10 GPH Adjustable Drippers on 360-Degree Spike (4-Pack)",..: 78686 78686 78686 78686 78686 78686 82253 82253 82253 37014 ...
 $ search_term  : Factor w/ 22427 levels "032888076358 pvc ball",..: 3247 13717 18403 18410 18407 22103 4340 4346 14633 18811 ...
In [6]:
### unique relevence scores
unique(train$relevance)
Out[6]:
3 2.5 2.33 2.67 2 1 1.67 1.33 1.25 2.75 1.75 1.5 2.25
In [7]:
##Train Set dim 
dim(train)
Out[7]:
74067 5
In [8]:
##Test Set dim 
dim(test)
Out[8]:
166693 4
In [9]:
setdiff(names(train), names(test))
Out[9]:
'relevance'
In [10]:
##Total unique search terams in train set
length(unique(train$search_term))
Out[10]:
11795
In [11]:
##search Term diffrence between the set
length(setdiff(unique(train$search_term), unique(test$search_term)))
Out[11]:
2174
In [12]:
###Lets see the search terams
unique(train$search_term)[1:30]
Out[12]:
angle bracket l bracket deck over rain shower head shower only faucet convection otr microwave over stove microwaves emergency light mdf 3/4 steele stake briggs and stratton lawn mower gas mowe honda mower hampton bay chestnut pull up shade disposer grill gazebo door guards metal plate cover gcfi radiator grate windows screens 1x1 rail decorative wood 4*8 beadboard paneling 4x8wood paneling MDF 4x8 wainscot chair rail wainscot plank paneling lawn sprkinler rainbird sprinkler PLATFORM FOR WASHERS
In [13]:
###their are 2174 terms which are not in test set
###Lets see the product title 
unique(train$product_title)[1:30]
Out[13]:
Simpson Strong-Tie 12-Gauge Angle BEHR Premium Textured DeckOver 1-gal. #SC-141 Tugboat Wood and Concrete Coating Delta Vero 1-Handle Shower Only Faucet Trim Kit in Chrome (Valve Not Included) Whirlpool 1.9 cu. ft. Over the Range Convection Microwave in Stainless Steel with Sensor Cooking Lithonia Lighting Quantum 2-Light Black LED Emergency Fixture Unit House of Fara 3/4 in. x 3 in. x 8 ft. MDF Fluted Casing Valley View Industries Metal Stakes (4-Pack) Toro Personal Pace Recycler 22 in. Variable Speed Self-Propelled Gas Lawn Mower with Briggs & Stratton Engine Hampton Bay Caramel Simple Weave Bamboo Rollup Shade - 96 in. W x 72 in. L InSinkErator SinkTop Switch Single Outlet for InSinkErator Disposers Sunjoy Calais 8 ft. x 5 ft. x 8 ft. Steel Tile Fabric Grill Gazebo MD Building Products 36 in. x 36 in. Cloverleaf Aluminum Sheet, Silver House of Fara 8 Linear ft. MDF Overlapping Wainscot Interior Paneling Kit 1804 Dual Spray Half Pattern 4 in Pop-Up Spray Head Samsung 4.2 cu. ft. Front Load Washer with Steam in White, ENERGY STAR Quikrete 80 lb. Crack-Resistant Concrete Nantucket Pavers Patio-on-a-Pallet 10 ft. x 10 ft. Concrete Tan Variegated Basketweave Yorkstone Paver UltraTouch 48 in. x 24 ft. Radiant Barrier Backyard X-Scapes 6 ft. H. x 16 ft. L Reed Fencing DecoArt Americana Decor 16-oz. Whisper Chalky Finish 9.1 in. x 5.8 in. White Designer Shelf Bracket HDX 48 in. W x 72 in. H x 18 in. D Decorative Wire Chrome Finish Commercial Shelving Unit Marshalltown Masonry Brush Rubbermaid 12 in. D Single Track Bracket Husky 9-Pocket Maintenance Pouch RIDGID X4 18-Volt 1/2 in. Impact Wrench (Tool Only) Emberglow 25,000 BTU Vent-Free Dual Fuel Gas Stove with Thermostat American Standard Cadet 3 FloWise 2-piece 1.28 GPF Round Front Toilet in Bone Sumner Street Home Hardware Grayson 2-1/2 in. Oil Rubbed Bronze Cup Pull HDX 6 ft. Heavy Duty Steel Green Painted T-Post
In [14]:
# The number of unique product titles in the training set
length(unique(train$product_title))
Out[14]:
53489
In [15]:
# The number of product titles that are only in the train set or only in the test set
length(setdiff(unique(train$product_title), unique(test$product_title)))
Out[15]:
25722
In [16]:
# The number of product titles that are in both the train and test sets
length(intersect(unique(train$product_title), unique(test$product_title)))
Out[16]:
27767
