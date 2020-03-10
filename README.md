field-burning
=============

Project goals
------------
- Determine independent and interacting effects of fuel load and fuel structure on fire behavior. 

- Examine fire severity and fire response of trees.

Methods
-------

### 1.Pre-fire measurements 





To determine fire effects on trees, we sampled 80 individuals of *J. virginiana* ( 1.5 m < tree height < 3.5m). Within each burning unit (in total 4 units) we sampled 20 individuals. Trees were sampled by picking a random direction within the unit and walking to find the nearest tree individual that is within the height range. Selected trees were separated by a minimum distance of 10m, and located on relatively flat ground. Trees that have recently experienced obvious stress, or that are close to snags or logs were avoided during sampling. We also avoid sampling trees that were within 10m from the edge of the burning unit. We tagged each tree and record the geographical location. Diameter of each tree was measured at breast height (1.3m above the ground). We also recorded tree heigh using a measuring stick. 


### 2.Fuel treatments




To mimic native grass fuels, we used locally grown cut hay as the fuel source. Fuel was manipulated to vary both fuel load and biomass allocation in vertical space with a full factorial experimental design (Table 1).We used 620g/m2 fuel load (low fuel load) to represent aboveground biomass produced by an annually burned tallgrass prairie (Abrams et al. 1986), and  1240g/m2 fuel load (high fuel load) to represent tallgrass prairies with relatively high fuel load (Kidnie 2009); Our “canopy trait” treatment included low biomass height ratio (70% total fuel load is allocated below 30cm height relative to ground), and high biomass height ratio (70% total fuel load is allocated above 30cm height relative to ground) treatments to represent the extremes of canopy architectures measured in our previous work (Gao and Schwilk 2018)

### **Table 1.Fuel treatment**



|                                            |                                          |
|--------------------------------------------|------------------------------------------|
| High fuel load * high biomass height ratio | High fuel load * low biomass height ratio|
|Low fuel load * high biomass height ratio   | Low fuel load * low biomass height ratio |





Prior to burning, three 0.5m * 1.0m plots evenly distributed at the canopy edge were chosen, and local vegetation present within these  plots was cut to ground level and replaced by cut hay at a fixed fuel load (620 g/m2 or 1240/m2 ). To achieve manipulation in fuel canopy architecture, we used wire mesh, rebar (60cm section), and wire to make a 0.5m width * 1.0m length * 0.2m depth fuel tray and place it 30cm above the ground level by wiring the tray to rebar at each corner. For high biomass height ratio treatment, 70% of total fuel load was set above the wire screen of the supporter; for low biomass height ratio treatment, 70% of total fuel load was set underneath the wire screen on the ground. To avoid disturbance and dramatic change in fuel moisture content, we deployed experiment fuel on the day of burning.





### 3.Fire behavior measurements

On the day of burning, we measured fire weather and fire behavior. Fire weather was measured prior to ignition and every 15 minutes during burning with a kestrel weather meter. Measurements included temperature, relative humidity, and wind speed 2m above the ground. We also measured flame temperature at soil surface and 1 m above ground by attaching thermocouple probe to the tree trunk. 



### 4.Post-fire measurements

The day after burning we measured fire severity according to three metrics: tree bole char height, percentage of pre-fire crown volume scorched, and percentage of bole circumference scorched at 30cm height (Peterson and Arbaugh 1986, Sieg et al. 2006, Catry et al. 2010). Tree bole char height is defined as the height relative to the ground where area on tree trunk that is blackened by fire ends.  We define crown scorch as crown that is completely consumed or scorched by fire that leaves and twigs turned into brown or black. To calculate this, we took images of tree crown after fire. Images was taken from four cardinal directions 3m away from the tree at a fixed resolution and height (1.3m) . We placed a metal ruler by the tree as a reference of scale.  Percentage of bole circumference scorched at 30cm is defined as percentage of bole circumference that is consumed or blackened by fire at 30cm above ground. 

Post-fire vegetation survey will be conducted the next summer (June ~ Aug) for trees. To determine fire effects on trees, we will check tree status half year and a year after fire (won’t be done by me). Tree mortality is defined as no green tissue present. Percentage of pre-fire crown alive will be recorded if there is only fire damage. 


Data
----

Each data file referred to has an associated machine readable metadata file. If data file is named as e.g. tree-traits.csv, then associated metadata file is names as tree-traits-metadata.csv. The metadata file describes each variable (column) in the data file including data type, unit and a brief description of the variable.

### sample geological location

data/sample-gps.csv stores the geological location for samples trees. It contains the Longitude, latitude, and elevation information along with the unique tree ID for each tree.

### Plant traits

data/tree-traits.csv and data/post-fire-severity.csv store pre-fire plant traits (height, DBH @ 1.3 m above ground) and post-fire severity data (bole char height, % of bole circumference scorched at 30cm above the ground, % of pre-fire crown volume scorched). Details of measuring method see method. These are used to determine how tree traits along with fire severity to influence post-fire response.


### hobo data

data/hobo/. stores all hobo files recording k-type temperature measured at 0cm and 100cm relative to the ground. Temp is recorded in
Celsius degree every second during the fire.  The file is named with hobo ID (e.g. 12) along with the measuring location (e.g. 'l' stands for long that measure temp at 100cm and 's' stands for short that measure temp at 0cm) and date (in mmdd format, year is skipped but all measurements were taken in 2019). To match the fire behavior measurement to trees, use data/tree-hobo-id.csv along with measuring date and time, as some hobo cases were used for multiple burns on different dates or on the same day but different burns.) 








