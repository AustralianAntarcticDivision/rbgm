# rbgm 0.0.5

* Included `rgeos` and `spdplyr` in Suggests, thanks to CRAN. 

* added new function 'read_bgm' as a more obvious version of the original read function 'bgmfile'

* removed names from component sp lists to avoid troubles in sf causing troubles in mapview

# rbgm 0.0.4

* added vignettes, examples, more doc

* removed some old functions

* cleaned up for CRAN

* fixed a gnarly bug due to lexicographic sorting on a list! Lines from faceSpatial were coming out in the sorted order of "face0", "face1", "face10", "face30" etc.

* renamed "leftbox" and "rightbox" to "left" and "right"

* readme includes all example files in bgmfiles

* new example files added

* added functions pointSpatial and nodeSpatial 

* new function bgmfile to replace deprecated read_bgm

* new tests and changes to capture Guam_utm1 variant

# rbgm 0.0.2

* First fully working build ready for check.



