# Load KC/Jackson County SocioEconomic Subset based on ZCTA from
# C:\Data\US-Government\Census-Bureau\Missouri-Census-Data-Center

# UMKC Center for Health Insights
# Earl F Glynn, 2014-04-14

DROP DATABASE IF EXISTS TestLoad;

create database TestLoad;
use TestLoad;

CREATE TABLE socioeconomic
(
  zcta5              CHAR( 5),
  ZIPName            CHAR(20),
  County             CHAR(20),
  County2            CHAR(20),

  MedianAge          DECIMAL(4,1),

  pctUnder18         DECIMAL(4,1),
  pctOver65          DECIMAL(4,1),
  pctWhite1          DECIMAL(4,1),
  pctBlack1          DECIMAL(4,1),
  pctAsian1          DECIMAL(4,1),
  pctHispanicPop     DECIMAL(4,1),

  MedianHHInc        DECIMAL(6,0),
  MedianFamInc       DECIMAL(6,0),

  PovUniverse        DECIMAL(6,0),
  pctpoor            DECIMAL(4,1),
  pctInCollege       DECIMAL(4,1),
  pctBachelorsormore DECIMAL(4,1),
  pctForeignBorn     DECIMAL(4,1),
  pctRenterOcc       DECIMAL(4,1),

  MedianHValue       DECIMAL(6,0),
  MedianGrossRent    DECIMAL(4,0)
);

LOAD DATA LOCAL INFILE 'C:/Data/US-Government/Census-Bureau/Missouri-Census-Data-Center/SocioEconomic-KCMO.csv'
INTO TABLE socioeconomic
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 LINES    # ignore headers
 (zcta5, ZIPName, County, County2, MedianAge,
  pctUnder18, pctOver65, pctWhite1, pctBlack1, pctAsian1, pctHispanicPop,
  @xMedianHHInc, @xMedianFamInc, PovUniverse,
  pctpoor, @xpctInCollege, pctBachelorsormore, pctForeignBorn, @xpctRenterOcc,
  @xMedianHValue, @xMedianGrossRent)
SET
  MedianHValue    = IF(@xMedianHValue    = '', NULL, CONVERT(@xMedianHValue,    DECIMAL(6,0)) ),
  pctInCollege    = IF(@xpctInCollege    = '', NULL, CONVERT(@xpctInCollege,    DECIMAL(4,1)) ),
  MedianGrossRent = IF(@xMedianGrossRent = '', NULL, CONVERT(@xMedianGrossRent, DECIMAL(4,0)) ),
  MedianHHInc     = IF(@xMedianHHInc     = '', NULL, CONVERT(@xMedianHHInc,     DECIMAL(6,0)) ),
  MedianFamInc    = IF(@xMedianFamInc    = '', NULL, CONVERT(@xMedianFamInc,    DECIMAL(6,0)) ),
  pctRenterOcc    = IF(@xpctRenterOcc    = '', NULL, CONVERT(@xpctRenterOcc,    DECIMAL(4,1)) );
;


SHOW WARNINGS;

