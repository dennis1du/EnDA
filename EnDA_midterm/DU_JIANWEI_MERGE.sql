--Create Tables
/*CREATE TABLE "DU_JIANWEI_EIA_PP"(
    Utility_ID int,
    Plant_Code int,
    Prime_Mover text,
    Energy_Source_1 text,
    Utility_Name text,
    Technology text,
    Plant_Name text,
    Nameplate_Capacity numeric,
    Operating_Year_avg numeric,
    Operating_Year_max int,
    Operating_Year_min int,
    NERC_Region text,
    Net_Generation_MWh int,
    Elec_Fuel_Consumption_MMBtu int,
    Capacity_Factor numeric,
    Heat_Rate numeric,
    Year integer
);
--grant access to josh
GRANT SELECT ON "DU_JIANWEI_EIA_PP" TO joshdr

CREATE TABLE "DU_JIANWEI_EIA_LOC"(
    Plant_ID int,
    Latitude numeric,
    Longitude numeric,
    Balancing_Authority_Code text
);
--grant access to josh
GRANT SELECT ON "DU_JIANWEI_EIA_LOC" TO*/

-- Merge Data
SELECT
    T1.Plant_Code,
    T1.Technology,
    T1.Nameplate_Capacity,
    T2.Latitude,
    T2.Longitude
FROM
    (SELECT
        Plant_Code,
        Technology,
        Nameplate_Capacity
    FROM
        "DU_JIANWEI_EIA_PP")T1
CROSS JOIN
    (SELECT
        Plant_ID,
        Latitude,
        Longitude
    FROM
        "DU_JIANWEI_EIA_LOC")T2
WHERE
    T1.Plant_Code = T2.Plant_ID