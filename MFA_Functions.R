# Functions for MFA 


########################## Select Organics #######################
# Selects organics from the Recycling PRA data
# also, reformats as needed
# @param x = the recycling PRA data
select_Organics <- function(x = recycling) {
    
    # start with highest level (Category). Include anything Organics or Other material category
    # "Other" includes a variety of Fertilizers and topsoil. Honestly not sure if we need those right now.
    # Its possible that these are not derived from waste, although some are specified as "Organic"
    organics <- x[x$Material_Category %in% c("Organics", "Other"),]
    
    # remove biosolids
    organics <- organics[!(organics$Material_Subcategory %in% c("Biosolids")),]
    
    # sift through the other description (an optional, lowest category) and remove as needed
    organics <- organics[!(organics$Other_Description %in% c("Mattresses for recycling.", 
                                                             "Fines for beneficial reuse", # this is glass recycling term
                                                             "Scrap plastic carts for recycling.",
                                                             "Lime")),]
    
    # Standardize syntax
    organics$Other_Description[organics$Other_Description == "Liquid Organic Fertilizers"] <- "Liquid Organic Fertilizer"
    organics$Material_Type[organics$Other_Description == "compost"] <- "Compost"
    organics$Other_Description[organics$Other_Description == "compost"] <- ""
    organics$Other_Description[organics$Other_Description == "Composted Overs / Mulch"] <- "Compost Overs"
    organics$Material_Type[organics$Material_Type == "Organics"] <- "Mixed Organics"
    organics$Material_Subcategory[organics$Material_Subcategory == "Organics"] <- "Mixed Organics"
    organics$Material_Type[organics$Other_Description == "food"] <- "Food Waste"
    organics$Other_Description[organics$Other_Description == "food"] <- ""
    organics$Material_Type[organics$Material_Type == "Post-Consumer Food Waste"] <- "Food Waste"
    
    # "Other description" is more accurate than material type "Other", move those over
    organics$Material_Type[organics$Other_Description != ""]<- 
        organics$Other_Description[organics$Other_Description != ""]
    organics$Other_Description <- ""
    
    # Standardize syntax again
    organics$Material_Type[organics$Material_Type == "Liquid digestate"] <- "Liquid Digestate"
    organics$Material_Type[organics$Material_Type == "Wood Chips"] <- "Mulch"
    
    # Standardize material categories/subcategories based on material type
    organics$Material_Subcategory[organics$Material_Type == "Food Waste"] <- "Food Waste"
    organics$Material_Category[organics$Material_Type == "Liquid Organic Fertilizer"] <- "Other"
    organics$Material_Subcategory[organics$Material_Type == "Liquid Organic Fertilizer"] <- "Other"
    organics$Material_Category[organics$Material_Type == "Desert Compost"] <- "Organics"
    organics$Material_Subcategory[organics$Material_Type == "Desert Compost"] <- "Compost"
    organics$Material_Category[organics$Material_Type == "Planter Mix"] <- "Organics"
    organics$Material_Subcategory[organics$Material_Type %in% 
                                      c("Topsoil", "Soil Amendment", "Planter Mix", 
                                        "Compost Soil Blend", "Compost Biosoil Blend", "Biosoil Blend")] <- "Other"
    organics$Material_Category[organics$Material_Type == "Topsoil"] <- "Other"
    
    
    # give an end user category to brokers/transporters and recycler/composters
    organics$End_User_Category[organics$Material_Stream == "Recycling/Composting"] <- "Recycler/Composter"
    organics$End_User_Category[organics$Material_Stream == "Brokering/Transporting"] <- "Broker/Transporter"
    
    # Fill in Jurisdiction, if blank, make same as destination region
    organics$Destination_Jurisdiction[organics$Destination_Jurisdiction == ""] <-
        organics$Destination_Region[organics$Destination_Jurisdiction == ""]
    
    # New column, Origin/Destination County
    organics$Origin_County <- organics$Origin_Region
    # When designated as (State) or (Country), change to out of state
    organics$Origin_County[grep("(Country)", organics$Origin_County)] <- "Out of State"
    organics$Origin_County[grep("(State)", organics$Origin_County)] <- "Out of State"
    organics$Origin_County <- gsub(" \\(County\\)", "", organics$Origin_County)
    
    organics$Destination_County <- organics$Destination_Region
    organics$Destination_County[grep("(Country)", organics$Destination_County)] <- "Out of State"
    organics$Destination_County[grep("(State)", organics$Destination_County)] <- "Out of State"
    organics$Destination_County <- gsub(" \\(County\\)", "", organics$Destination_County)
    
    
    
    organics$Year <- substr(organics$Year_Quarter, 1,4)
    organics$Quarter <- substr(organics$Year_Quarter, 5,8)
    
    
    return(organics)
} # close select organics function

########################## Select Green Waste #################
select_GreenWaste <- function(x = disposal, ID_table = ID) {
    
    # filter from largest category
    green <- x[x$Material_Category %in% c("Mixed", "Organics", "Mixed Residuals", "Other"),]
    
    # remove biosolids
    green <- green[green$Material_Subcategory != "Biosolids",]
    
    # remove other non-organic/green materials
    green <- green[green$Material_Subcategory != "Mixed Solid Waste",]
    green <- green[green$Material_Type != "Treated Automobile Shredder Waste",]
    
    # these are assumed to be MRF residuals. Compost facilities have their own nomenclature (overs)
    green <- green[green$Material_Type != "Processing Residuals",] 
    
    # standardize language/syntax
    green$Other_Description[green$Other_Description %in% 
                                c("materials picked out of green waste and screen",
                                  "Mixed contaminates picked out of green waste",
                                  "Materials picked out of green waste", 
                                  "Materials removed from green waste",
                                  "mixed materials picked out of green waste")] <- "Green Waste Residuals"
    green$Other_Description[green$Other_Description ==
                                "ADC derived from food waste and yard waste"] <- "Green Material for Beneficial Reuse"
    green$Other_Description[green$Other_Description == "Overs"] <- "Compost Overs"


    
    # Move "Other Description" to Material Type
    # In these instances Material Type is "Other" making the "Other Description" column more descriptive
    green$Material_Type[green$Other_Description != ""] <- green$Other_Description[green$Other_Description != ""]
    green$Other_Description <- ""
    

    # remove two more non-green/organics from MRFs
    green <- green[green$Material_Type != "Non-Green Material for Beneficial Reuse",]
    green <- green[green$Material_Type != "Outbound Residuals",]
    
    # standardize some more
    # some are Other/other material cat, standardize to organics/compost
    green$Material_Category[green$Material_Type == "Compost Overs"] <- "Organics"
    green$Material_Subcategory[green$Material_Type == "Compost Overs"] <- "Compost"
    green$Material_Type[green$Material_Type == "Branches and/or Stumps"] <- "Wood"
    
    green$Material_Stream[green$Material_Type == 
                              "Green Material for Beneficial Reuse"] <- "Green Material for Beneficial Reuse"
    green$Material_Category[green$Material_Type == "Green Material for Beneficial Reuse"] <- "Organics"
    green$Material_Subcategory[green$Material_Type == "Green Material for Beneficial Reuse"] <- "Green Waste"
    green$Material_Type[green$Material_Type == "Green Material for Beneficial Reuse"] <- "Green Waste"
    
    
    green$Material_Subcategory[green$Material_Type == "Compost"] <- "Compost"
    
    # add RD to the front of the RDRS IDs
    green$Destination_RD <- paste0("RD", green$Destination_RD)
    green$Origin_RD <- paste0("RD", green$Origin_RD)
    
    # remove origin SWIS and add SWIS numbers from they key dataset bc some are missing anyways
    green <- green[, -which(colnames(green) == "Origin_SWIS")]
    
    # add SWIS
    temp <- colnames(ID_table)
    colnames(ID_table) <- paste0("Origin_", colnames(ID_table))
    green <- merge(green, ID_table, 
                   by.x = c("Origin_RD", "Origin_Entity"),
                   by.y = c("Origin_RDRS_ID", "Origin_Dataset_Name"), all.x = TRUE)
    
    colnames(ID_table) <- temp
    colnames(ID_table) <- paste0("Destination_", colnames(ID_table))
    green <- merge(green, ID_table, 
                   by.x = c("Destination_RD", "Destination_Entity"),
                   by.y = c("Destination_RDRS_ID", "Destination_Dataset_Name"), all.x = TRUE)
    
    colnames(ID_table) <- temp
    
  
    

    colnames(green)[colnames(green) == "Origin_Activity"] <- "Origin_Activity_Type" #match destination nomenclature
    
    
    
    # New column, Origin/Destination County
    green$Origin_County <- green$Origin_Region
    green$Origin_County[grep("(Country)", green$Origin_County)] <- "Out of State"
    green$Origin_County[grep("(State)", green$Origin_County)] <- "Out of State"
    green$Origin_County <- gsub(" \\(County\\)", "", green$Origin_County)
    
    green$Destination_County <- green$Destination_Region
    green$Destination_County[grep("(Country)", green$Destination_County)] <- "Out of State"
    green$Destination_County[grep("(State)", green$Destination_County)] <- "Out of State"
    green$Destination_County <- gsub(" \\(County\\)", "", green$Destination_County)
    
    
    green$Year <- substr(green$Year_Quarter, 1,4)
    green$Quarter <- substr(green$Year_Quarter, 5,8)
    
    # select needed columns
    green <- green[,c("Year", "Quarter", 
                      "Origin_RD", "Origin_SWIS_Number", "Origin_Full_ID", "Origin_Standard_Name",
                      "Origin_Activity_Type", "Origin_County", "Origin_Jurisdiction", 
                      "Destination_RD", "Destination_SWIS_Number", "Destination_Full_ID", "Destination_Standard_Name", 
                      "Destination_Activity_Type", "Destination_County", "Destination_Jurisdiction", 
                      "Material_Stream", "Material_Category", "Material_Subcategory", "Material_Type", "Tons_Sent")]
    
    colnames(green)[which(colnames(green) %in% c("Origin_SWIS_Number",
                                                 "Origin_Standard_Name", 
                                                 "Destination_SWIS_Number",
                                                 "Destination_Standard_Name"))] <- c("Origin_SWIS", "Origin_Facility",
                                                                                     "Destination_SWIS", "Destination_Facility")

    
    return(green)
    
} # close select greenwaste function


########################## Clean Transfer Station Data ######################
# cleans transfer station dataset
# adds RDRS/SWIS IDs, puts everything in columns, corrects typos, etc
## @param transfer2 = the transfer station dataset
clean_transfer <- function(transfer2){
    
    # Destination RDRS_ID to its own column
    transfer2$Destination_RDRS <- substr(transfer2$Destination_Organization,
                                         nchar(transfer2$Destination_Organization) - 8 + 1, nchar(transfer2$Destination_Organization)) 
    transfer2$Destination_RDRS <- gsub(")", "", transfer2$Destination_RDRS)
    
    
    # Origin RDRS ID to its own column
    transfer2$Transfer_RDRS <- substr(transfer2$Originating_Organization,
                                      nchar(transfer2$Originating_Organization) - 8 + 1, nchar(transfer2$Originating_Organization))
    
    
    transfer2$Transfer_RDRS <- gsub(")", "", transfer2$Transfer_RDRS)
    
    
    # add the IDs from info key for final destination
    transfer2 <- merge(transfer2, key[!duplicated(key$RDRS_ID),], by.x = "Destination_RDRS", by.y = "RDRS_ID", all.x = TRUE)
    colnames(transfer2)[which(colnames(transfer2) %in%
                                  c("Standard_Name", "SWIS_Number", "Full_ID"))] <- c("Destination_Facility",
                                                                                      "Destination_SWIS",
                                                                                      "Destination_Full_ID")
    
    
    # add the IDs for transfer stations
    transfer2 <- merge(transfer2, key[!duplicated(key$RDRS_ID),], by.x = "Transfer_RDRS", by.y = "RDRS_ID", all.x = TRUE)
    colnames(transfer2)[which(colnames(transfer2) %in%
                                  c("Standard_Name",
                                    "SWIS_Number", "Full_ID"))] <- c("Transfer_Facility",
                                                                     "Transfer_SWIS", "Transfer_Full_ID")
    
    
    # add the county to Origin Jurisdictions, Origin Facilities, and Destination Facilities
    ## origin jurisdiction
    transfer2 <- merge(transfer2, as.data.frame(cities)[,c("COUNTY", "CITY")],
                       by.x = "Jurisdiction", by.y = "CITY", all.x = TRUE)
    
    ## some cities manually (have typos so no match)
    transfer2$COUNTY[transfer2$Jurisdiction == "California City"] <- "Kern"
    transfer2$COUNTY[transfer2$Jurisdiction == "La Canada Flintridge"] <- "Los Angeles"
    transfer2$COUNTY[transfer2$Jurisdiction == "San Buenaventura"] <- "Ventura"
    
    transfer2$COUNTY <- gsub(" County", "", transfer2$COUNTY) # remove " County" from County name
    colnames(transfer2)[which(colnames(transfer2) %in% c("Jurisdiction", "COUNTY"))] <- c("Origin_Jurisdiction","Origin_County")
    
    
    ## Transfer Station County
    transfer2 <- merge(transfer2, SWIS[!duplicated(SWIS$SWIS_Number),c("SWIS_Number", "County")],
                       by.x = "Transfer_SWIS", by.y = "SWIS_Number", all.x = TRUE)
    colnames(transfer2)[which(colnames(transfer2) == "County")] <- "Transfer_County"
    
    ## Destination Facility County
    transfer2 <- merge(transfer2, SWIS[!duplicated(SWIS$SWIS_Number),c("SWIS_Number", "County")],
                       by.x = "Destination_SWIS", by.y = "SWIS_Number", all.x = TRUE)
    colnames(transfer2)[which(colnames(transfer2) == "County")] <- "Destination_County"
    
    
    # Deal with Regional Agencies (convert to member cities and add county)
    Regional_Agencies <- read.csv(paste0(mypath, "MFA_Data/Regional_Waste_Agencies.csv"))
    transfer2 <- merge(transfer2, Regional_Agencies, 
                       by.x = "Origin_Jurisdiction", by.y = "Regional_Agency", all = TRUE)
    
    
    transfer2$Origin_County[transfer2$Origin_Jurisdiction %in%
                                unique(Regional_Agencies$Regional_Agency)] <- transfer2$County[transfer2$Origin_Jurisdiction %in% 
                                                                                                   unique(Regional_Agencies$Regional_Agency)]
    # when origin jurisdiction a regional agency, it will then become the jurisdiction column
    transfer2$Origin_Jurisdiction[transfer2$Origin_Jurisdiction %in%
                                      unique(Regional_Agencies$Regional_Agency)] <- transfer2$Jurisdiction[transfer2$Origin_Jurisdiction %in% 
                                                                                                               unique(Regional_Agencies$Regional_Agency)]
    # Remove "Community Services Districts" as these serve unincorporated areas
    ## and are not on the rest of the maps
    temp <- unique(transfer2$Origin_Jurisdiction[grep(" Community Services District", transfer2$Origin_Jurisdiction)])
    transfer2 <- transfer2[!(transfer2$Origin_Jurisdiction %in% temp),]
    transfer2 <- transfer2[transfer2$Origin_Jurisdiction != "Cayucos Sanitary District",]
    
    
    
    # select columns to return and rename
    main_cols <- which(colnames(transfer2) %in% c("Year", "Quarter", "Material_Stream",
                                                  "Host_Assigned", "Export",
                                                  "Originating_Type", "Total_Tons")) # main columns to keet
    
    # choose main column + cols that start with Origin_ or Transfer_
    transfer2 <- transfer2[,c(main_cols,
                              grep("Origin_", colnames(transfer2)),
                              grep("Transfer_", colnames(transfer2)), grep("Destination_", colnames(transfer2)))]
    transfer2 <- transfer2[!names(transfer2) %in% c("Destination_Organization")]
    colnames(transfer2)[which(colnames(transfer2) == "Originating_Type")] <- "Flow_Type"
    transfer2$Transfer_RDRS[transfer2$Transfer_RDRS == ""] <- NA
    
    
    transfer2$Material_Class <- NA
    transfer2$Material_Class[transfer2$Material_Stream != "Solid Waste"] <- "Green Materials"
    transfer2$Material_Class[is.na(transfer2$Material_Class)] <- "Solid Waste"
    
    return(transfer2)
}


########################## Clean JurisdictionDisposalAndBeneficial dataset #########################
# cleans and formats JurisdictionDisposalAndBeneficial.csv
# @param juris_disposal - the csv
# returns - a cleaned version of the df
clean_JurisdictionDisposal <- function(juris_disposal){
    
    # rename some things (remove extra parenthesis)
    # bc its easiest way to get the str_split stuff below to work
    temp <- "CR&R Material Recovery & Transfer Op. (El Centro) (CR&R Material Recovery & Transfer Op. (El Centro) - RD10953)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <- 
        "CR&R Material Recovery & Transfer Op. - El Centro (CR&R Material Recovery & Transfer Op. - El Centro - RD10953)"
    
    temp <- "El Centro Direct Transfer (Dogwood) (El Centro Direct Transfer (Dogwood) - RD10954)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <-
        "El Centro Direct Transfer Dogwood (El Centro Direct Transfer Dogwood - RD10954)"
    
    temp <- "South Tahoe Refuse Co. (Transfer/Processing Facility (MRF) - RD10589)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <- 
        "South Tahoe Refuse Co. (Transfer/Processing Facility MRF - RD10589)"
    
    temp <- "Waste Management of California, Inc (El Cajon) (Universal Refuse Removal Recycling and Transfer Facility  - RD10398)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <- 
        "Waste Management of California, Inc El Cajon (Universal Refuse Removal Recycling and Transfer Facility  - RD10398)"
    
    temp <- "Bakersfield Metropolitan (Bena) Sanitary Landfill (Bakersfield Metropolitan (Bena) SLF - RD10487)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <- 
        "Bakersfield Metropolitan Bena Sanitary Landfill (Bakersfield Metropolitan Bena SLF - RD10487)"
    
    temp <- "Pebbly Beach (Avalon) Disposal Site (Pebbly Beach (Avalon) Disposal - RD10947)"
    juris_disposal$Destination_Facility[juris_disposal$Destination_Facility == temp] <-
        "Pebbly Beach Avalon Disposal Site (Pebbly Beach Avalon Disposal - RD10947)"
    
    
    # get first part as facility name
    juris_disposal$Destination_Facility_Name <- trimws(str_split(juris_disposal$Destination_Facility, "\\(", simplify = TRUE)[,1])
    
    
    # second part as entity name (inside parenthesis), then remove RDRS part
    juris_disposal$Destination_Entity_Name <- trimws(str_split(juris_disposal$Destination_Facility, "\\(", simplify = TRUE)[,2])
    juris_disposal$Destination_Entity_Name <- trimws(str_sub(juris_disposal$Destination_Entity_Name, end = -11))
    
    
    #RDRS ID (inside parenthesis, after "-")
    juris_disposal$Destination_RDRS <- trimws(str_split(juris_disposal$Destination_Facility, "\\(", simplify = TRUE)[,2])
    juris_disposal$Destination_RDRS <- str_sub(juris_disposal$Destination_RDRS, start = -8, end = -2)
    
    
    
    # merge the SWIS numbers onto it
    temp <- key[!duplicated(key$Full_ID), 
                c("RDRS_ID", "SWIS_Number","Full_ID", "Standard_Name")]
    temp <- temp[!duplicated(temp$RDRS_ID),]
    
    juris_disposal <- merge(juris_disposal, temp, by.x = "Destination_RDRS", by.y = "RDRS_ID", all.x = TRUE)
    
    
    
    # merge the counties from the SWIS dataset
    temp <- as.data.frame(SWIS)
    temp <- temp[!duplicated(temp$SWIS_Number), c("SWIS_Number", "County")]
    
    juris_disposal <- merge(juris_disposal, temp, by = "SWIS_Number", all.x = TRUE)
    colnames(juris_disposal)[which(colnames(juris_disposal) %in%
                                       c("County.x", "County.y"))] <- c("Origin_County", "Destination_County")
    
    
    
    # add some counties manually when missing SWIS
    # other missing SWIS are out of state
    juris_disposal$Destination_County[juris_disposal$Standard_Name == "Covanta Stanislaus, Inc"] <- "Stanislaus"
    juris_disposal$Destination_County[juris_disposal$Standard_Name == "C&S Waste Solutions of Lassen County"] <- "Lassen"
    
    return(juris_disposal)} # close clean_JurisdictionDisposal function 



########################## Assign Designated Facilities #########################
# assigns designated facilities based on this hierarchy
## 1. Facilities listed in Franchise Agreement (FA)
## 1a. If FA explicitly states "facility operated by CONTRACTOR" (or similar), use closest facility operated by Hauler
## 2. If city is part of a JPA, use closest facility also used by JPA members
## 3. If city's waste hauler operates a facility within the county, use that facility
## 4. If the county operates a facility, send waste from cities within that county there
## 5. If the city's waste hauler operates any facility, send waste to closest one
## 6. Remaining cities go to closest facility
# Eligible facilities are any facility classified as "Compost Facility (Mixed)" or "Vegetative Food Compost Facility" in SWIS

# The code on this is pretty hacky. There's probably a simpler way to code this, but oh well....

# @param FA2 - the cleaned franchise agreement dataset
# @param key2 - the facility names key dataset
# @param JPA2 - the JPA dataset, lists cities within JPAs
# @param owners2 - compost facility owners/operators. A cleaned version of the two datasets from SWIS
# @param cities2 - cities spatVect object
# @param SWIS2 - SWIS facilities spatVect object

assign_Designated_Organics_Facilities <- function(FA2, key2, JPA2, owners2, cities2, SWIS2) {
    
    
    ################# Franchise Agreement Data #########################
    
    
    # filter FA data to Des_Facs columns
    des_facs <- FA2[c("City", "Year_Entered", "Organic_Fac")]
    
    
    # text to columns
    des_facs$Organic_Fac <- str_replace_all(des_facs$Organic_Fac, ";", ",")
    des_facs <- separate_longer_delim(des_facs, "Organic_Fac", delim = ",")
    

    # remove period and leading/trailing whitespace
    des_facs$Organic_Fac <- str_replace_all(des_facs$Organic_Fac, "\\.", "")
    des_facs$Organic_Fac <- trimws(des_facs$Organic_Fac)
    
    
    
    # remove not specified
    des_facs$Organic_Fac[des_facs$Organic_Fac %in% c("No Designated Site", "None", "None specified",
                                                     "Not applicable C&D waste or commercial recyclables only",
                                                     "not specifid", "not specified", "Not specified")] <- "Not Specified"
    des_facs <- des_facs[des_facs$Organic_Fac != "Not Specified",]
    

    
    # get correct name and RDRS_ID
    # only merge to compost facilities
    # some designated facilities are actually just transfer facilities
    des_facs <- merge(des_facs, key2[key2$Type == "Composter",
                                    c("Dataset_Name", "Standard_Name", "RDRS_ID", "SWIS_Number", "Full_ID")],
                      by.x = "Organic_Fac", by.y = "Dataset_Name", all.x = TRUE)
    
    
    
    # standardize some language
    des_facs$Organic_Fac[des_facs$Organic_Fac %in% c("Republic-designated facilities",
                                                     "Republic-owned organics processing facility", 
                                                     "Republic services facilities",
                                                     "composting facility operated by Solano Garbage Company",
                                                     "allieds choice")] <- "Any Republic Services Facility"
    
    des_facs$Organic_Fac[des_facs$Organic_Fac %in% c("Organic Waste Processing Facility selected by Recology",
                                                     "Pacheco Pass Landfill", 
                                                     "Recology facilities approved under SBWMA", 
                                                     "Recology organics processing facilities", 
                                                     "Recology South Bay")] <- "Any Recology Facility"
    des_facs$Organic_Fac[des_facs$Organic_Fac %in%
                             c("any High Diversion Waste Facility",
                               "Turlock Scavengers choice")] <- NA #removed bc they don't actually follow this irl
    
    des_facs$Organic_Fac[des_facs$Organic_Fac %in%
                             c("facilities designated by GreenWaste")] <- "Any GreenWaste Facility"
    
    
    des_facs$Organic_Fac[des_facs$Organic_Fac %in% c("Compost facility located at Central Landfill",
                                                     "Compost Facility on landfill referenced", 
                                                     "Sonoma Compost", 
                                                     "Sonoma Compost - Central Compost Site SWIS 49-AA-0260",
                                                     "Sonoma Compost Facility", 
                                                     "Sonoma County Landfill/Transfer System")] <- "Sonoma County System"
    
    
    # remove duplicate rows
    des_facs <- des_facs[!duplicated(des_facs[c("City", "Year_Entered", "Full_ID")]),]
    
    
    # separate JPAs into member cities
    des_facs <- merge(des_facs, JPA2, 
                      by.x = "City", by.y = "Regional_Agency", all.x = TRUE)
    des_facs$City[!is.na(des_facs$Jurisdiction)] <- des_facs$Jurisdiction[!is.na(des_facs$Jurisdiction)]
    
    # remove rows where des_fac not a compost facility in names key data
    ## merged with key only to composting facilities, 
    ## so some will be missing a designated facility bc the FA designates a transfer station as a destination
    des_facs <- des_facs[!is.na(des_facs$Standard_Name) |
                             des_facs$Organic_Fac %in% c("Any Republic Services Facility",
                                                         "Any Recology Facility", 
                                                         "Any Solano Garbage Company Facility", 
                                                         "Any GreenWaste Facility", 
                                                         "Any High Diversion Waste Facility",
                                                         "Turlock Scavengers choice"),]
    
    des_facs$Standard_Name[is.na(des_facs$Standard_Name)] <- des_facs$Organic_Fac[is.na(des_facs$Standard_Name)]
    
    
    # select cities in study area
    cities2$COUNTY <- sub(" County", "", cities2$COUNTY)
    cities2$CITY <- str_replace_all(cities2$CITY,"\\.", "")
    study_cities_vect <- cities2[cities2$COUNTY %in% c("Alameda", "Contra Costa",
                                                     "Marin", "Napa", "Sacramento",
                                                     "San Francisco", "San Joaquin", 
                                                     "San Mateo", "Santa Clara", "Santa Cruz", 
                                                     "Solano", "Sonoma", "Stanislaus", "Yolo")]
 

    # add the known designated facility to the cities
    study_cities_vect <- merge(study_cities_vect[c("COUNTY", "CITY")],
                               des_facs[c("City", "Standard_Name", "RDRS_ID", "SWIS_Number", "Full_ID")], 
                               by.x = "CITY", by.y = "City", all.x = TRUE)
    
    
    # add the JPA to the cities
    study_cities_vect <- merge(study_cities_vect, JPA2[c("Jurisdiction", "Regional_Agency")], 
                               by.x = "CITY", by.y = "Jurisdiction", all.x = TRUE)
    

    # prep most recent FA
    ## for each city, we want the most recent waste company FA
    ## the aggregate selects the max year for each city's FA, the merge adds it back to cities dataset
    temp <- merge(aggregate(Year_Entered ~ City, FA2, FUN = max), FA2, by = c("Year_Entered", "City"))
    

    
    ## remove some that are from the same year
    ## these are WDAs, Recyclables collection only, or City Ordinances
    ## a column in the FA data that specifies contract type would make this simpler!!
    temp <- temp[!(temp$File_Name %in% c("Exhibit A Terms Conditions 2017_West_Sac.pdf", 
                                         "Greenwaste Recovery Agreement_Santa_Clara", 
                                         "Recology Agreement_Santa_Clara",
                                         "WDA-FHA Amdt. (Healdsburg) 1-1-22 (fully executed)_omnibus.pdf", 
                                         "Ordinance No 1287 SB1383 SMC 1224 Amende_Sausalito.pdf",
                                         "Tri-CED FA 2025_UnionCity")),]
    
    ## convert JPA FAs to member study_cities
    temp <- merge(temp, JPA2[c("Jurisdiction", "Regional_Agency")],
                  by.x = "City", by.y = "Regional_Agency", all.x = TRUE)
    temp$City[!is.na(temp$Jurisdiction)] <- temp$Jurisdiction[!is.na(temp$Jurisdiction)]
    

    # now, add most recent hauler to the cities
    study_cities_vect <- merge(study_cities_vect, temp[c("City", "Waste_Company", "Parent_Company")], 
                               by.x = "CITY", by.y = "City", all.x = TRUE)
    
    
    
    # the study_cities with no waste company collection their own waste, or couldnt find
    ## This is confirmed in /Franchise_Agreements/Cities_list.csv
    study_cities_vect$Parent_Company[study_cities_vect$CITY %in% c("Belvedere", "Pleasanton")] <- "No FA Found"
    study_cities_vect$Parent_Company[is.na(study_cities_vect$Parent_Company)] <- "City Services"
    study_cities_vect$Parent_Company[study_cities_vect$Parent_Company %in%
                                         c("City Collection",
                                           "City of Santa Cruz (municipal collection services)")] <- "City Services"
    
    
    study_cities_vect$Waste_Company[study_cities_vect$CITY %in% c("Belvedere", "Pleasanton")] <- "No FA Found"
    study_cities_vect$Waste_Company[study_cities_vect$Parent_Company == "City Services"] <- "City Services"
    
    
    # select needed columns
    study_cities_vect <- study_cities_vect[c("CITY", "COUNTY", "Regional_Agency", "Waste_Company", "Parent_Company", 
                                             "Standard_Name", "SWIS_Number", "RDRS_ID", "Full_ID")]
    
    names(study_cities_vect) <- c("City", "County", "JPA", "Waste_Hauler", "Hauler_Parent", 
                                  "Des_Org_Facility", "SWIS_Number", "RDRS_ID", "Full_ID")
    
    
    
    # convert to df, we don't want to deal with annoying spatVect objects
    # we separate the spatVect and the dataframe into two variables
    ## assigning columns in spatVect objects is a pain in the ass
    ## since we only need the spatVect for the nearest neighbor function, we don't need it to hold the data
    study_cities <- as.data.frame(study_cities_vect)
    

    ######################## Eligible Compost Facilities #############################
    # for filling in blanks in the designated facilities dataset
    
    # swis facilities with Active activities
    # SWIS composting facilities that will accept residential organics
    comp_facs_vect <- SWIS2[SWIS2$Activity_Operational_Status == "Active" & 
                               SWIS2$Activity %in% c("Composting Facility (Mixed)",
                                                    "Vegetative Food Material Composting Facility"),]
    
    
    # add site owner/operators to SWIS
    comp_facs_vect <- merge(comp_facs_vect, owners2[c("SWIS_Number", "Owner_Name", "Owner_Parent", "Owner_Type",
                                                     "Operator_Name", "Operator_Parent", "Operator_Type")],
                            by = "SWIS_Number", all.x = TRUE)

    # select needed columns
    comp_facs_vect <- comp_facs_vect[c("SWIS_Number", "Site_Name", "Owner_Name", "Owner_Parent", "Owner_Type", 
                                       "Operator_Name", "Operator_Parent", "Operator_Type",
                                       "Activity", "Activity_Category", "County")]
    
    # we separate the spatVect and the dataframe into two variables
    ## assigning columns in spatVect objects is a pain in the ass
    ## since we only need the spatVect for the nearest neighbor function, we don't need it to hold the data
    comp_facs <- as.data.frame(comp_facs_vect)
    
    
    ########################## Assign Any [Company] Facility ########################
    
    
    # THIS CAN BE MADE A FUNCTION OR SOMETHING, but I just did it manually bc there are only 3 companies
    
    ###### Recology facilities
    # nearest neighbors compost facilities owned or operated by Recology
    # which() is used to select the appropriate spatVect rows based on the df columns
    n <- nearest(study_cities_vect[which(study_cities$Des_Org_Facility == "Any Recology Facility")], 
                 comp_facs_vect[which(comp_facs$Owner_Parent == "Recology Inc." |
                                          comp_facs$Operator_Parent == "Recology Inc.")])
    n <- as.data.frame(n)
    
    
    # assign SWIS and Facility Name
    temp <- comp_facs[which(comp_facs$Owner_Parent == "Recology Inc." |
                                comp_facs$Operator_Parent == "Recology Inc."),]
    
    study_cities$SWIS_Number[which(study_cities$Des_Org_Facility == "Any Recology Facility")] <- temp$SWIS_Number[n$to_id]
    study_cities$Des_Org_Facility[which(study_cities$Des_Org_Facility == "Any Recology Facility")] <- temp$Site_Name[n$to_id]
    
    
    ##### Republic Services
    # nearest neighbors
    n <- nearest(study_cities_vect[which(study_cities$Des_Org_Facility == "Any Republic Services Facility")], 
                 comp_facs_vect[which(comp_facs$Owner_Parent == "Republic Services Inc." | 
                                          comp_facs$Operator_Parent == "Republic Services Inc.")])
    n <- as.data.frame(n)
    
    
    # assign
    temp <- comp_facs[comp_facs$Owner_Parent == "Republic Services Inc." | 
                          comp_facs$Operator_Parent == "Republic Services Inc.",]
    
    study_cities$SWIS_Number[which(study_cities$Des_Org_Facility ==
                                       "Any Republic Services Facility")] <- temp$SWIS_Number[n$to_id]
    study_cities$Des_Org_Facility[which(study_cities$Des_Org_Facility ==
                                            "Any Republic Services Facility")] <- temp$Site_Name[n$to_id]
    
    
    #### GreenWaste
    # nearest neighbors
    n <- nearest(study_cities_vect[which(study_cities$Des_Org_Facility == "Any GreenWaste Facility")], 
                 comp_facs_vect[which(comp_facs$Owner_Parent == "GreenWaste Recovery Inc." | 
                                          comp_facs$Operator_Parent == "GreenWaste Inc.")])
    
    
    # assign
    temp <- comp_facs[comp_facs$Owner_Parent == "GreenWaste Recovery Inc." | 
                          comp_facs$Operator_Parent == "GreenWaste Inc.",]
    
    study_cities$SWIS_Number[which(study_cities$Des_Org_Facility == "Any GreenWaste Facility")] <- temp$SWIS_Number[n$to_id]
    study_cities$Des_Org_Facility[which(study_cities$Des_Org_Facility == "Any GreenWaste Facility")] <- temp$Site_Name[n$to_id]
    

    ###################### Assign JPA Member Cities ################################
    
    # functoin that assigns the designated facilities based on the JPA
    # designed to be used in a loop
    # @param JPA_name - the name of a JPA 
    # returns: a df of cities in that JPA with facilities assigned
    assign_Des_Facs_by_JPA <- function(JPA_name){
        
        # select facilities used by JPA members
        JPA_facs <- study_cities$SWIS_Number[study_cities$JPA == JPA_name]
        JPA_facs <- unique(JPA_facs[!is.na(JPA_facs)])
        
        ## Get the spatial version of the JPA facilities
        JPA_facs <- SWIS2[SWIS2$SWIS_Number %in% JPA_facs]
        JPA_facs <- JPA_facs[!duplicated(JPA_facs$SWIS_Number)] # remove duplicates
        
        # if no facilities used by JPA members, then return the original df
        if(nrow(JPA_facs) == 0) {
            return(study_cities[which(study_cities$JPA == JPA_name),])} 
        
        # if all cities in JPA have a designated facility, then skip iteration
        if(nrow(study_cities[which(study_cities$JPA == JPA_name &
                                   is.na(study_cities$Des_Org_Facility) == TRUE),]) == 0){
            return(NULL)
        } else{
            
            # now for each in the JPA that doesn't have a designated facility, find the closest one from the list
            # the cities to assign a facility to
            temp2 <- study_cities[which(study_cities$JPA == JPA_name & is.na(study_cities$Des_Org_Facility) == TRUE),]
            
            
            # find nearest JPA facility
            n <- nearest(study_cities_vect[which(study_cities$JPA == JPA_name &
                                                     is.na(study_cities$Des_Org_Facility) == TRUE)], JPA_facs)
            
            
            n <- as.data.frame(n)
            JPA_facs <- as.data.frame(JPA_facs)
            
            
            # add nearest facility to the study_cities dataset
            temp2$SWIS_Number <- JPA_facs$SWIS_Number[n$to_id]
            temp2$Des_Org_Facility <- JPA_facs$Site_Name[n$to_id]
            
            return(temp2)}}
    

    
    # loop through JPAs, assign the designated facilities
    # using a loop here bc I don't want it returning a list, just immediately reassigning the study_cities df
    for(x in levels(as.factor(study_cities$JPA))){
        temp <- assign_Des_Facs_by_JPA(x)
        study_cities[which(study_cities$JPA == x &
                               is.na(study_cities$Des_Org_Facility) == TRUE),] <- temp
    }
    

    ############### Assign Hauler-Owned Facilities within County #############################
    
    # standardize names
    study_cities$Hauler_Parent <- str_replace_all(study_cities$Hauler_Parent, ",", "")
    comp_facs$Operator_Parent <- str_replace_all(comp_facs$Operator_Parent, ",", "")
    
    study_cities$Hauler_Parent <- str_replace_all(study_cities$Hauler_Parent, "\\.", "")
    comp_facs$Operator_Parent <- str_replace_all(comp_facs$Operator_Parent, "\\.", "")
    
    
    # loop through counties in study area
    for(x in unique(study_cities$County[which(is.na(study_cities$Des_Org_Facility))])){
        
        # subset cities in county X that is missing a Designated Facility
        temp <- study_cities[which(study_cities$County == x & is.na(study_cities$Des_Org_Facility)),]
        
        # loop through the haulers in that county
        for(i in levels(as.factor(temp$Hauler_Parent))){
            
            # get facilities in the County x with Operator i
            temp2 <- comp_facs[which(comp_facs$County == x & comp_facs$Operator_Parent == i),]
            
            # if there is a facility for that Operator, add it to the temp
            if(length(temp2$SWIS_Number) == 1){
                temp$SWIS_Number[temp$Hauler_Parent == i] <- temp2$SWIS_Number
                temp$Des_Org_Facility[temp$Hauler_Parent == i] <- temp2$Site_Name}
        } # close inner for
        
        # use temp to assign the facility based on hauler
        ## if there is no in-county facility for a hauler, the temp is just the original df subset being assigned
        study_cities[which(study_cities$County == x & is.na(study_cities$Des_Org_Facility)),] <- temp
        
    } # close for loop
    #using for loops instead of apply functions to directly change df, not return something to change later
    

    ####################### Assign County-Owned Facilities ###############################
    #Stanislaus and Yolo Counties are the only ones in study area that own a composting facility
    #for the cities in those two counties without a designated facility, assign the county-owned one
    
    # technically, this should also go for cities. If a city owns a facility, that cities waste goes there
    ## This is true for Napa and Modesto, but this is already covered, so not explicitly coded in
    
    
    # for cities without a designated facility & are in Yolo County
    # assign the Yolo County-owned compost facility (YCCL)
    study_cities$SWIS_Number[which(is.na(study_cities$Des_Org_Facility) &
                                       study_cities$County == "Yolo")] <- 
        comp_facs$SWIS_Number[which(comp_facs$Owner_Type =="County" &
                                        comp_facs$County =="Yolo")]
    
    study_cities$Des_Org_Facility[which(is.na(study_cities$Des_Org_Facility) &
                                            study_cities$County == "Yolo")] <- 
        comp_facs$Site_Name[which(comp_facs$Owner_Type =="County" &
                                      comp_facs$County =="Yolo")]
    
    # for cities without a designated facility & are in Stanislaus County
    # assign the Stanislaus County-owned compost facility (Modesto Co-compost)
    study_cities$SWIS_Number[which(is.na(study_cities$Des_Org_Facility) &
                                       study_cities$County == "Stanislaus")] <- 
        comp_facs$SWIS_Number[which(comp_facs$Owner_Type =="County" &
                                        comp_facs$County =="Stanislaus")]
    
    study_cities$Des_Org_Facility[which(is.na(study_cities$Des_Org_Facility) &
                                            study_cities$County == "Stanislaus")] <- 
        comp_facs$Site_Name[which(comp_facs$Owner_Type =="County" &
                                      comp_facs$County =="Stanislaus")]
    

    ####################### Assign Any Hauler-Owned Facility ###############################
    study_cities$Hauler_Parent[study_cities$Hauler_Parent ==
                                   "Tracy Delta Solid Waste Management"] <- "Tracy Delta Solid Waste Management Inc"
    
    
    # Loop through haulers
    for(i in unique(study_cities$Hauler_Parent[which(is.na(study_cities$Des_Org_Facility))])){
        
        # cities with hauler i 
        temp <- study_cities_vect[which(study_cities$Hauler_Parent == i &
                                            is.na(study_cities$Des_Org_Facility))]
        #facilities operated by company i
        temp2 <- comp_facs_vect[which(comp_facs$Operator_Parent == i)]
        
        # if there is no facility operated by company i, skip this loop
        if(nrow(as.data.frame(temp2)) == 0){next}
        
        
        # nearest facility
        n <- nearest(temp, temp2)
        n <- as.data.frame(n)
        
        temp2 <- as.data.frame(temp2)
        
        # add to study cities
        study_cities$SWIS_Number[which(study_cities$Hauler_Parent == i &
                                           is.na(study_cities$Des_Org_Facility))] <- temp2$SWIS_Number[n$to_id]
        
        study_cities$Des_Org_Facility[which(study_cities$Hauler_Parent == i &
                                                is.na(study_cities$Des_Org_Facility))] <- temp2$Site_Name[n$to_id]
        
        
    } # close for loop

    ######################### Assign Any Closest Facility #####################
    # find closest facility
    n <- nearest(study_cities_vect[which(is.na(study_cities$Des_Org_Facility))],
                 comp_facs_vect)
    n <- as.data.frame(n)
    
    
    # assign facility
    study_cities$SWIS_Number[which(is.na(study_cities$Des_Org_Facility))] <- comp_facs$SWIS_Number[n$to_id]
    study_cities$Des_Org_Facility[which(is.na(study_cities$Des_Org_Facility))] <- comp_facs$Site_Name[n$to_id]
   
    ################## Prep Return Value ###########################
    # fill in missing RDRS_IDs (missing bc info came from SWIS)
    ## temporary dataset of existing SWIS/RDRS_ID combos
    temp <- study_cities[which(!is.na(study_cities$RDRS_ID)), c("SWIS_Number", "RDRS_ID", "Full_ID")]
    temp <- temp[!duplicated(temp$SWIS_Number),]
    colnames(temp) <- c("SWIS_Number", "RDRS_ID2", "Full_ID2")
    
    study_cities <- merge(study_cities, temp, by = "SWIS_Number") # merge to study_cities to get RDRS_IDs
    
    
    # add county for designated facilities 
    ## (would have to do in multiple places in above code, so just doing at once here)
    temp <- as.data.frame(SWIS2[!duplicated(SWIS2$SWIS_Number)])
    colnames(temp)[which(colnames(temp) == "County")] <- "Fac_County"
    
    study_cities <- merge(study_cities, temp[c("SWIS_Number", "Fac_County")], by = "SWIS_Number", all.x = TRUE)
    

    # select/rearrage columns
    study_cities <- study_cities[c("City", "County", "JPA", "Waste_Hauler", "Hauler_Parent",
                                   "Des_Org_Facility", "SWIS_Number", "RDRS_ID2", "Full_ID2", "Fac_County")]
    
    
    colnames(study_cities)[which(colnames(study_cities) %in% c("RDRS_ID2", "Full_ID2"))] <- c("RDRS_ID", "Full_ID")
    
    
    # remove designated facilities that do not exist in SWIS
    ## (This is only "Soils Plus" for Sebastopol)
    study_cities <- study_cities[!is.na(study_cities$SWIS_Number),]
    
    
    # remove duplicates
    study_cities <- study_cities[!duplicated(study_cities[c("City", "Full_ID")]),]

    return(study_cities)
    
    
} # close Assign_Designated_Organic_Facilities()


########################## Estimate Organic Waste Generation ###########################
# Calculates Estimated organic waste generation 
# using waste characterization studies and OverallJurisdictionTonsForDisposal data
# @param wcs_data - the waste characterization data Cities_List_clean.csv
# @param gen - the disposal data
# @param JPA2 - the JPA data (set as a default)
# @param study_cities2 - study_cities spatVect (set as a default)
# returns df of estimated waste generation for each city in the study area
estimate_Waste_Generation <- function(wcs_data, gen, JPA2 = JPA, study_cities2 = study_cities){
    
    
    # change JPAs to member cities
    gen$Jurisdiction[gen$Jurisdiction ==
                         "Central Contra Costa Solid Waste Authority (CCCSWA)"] <- "Central Contra Costa Solid Waste Authority"
    gen <- merge(gen, JPA2, by.x = "Jurisdiction", by.y = "Regional_Agency", all.x = TRUE)
    
    gen$JPA <- NA
    gen$JPA[!is.na(gen$Jurisdiction.y)] <- gen$Jurisdiction[!is.na(gen$Jurisdiction.y)] # JPA col
    
    gen$Jurisdiction[!is.na(gen$Jurisdiction.y)] <- gen$Jurisdiction.y[!is.na(gen$Jurisdiction.y)] # names
    
    
    # Separate the disposal values for the JPA cities by population
    
    ## Population data from State Controllers Office
    ## In the future, I would prefer to use a CalRecycle or official census estimate
    ## This was just the easiest dataset to work with I could find on Cal Data Portal
    temp <- read.csv(paste0(mypath, "MFA_Data/City_Expenditures_Per_Capita.csv"))
    temp <- temp[temp$Fiscal.Year == 2023, c("Entity.Name", "Estimated.Population")]  
    temp$Entity.Name[temp$Entity.Name == "Amador"] <- "Amador City"
    temp$Entity.Name[temp$Entity.Name == "Angels"] <- "Angels Camp"
    temp$Entity.Name[temp$Entity.Name == "Carmel-By-The-Sea"] <- "Carmel-by-the-Sea"
    temp$Entity.Name[temp$Entity.Name == "Mt. Shasta"] <- "Mount Shasta"
    temp$Entity.Name[temp$Entity.Name == "St. Helena"] <- "St Helena"
    
    
    gen <- merge(gen, temp, by.x = "Jurisdiction", by.y = "Entity.Name", all.x = TRUE)
    gen <- gen[!is.na(gen$Estimated.Population),] # remove unincorporated
    
    
    
    ## calculate the total population for the JPA (aggregate calculates, setNames sets colnames)
    gen <- merge(gen, setNames(aggregate(Estimated.Population ~ JPA, gen, FUN = sum), c("JPA", "JPA.Pop")),
                 by = "JPA", all.x = TRUE)
    gen$JPA.Pop[is.na(gen$JPA.Pop)] <- gen$Estimated.Population[is.na(gen$JPA.Pop)] #no jpa
    
    ## Proportion of the JPAs population for each city (or 1.00 if no JPA)
    gen$pop_pct <- gen$Estimated.Population / gen$JPA.Pop
    
    ## Calculate Landfilled MSW and Green waste ADC to landfills
    gen$Landfilled_MSW <- gen$Landfill..Includes.host.assigned.tons.due.to.missing.reports. * gen$pop_pct
    gen$Green_ADC_Landfill <- gen$Green.Material.ADC..Includes.host.assigned.tons.due.to.missing.reports. * gen$pop_pct
    
    
    ########################## Calculate Organic Waste ######################
    ## Merge waste generation and wcs data (and select columns)
    gen <- merge(gen[c("Year", "Jurisdiction", "County.x", 
                       "Landfilled_MSW", "Green_ADC_Landfill")], 
                 wcs_data[c("City", "Source_Type", "Source_Year", 
                            "MSW_SF_Res_FW_Pct", "MSW_MF_Res_FW_Pct", "MSW_Com_FW_Pct")],
                 by.x = "Jurisdiction", by.y = "City", all.x = TRUE)
    
    # select study area cities
    gen <- gen[!is.na(gen$Source_Type),]
    
    
    # some cities WCS separate Multi-Family and Commercial, some report them together
    # find the average between the two and just use one number
    gen$MSW_MFD_Com_Pct_Avg <- (gen$MSW_Com_FW_Pct + gen$MSW_MF_Res_FW_Pct) / 2
    
    
    # residential and commercial MSW
    ## From CalRecycle 2021 Disposal-Based WCS
    gen$Residential_MSW <- gen$Landfilled_MSW * 0.321
    gen$Commercial_MSW <- gen$Landfilled_MSW * 0.481
    
    # estimated organic tonnage
    gen$Est_Res_Organics <- gen$Residential_MSW * (gen$MSW_SF_Res_FW_Pct / 100)
    gen$Est_Com_Organics <- gen$Commercial_MSW * (gen$MSW_MFD_Com_Pct_Avg / 100)
    
    gen$Est_Total_Organics <- gen$Est_Res_Organics + gen$Est_Com_Organics
    
    
    # merge generation to study cities
    study_cities2 <- merge(study_cities2, gen[c("Jurisdiction", "Residential_MSW", "Commercial_MSW",
                                                "Est_Res_Organics", "Est_Com_Organics", "Est_Total_Organics")],
                           by.x = "City", by.y = "Jurisdiction", all.x = TRUE)
    
    
    # fix colnames
    colnames(study_cities2)[which(colnames(study_cities2) %in% c("City",
                                                                 "County", 
                                                                 "Des_Org_Facility", 
                                                                 "SWIS_Number", 
                                                                 "RDRS_ID", 
                                                                 "Full_ID",
                                                                 "Fac_County"))] <- c("Origin_City",
                                                                                      "Origin_County",
                                                                                      "Designated_Facility",
                                                                                      "Designated_SWIS",
                                                                                      "Designated_RDRS_ID",
                                                                                      "Designated_Full_ID",
                                                                                      "Designated_County")
    
    return(study_cities2)
    
    
} # close estimate waste generation function


########################## Calculate Direct, Transfer, and Exported Flows ##########################

# Uses CalRecycle JurisdictionTransferStation data to determine flows, based on the Estimated Generation data
# The TransferStation data doesn't differentiate Organics as a material, so Solid Waste is used as a proxy to determine
## the % of waste going through a transfer station for each city. 
# This % is then used with generation data to determine the actual Tons Direct, Transfered, or Exported
# @param trans - the cleaned transfer station data
# @param waste_gen - cleaned waste generation data for each city
# @param material - which material to filter transfer station data by
# @param flow_type - direct, transferred, or exported
# @param threshold - minimum flow to include in results (in tons)
calculate_direct_transfer_export_flows <- function(trans, waste_gen, material,
                                                   flow_type, threshold = 100, sw = sitewaste) {
    
    
    # remove transfer stations that will not accept Food Waste, Green Waste, or Mixed Municipal
    ## These are often only accept special wastes (hazardous, asbestos, ash, etc)
    ## or they are CDI-only facilities
    ## Flows going to these facilities cannot include organics and should not be counted
    sw <- sitewaste[sw$Waste_Type %in% c("Food Wastes", "Green Materials", 
                                                       "Mixed Municipal"),]
    sw <- sw[!duplicated(sw$SWIS_Number),]
    
    trans <- trans[trans$Transfer_SWIS %in% sw$SWIS_Number | is.na(trans$Transfer_SWIS),]
    
    
    
    
    ######## Calculate % of Flows through Transfer #############
    # aggregate transfer data
    pct_trans <- aggregate(Total_Tons ~ Flow_Type + Material_Class + Origin_Jurisdiction,
                           FUN = sum, data = trans)

    # filter to the desired material
    pct_trans <- pct_trans[pct_trans$Material_Class == material,]
    
    # for each city, add the % of the material going thru transfer or not
    pct_trans$Pct <- unlist(lapply(levels(as.factor(pct_trans$Origin_Jurisdiction)), function(x){

        tot <- sum(pct_trans$Total_Tons[pct_trans$Origin_Jurisdiction == x])

        ret <- pct_trans$Total_Tons[pct_trans$Origin_Jurisdiction == x] / tot
        return(ret)
    }))

    
    # get each of the dispositions (direct, thru transfer, export) into their own column
    waste_gen <- merge(waste_gen,
                       pct_trans[pct_trans$Flow_Type == "Sent directly to disposal in California",
                                 c("Origin_Jurisdiction", "Pct")],
                       by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
    colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "direct_pct"


    waste_gen <- merge(waste_gen,
                       pct_trans[pct_trans$Flow_Type == "Sent to California disposal via Transfer/Processor",
                                 c("Origin_Jurisdiction", "Pct")],
                       by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
    colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "trans_pct"


    waste_gen <- merge(waste_gen,
                       pct_trans[pct_trans$Flow_Type %in%
                                     c("Sent directly to exporting Contract Hauler",
                                       "Sent directly to exporting Transfer/Processor",
                                       "Sent to exporting Transfer/Processor via Transfer/Processor"),
                                 c("Origin_Jurisdiction", "Pct")],
                       by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
    colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "export_pct"


    ####### calculate direct and transfer tons for each city ###############
    waste_gen$Total_Direct_Tons <- waste_gen$Est_Total_Organics * waste_gen$direct_pct
    waste_gen$Total_Transfer_Tons <- waste_gen$Est_Total_Organics * waste_gen$trans_pct
    waste_gen$Total_Export_Tons <- waste_gen$Est_Total_Organics * waste_gen$export_pct
    
    
    # if a city isn't present in the transfer data,we assume all waste is direct to disposal
    ##  count NAs for each flow column
    waste_gen$n_NA <- apply(waste_gen[,c("Total_Direct_Tons",
                                         "Total_Transfer_Tons",
                                         "Total_Export_Tons")],
                            MARGIN = 1, function(x) sum(is.na(x))) 

    ## if all flow columns are NAs, then all waste goes directly to facility
    waste_gen$Total_Direct_Tons[waste_gen$n_NA ==3] <- waste_gen$Est_Total_Organics[waste_gen$n_NA == 3] 

    ## NAs to 0
    waste_gen$Total_Direct_Tons[is.na(waste_gen$Total_Direct_Tons)] <- 0
    waste_gen$Total_Transfer_Tons[is.na(waste_gen$Total_Transfer_Tons)] <- 0
    waste_gen$Total_Export_Tons[is.na(waste_gen$Total_Export_Tons)] <- 0

    ############## Return Desired Data ###############################
    # Direct Flows
    if(tolower(flow_type) == "direct"){
        ret <- waste_gen[,c("Origin_City", "Origin_County", "Total_Direct_Tons",
                            "Designated_Facility", "Designated_Full_ID", "Designated_County")]
        colnames(ret)[which(colnames(ret) == "Total_Direct_Tons")] <- "Tons_Sent"
        ret$Flow_Type <- "Generation Direct to Compost Facility"


        # remove flows less than 100 tons
        ret <- ret[ret$Tons_Sent > threshold,]
        
        # add material class column, older versions differentiate between green waste and organics
        ## This is just organics bc the JurisdictionDisposal + local WCS data does not differentiate
        ### between organics and green waste
        ret$Material_Class <- "Organics"

        return(ret)
    # Export Flows
    } else if(tolower(flow_type) == "export"){
        ret <- waste_gen[,c("Origin_City", "Origin_County", "Total_Export_Tons",
                            "Designated_Facility", "Designated_Full_ID", "Designated_County")]
        # we could get the actual facilities they go to here same as transfer
        ret$Designated_Facility <- "Out of State"
        ret$Designated_Full_ID <- "XX-XX-XXXX-RDXXXXX"
        ret$Designated_County <- "Out of State"

        colnames(ret)[which(colnames(ret) == "Total_Export_Tons")] <- "Tons_Sent"

        ret$Flow_Type <- "Exported Wastes"
        ret <- ret[ret$Tons_Sent > threshold,]
        
        # add material class column, older versions differentiate between green waste and organics
        ## This is just organics bc the JurisdictionDisposal + local WCS data does not differentiate
        ### between organics and green waste
        ret$Material_Class <- "Organics"
        
        return(ret)
   
    # Transfer Flows
    } else if(tolower(flow_type) == "transfer"){

        # add the specific transfer station to each city
        # sum total tons through each transfer station for each city
        city_TS <- aggregate(Total_Tons ~ Origin_Jurisdiction + Material_Class +
                                 Transfer_Full_ID + Transfer_Facility + Transfer_County,
                             FUN = sum, data = trans[trans$Flow_Type == "Sent to California disposal via Transfer/Processor",])


        city_TS <- city_TS[city_TS$Material_Class == material,] # filter to material

        # accepts organics?? Skip for now, we assume they do
        # city_TS <- merge(city_TS, sitewaste[,c("Full_ID", "Mixed_Municipal", "Green_Materials",
        #                                        "Food_Wastes", "Digestate")],
        #                  by.x = "trans_Full_ID", by.y = "Full_ID")


        # remove trans stations that accept less than 100 tons solid waste/year (from transfer data)
        city_TS <- city_TS[city_TS$Total_Tons > 100,]



        city_TS$Origin_Jurisdiction <- as.factor(city_TS$Origin_Jurisdiction)
        city_TS <- city_TS[order(city_TS$Origin_Jurisdiction),] # have to order to make the lapply work

        # For each city, calc the % of total transferred waste sent to each trans station
        city_TS$pct_sent <- unlist(lapply(levels(city_TS$Origin_Jurisdiction), function(x){

            total <- sum(city_TS$Total_Tons[city_TS$Origin_Jurisdiction == x])
            ret <- city_TS$Total_Tons[city_TS$Origin_Jurisdiction == x] / total

            return(ret)}))

        # remove total tons column bc it confuses me lol
        # This value comes from the JurisdictionThruTransfer dataset
        city_TS <- city_TS[,-which(colnames(city_TS) == "Total_Tons")]


        # add the city's transfer stations to waste generation
        waste_gen <- merge(waste_gen, city_TS, by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)


        # calculate tons sent through each trans station in each city
        waste_gen$Transfer_Tons <- waste_gen$Total_Transfer_Tons * waste_gen$pct_sent

        waste_gen$Transfer_Tons[is.na(waste_gen$Transfer_Tons)] <- 0

        # choose columns to return
        ret <- waste_gen[,c("Origin_City", "Origin_County", "Transfer_Tons",
                            "Transfer_Full_ID", "Transfer_Facility", "Transfer_County")]

        colnames(ret)[which(colnames(ret) == "Transfer_Tons")] <- "Tons_Sent"

        ret$Flow_Type <- "Transfered Waste"

        ret <- ret[ret$Tons_Sent > threshold,]
        
        
        # add material class column, older versions differentiate between green waste and organics
        ## This is just organics bc the JurisdictionDisposal + local WCS data does not differentiate
        ### between organics and green waste
        ret$Material_Class <- "Organics"

        return(ret)
    }else{print("Choose flow_type: direct, export, or transfer")}
    
} # close calculate_transfer_export_direct function



########################## Add Transfer to Compost Links ################

# adds the transfer station to compost facility links to the transfer_links df
# @param tlinks is the transfer links (df)
# returns the same df, but with transfer to compost facility links added
add_transfer_to_compost_links <- function(tlinks = transfer_links){
    
    # gets largest compost facility in the same county as a transfer station
    get_county_compost_facilities <- function(links2){
        # get compost facilities in counties of transfer facilities
        temp <- STAR[STAR$County %in% links2$County,]
        
        # subset to active compost/green material facilities + select needed columns
        temp <- temp[temp$Activity %in% c("Composting Facility (Mixed)", "Composting Facility (Other)"),
                     c("SWIS_Number","Site_Name", "Activity",
                       "County", "Throughput_Permit_Normalized", "Thro_Norm_Units")]
        
        temp <- temp[!is.na(temp$Throughput_Permit_Normalized),]
        
        
        # Select site with highest throughput in each county
        ## The aggregate finds the highest
        ## the merge adds the other info back onto aggregate's output
        temp <- merge(aggregate(Throughput_Permit_Normalized ~ County, temp, FUN=max),
                      temp, by = c("Throughput_Permit_Normalized", "County"))
        
        return(temp[,c("County", "SWIS_Number")])} # close get county compost facility
    
    # if a transfer station has no compost facility in-county,
    # gets the compost facility nearest to that transfer station
    get_nearest_ooc_compost_facilities <- function(links2){
        # transfer stations with no compost facility in its county
        no_fac <- unique(links2$Origin_SWIS[is.na(links2$Destination)])
        no_fac <- SWIS[SWIS$SWIS_Number %in% no_fac,
                       c("SWIS_Number", "Site_Name", "Activity")] # get shp of those stations
        no_fac <- no_fac[!duplicated(no_fac$SWIS_Number),]
        
        
        # eligible composting facilities
        cf <- SWIS[SWIS$Activity %in% c("Composting Facility (Mixed)", "Composting Facility (Other)"),]
        cf <- cf[!duplicated(cf$SWIS_Number),]
        
        
        # get compost facility nearest those transfer stations
        n <- nearby(no_fac, cf, k = 1) # find idx of nearest
        n <- cf[n[,2], c("SWIS_Number", "Site_Name")] # select nearest from eligible compost facilities
        n <- as.data.frame(n)
        colnames(n) <- c("Nearest_SWIS", "Nearest_Site_Name")
        
        # make a df of transfer stations with no in-county compost facility and their nearest compost facility
        ooc_fac <- cbind(as.data.frame(no_fac), n)
        
        return(ooc_fac)} # close get nearest ooc facility
    
    
    # select needed columns from transfer data
    links <- tlinks[,c("Destination", "Tons_Sent", "Material_Type", "Flow_Type", "Destination_Type")]
    
    # Destination transfer stations are now the Origin
    colnames(links)[which(colnames(links) %in% c("Destination", "Destination_Type"))] <- c("Origin", "Origin_Type")
    
    # get just the SWIS for the transfer facility
    links$Origin_SWIS <- substr(links$Origin, 1, 10)
    
    # add the transfer station county
    links <- merge(links, SWIS[,c("SWIS_Number", "County")], 
                   by.x = "Origin_SWIS", by.y = "SWIS_Number", all.x = TRUE)
    
    
    # get the within county compost facilities
    temp <- get_county_compost_facilities(links)
    
    
    # add within county compost facilities to the transfer links
    links <- merge(links, temp, by = "County", all.x = TRUE)
    colnames(links)[which(colnames(links) == "SWIS_Number")] <- "Destination_SWIS"
    
    # for transfer stations without an in-county facility,
    # get the nearest compost facility
    temp <- get_nearest_ooc_compost_facilities(links)
    
    
    # add the nearest facility to the links
    links <- merge(links, temp, by.x = "Origin_SWIS", by.y = "SWIS_Number", all.x = TRUE)
    links$Destination_SWIS[is.na(links$Destination_SWIS)] <- links$Nearest_SWIS[is.na(links$Destination_SWIS)]
    
    
    
    # get full ID for each
    full_ids <- key[key$Type == "Composter",]
    full_ids <- full_ids[!duplicated(full_ids$Full_ID),]
    links <- merge(links, full_ids[!duplicated(full_ids$SWIS_Number),], by.x = "Destination_SWIS", by.y = "SWIS_Number")
    
    
    # select columns
    links$Destination_Type <- "Full"
    colnames(links)[which(colnames(links) == "Full_ID")] <- "Destination"
    
    links <- links[,colnames(tlinks)]
    
    return(rbind(tlinks, links))
    
    
} # close add transfer links()


########################## Calculate Compost Outflows ###############################
# Calculates compost outflows from the Public Records Request Organics data. 
## This calculation takes into account the origin county of the waste used to produce the compost
## It takes a fraction of the compost outflow proportional to the county-to-county flows in CalRec disposal data
# @ param organics_df - the cleaned PRA organics data
# @ the material to calculate for - the allows for lapplying
# @ param juris_disposal - cleaned JurisdictionDisposalAndBeneficial.csv
# @ threshold to remove small flows (in tons/year)
calculate_compost_outflows <- function(organics_df, material, juris_disposal,
                                       threshold = 100, 
                                       study_counties = c("Alameda", "Contra Costa",
                                                          "Marin", "Napa", "Sacramento",
                                                          "San Francisco", "San Joaquin", 
                                                          "San Mateo", "Santa Clara", "Santa Cruz", 
                                                          "Solano", "Sonoma", "Stanislaus", "Yolo")){
    
    ####### Calculate overall compost flows ##############
    flows <- aggregate(Tons_Sent ~ Origin_County + Destination_County + End_User_Category,
                       FUN = sum, data = organics_df[organics_df$Material_Subcategory == material,])
    
    flows$Material_Class <- material
    colnames(flows) <- c("Origin_County", "Destination_County", "Flow_Type", "Tons_Sent", "Material_Class")
    
    
    ######### Calculate Flows from Study Area ###########
    # landfill tons from each origin county to each destination county
    pcts <- aggregate(Landfill_Tons ~ Origin_County + Destination_County, data = juris_disposal, FUN = sum)
    
    
    # add total tons for the destination county
    # the aggregates finds total tons, merge connects it
    pcts <- merge(pcts,
                  setNames(aggregate(Landfill_Tons ~ Destination_County, data = pcts, FUN = sum),
                           c("Destination_County", "Total_Tons")),
                  by = "Destination_County")
    
    
    # origin counties within our study area
    pcts$Study_Area <- FALSE
    pcts$Study_Area[pcts$Origin_County %in% study_counties] <- TRUE
    
    
    # total tons where origin county is in our study area
    # at this point, the exact origin counties don't matter
    pcts <- merge(pcts, 
                  setNames(aggregate(Landfill_Tons ~ Destination_County + Study_Area,
                                     data = pcts[pcts$Study_Area == TRUE,], FUN = sum),
                           c("Destination_County", "Study_Area", "Study_Area_Tons")),
                  by = "Destination_County")
    
    
    # calculate % of total tons that are study area tons
    pcts$Study_Area_Tons_Pct <- pcts$Study_Area_Tons / pcts$Total_Tons
    
    # select needed rows/columns
    pcts <- pcts[!duplicated(pcts$Destination_County), c("Destination_County", "Total_Tons",
                                                         "Study_Area_Tons", "Study_Area_Tons_Pct")]
    
    # add the %s and calculate flows
    flows <- merge(flows, pcts, by.x = "Origin_County", by.y = "Destination_County", all.x = TRUE)
    
    # All tons sent are reduced by the % of total disposal tons originating in the study area
    ## This will misrepresent the amount bc likely it will be one type of flow that comes from study area, 
    ## not just a reduction in all flows
    flows$Tons_Sent <- flows$Tons_Sent * flows$Study_Area_Tons_Pct
    
    
    # remove flows less than 100 tons
    flows <- flows[flows$Tons_Sent > threshold,]
    
    # select columns
    flows <- flows[c("Origin_County", "Destination_County", "Flow_Type", "Tons_Sent", "Material_Class")]
    
    return(flows)} # close calculate compost outflows function



########################## Calculate Green Outflows ####################################
# Calculates green waste outflows from Public Records Request data
# @param green_df - cleaned greenwaste PRA data
# @ threshold to remove small flows (in ton/year)
calculate_green_outflows <- function(green_df, threshold = 100){
    
    # aggregate to county level to match compost outflows
    green_df <- aggregate(Tons_Sent ~ Origin_County + Destination_Full_ID + Material_Stream + Material_Subcategory +
                              Destination_Facility + Destination_County,
                          FUN = sum, data = green_df)
    colnames(green_df)[which(colnames(green_df) == "Material_Stream")] <- "Flow_Type"
    colnames(green_df)[which(colnames(green_df) == "Material_Subcategory")] <- "Material_Class"
    
    # remove flows less than 100 tons
    green_df <- green_df[green_df$Tons_Sent > threshold,]
    
    return(green_df)}




########################## Create Links df #####################################
# creates the links df for igraph network
## adds the vertex information as well
# @param df - the flows df to convert to links df
# @param origin_col - column that specifies the origins
# @param dest_col - column that specifies the destinations
## specifying column allows us to change what type of origin or destination
## for example, are ODs counties, cities, specific facilities, etc
create_links <- function(df, origin_col, dest_col){
    
    
    otype <- strsplit(origin_col, "_")[[1]][2]
    dtype <- strsplit(dest_col, "_")[[1]][2]
    

    
    origin_col <- which(colnames(df) == origin_col)
    dest_col <- which(colnames(df) == dest_col)
    Tons_Sent <- which(colnames(df) == "Tons_Sent")
    Flow_Type <- which(colnames(df) == "Flow_Type")
    Material_Type <- which(colnames(df) == "Material_Class")
    
    
    links <- df[c(origin_col, dest_col, Tons_Sent, Material_Type, Flow_Type)]
    colnames(links) <- c("Origin", "Destination", "Tons_Sent","Material_Type","Flow_Type")
    
   
    links$Origin_Type <- otype
    links$Destination_Type <- dtype
    

    links <- links[is.na(links$Tons_Sent) == FALSE,]
    
    return(links)}


########################## Create Vertex info DF ################################
# Creates a vertext info df for use with igraph
# @param links - links df (output of create_links)
create_vertex_info <- function(links, city_shp = cities, county_shp = counties, SWIS_shp = SWIS){
    vertex_info <- data.frame(
        name = c(links$Origin, links$Destination), 
        Type = c(links$Origin_Type, links$Destination_Type))
    vertex_info <- vertex_info[!duplicated(vertex_info$name),]
    
    
    ## latlongs for vertex
    city_centroids <- centroids(city_shp)
    county_centroids <- centroids(county_shp)
    
    vertex_info$lat <- NA
    vertex_info$long <- NA
    
    # have to use sapply to get in right order
    vertex_info$lat <- sapply(vertex_info$name, function(x){
        
        ty <- vertex_info$Type[vertex_info$name == x] # type of vertex
        
        if(ty == "City"){
            ret <- crds(city_centroids[city_centroids$CITY == x])[,1]
        } else if (ty == "County"){
            ret <- crds(county_centroids[county_centroids$NAME == x])[,1]
        } else if(ty == "Full"){
            sn <- substr(x, 1, 10) # get SWIS number for the row
            
            sn <- SWIS_shp[SWIS_shp$SWIS_Number == sn,] # get facility points for that SWIS
            sn <- sn[!duplicated(sn$SWIS_Number),] # remove duplicates
            ret <- crds(sn)[,1] #get lat/longs
        } else {
            print("Wrong Type")}
        return(ret)})
    
    
    vertex_info$long <- sapply(vertex_info$name, function(x){
        
        ty <- vertex_info$Type[vertex_info$name == x] # type of vertex
        
        if(ty == "City"){
            ret <- crds(city_centroids[city_centroids$CITY == x])[,2]
        } else if (ty == "County"){
            ret <- crds(county_centroids[county_centroids$NAME == x])[,2]
        } else if(ty == "Full"){
            sn <- substr(x, 1, 10) # get SWIS number from the Full_ID
            sn <- SWIS_shp[SWIS_shp$SWIS_Number == sn,] # get facility points for that SWIS
            sn <- sn[!duplicated(sn$SWIS_Number),] # remove duplicates
            ret <- crds(sn)[,2] #get lat/longs
        } else {
            print("Wrong Type")}
        return(ret)})
    
    
    # specify color based on vertex type
    vertex_info$color <- "black"
    vertex_info$color[vertex_info$Type == "County"] <- "blue"
    vertex_info$color[vertex_info$Type == "City"] <- "red"
    vertex_info$color[vertex_info$Type == "Full"] <- "green"
    
    return(vertex_info)} # close create_Vertex_info

########################## Old Versions ################################

# Uses the Waste Characterization Data and the JurisdictionTransferStation data to determine flows
# The TransferStation data doesn't differentiate Organics as a material, so Solid Waste is used as a proxy to determine
## the % of waste going through a transfer station for each city. 
# This % is then used with WCS data to determine the actual Tons Direct, Transfered, or Exported
# @param trans - the transfer station data
# @param waste_gen - the WCS waste generation data
# @param material - Material Category for transfer data (Solid Waste or Green Materials)
# @param waste_gen_materail - Material Category for waste gen data (Organics or Green Materials)
# @param flow_type - flow type to calculate (direct, transfer, or export)
# calculate_direct_transfer_export_flows_wcs <- function(trans, waste_gen, material,
#                                                    waste_gen_material, flow_type, threshold = 100){
#     
#     
# 
#     ###### Calculate Direct, Transferred, and Exported Flows old#######
#     #aggregate transfer data
#     pct_trans <- aggregate(Total_Tons ~ Flow_Type + Material_Class + Origin_Jurisdiction,
#                            FUN = sum, data = trans)
#     
# 
#     # filter to the desired material
#     pct_trans <- pct_trans[pct_trans$Material_Class == material,]
#     
# 
#     # for each city, add the % of the material going thru transfer or not
#     pct_trans$Pct <- unlist(lapply(levels(as.factor(pct_trans$Origin_Jurisdiction)), function(x){
#         
#         tot <- sum(pct_trans$Total_Tons[pct_trans$Origin_Jurisdiction == x])
#         
#         ret <- pct_trans$Total_Tons[pct_trans$Origin_Jurisdiction == x] / tot
#         return(ret)
#     }))
#     
#     
# 
#     
#     # filter waste_generation data to desired material
#     # this is different parameter bc Organics in WCS data is part of Solid Waste in Transfer data
#     waste_gen <- waste_gen[waste_gen$Material_Class == waste_gen_material,]
#     
# 
#     # get each of the dispositions (direct, thru transfer, export) into their own column
#     waste_gen <- merge(waste_gen, 
#                        pct_trans[pct_trans$Flow_Type == "Sent directly to disposal in California",
#                                  c("Origin_Jurisdiction", "Pct")], 
#                        by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
#     colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "direct_pct"
#     
# 
#     waste_gen <- merge(waste_gen, 
#                        pct_trans[pct_trans$Flow_Type == "Sent to California disposal via Transfer/Processor",
#                                  c("Origin_Jurisdiction", "Pct")],
#                        by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
#     colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "trans_pct"
#     
# 
#     waste_gen <- merge(waste_gen, 
#                        pct_trans[pct_trans$Flow_Type %in%
#                                      c("Sent directly to exporting Contract Hauler",
#                                        "Sent directly to exporting Transfer/Processor",
#                                        "Sent to exporting Transfer/Processor via Transfer/Processor"),
#                                  c("Origin_Jurisdiction", "Pct")],
#                        by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
#     colnames(waste_gen)[which(colnames(waste_gen) == "Pct")] <- "export_pct"
# 
#     
#     # calculate direct and transfer tons for each city
#     waste_gen$Total_Direct_Tons <- waste_gen$Total.Residential.Tons * waste_gen$direct_pct
#     waste_gen$Total_Transfer_Tons <- waste_gen$Total.Residential.Tons * waste_gen$trans_pct
#     waste_gen$Total_Export_Tons <- waste_gen$Total.Residential.Tons * waste_gen$export_pct
#     
#     # if a city isn't present in the transfer data,
#     ## we assume all waste is direct to disposal
#     waste_gen$n_NA <- apply(waste_gen[,c("Total_Direct_Tons",
#                                          "Total_Transfer_Tons", 
#                                          "Total_Export_Tons")],
#                             MARGIN = 1, function(x) sum(is.na(x))) # count NAs for each column
#     
#     waste_gen$Total_Direct_Tons[waste_gen$n_NA ==3] <- waste_gen$Total.Residential.Tons[waste_gen$n_NA == 3]
#     
#     # NAs to 0
#     waste_gen$Total_Direct_Tons[is.na(waste_gen$Total_Direct_Tons)] <- 0
#     waste_gen$Total_Transfer_Tons[is.na(waste_gen$Total_Transfer_Tons)] <- 0
#     waste_gen$Total_Export_Tons[is.na(waste_gen$Total_Export_Tons)] <- 0
#     
#     
#     ############## Return Desired Data ###############################
#     
#     # Direct Flows
#     if(tolower(flow_type) == "direct"){
#         ret <- waste_gen[,c("Origin_City", "Origin_County","Material_Class", "Total_Direct_Tons",
#                             "Designated_Facility", "Designated_Full_ID", "Designated_County")]
#         colnames(ret)[which(colnames(ret) == "Total_Direct_Tons")] <- "Tons_Sent"
#         ret$Flow_Type <- "Generation Direct to Compost Facility"
# 
#         
#         # remove flows less than 100 tons
#         ret <- ret[ret$Tons_Sent > threshold,]
# 
#         return(ret) 
#     # Export Flows
#     } else if(tolower(flow_type) == "export"){
#         ret <- waste_gen[,c("Origin_City", "Origin_County","Material_Class", "Total_Export_Tons",
#                             "Designated_Facility", "Designated_Full_ID", "Designated_County")]
#         # we could get the actual facilities they go to here same as transfer
#         ret$Designated_Facility <- "Out of State"
#         ret$Designated_Full_ID <- "XX-XX-XXXX-RDXXXXX"
#         ret$Designated_County <- "Out of State"
#         
#         colnames(ret)[which(colnames(ret) == "Total_Export_Tons")] <- "Tons_Sent"
# 
#         ret$Flow_Type <- "Exported Wastes"
#         ret <- ret[ret$Tons_Sent > threshold,]
#         return(ret)
#         
#     # Transfer Flows
#     } else if(tolower(flow_type) == "transfer"){
# 
#         # add the transfer station
#         # sum total tons through each transfer station
#         city_TS <- aggregate(Total_Tons ~ Origin_Jurisdiction + Material_Class +
#                                  Transfer_Full_ID + Transfer_Facility + Transfer_County, 
#                              FUN = sum, data = trans[trans$Flow_Type == "Sent to California disposal via Transfer/Processor",])
#         
# 
#         city_TS <- city_TS[city_TS$Material_Class == material,]
# 
#         # accepts organics?? Skip for now
#         # city_TS <- merge(city_TS, sitewaste[,c("Full_ID", "Mixed_Municipal", "Green_Materials",
#         #                                        "Food_Wastes", "Digestate")],
#         #                  by.x = "trans_Full_ID", by.y = "Full_ID")
#         
#         
#         # each city send a certian amount through a trans station
#         # remove trans stations that accept less than 100 tons solid waste/year (from transfer data)
#         # leave all green waste facilities regardless of size
# 
#         if(material == "Solid Waste") {city_TS <- city_TS[city_TS$Total_Tons > 100,]}
# 
# 
#         
#         city_TS$Origin_Jurisdiction <- as.factor(city_TS$Origin_Jurisdiction)
#         city_TS <- city_TS[order(city_TS$Origin_Jurisdiction),] # have to order to make the lapply work
# 
#         # For each city, calc the % of total waste sent to each trans station
#         city_TS$pct_sent <- unlist(lapply(levels(city_TS$Origin_Jurisdiction), function(x){
#             
#             total <- sum(city_TS$Total_Tons[city_TS$Origin_Jurisdiction == x])
#             ret <- city_TS$Total_Tons[city_TS$Origin_Jurisdiction == x] / total
#             
#             return(ret)}))
# 
#         # remove total tons column bc it confuses me lol
#         # This value comes from the JurisdictionThruTransfer dataset
#         # which we are not using because the material types cannot be separated into organics
#         city_TS <- city_TS[,-which(colnames(city_TS) == "Total_Tons")]
# 
#         
#         # add the transfer stations to waste generation
#         waste_gen <- merge(waste_gen, city_TS, by.x = "Origin_City", by.y = "Origin_Jurisdiction", all.x = TRUE)
#         
#         
#         # calculate tons sent through each trans station in each city
#         waste_gen$Transfer_Tons <- waste_gen$Total_Transfer_Tons * waste_gen$pct_sent
#         
#         waste_gen$Transfer_Tons[is.na(waste_gen$Transfer_Tons)] <- 0
#         
#         # choose columns to return
#         ret <- waste_gen[,c("Origin_City", "Origin_County", "Material_Class.x", "Transfer_Tons",
#                             "Transfer_Full_ID", "Transfer_Facility", "Transfer_County")]
#         
#         colnames(ret)[which(colnames(ret) == "Material_Class.x")] <- "Material_Class"
#         colnames(ret)[which(colnames(ret) == "Transfer_Tons")] <- "Tons_Sent"
# 
#         ret$Flow_Type <- "Transfered Waste"
#         
#         ret <- ret[ret$Tons_Sent > threshold,]
# 
#         return(ret)
#     }else{print("Choose flow_type: direct, export, or transfer")}}
# 


######################################### Get OD Data RDRS #############################
# creates OD matrix from RDRS data for a given county
# @param x = the county
# @param RDRS = the rdrs data
# returns: df $Origin_County, $Destination_County, $Tons_Sent, 
# get_county_OD_data_RDRS <- function(x, RDRS, type = "Total_Tons"){
#     
#     if(type == "Total_Tons"){
#         # to x (destination county)
#         to <- aggregate(Tons_Sent ~ Origin_County + Destination_County,
#                         FUN = sum, 
#                         subset = Destination_County == x,
#                         data = RDRS)
#         to$Destination_County <- x
#         
#         # from x (origin county)
#         from <- aggregate(Tons_Sent ~ Origin_County + Destination_County,
#                           FUN = sum,
#                           subset = Origin_County == x,
#                           data = RDRS)
#         from$Origin_County <- x
#         
#     }else if(type == "Material_Subcategory"){
#         to <- aggregate(Tons_Sent ~ Origin_County + Destination_County + Material_Subcategory,
#                         FUN = sum, 
#                         subset = Destination_County == x,
#                         data = RDRS)
#         to$Destination_County <- x
#         
#         # from x (origin county)
#         from <- aggregate(Tons_Sent ~ Origin_County + Destination_County + Material_Subcategory,
#                           FUN = sum,
#                           subset = Origin_County == x,
#                           data = RDRS)
#         from$Origin_County <- x
#     } else {
#         return("choose type = Total_Tons, Material_Subcategory")
#     }
#     
#     #combine
#     both <- rbind(to, from)
#     both <- both[duplicated(both) == FALSE,]
#    return(both)}


