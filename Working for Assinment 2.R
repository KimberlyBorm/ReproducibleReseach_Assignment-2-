
#Download and read in data
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    destfile <- sprintf("StormData_%s.csv.bz2", timestamp)    
    #download file
    if(!file.exists(destfile)) {
        download.file(url, destfile, mode = "wb")
    }
    #print to report 
    timestamp <- as.POSIXct(timestamp, format = "%Y%m%d_%H%M%S") 
    human_readable <- format(timestamp, "%B %d, %Y %H:%M:%S")
    print(human_readable)
    #read compressed csv file
    stormdata <- read.csv(bzfile(destfile))
    #check data
    str(stormdata)
    names(stormdata)
    
#1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
    #find most deadly event types by looking at only fatalities first, later explore fatalities and injuries combine. 
        #load required packages 
        library(dplyr)
        library(ggplot2)
        #group fatalities by event type
        fatalities_by_event <- stormdata %>%
            group_by(EVTYPE) %>%
            summarise(total_fatalities = sum(FATALITIES, na.rm = TRUE)) %>%
            filter(total_fatalities > 0) %>%
            arrange(desc(total_fatalities))
        #select the top 5
        top_fatalities <- fatalities_by_event %>% top_n(5, total_fatalities)
        #create a horizontal bar plot
        ggplot(top_fatalities, aes(x = reorder(EVTYPE, total_fatalities), y = total_fatalities)) +
            geom_bar(stat = "identity", fill = "red") +
            coord_flip() +
            labs(title = "Top 13 Event Types by Fatalities",
                 x = "Event Type",
                 y = "Total Fatalities")
#From this figure we can see that tornadoes are the most dangerous weather event for overall deaths and excessive heat is next. Knowing the percent of averall deaths due to storm related fatalities can help put thier impact in perspective. 
        
    #Print percentage of overall fatalities 
        #find overall fatalities from the dataset
        overall_fatalities <- sum(stormdata$FATALITIES, na.rm = TRUE)
        #find the percentage contribution for each top event
        top_events <- top_events %>%
            mutate(percentage = total_fatalities / overall_fatalities * 100)
        #print
        head(top_events, 5)
        # Display the table
        print(top_events)
# Tornadoes comprise to about 37% of storm related fatalities and tornadoes and excessive heat combined comprises about 50% of all storm related fatalities. 
##Now let's explore population health overall by looking at fatalities and injuries combined. 
    
    #explore fatalities and injuries combined
        #find most dangerous events by combining fatalities and injuries
            health_impact <- stormdata %>%
                group_by(EVTYPE) %>%
                summarise(total_health = sum(FATALITIES + INJURIES, na.rm = TRUE)) %>%
                filter(total_health > 0) %>%
                arrange(desc(total_health))
            #reorder data
            top_health <- health_impact %>% top_n(5, total_health)
            
            ggplot(top_health, aes(x = reorder(EVTYPE, total_health), y = total_health)) +
                geom_bar(stat = "identity", fill = "blue") +
                coord_flip() +
                labs(title = "Top 10 Event Types by Total Health Impact",
                     x = "Total Health Impact (Fatalities + Injuries)",
                     y = "Event Type")
#This shows that tornado are the most harmful to overall health. Excessive heat is next but a much smaller portion of risk to population health. 
    #Print percentage of overall fatalities 
            #find overall injuries from the dataset
            overall_health <- sum(stormdata$FATALITIES + stormdata$INJURIES, na.rm = TRUE)
            #find the percentage contribution for each top event
            top_health <- top_health %>%
                mutate(percentage = total_health / overall_health * 100)
            #print
            head(top_health, 5)
#Tornadoes account for about 62% of all storm related injuries and deaths, and all others events account for less than 6%. 
#this confirms that tonadoes aer the largest risk to population health of all storm related events. 

        
#2. Across the United States, which types of events have the greatest economic consequences?

#Economic consequences can be measured in the combine cost of property and crop damage. an important note is that the damage expont is lists in a seperate column from hte amount and we will have to mutate the data to reflect the actual dollar amount of damage. 
            #convert symbols to multipliers
            exp_to_multiplier <- function(exp) {
                if(exp %in% c("", "0")) {
                    1
                } else if(exp %in% c("K", "k")) {
                    1e3
                } else if(exp %in% c("M", "m")) {
                    1e6
                } else if(exp %in% c("B", "b")) {
                    1e9
                } else {
                    1
                }
            }
            # Create multiplier columns for property and crop damage
            stormdata$prop_multiplier <- sapply(stormdata$PROPDMGEXP, exp_to_multiplier)
            stormdata$crop_multiplier <- sapply(stormdata$CROPDMGEXP, exp_to_multiplier)
            # Calculate property and crop damages in dollars
            stormdata$prop_dollars <- stormdata$PROPDMG * stormdata$prop_multiplier
            stormdata$crop_dollars <- stormdata$CROPDMG * stormdata$crop_multiplier
            #find total economic damage
            stormdata$total_damage <- stormdata$prop_dollars + stormdata$crop_dollars
           
             # find total damage by event type
            damage_by_event <- stormdata %>%
                group_by(EVTYPE) %>%
                summarise(total_damage = sum(total_damage, na.rm = TRUE)) %>%
                filter(total_damage > 0) %>%
                arrange(desc(total_damage))
            # select  top 10 events
            top_damage <- damage_by_event %>% top_n(10, total_damage)
     # Create a horizontal bar plot of the top events by economic damage
            ggplot(top_damage, aes(x = reorder(EVTYPE, total_damage), y = total_damage)) +
                geom_bar(stat = "identity", fill = "orange") +
                coord_flip() +
                labs(title = "Top Event Types by Economic Consequences",
                     x = "Event Type",
                     y = "Total Damage (USD)") +
                scale_y_continuous(labels = scales::dollar_format())
#Flood contribute to the largest economic damage, followed by ttyphoons/hurricanes, and tornadoes. 
#It is worth noting that flood damage is more than double the damage cause by any other weather event. 
    #calculate percent of overall economic damage  
            #find total damage by event type
            damage_by_event <- stormdata %>%
                group_by(EVTYPE) %>%
                summarise(total_damage = sum(total_damage, na.rm = TRUE)) %>%
                filter(total_damage > 0) %>%
                arrange(desc(total_damage))
            #select the top 10 (or top 5) event types
            top_damage <- damage_by_event %>% top_n(10, total_damage)
            #find overall economic damage across all events
            overall_damage <- sum(stormdata$total_damage, na.rm = TRUE)
            #find percentage contribution for each top event
            top_damage <- top_damage %>%
                mutate(percentage = total_damage / overall_damage * 100)
            #print the table to see the results
            head(top_damage, 3)

            