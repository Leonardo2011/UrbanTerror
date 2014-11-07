#Here we clean the GTD's country data

C <- GTD2$country_txt
C <- as.character(C)

# Combining formerly seperated countries

GTD2$country_txt[GTD2$country_txt == "East Germany (GDR)"] <- "Germany"
GTD2$country_txt[GTD2$country_txt == "West Germany (FRG)"] <- "Germany"

#Splitting up formerly united countries. Since we have WDI data going back 1970 on a per country scale also for countries
#that were not independent in 1970 (like the Soviet Union countries) we can just treat the cities the attack took place as 
#being part of the country they are a part of now.

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Moscow" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Yerevan" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ostankinskiy" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Sukhumi" ] <- "Georgia" # It is Abchasia, just fyc
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Yevlakh" ] <- "Azerbaijan"

#The following all seem to belong to the Nagorno-Karabakh region, contested by Armenia and Azerbaijan, but currently governed by 
#Azerbaijan, though populated mostly by Armenians.
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Nagorno-Karabakh (Autonomous Oblast)" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Askeran" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Askeran (District)" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Khojali" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Khodzhaly" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Baganis-Airum" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="baganis-airum" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Airum Pirili" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Chaily" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Drmbon" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Stepanakert" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Kazakh" ] <- "Azerbaijan"
###################################
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Buynaksk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Aterk" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Meghri" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Aygepar" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Erubuni" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Osh" ] <- "Kyrgyzstan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Kara-Su" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Charektar" ] <- "Armenia"
#These are two attacks in the early 1990s that seem to have targeted the Ararat Region outside of any city.
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Unknown" & GTD$provstate == "Armenia" ] <- "Armenia"
###################################
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Gyandzha" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Kirovakan" ] <- "Armenia"
#It is on Crimea. We make a judgement call here and go for the country currently in charge.
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Feodocia" ] <- "Russia"
####################################
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Novgorod" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Vasyurinskaya" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tbilisi" ] <- "Georgia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tallinn" ] <- "Estonia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Leningrad" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Riga" ] <- "Latvia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Sverdlovsk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tatli" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Lavoriskes" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Birzai" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Vilnius" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Salociai" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ikla" ] <- "Estonia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ainazi" ] <- "Latvia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Grebenskaya" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Temirgoye" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Irkutsk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Medininkai" ] <- "Lithuania"





GTD.sub <- subset(GTD2, GTD2$country_txt == "Soviet Union")
