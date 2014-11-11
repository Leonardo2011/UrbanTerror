#Here we clean the GTD's country data



  # 1. Combining formerly seperated countries

GTD2$country_txt[GTD2$country_txt == "East Germany (GDR)"] <- "Germany"
GTD2$country_txt[GTD2$country_txt == "West Germany (FRG)"] <- "Germany"

GTD2$country_txt[GTD2$country_txt == "North Yemen"] <- "Yemen"
GTD2$country_txt[GTD2$country_txt == "South Yemen"] <- "Yemen"

GTD2$country_txt[GTD2$country_txt == "Falkland Islands" ] <- "United Kingdom"

GTD2$country_txt[GTD2$country_txt == "New Hebrides" ] <- "Vanuatu"

GTD2$country_txt[GTD2$country_txt == "South Vietnam" ] <- "Vietnam"

  # 2. Splitting up formerly united countries 

  #Since we have WDI data going back 1970 on a per country scale also for countries
  #that were not independent in 1970 (like the Soviet Union countries) we can just treat the cities the attack took place as 
  #being part of the country they are a part of now.

    # 2.1. The Soviet Union
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Yerevan" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Meghri" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Aygepar" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Erubuni" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Charektar" ] <- "Armenia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Kirovakan" ] <- "Armenia"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Yevlakh" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Aterk" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Gyandzha" ] <- "Azerbaijan"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tatli" ] <- "Azerbaijan"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tallinn" ] <- "Estonia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ikla" ] <- "Estonia"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Tbilisi" ] <- "Georgia"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Osh" ] <- "Kyrgyzstan"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Riga" ] <- "Latvia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ainazi" ] <- "Latvia"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Lavoriskes" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Birzai" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Vilnius" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Salociai" ] <- "Lithuania"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Medininkai" ] <- "Lithuania"

GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Moscow" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Buynaksk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Kara-Su" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Novgorod" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Vasyurinskaya" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Ostankinskiy" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Grebenskaya" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Temirgoye" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Irkutsk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Sverdlovsk" ] <- "Russia"
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Leningrad" ] <- "Russia"

    # 2.2. Czechoslovakia
GTD2$country_txt[GTD2$country_txt == "Czechoslovakia" & GTD2$city =="Prague" ] <- "Czech Republic"
GTD2$country_txt[GTD2$country_txt == "Czechoslovakia" & GTD2$city =="Usti Nad Labem" ] <- "Czech Republic"
GTD2$country_txt[GTD2$country_txt == "Czechoslovakia" & GTD2$city =="Halickuv Brod" ] <- "Czech Republic"
GTD2$country_txt[GTD2$country_txt == "Czechoslovakia" & GTD2$city =="Hradec Kralove" ] <- "Czech Republic"

GTD2$country_txt[GTD2$country_txt == "Czechoslovakia" & GTD2$city =="Bratislava" ] <- "Slovak Republic"

    # 2.3. Serbia-Montenegro
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Bujanovac" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Zarkovo" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Presevo" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Belgrade" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Dobrosin" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Subotica" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Velki Trnovac" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Novi Pazar" ] <- "Serbia"

    # 2.4. Yugoslavia
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bogicevica" ] <- "Albania"


GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Belgrade" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Banjica" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Blace" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Borovica" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bujanovac" ] <- "Serbia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bujic" ] <- "Serbia"

GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Skopje" ] <- "Macedonia, FYR"

GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Berane" ] <- "Montenegro"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Budva" ] <- "Montenegro"


GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Ljubljana" ] <- "Slovenia"


GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Dubrovnik" ] <- "Croatia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Osijek" ] <- "Croatia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Benkovac" ] <- "Croatia"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Borovac" ] <- "Croatia"

GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Pristina" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Suva Reka" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Podujeva" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Titova Mitrovica" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Batlava" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Besinje" ] <- "Kosovo"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bradas" ] <- "Kosovo"

GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Banja Luka" ] <- "Bosnia and Herzegovina"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bijelina" ] <- "Bosnia and Herzegovina"
GTD2$country_txt[GTD2$country_txt == "Yugoslavia" & GTD2$city =="Bosanski Brod" ] <- "Bosnia and Herzegovina"

  # 3. Problematic recodings (e.g. contested regions, typos etc.)

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

#This attack was coded as having happened in Serbia-Montenegro, but according to lang-log data, the place the attack occurred is
# in Estonia
GTD2$country_txt[GTD2$country_txt == "Serbia-Montenegro" & GTD2$city =="Levosje" ] <- "Estonia"

#These are two attacks in the early 1990s that seem to have targeted the Ararat Region outside of any city.
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Unknown" & GTD$provstate == "Armenia" ] <- "Armenia"

#It is on Crimea. We make a judgement call here and go for the country currently in charge.
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Feodocia" ] <- "Russia"

# This is Abchasia
GTD2$country_txt[GTD2$country_txt == "Soviet Union" & GTD2$city =="Sukhumi" ] <- "Georgia"

# This is one attack happening in the Gulf of Aden, off the coast of Yemen
GTD2$country_txt[GTD2$country_txt == "International" ] <- "Yemen"

# 4. Renaming Countries to make them match the WDIData
GTD2$country_txt[GTD2$country_txt == "West Bank and Gaza Strip" ] <- "West Bank and Gaza"
GTD2$country_txt[GTD2$country_txt == "Russia" ] <- "Russian Federation"
GTD2$country_txt[GTD2$country_txt == "Rhodesia" ] <- "Zimbabwe"
GTD2$country_txt[GTD2$country_txt == "Congo (Kinshasa)" ] <- "Congo, Dem. Rep."
GTD2$country_txt[GTD2$country_txt == "Congo (Brazzaville)" ] <- "Congo, Rep."
GTD2$country_txt[GTD2$country_txt == "Hong Kong" ] <- "Hong Kong SAR, China"
GTD2$country_txt[GTD2$country_txt == "Great Britain" ] <- "United Kingdom"
GTD2$country_txt[GTD2$country_txt == "Macedonia" ] <- "Macedonia, FYR"

#Just for testing whether we eliminated all.
GTD.sub <- subset(GTD2, GTD2$country_txt == "Soviet Union")

liste <- data.frame(c(unique(WDIData$country)), unique(GTD2$country_txt))
