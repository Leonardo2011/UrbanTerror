#Here we clean the GTD's country data

GTD$country_txt <- as.character(GTD$country_txt)

  # 1. Combining formerly seperated countries

GTD$country_txt[GTD$country_txt == "East Germany (GDR)"] <- "Germany"
GTD$country_txt[GTD$country_txt == "West Germany (FRG)"] <- "Germany"

GTD$country_txt[GTD$country_txt == "North Yemen"] <- "Yemen"
GTD$country_txt[GTD$country_txt == "South Yemen"] <- "Yemen"

GTD$country_txt[GTD$country_txt == "Falkland Islands" ] <- "United Kingdom"

GTD$country_txt[GTD$country_txt == "New Hebrides" ] <- "Vanuatu"

GTD$country_txt[GTD$country_txt == "South Vietnam" ] <- "Vietnam"

  # 2. Splitting up formerly united countries 

  #Since we have WDI data going back 1970 on a per country scale also for countries
  #that were not independent in 1970 (like the Soviet Union countries) we can just treat the cities the attack took place as 
  #being part of the country they are a part of now.

    # 2.1. The Soviet Union
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Yerevan" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Meghri" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Aygepar" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Erubuni" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Charektar" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kirovakan" ] <- "Armenia"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Yevlakh" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Aterk" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Gyandzha" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tatli" ] <- "Azerbaijan"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tallinn" ] <- "Estonia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ikla" ] <- "Estonia"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tbilisi" ] <- "Georgia"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Osh" ] <- "Kyrgyzstan"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Riga" ] <- "Latvia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ainazi" ] <- "Latvia"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Lavoriskes" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Birzai" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Vilnius" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Salociai" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Medininkai" ] <- "Lithuania"

GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Moscow" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Buynaksk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kara-Su" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Novgorod" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Vasyurinskaya" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ostankinskiy" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Grebenskaya" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Temirgoye" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Irkutsk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Sverdlovsk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Leningrad" ] <- "Russia"

    # 2.2. Czechoslovakia
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Prague" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Usti Nad Labem" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Halickuv Brod" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Hradec Kralove" ] <- "Czech Republic"

GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Bratislava" ] <- "Slovak Republic"

    # 2.3. Serbia-Montenegro
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Bujanovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Zarkovo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Presevo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Belgrade" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Dobrosin" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Subotica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Velki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Novi Pazar" ] <- "Serbia"

    # 2.4. Yugoslavia
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bogicevica" ] <- "Albania"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Banjica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Belgrade" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Blace" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Borovica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bujanovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bujic" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cacak" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cerevajka" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dolovo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Konculj" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Lisani" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Loznica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Medvedja" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Muhovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Nis" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Novi Sad" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Oraovica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Popuke" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pozarevac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Subotica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Surcin" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Valjevo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Velika Reka" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Veliki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Velki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Vranje" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Zemun" ] <- "Serbia"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Celopek" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prilep" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Skopje" ] <- "Macedonia, FYR"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Berane" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Budva" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cetinje" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podgorica" ] <- "Montenegro"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Ljubljana" ] <- "Slovenia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stari" ] <- "Slovenia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stari Trg" ] <- "Slovenia"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Benkovac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Borovac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dolac"] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dubrovnik" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dvor na Uni" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kijevo" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Knin" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Lipik" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Osijek" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pakrac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Petrinja" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Polaca" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pribudic" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Sotin" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Titova Korenica" ] <- "Croatia"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Batlava" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Besinje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bradas" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Careva Cesma" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Decani" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Djakovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donje Ljupce" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donje Prekaze" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donji Crnobreg" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dulje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Glogovac" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Gornja Klina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Grabanic‰" ] <- "Kosovo"  ######## This may be an encoding problem
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Grabc" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Istinic" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Klincina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kline" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Komoran" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kosovo Polje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kosovska Mitrovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Krpimej" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kruska" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Ljubenic" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Magur‰" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Malisevo" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Opterusa" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Orlat" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Peje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podujeva" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podujev‰" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prekale" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pristina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prizren" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Runik" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Srbica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stimlje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Streoc" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Surkis" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Suva Reka" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Titova Mitrovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Tusuz" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Urosevac" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Voksh" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Vucitrn" ] <- "Kosovo"

GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Banja Luka" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bijelina" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bosanski Brod" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kalesija" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Mostar" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Odzak" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Sarajevo" ] <- "Bosnia and Herzegovina"

  # 3. Problematic recodings (e.g. contested regions, typos etc.)

  #The following all seem to belong to the Nagorno-Karabakh region, contested by Armenia and Azerbaijan, but currently governed by 
  #Azerbaijan, though populated mostly by Armenians.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Nagorno-Karabakh (Autonomous Oblast)" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Askeran" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Askeran (District)" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Khojali" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Khodzhaly" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Baganis-Airum" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="baganis-airum" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Airum Pirili" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Chaily" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Drmbon" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Stepanakert" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kazakh" ] <- "Azerbaijan"

#This attack was coded as having happened in Serbia-Montenegro, but according to lang-log data, the place the attack occurred is
# in Estonia
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Levosje" ] <- "Estonia"

#These are two attacks in the early 1990s that seem to have targeted the Ararat Region outside of any city.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Unknown" & GTD$provstate == "Armenia" ] <- "Armenia"

#It is on Crimea. We make a judgement call here and go for the country currently in charge.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Feodocia" ] <- "Russia"

# This is Abchasia
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Sukhumi" ] <- "Georgia"

# This is one attack happening in the Gulf of Aden, off the coast of Yemen
GTD$country_txt[GTD$country_txt == "International" ] <- "Yemen"

# 4. Renaming Countries to make them match the WDIData

GTD$country_txt[GTD$country_txt == "Bahamas" ] <- "Bahamas, The"
GTD$country_txt[GTD$country_txt == "Bosnia-Herzegovina" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Central African Republic" ] <- "Central African Republic"
GTD$country_txt[GTD$country_txt == "Congo (Brazzaville)" ] <- "Congo, Dem. Rep."
GTD$country_txt[GTD$country_txt == "Congo (Kinshasa)" ] <- "Congo, Rep."
GTD$country_txt[GTD$country_txt == "Corsica" ] <- "France"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" ] <- ""
GTD$country_txt[GTD$country_txt == "Egypt" ] <- "Egypt, Arab Rep."
GTD$country_txt[GTD$country_txt == "French Guiana" ] <- "France"
GTD$country_txt[GTD$country_txt == "Gambia" ] <- "Gambia, The"
GTD$country_txt[GTD$country_txt == "Great Britain" ] <- "United Kingdom"
GTD$country_txt[GTD$country_txt == "Guadeloupe" ] <- "France"
GTD$country_txt[GTD$country_txt == "Hong Kong" ] <- "Hong Kong SAR, China"
GTD$country_txt[GTD$country_txt == "Iran" ] <- "Iran, Islamic Rep."
GTD$country_txt[GTD$country_txt == "Ivory Coast" ] <- "Cote d'Ivoire"
GTD$country_txt[GTD$country_txt == "Kyrgyzstan" ] <- "Kyrgyz Republic"
GTD$country_txt[GTD$country_txt == "Laos" ] <- "Lao PDR"
GTD$country_txt[GTD$country_txt == "Macau" ] <- "Macao SAR, China"
GTD$country_txt[GTD$country_txt == "Macedonia" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Martinique" ] <- ""
GTD$country_txt[GTD$country_txt == "North Korea" ] <- "Korea, Dem. Rep."
GTD$country_txt[GTD$country_txt == "Northern Ireland" ] <- "United Kingdom"
GTD$country_txt[GTD$country_txt == "Rhodesia" ] <- "Zimbabwe"
GTD$country_txt[GTD$country_txt == "South Korea" ] <- "Korea, Rep."
GTD$country_txt[GTD$country_txt == "Syria" ] <- "Syrian Arab Republic"
GTD$country_txt[GTD$country_txt == "Taiwan" ] <- "China"
GTD$country_txt[GTD$country_txt == "Venezuela" ] <- "Venezuela, RB"
GTD$country_txt[GTD$country_txt == "Wallis and Futuna" ] <- "France"
GTD$country_txt[GTD$country_txt == "West Bank and Gaza Strip" ] <- "West Bank and Gaza"
GTD$country_txt[GTD$country_txt == "Western Sahara" ] <- "Spain"
GTD$country_txt[GTD$country_txt == "Yemen" ] <- "Yemen, Rep."

# create uniform country names like in the other datasets without special characters
X <- GTD$country_txt
source('SmallScripts/delete_country_special_characters.R')
GTD$country_txt <- X
rm(X)




