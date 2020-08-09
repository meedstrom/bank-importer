# nordea_import.R -- Import Nordea CSVs, categorize, and generate Ledger file.
# Copyright (C) 2020 Martin Edström

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# On the problem of assigning IDs to identical datapoints: Consider that the
# total count of transactions in a month will not change after that month. So if
# I re-run the importer on another machine which collates differently and cause
# a pair of identical rows to switch places, say 276 and 277, the rows with IDs
# 275 and 278 are still the same, so there is no real harm done. And anything I
# do after-the-fact to a row that has a duplicate, I'm likely to do to its
# duplicate as well, so the important thing is that the duplicates together have
# the same set of ids.

source("common.R")

nordea_raw <-
    with_dir("nordea_personkonto",
             list.files(pattern = "^export.*csv") %>%
             map(read_csv, locale = locale(decimal_mark = ",")) %>%
             reduce(full_join))

nordea_sane <- nordea_raw %>%
  rename("posted" = "Datum") %>%
  rename("memo"   = "Transaktion") %>%
  rename("amount" = "Belopp") %>%
  arrange(posted, amount) %>% # for reproducible row number
  mutate(id = row_number()) %>%
  select(-Kategori, -Saldo)

to_nordea <- nordea_sane %>%
  filter(amount > 0) %>%
  mutate(credit_to = "Assets:Nordea:Personkonto") %>%
  mutate(debit_from = case_when(
    id %in% c(2297) ~ "Assets:Reimbursements:Lena"
  , id %in% c(2426, 2427, 2428, 2429) ~ "Assets:Reimbursements:Diana"
  , id %in% c(125, 2447) ~ "Assets:Reimbursements:Mischarge" # see 126, 2448
  , id %in% c(2377) ~ "Expenses:Insurance:Home"
  , year(posted) < 2020 & str_detect(memo, "DIANA-OKT|HYRA") ~ "Assets:Reimbursements:Diana"
  , year(posted) < 2020 & str_detect(memo, "DIANA.MAT|MAT-OKT|MATKOSTNAD") ~ "Expenses:Groceries"
  , year(posted) < 2020 & str_detect(memo, "IBIS|TÅG") ~ "Expenses:Travel"
  , year(posted) < 2020 & str_detect(memo, "520M|DIANA|^D |PIZZA|WIFIKORT") ~ "Income:Unsorted"
  , year(posted) < 2020 & str_detect(memo, "IPADPENNA|Lena Andersson|FRÅN MAMMA|GRATTIS ..ÅR|MATTA|RESA|MATKURSBOK|KLÄDER|FÖDELSEDAG") ~ "Income:Gifts:Lena"
  , year(posted) %in% 2018:2019 & str_detect(memo, "ARVID|EDSTRÖM,RICKARD") ~ "Assets:Reimbursements"
  , str_detect(memo, "TRA345119919") ~ "Income:Sales" # Surface Pro 4
  , str_detect(memo, "MULBERRY") ~ "Income:Gifts:Diana"
  , str_detect(memo, "Återköp.*UNIQLO") ~ "Liabilities:Diana"
  , str_detect(memo, "DIANA|YY:") ~ "Assets:Reimbursements:Diana"
  , str_detect(memo, "Överföring.*EDSTRÖM,JOEL") ~ "Income:Sales" # Macbook
  , str_detect(memo, "Swish.*EDSTRÖM,JOEL") ~ "Income:Sales"
  , str_detect(memo, "Swish.*MIODRAG") ~ "Assets:Reimbursements:Miodrag"
  , str_detect(memo, "Swish.*ALIREZA") ~ "Assets:Reimbursements:Alireza"
  , str_detect(memo, "Swish.*CLARENCE") ~ "Assets:Reimbursements:Clarence"
  , str_detect(memo, "Swish.*FABIAN") ~ "Assets:Reimbursements:Fabian"
  , str_detect(memo, "Studiestöd|Utbetalningskort") ~ "Income:CSN" # NOTE: check if there's a debt portion
  , str_detect(memo, "Lön|UTLÄGG TSC") ~ "Income:Salary"
  , str_detect(memo, "HE/VB|Ersättn från Fkassan") ~ "Income:HK-bidrag"
  , str_detect(memo, "Rättelse|ÅTER") ~ "Assets:Reimbursements:Mischarge"
  , str_detect(memo, "Deposition") ~ "Assets:Reimbursements"
  , str_detect(memo, "Återköp") ~ "Income:Returns"
  , str_detect(memo, "Skatt") ~ "Income:Tax-Returns"
  , str_detect(memo, "3654 22 44077") ~ "Assets:Nordea:Kapitalkonto"
  , str_detect(memo, "3036 34 56171") ~ "Assets:Nordea:Kontantinsatsen"
  , str_detect(memo, "3036 03 72589") ~ "Assets:Nordea:Kortkonto"
  , TRUE ~ "Income:Unsorted"
  ))

from_nordea <- nordea_sane %>%
  filter(amount < 0) %>%
  mutate(amount = -amount) %>%
  mutate(debit_from = "Assets:Nordea:Personkonto") %>%
  mutate(credit_to = case_when(
    id %in% c(2186) ~ "Expenses:Gifts"
  , id %in% c(126, 2448) ~ "Assets:Reimbursements:Mischarge" # See 125, 2447
  , id %in% c(2304, 2420, 2442) ~ "Assets:Reimbursements:Clarence"
  , year(posted) < 2020 & str_detect(memo, "OMBORD|TÖNNEBRO|VEOLIA FJERRTAG|SNELLTAGET KROGEN|AAKROKEN|FRESCATI STORIES|SSCO SERVICE|SSCO Service|F SEKTIONEN|PG GOURMET|Swish.*TECKENSP") ~ "Expenses:Cafe"
  , year(posted) < 2020 & str_detect(memo, "AUDIO VIDEO|ITUNES|Tommy Öhman|191023 TRADERA") ~ "Expenses:Telephony"
  , year(posted) < 2020 & str_detect(memo, "HALLBERGS GULD|TRAMPUSH|SOSTRENE|ZALANDO|LAGERHAUS|Lustjakt") ~ "Expenses:Gifts:Diana"
  , str_detect(memo, "200623 AMZN") ~ "Assets:Reimbursements"
  , str_detect(memo, "181123 CIRCLE K") ~ "Expenses:Transport:Car"
  , str_detect(memo, "200608 CLAS OHLSON|200525 AMZN") ~ "Expenses:Home"
  , str_detect(memo, "200727.*TEKNIK") ~ "Expenses:Donations"
  , str_detect(memo, "200520.*Apotea|200727.*Apohem") ~ "Expenses:Health:Cosmetics"
  , str_detect(memo, "161125 PAYPAL|SAROS FARIGHI|HENRIKFABIA") ~ "Expenses:Tech"
  , str_detect(memo, "BG 520-0878") ~ "Expenses:Fines"
  , str_detect(memo, "Martin owed|200525 TRADERA|Euroflorist") ~ "Expenses:Gifts:Diana"
  , str_detect(memo, "200531 SELLPY|200603 KAHLS") ~ "Expenses:Groceries:Coffee"
  , str_detect(memo, "RAWCOFFEE|BARISTA|ROAST") ~ "Expenses:Groceries:Coffee"
  , str_detect(memo, "MNordin Skogstj|Rafnaslakt") ~ "Expenses:Groceries" # naturbete
  , str_detect(memo, "Martin owed") ~ "Expenses:Gifts:Diana"
  , str_detect(memo, "AIKO|LUND OLYMPEN|NAMDO|KIMCHISTAN|VALVET|KHATOON|BITE LINE|KITCHN EMPORIA|MAX NOVA|ROSEGARDEN|MAX SUNDSVALL|TRATTORIA|SPISA|MATVERKSTAD|Delivery Hero|VEZZO|GAMBRINUS|CHINA|MAX I SUNDSVALL|PANINI|HALONG|SIBYLLA|MAX SUNDSVALL|GOTT OCH GRÖNT|BAGUETTE") ~ "Expenses:Restaurant"
  , str_detect(memo, "OLEARYS|LUCKY BAR|HUMANISTISKA F|NATURVETENSKAPLIGA F|THE BISHOP|BOJAN|LION BAR") ~ "Expenses:Restaurant:Pub"
  , str_detect(memo, "INTO MUSIC") ~ "Expenses:Other:Music-Instrument"
  , str_detect(memo, "BOPLATS SYD") ~ "Expenses:Lodging"
  , str_detect(memo, "PRESSBYRÅN|CAFE|COFFEE|Coffee|7 ELEVEN|ESPRESSO|KIOSK|Selecta") ~ "Expenses:Cafe"
  , str_detect(memo, "REKO LIFE|MASSAGE|BODYSTORE|Upgrit|BERLINDA|DELPHINENB") ~ "Expenses:Fitness"
  , str_detect(memo, "TUYANA|AF BOSTÄDER|Hyra|SHURGARD|Svenska Turistf.reni|STF") ~ "Expenses:Lodging"
  , str_detect(memo, "IBIS|Flixbus|GoEuro|AIR FRANCE|POLISEN") ~ "Expenses:Travel"
  , str_detect(memo, "SUNDSVALL CENTRUM|REGION SKANE|Region Skåne|DINTUR|DinTur|SKANETRAFIK|SKÅNETRAFIKEN|Skanetrafik|SL [BANDHAGEN|HÖTORGET|OSTER|T|UNI|KALL]|127.+?UNIV|147.+?BAND|AB STORSTOCKHOLMS LO|KOMM.FÖRB.KOLLEKTIVT|CITYBANAN") ~ "Expenses:Transport:Kollektivtrafik"
  , str_detect(memo, "SJ INTERNET|SJ AB|TOALETT") ~ "Expenses:Transport:Train"
  , str_detect(memo, "ALEXANDER BYRINDER|BIKE|Bikester|Jessica Ljung") ~ "Expenses:Transport:Bicycle"
  , str_detect(memo, "ACCENT|SABAAI|tradera sock|UR O PENN|CAMILLA MALMSTROMS|LINDEX|CUBUS|KAPPAHL|JCDECAUX|HUMANA|H M|INTERSPORT|ANDRAHAND|MYRORNA|MQ|STADIUM|LEVIS|XXL|HM SE|HOM SE|ERIKSHJ|ERIKHJ|DRESSMANN|Brothers|190716 TRADERA") ~ "Expenses:Clothes"
  , str_detect(memo, "180426 MEKONOMEN|BAREFOOTWOR|XEROSHOES|DIN SKO|SCORETT|ECCO") ~ "Expenses:Clothes:Shoes"
  , str_detect(memo, "190720 Arlanda T2 Tax Free|Guapo|Lyko|QUALITYRAZORBLAD|180725 LIFE|180731 LUNDS HALSABUTIK") ~ "Expenses:Health:Cosmetics"
  , str_detect(memo, "EHL BIBLIOTEK") ~ "Expenses:Late-Fees"
  , str_detect(memo, "BIBLIOTE|PayEx") ~ "Expenses:Printer"
  , str_detect(memo, "Werlabs|SJUKHUS|APOTEK|VÅRDCENTRAL|KAROLINSKA|LANDSTINGET VÄS|ST ERIKS VC|Region Västerno") ~ "Expenses:Health"
  , str_detect(memo, "IKEA|JYSK|HK SUNDSV|TEHUSET JAVA") ~ "Expenses:Home"
  , str_detect(memo, "BOKUS|Adlibris") ~ "Expenses:Bookstore:Books"
  , str_detect(memo, "OFFICE DEPOT|CAMPUSBUTIKEN|AKADEMIBOK") ~ "Expenses:Bookstore"
  , str_detect(memo, "PLANTAGEN|BLOMSTERLANDET|GRANNGÅRDEN|BLOMMAN") ~ "Expenses:Home:Plants"
  , str_detect(memo, "SPECSAVERS|Lensway|ZENNI") ~ "Expenses:Health:Eyesight"
  , str_detect(memo, "mullv|mullmorgon") ~ "Expenses:VPN"
  , str_detect(memo, "Swish.*LIND,FABIAN") ~ "Assets:Reimbursements:Fabian"
  , str_detect(memo, "Swish.*SAMER MEHJAZI") ~ "Assets:Reimbursements:Samer"
  , str_detect(memo, "Swish.*Lena") & amount == 2400 ~ "Expenses:Transport:Car"
  , str_detect(memo, "Swish.*CARINA ÅKERSTRÖM") ~ "Expenses:Gifts"
  , str_detect(memo, "Swish.*CLARENCE") & amount == 150 ~ "Expenses:Gifts"
  , str_detect(memo, "Swish.*SAMMER") & amount == 200 ~ "Expenses:Telephony"
  , str_detect(memo, "Autogiro FITNESS24|ACTIC") ~ "Expenses:Fitness:Gym"
  , str_detect(memo, "Autogiro FOLKSAM|Folksam") ~ "Expenses:Insurance:Home"
  , str_detect(memo, "Autogiro FOLKT") ~ "Expenses:Insurance:Dental"
  , str_detect(memo, "OKQ8|PREEM|SHELL|INGO|ST1|GULF|CIRCLE K") & amount > 74 ~ "Expenses:Transport:Car:Fuel"
  , str_detect(memo, "OKQ8|PREEM|SHELL|INGO|ST1|GULF|CIRCLE K") & amount <= 74 ~ "Expenses:Cafe"
  , str_detect(memo, "Bankomat") ~ "Assets:Cash"
  , str_detect(memo, "REVOLUT") ~ "Assets:Revolut"
  , str_detect(memo, "ELGIGANTEN|Elgiganten|TEKNIKMAGASINET|Micro Kit|LAPTOP|KJELL") ~ "Expenses:Tech"
  , str_detect(memo, "SUNDSVALLS KOMMUN|PARKERING|PARKSTER") ~ "Expenses:Transport:Car"
  , str_detect(memo, "CENTRALA STUDIE") ~ "Liabilities:CSN" # TODO: track the administration fee
  , str_detect(memo, "Månadspris kort|Vardagspaketet|Kontoutdragsavgift") ~ "Expenses:Bank-Fees"
  , str_detect(memo, "STOCKHOLMS UNIVERSIT|SEKTIONEN EKONOMI|SVERIGES DÖVAS|Terminsräkningsf|TERMINSREKNING") ~ "Expenses:Memberships"
  , str_detect(memo, "3654 22 44077") ~ "Assets:Nordea:Kapitalkonto"
  , str_detect(memo, "3036 34 56171") ~ "Assets:Nordea:Kontantinsatsen"
  , str_detect(memo, "3036 03 72589") ~ "Assets:Nordea:Kortkonto"
  , str_detect(memo, "VIMLA|TELENOR|COMVIQ|TELE2") ~ "Expenses:Telephony"
  , str_detect(memo, "Donation") ~ "Expenses:Donations"
  , str_detect(memo, "SYSTEMBOLAGET") ~ "Expenses:Alcohol"
  , str_detect(memo, "AVANZA|95545739590") ~ "Assets:Avanza"
  , str_detect(memo, "GOG COM") ~ "Expenses:Games"
  , str_detect(memo, "F.RENING") ~ "Expenses:Memberships"
  , str_detect(memo, "SUSHI|REST|GRILL|PIZZ|SUBWAY|KEBAB|BURGER|Delivery Hero|MCDON") ~ "Expenses:Restaurant"
  , str_detect(memo, "ICA|VALLES|THAI BUTIK|OOB SUNDS|COOP|LIDL|WILLYS|HEMKÖP|OOB|CITY GROSS|MATDAX|CANDY|LIVS|NETTO") ~ "Expenses:Groceries"
  , str_detect(memo, "821499135650001") ~ "Assets:Reimbursements:Diana"
  , TRUE ~ "Expenses:Unsorted"
  ))

# Reasonable!
to_nordea$amount %>% sum
from_nordea$amount %>% sum

nordea_merged <-
  full_join(to_nordea, from_nordea) %>%
  mutate(ledger_output = slide(., with, str_glue(
    "{posted} * \"{memo}\"
  ; id: {id}
  {credit_to}        {amount} SEK
  {debit_from}
")))

nordea_final$amount %>% sum

# Datapoints to be manually edited
exception_ids <- c(
  252  # from Diana: 1600 rent 700 groceries
, 1893, 1894, 1895, 1898, 1899  # JOHANNA ANDERSSON
  # , 2147, 2148 # Julbord pub (180 cash)
, 2169  # Uniqlo (900 bras half-reimbursed, 200 umbrella)
, 2324 # 138 me, 78 Clarence
, 2417 # ahlens (my part cost 40)
, 2430 # uniqlo (my part cost 149)
, 2449 # dermastore perfumes (my part cost 359.10 + 566.10)
, 2451 # 89 Clarence, 252 me, 79 split w/ Diana
, 2499 # porcelain (200 me, 150 Diana)
)

not_split_ids <- c(
  2410, 2418 # coffee things
, 2402, 2464, 2481, 2488, 2492 # food/alcohol during
  # , 2505, 2506, 2507, 2514 # food/alcohol when Diana left
)

split_with_diana <- nordea_merged %>%
  filter(not(id %in% exception_ids) &
           not(id %in% not_split_ids) &
           posted >= ymd("2020-06-01") & posted <= ymd("2020-07-20") &
           #month(posted) %in% c(6, 7) & year(posted) == 2020 &
           str_detect(credit_to, "Groceries|Fuel|Cafe|Alcohol|Restaurant")
  ) %>%
  mutate(ledger_output = slide(., with, str_glue(
           "{posted} * \"{memo}\"
  ; id: {id}
  ; full amount: {amount}
  {credit_to}        {amount/2} SEK
  Assets:Reimbursements:Diana      {amount/2} SEK
  {debit_from}
")))

# Drop observations to treat manually
nordea_final_exceptions <-
  filter(nordea_merged,
         id %in% exception_ids
         | str_detect(memo, "FOREX")
         | str_detect(memo, "3029 01 14195|AF BOSTÄDER")
         | (str_detect(memo, "Studiestöd|Utbetalningskort") & posted > ymd("2016-06-01") & posted < ymd("2018-06-01"))
         ) %>%
  arrange(id)

nordea_final <- nordea_merged %>%
  anti_join(nordea_final_exceptions, by = "id") %>%
  anti_join(split_with_diana, by = "id") %>%
  full_join(split_with_diana) %>% # For updated ledger_output
  arrange(id)

# Write a ledger file
invisible(suppressWarnings(file.remove("nordea_imported.ledger")))
walk(nordea_final$ledger_output,
     write_lines,
     path = "nordea_imported.ledger",
     append = TRUE)

# Write the exceptions to a different file for manual editing
invisible(suppressWarnings(file.remove("nordea_imported_exceptions.ledger")))
walk(nordea_final_exceptions$ledger_output,
     write_lines,
     path = "nordea_imported_exceptions.ledger",
     append = TRUE)
