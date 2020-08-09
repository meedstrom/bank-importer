# avanza_import.R -- Categorize Avanza data and output Ledger file.
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

source("common.R")

avanza_raw <- read_csv2("transaktioner_20160613_20200731.csv", na = "-")

avanza_sane <- avanza_raw %>%
  rename("posted"     = "Datum") %>%
  rename("type"       = "Typ av transaktion") %>%
  rename("payee"      = "Värdepapper/beskrivning") %>%
  rename("currency"   = "Valuta") %>%
  rename("amount"     = "Belopp") %>%
  rename("lot_value"  = "Kurs") %>%
  rename("lots"       = "Antal") %>%
  arrange(posted, amount) %>% # for reproducible row number
  mutate(id = row_number() + 900000) %>%
  filter(not(id == 900001)) %>% # posted before my "beginning of time" 2016-11-01
  filter(str_detect(type, "Köp|Utdelning")) %>%
  mutate(debit_from = if_else(amount < 0, "Assets:Avanza", "Income:Dividends")) %>%
  mutate(credit_to  = if_else(amount < 0, "Assets:Stocks", "Assets:Avanza")) %>%
  mutate(amount     = if_else(amount < 0, -amount, amount))

avanza_final <- avanza_sane %>%
  mutate(commodity = case_when(
    payee == "Hennes & Mauritz B" ~ "HMB"
  , payee == "Castellum" ~ "CAST"
  , payee == "Sampo Oyj A" ~ "SAMPO"
  , payee == "Avanza Global" ~ "AVAGLOBAL"
  , payee == "Spiltan Aktiefond Investmentbolag" ~ "SPILTAN"
  )) %>%
  mutate(price = if_else(
    credit_to == "Assets:Stocks",
    str_glue("{lots} {commodity} {{{lot_value} {currency}}}"),
    str_glue("{amount} {currency}")
  )) %>%
  mutate(fee = if_else(is.na(Courtage), str_glue(""), str_glue(
    "\n  Expenses:Courtage    {Courtage} SEK", .trim = FALSE))) %>%
  mutate(ledger_output = slide(., with, str_glue(
    "{posted} * \"{payee}\"
  ; id:{id}
  {credit_to}        {price}{fee}
  {debit_from}
"))) %>%
  arrange(id)

test_that("no unclassified datapoints",
          with(avanza_final, expect_false(anyNA(c(debit_from, credit_to)))))

suppressWarnings(file.remove("avanza_imported.ledger"))
walk(avanza_final$ledger_output,
     write_lines,
     path = "avanza_imported.ledger",
     append = TRUE)
