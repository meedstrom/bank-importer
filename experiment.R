# experiment.R -- just a scratchpad, does not produce anything
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

csn_transactions <- nordea_merged %>%
  filter(
    str_detect(debit_from, "Liabilities:CSN")
    | str_detect(credit_to, "Liabilities:CSN")
  )

totals_csn <- csn_transactions %>%
    group_by(credit_to, debit_from) %>%
    summarise(sum(belopp))

# Hypothetical amount owed if it was all loans. But part of it is income, not loans. Figure out how much, from CSN.se.
exec(subtract, !!!pluck(totals_csn, "sum(belopp)"))

# Amount owed, based on CSN emails
csn_based_on_mail <-
    tribble(~datum,            ~subsidy,   ~loan
          , ymd("2016-08-29"),  1396-150,   3564
          , ymd("2016-09-23"),  1396,       3564
          , ymd("2016-10-25"),  1396,       3564
            # (My bank records only go back this far)
          , ymd("2016-11-25"),  1396,       3564
          , ymd("2016-12-23"),  1404,       3584
          , ymd("2017-01-16"),  4272-150,  10752
          , ymd("2017-02-24"),  2848,       7168
          , ymd("2017-03-24"),  2848,       7168 
          , ymd("2017-04-25"),  2848,       7168
          , ymd("2017-05-24"),  1424,       3584
          , ymd("2017-08-28"),  3560-150,   8960
          , ymd("2017-09-25"),  2848,       7168
          , ymd("2017-10-25"),  2848,       7168
          , ymd("2017-11-24"),  2848,       7168
          , ymd("2017-12-22"),  2136,       5376) %>%
    mutate(belopp = subsidy + loan)

actual_payments <-
    tribble(~datum, ~fees, ~interest, ~paid
          , ymd("2019-03-25"), 600, 34, 1282+316
          , ymd("2019-08-29"),   0, 60, 2612
          , ymd("2019-11-27"),   0, 34, 2637)

paid_back <- sum(actual_payments$paid)

print(csn_based_on_mail)
print(csn_transactions)

# Amount loaned, if the mails are to be believed. Subtract what I know I've paid.
sum(csn_based_on_mail$loan)
sum(csn_based_on_mail$loan) - paid_back

# The loan portion averages 72%
mean(csn_based_on_mail$loan / csn_based_on_mail$belopp)

# Amount loaned, if bank transactions are to be believed, assuming 72% loan portion. Here, three months are missing -- totaling about 10 000.
totals_csn[1,3] * 0.72
totals_csn[1,3] * 0.72 - paid_back

# Hypothetical amount owed if the loan portion is 72%
exec(subtract, !!!pluck(totals_csn, "sum(belopp)")) * 0.72
