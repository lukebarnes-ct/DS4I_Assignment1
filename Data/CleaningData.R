
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)


## Cleaning Speech Data into usable dataframes

filenames = c('1994_post_elections_Mandela.txt', '1994_pre_elections_deKlerk.txt', '1995_Mandela.txt', '1996_Mandela.txt', '1997_Mandela.txt', '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', '1999_pre_elections_Mandela.txt', '2000_Mbeki.txt', '2001_Mbeki.txt', '2002_Mbeki.txt', '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', '2004_pre_elections_Mbeki.txt', '2005_Mbeki.txt', '2006_Mbeki.txt', '2007_Mbeki.txt', '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', '2009_pre_elections_ Motlanthe.txt', '2010_Zuma.txt', '2011_Zuma.txt', '2012_Zuma.txt', '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', '2014_pre_elections_Zuma.txt', '2015_Zuma.txt', '2016_Zuma.txt', '2017_Zuma.txt', '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', '2019_pre_elections_Ramaphosa.txt', '2020_Ramaphosa.txt', '2021_Ramaphosa.txt', '2022_Ramaphosa.txt', '2023_Ramaphosa.txt')


this_speech = c()
this_speech[1] = readChar('Data/1994_post_elections_Mandela.txt', nchars = 27050)
this_speech[2] = readChar('Data/1994_pre_elections_deKlerk.txt', nchars = 12786)
this_speech[3] = readChar('Data/1995_Mandela.txt', nchars = 39019)
this_speech[4] = readChar('Data/1996_Mandela.txt', nchars = 39524)
this_speech[5] = readChar('Data/1997_Mandela.txt', nchars = 37489)
this_speech[6] = readChar('Data/1998_Mandela.txt', nchars = 45247)
this_speech[7] = readChar('Data/1999_post_elections_Mandela.txt', nchars = 34674)
this_speech[8] = readChar('Data/1999_pre_elections_Mandela.txt', nchars = 41225)
this_speech[9] = readChar('Data/2000_Mbeki.txt', nchars = 37552)
this_speech[10] = readChar('Data/2001_Mbeki.txt', nchars = 41719)
this_speech[11] = readChar('Data/2002_Mbeki.txt', nchars = 50544)
this_speech[12] = readChar('Data/2003_Mbeki.txt', nchars = 58284)
this_speech[13] = readChar('Data/2004_post_elections_Mbeki.txt', nchars = 34590)
this_speech[14] = readChar('Data/2004_pre_elections_Mbeki.txt', nchars = 39232)
this_speech[15] = readChar('Data/2005_Mbeki.txt', nchars = 54635)
this_speech[16] = readChar('Data/2006_Mbeki.txt', nchars = 48643)
this_speech[17] = readChar('Data/2007_Mbeki.txt', nchars = 48641)
this_speech[18] = readChar('Data/2008_Mbeki.txt', nchars = 44907)
this_speech[19] = readChar('Data/2009_post_elections_Zuma.txt', nchars = 31101)
this_speech[20] = readChar('Data/2009_pre_elections_ Motlanthe.txt', nchars = 47157)
this_speech[21] = readChar('Data/2010_Zuma.txt', nchars = 26384)
this_speech[22] = readChar('Data/2011_Zuma.txt', nchars = 33281)
this_speech[23] = readChar('Data/2012_Zuma.txt', nchars = 33376)
this_speech[24] = readChar('Data/2013_Zuma.txt', nchars = 36006)
this_speech[25] = readChar('Data/2014_post_elections_Zuma.txt', nchars = 29403)
this_speech[26] = readChar('Data/2014_pre_elections_Zuma.txt', nchars = 36233)
this_speech[27] = readChar('Data/2015_Zuma.txt', nchars = 32860)
this_speech[28] = readChar('Data/2016_Zuma.txt', nchars = 32464)
this_speech[29] = readChar('Data/2017_Zuma.txt', nchars = 35981)
this_speech[30] = readChar('Data/2018_Ramaphosa.txt', nchars = 33290)
this_speech[31] = readChar('Data/2019_post_elections_Ramaphosa.txt', nchars = 42112)
this_speech[32] = readChar('Data/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
this_speech[33] = readChar('Data/2020_Ramaphosa.txt', nchars = 47910)
this_speech[34] = readChar('Data/2021_Ramaphosa.txt', nchars = 43352)
this_speech[35] = readChar('Data/2022_Ramaphosa.txt', nchars = 52972)
this_speech[36] = readChar('Data/2022_Ramaphosa.txt', nchars = 52972)


sona = data.frame(filename = filenames, 
                  speech = this_speech, 
                  stringsAsFactors = FALSE)

## extract year and president for each speech
sona$year = str_sub(sona$filename, start = 1, end = 4)
sona$president_13 = str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

## clean the sona dataset by adding the date and removing unnecessary text
replace_reg = '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

sona = sona %>%
  mutate(speech = str_replace_all(speech, replace_reg , ' ')
         ,date = str_sub(speech, start = 1, end = 30)
         ,date = str_replace_all(date, "February", "02")
         ,date = str_replace_all(date, "June", "06")
         ,date = str_replace_all(date, "Feb", "02")
         ,date = str_replace_all(date, "May", "05")
         ,date = str_replace_all(date, "Jun", "06")
         ,date = str_replace_all(date, "Thursday, ","")
         ,date = str_replace_all(date, ' ', '-')        
         ,date = str_replace_all(date, "[A-z]",'')
         ,date = str_replace_all(date, '-----', '')
         ,date = str_replace_all(date, '----', '')
         ,date = str_replace_all(date, '---', '')
         ,date = str_replace_all(date, '--', ''))

### Change speeches into sentences

speechList = list()
# speechList[[1]] = tokenize_sentences(sona$speech[1])

for (i in 1:36){
  
  speechList = append(speechList, tokenize_sentences(sona$speech[i]))
}

# speechList[[1]] = strsplit(sona$speech[1], "(?<=[^.][.][^.])", perl = TRUE)

save(sona, speechList, file = "SonaData.RData")
