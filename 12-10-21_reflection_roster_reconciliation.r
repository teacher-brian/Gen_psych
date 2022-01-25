---
title: "reflection_roster_reconciliation"
author: "Brian Holt"
date: "12/10/2021"
output: html_document
---


library(tidyverse)
library(lubridate)
library(googlesheets4)
library(RSelenium)
library(rvest)

s<- read_sheet('https://docs.google.com/spreadsheets/d/1skEgkolZpr4r_iMaBM0qMxtuNAKATgyB0X2QDSpzor8/edit?resourcekey#gid=271104061')


(p<- s[!(duplicated(s$`What's your Last name?`) & duplicated(s$`What's your first name?`)),] %>%
  mutate(f_name=tools::toTitleCase(`What's your first name?`), l_name=tools::toTitleCase(`What's your Last name?`)) %>%
  select(f_name,l_name,Timestamp) %>%
  arrange(-desc(Timestamp))) %>% print(.,n=100)

p %>% ggplot(aes(x=Timestamp,y=l_name,label=l_name))+geom_text(check_overlap = T)


# Next is to get the roster out of ctclink.  Probably need selenium





# Log in to inside seattle -------------------------------------------------------------------------

# open driver  ------------------------------------------------------------

driver <- rsDriver(browser =c("firefox"))
remote_driver <- driver[["client"]]

#remote_driver$close()

# navigtate to briefcase --------------------------------------------------

remote_driver$navigate("https://myaccount.ctclink.us/")
RSelenium:::selKeys %>% names() %>% sort
remote_driver$refresh()


# find login --------------------------------------------------------------

?Sys.getenv()


el<- remote_driver$findElement(using='xpath', '//*[@name="username"]')
el$highlightElement()

el$clickElement()


el$sendKeysToElement(list('id'))  #change to user id #
el.submit <- remote_driver$findElement(using = 'xpath', '//*[@id="okta-signin-submit"]') # submit

el.submit$clickElement()


el.1 <- remote_driver$findElement(using = 'xpath', '//*[@name="password"]') # ctc password
el.1$highlightElement()
el.1$clearElement()
el.1$sendKeysToElement(list('pass'))  #change to Pass ID
el.pass.submit <- remote_driver$findElement(using = 'xpath', '//*[@type="submit"]') # submit
el.pass.submit$clickElement()

## go to faculty center
url.fac.center <- "https://csprd.ctclink.us/psc/csprd/EMPLOYEE/SA/c/NUI_FRAMEWORK.PT_AGSTARTPAGE_NUI.GBL?CONTEXTIDPARAMS=TEMPLATE_ID:PTPPNAVCOL&scname=CTC_FACULTY_CENTER_NAVCOL&AJAXTRANSFER=Y"
remote_driver$navigate(url.fac.center)



class.1<- remote_driver$findElement(using='xpath', '//*[@id="win0divCLASSROSTER$0"]')

remote_driver$goBack()
# choose term

##might need to do some javascript

# get list of courses using rvest html_table

url.fac.center %>% read_html() %>%  html_nodes(xpath="/html/body/form/div[5]/table/tbody/tr/td/div/table/tbody/tr[6]/td[2]/div/table/tbody/tr/td/table/tbody/tr[2]/td[2]/div/table/tbody/tr/td/table/tbody/tr[4]/td[2]/div/table/tbody/tr[2]/td/table/tbody/tr[2]/td[2]/div/table/tbody/tr[2]/td/table")








