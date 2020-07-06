#!/usr/bin/env python3

# ============================================================================
# Packages 
# ============================================================================

import os 
import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
import re
from datetime import timedelta, datetime
from functools import partial 

# where you wish to store the data
os.chdir("/Users/ryanlongmuir/Desktop/Python/grad_cafe/data")

# ============================================================================
# Functions
# ============================================================================

# ----------------------------------------------------------------------------
# try_fmt
# ----------------------------------------------------------------------------

def tryFormats(date_str):
    for fmt in ("%d %b %Y", "%d %b %Y "):
        try:
            return datetime.strptime(date_str, fmt).strftime("%Y-%m-%d")
        except ValueError:
            pass
    return 'no valid format found'

# ----------------------------------------------------------------------------
# propNA
#   return proporation of NA's in series 
# ----------------------------------------------------------------------------

def propNA(ser): 
   return ser.apply(lambda x: int(x is np.nan or x == 'n/a')).agg('mean')

# -----------------------------------------------------------------------------
# betweenStrings
#   return sub-string between to strings 
# -----------------------------------------------------------------------------

def betweenStrings(start, end, string): 
    try:
        return re.search(start + "(.*?)" + end, string).group(1)
    except: 
        return np.nan

# ----------------------------------------------------------------------------
# scrapeGradCafe
#   return a dataframe containing search results from the grad cafe website
#   https://www.thegradcafe.com/survey/index.php . takes a start date and a search 
#   query. enter search queries as you would on the grad cafe website e.g., "econ* harvard"
# ----------------------------------------------------------------------------

def scrapeGradCafe(search, start_date = pd.datetime.now() - timedelta(days=365)):
    
    page = 1 
    start_date = pd.to_datetime(start_date)
    min_date = pd.datetime.now()
        
    # pull in data from each of the rows 
    cols = ['inst', 'prg', 'dec', 'int', 'date', 'note']    
    df = pd.DataFrame(columns = cols)
    
    while start_date < min_date:
    
        # write url, grabs 250 rows at a time
        template =  "https://www.thegradcafe.com/survey/index.php?q={0}&t=a&pp=250&o=d&p={1}"
        url = template.format(search.replace(" ", "+"), page)
        webpage = requests.get(url)
        
        # find individual rows 
        soup = BeautifulSoup(webpage.content, 'html.parser')
        rows = soup.find_all('tr', class_= re.compile("row"))
        
        # iterates through rows, returns df if start date is reached 
        for row in rows:
            vals = [x.get_text() for x in row.find_all('td', class_=re.compile("tcol"))]
            ser = pd.Series(vals, index = df.columns)
            df = df.append(ser, ignore_index=True)
            if pd.to_datetime(ser.date, format = "%d %b %Y") < start_date:
                return df
            
        page += 1    
        min_date = pd.to_datetime(df.date, format = "%d %b %Y").min()
    
    return df

# ----------------------------------------------------------------------------
# pull in and clean data  
# ----------------------------------------------------------------------------

# today's date 
date = pd.datetime.now().strftime("%m%d%Y")

# apply scrapeGradCafe to multiple schools and rbind the datasets together 
# i'm affraid to scraping the website too often so i have commented this code out
schools = [ 'harvard', 'mit', 'princeton', 'stanford', 'berkeley', 'yale', 'chicago']
search = ["econ* " + x for x in schools]
df = pd.concat(map(partial(scrapeGradCafe, start_date = "2015-01-01"), search))
df.to_csv("data/raw_grad_cafe_{0}.csv".format(date), index = False)

#  pull in raw data 
df = pd.read_csv("data/raw_grad_cafe_{0}.csv".format(date))

# manipulate data 
df = df\
    .assign(
        acc = lambda x : x['dec'].str[0:9],
        date_str = lambda x: x['dec'].apply(lambda x: x[x.find("on ")+3: x.find("on ")+14]),
        date = lambda x: x['date_str'].map(tryFormats),
        season = lambda x: x['prg'].apply(lambda x: betweenStrings("\(F", "\)", x)),
        gpa = lambda x: x['dec'].apply(lambda x: betweenStrings("GPA: ", "GRE", x)),
        gre = lambda x: x['dec'].apply(lambda x: betweenStrings("W\): ", "GRE", x)),
        verbal = lambda x: x['gre'].str.split("/").str.get(0),
        quant = lambda x: x['gre'].str.split("/").str.get(1),
        awa = lambda x: x['gre'].str.split("/").str.get(2)
    )\
    .query('prg.str.contains("Economics, PhD")', engine='python')\
    .drop(columns = ['gre', 'dec', 'date_str', ])

# save clean data 
df.to_csv("clean_grad_cafe_{0}.csv".format(date), index = False) 
    
# ----------------------------------------------------------------------------
# Check how populated the data are 
# ----------------------------------------------------------------------------    
    
df.agg({
  'gpa': [propNA],
  'quant': [propNA],
  'season': [propNA]
})
    


