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
import time 
from math import ceil

# specify where to store data  
os.chdir("/Users/ryanlongmuir/Desktop/Git/Grad_Cafe/")

# ============================================================================
# Functions
# ============================================================================

# -----------------------------------------------------------------------------
# betweenStrings
#   return sub-string between two strings 
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

def scrapeGradCafe(query, start_date = datetime.today() - timedelta(days=365)):

    page = 1
    page_max = 2 
    min_date = datetime.today()
    columns = ['query', 'page', 'inst', 'prg', 'dec', 'int', 'date', 'note']
    data = []
    
    print('Scraping admissions data for {}'.format(query), flush = True)
    
    while start_date < min_date and page <= page_max:
    
        # grab html, grabs 250 rows at a time
        template =  "https://www.thegradcafe.com/survey/index.php?q={0}&t=a&pp=250&o=d&p={1}"
        url = template.format(query.replace(" ", "+"), page)
        webpage = requests.get(url)
        
        # parse individual rows 
        soup = BeautifulSoup(webpage.content, 'html.parser')
        rows = soup.find_all('tr', class_= re.compile("row"))
        
        # the website will return the last page available if you request a page  
        # number that doesn't exist. Therefore, to control for duplicates we check 
        # the maximum number of pages for a search.
        results_info = soup.find_all('div', class_="admission-search pagination")[0].get_text()
        page_max = ceil(int(betweenStrings("Showing", "results", results_info))/250)
        
        for row in rows:
            vals = [query, page] + [x.get_text() for x in row.find_all('td', class_=re.compile("tcol"))]
            data.append(vals)
            
        page += 1    
        min_date = datetime.strptime(data[-1][6].rstrip(), "%d %b %Y") 
        time.sleep(7)
    
    return pd.DataFrame(data, columns = columns) 

# ============================================================================
# Call functions 
# ============================================================================

# today's date, for csv  
file_date = datetime.today().strftime("%m%d%Y")

# apply scrapeGradCafe to multiple schools and rbind the datasets together 
school_list = list(pd.read_csv("data/usnews_ranking.csv").iloc[:,2])
search_list = ["econ* " + school.strip() for school in school_list]
raw_df = pd.concat(map(partial(scrapeGradCafe, start_date = datetime(2010,1,1)), search_list))

# output raw 
raw_df.to_csv("data/raw_grad_cafe.csv.gz", index = False, compression = "gzip")

# manipulate data 
clean_df = raw_df\
    .query('prg.str.contains("PhD")', engine='python')\
    .assign(
        acc = lambda x : x['dec'].str[0:9],
        season = lambda x: x['prg'].apply(lambda x: betweenStrings("\(F", "\)", x)),
        gpa = lambda x: x['dec'].apply(lambda x: betweenStrings("GPA: ", "GRE", x)),
        gre = lambda x: x['dec'].apply(lambda x: betweenStrings("W\): ", "GRE", x)),
        verbal = lambda x: x['gre'].str.split("/").str.get(0),
        quant = lambda x: x['gre'].str.split("/").str.get(1),
        awa = lambda x: x['gre'].str.split("/").str.get(2)
    )\
    .drop(columns = ['gre', 'dec'])

# save clean data 
clean_df.to_csv("data/clean_grad_cafe_step1.csv.gz", index = False, compression = "gzip")

