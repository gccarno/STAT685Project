# -*- coding: utf-8 -*-
"""
Created on Mon Aug 26 19:07:21 2019

@author: gccar
"""

from bs4 import BeautifulSoup
import urllib3
import certifi
import re

http = urllib3.PoolManager(cert_reqs='CERT_REQUIRED',ca_certs=certifi.where())

url_par = "https://tea.texas.gov/Student_Testing_and_Accountability/Testing/State_of_Texas_Assessments_of_Academic_Readiness_(STAAR)/STAAR_Aggregate_Data_for_"

download_dir = "E:/STAT685/Data/"

ends= ["201"+str(i)+"-201"+str(i+1) for i in range(1,9)]

link_sp = 'https://tea.texas.gov'
link_sp2 = 'https://tea.texas.gov/sites/default/files/'
for end in ends:
    url2 = url_par + end
    print(url2)
    content2 = http.request('GET', url2)
    soup2 = BeautifulSoup(content2.data)
    for dat in soup2.findAll('a', attrs={
            #'href': re.compile('*.dat')
            'title' : re.compile('dfy.*[^p]$')} # d for district, r for region, c campus, s state 
            ,text=re.compile('Algebra I|Grade 8')):
        dat_boy = dat
        print(dat_boy)
        cnt = http.request('GET', link_sp + dat_boy['href'])
        with open(download_dir + dat_boy['title'] + '.csv', 'wb') as f:
            f.write(cnt.data)            
    for dat in soup2.findAll('a', attrs={'href': re.compile('dfy.*.dat')}
    ,text=re.compile('Algebra I|Grade 8')):
        dat_boy = dat
        print(dat_boy)
        cnt = http.request('GET', link_sp2 + dat_boy['href'].split('/')[-1])
        with open(download_dir + dat_boy['href'].split('/')[-1], 'wb') as f:
            f.write(cnt.data)
        
https://tea.texas.gov/sites/default/files/sfy14e8.dat