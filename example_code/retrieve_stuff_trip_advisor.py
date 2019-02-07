import mechanize
from bs4 import BeautifulSoup
import pandas
import os
import json

br = mechanize.Browser()
br.set_handle_robots(False)   # ignore robots
br.set_handle_refresh(False)
br.addheaders =[('User-agent', 'Firefox')]

my_url = "https://www.tripadvisor.com/ShowUserReviews-g60763-d611947-r650559310-New_York_Hilton_Midtown-New_York_City_New_York.html"

response = br.open(my_url)

soup = BeautifulSoup(br.response().read(),"lxml")

states = soup("script",{"type":"application/ld+json"})


file = open("testfile.txt","w")
file.write(str(states))
file.close()