# -*- coding: utf-8 -*-
"""
Created on Tue Feb  9 14:25:53 2016

@author: SumanthSwaminathan
"""

import socks
import socket
import resource, sys
import os

#resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
#sys.setrecursionlimit(10**6)
#resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))

sys.setrecursionlimit(35000)

#socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS4, "127.0.0.1", 9150, True)
#socket.socket = socks.socksocket

#from urllib.request import urlopen

#print (urlopen("http://worldwide.espacenet.com/publicationDetails/citingDocuments?CC=US&NR=3737433B1&KC=B1&FT=D&ND=4&date=19870310&DB=EPODOC&locale=en_EP").read())

def create_connection(address, timeout=None, source_address=None):
    sock = socks.socksocket()
    sock.connect(address)
    return sock

socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS5, "127.0.0.1", 9150)

# patch the socket module
socket.socket = socks.socksocket
socket.create_connection = create_connection

from urllib.request import urlopen

print (urlopen("http://stackoverflow.com/questions/5148589/python-urllib-over-tor").read())



                                                     
#from urllib import parse                                                    
#
#SOCKS_HOST = '127.0.0.1'                                             
#SOCKS_PORT = 9150                                                    
#SOCKS_TYPE = socks.PROXY_TYPE_SOCKS5                                 
#
#url = 'http://www.whatismyip.com/automation/n09230945.asp'           
#parsed = parse.urlparse(url)                                      
#
#socket = socks.socksocket()                                          
#socket.setproxy(SOCKS_TYPE, SOCKS_HOST, SOCKS_PORT)                  
#socket.connect((parsed.netloc, 9150))                                  
#socket.send('''GET %(uri)s HTTP/1.1                                  
#host: %(host)s                                                       
#connection: close                                                    
#
#''' % dict(                                                          
#    uri=parsed.path,                                                 
#    host=parsed.netloc,                                              
#))                                                                   
#
#print (socket.recv(1024))                                         
#socket.close()