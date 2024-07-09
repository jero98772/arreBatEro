import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
from subprocess import run
import re
import json


def write_txt(name,content,mode='a'):
    """
    write_txt(name,content) , write in txt file something  
    """
    content =str(content)
    with open(name, mode) as file:
        file.write(content)
        file.close()

def scrapper(url):
    """
    this function get the webpage and apply 2 functions get_links(),get_jobs()
    """
    response = requests.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    links = get_links(soup,url)
    return links
def crawl(url, depth=6):
    visited = set()
    queue = [(url, 0)]
    while queue:
        current_url, current_depth = queue.pop(0)
        
        if current_url in visited or current_depth >= depth:
            continue
        
        visited.add(current_url)
        print(f'Crawling {current_url} at depth {current_depth}')
        links = scrapper(current_url)
        for link in links:
            queue.append((link, current_depth + 1))
            write_txt(FILENAME,link+"\n")          


def read_data_from_json(file_path):
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
            
            # Convert keys of 'graph' dictionary to integers if they are strings
            if 'graph' in data:
                data['graph'] = {int(key): value for key, value in data['graph'].items()}
                data['page_content'] = {int(key): value for key, value in data['page_content'].items()}
            
            return data
    except FileNotFoundError:
        print(f"Error: File '{file_path}' not found.")
        return None



def create_index(page_content):
    index = {}
    for page_id, content in page_content.items():
        words = re.findall(r'\w+', content.lower())
        for word in words:
            if word not in index:
                index[word] = set()
            index[word].add(page_id)
    return index


def search(query, pagerank_scores,inverted_index,pages):
    query_words = query.lower().split()
    
    # Convert page names to their corresponding integers
    page_integers = {v: k for k, v in pages.items()}
    
    # Initialize result_urls with all page integers
    result_urls = set(page_integers.keys())
    
    for word in query_words:
        if word in inverted_index:
            result_urls.intersection_update(inverted_index[word])
        else:
            # If a word is not in any document, return an empty result
            return []
    
    # Sort the results based on PageRank scores
    ranked_results = sorted(result_urls, key=lambda url: pagerank_scores[url], reverse=True)
    
    # Convert the result back to page names
    ranked_results = [page_integers[url] for url in ranked_results]
    
    return ranked_results



