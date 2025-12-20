from bs4 import BeautifulSoup
import requests

MAX_DEPTH = 5
links = set()

def get_page_content(url):
    response = requests.get(url)
    return response.text

def get_page_links(url, depth):
    if url not in links and depth < MAX_DEPTH:
        print(f"Depth: {depth} [{url}]")
        links.add(url)
        
        html_content = get_page_content(url)
        print(f"\n--- Content of {url} ---")
        print(html_content)
        print(f"--- End of content ---\n")
        
        document = BeautifulSoup(html_content, 'html.parser')
        links_on_page = document.find_all('a', href=True)
        depth += 1
        
        for page in links_on_page:
            href = page['href']
            if href.startswith('http'):
                get_page_links(href, depth)
            elif href.startswith('/'):
                base_url = url.split('/')[0] + '//' + url.split('/')[2]
                get_page_links(base_url + href, depth)

get_page_links("https://example.com", 0)
