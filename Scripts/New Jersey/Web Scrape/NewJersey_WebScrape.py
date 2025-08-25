import os
import requests
from bs4 import BeautifulSoup

webpage_url = "https://www.nj.gov/humanservices/dfd/news/cps.html"

folder_path = r"C:\Wu - RA\SNAP County Level Data\New Jersey"
os.makedirs(folder_path, exist_ok=True)  # Create folder if it doesn't exist

response = requests.get(webpage_url)
if response.status_code == 200:
    webpage_content = response.content
else:
    print("Failed to retrieve the webpage.")
    exit()
    
soup = BeautifulSoup(webpage_content, 'html.parser')
pdf_links = [a['href'] for a in soup.find_all('a', href=True) if a['href'].lower().endswith('.pdf')] 

def download_pdf(url, folder_path):
    response = requests.get(url)
    if response.status_code == 200:
        file_name = os.path.basename(url)  # Get the file name from the URL
        file_path = os.path.join(folder_path, file_name)  # Full path to save the file
        with open(file_path, 'wb') as file:
            file.write(response.content)  # Write the content to a file
        print(f"Downloaded: {file_name}")
    else:
        print(f"Failed to download: {url}")

        
for pdf_url in pdf_links:
    # Ensure the URL is absolute
    if not pdf_url.startswith('http'):
        pdf_url = requests.compat.urljoin(webpage_url, pdf_url)
    download_pdf(pdf_url, folder_path)  # Download the PDF to the folder