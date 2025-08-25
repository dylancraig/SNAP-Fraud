import os
import requests
from bs4 import BeautifulSoup

# Step 1: Define the URL of the webpage to scrape PDFs from
webpage_url = "https://dhr.alabama.gov/facts-and-figures/"

# Step 2: Specify the full path to the folder
folder_path = r"C:\Wu - RA\SNAP County Level Data\Alabama"
os.makedirs(folder_path, exist_ok=True)  # Create folder if it doesn't exist

# Step 3: Fetch the content of the webpage
response = requests.get(webpage_url)
if response.status_code == 200:
    webpage_content = response.content
else:
    print("Failed to retrieve the webpage.")
    exit()

# Step 4: Parse the HTML content to find all PDF links
soup = BeautifulSoup(webpage_content, 'html.parser')
pdf_links = [a['href'] for a in soup.find_all('a', href=True) if a['href'].lower().endswith('.pdf')]

# Step 5: Define the function to download a PDF
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
        
# Step 6: Define the function to check if the link text contains a year or "Fiscal Year"
def should_skip_link(text):
    # Check if the text contains a year
    if re.search(r'\b(19|20)\d{2}\b', text):
        return True
    # Check if the text contains "Fiscal Year"
    if "Fiscal Year" in text:
        return True
    return False
    if "Fiscal Yeal" in text:
        return True
    return False

# Step 6: Download each PDF found
for pdf_url in pdf_links:
    # Ensure the URL is absolute
    if not pdf_url.startswith('http'):
        pdf_url = requests.compat.urljoin(webpage_url, pdf_url)
    download_pdf(pdf_url, folder_path)  # Download the PDF to the folder

