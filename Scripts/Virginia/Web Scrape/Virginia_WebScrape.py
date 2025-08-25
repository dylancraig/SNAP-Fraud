import os
import requests
from bs4 import BeautifulSoup

# Step 1: Define the URL of the webpage to scrape PDFs and XLS files from
webpage_url = "https://www.dss.virginia.gov/geninfo/reports/financial_assistance/snap_participation_archive.cgi"

# Step 2: Specify the full path to the folder
folder_path = r"C:\Wu - RA\SNAP County Level Data\Virginia"
os.makedirs(folder_path, exist_ok=True)  # Create folder if it doesn't exist

# Step 3: Fetch the content of the webpage
response = requests.get(webpage_url)
if response.status_code == 200:
    webpage_content = response.content
else:
    print("Failed to retrieve the webpage.")
    exit()

# Step 4: Parse the HTML content to find all PDF and XLS links
soup = BeautifulSoup(webpage_content, 'html.parser')
file_links = [(a['href'], a.text.strip()) for a in soup.find_all('a', href=True) if a['href'].lower().endswith(('.pdf', '.xls', '.xlsx'))]

# Step 5: Define the function to download a file
def download_file(url, text, folder_path):
    response = requests.get(url)
    if response.status_code == 200:
        # Clean the text to create a valid filename
        file_name = "".join(x for x in text if x.isalnum() or x in (" ", "_")).strip()
        file_extension = os.path.splitext(url)[1]  # Extract the file extension from the URL
        file_name += file_extension  # Append the file extension to the cleaned text
        file_path = os.path.join(folder_path, file_name)  # Full path to save the file
        with open(file_path, 'wb') as file:
            file.write(response.content)  # Write the content to a file
        print(f"Downloaded: {file_name}")
    else:
        print(f"Failed to download: {url}")

# Step 6: Download each file found
for file_url, link_text in file_links:
    # Ensure the URL is absolute
    if not file_url.startswith('http'):
        file_url = requests.compat.urljoin(webpage_url, file_url)
    download_file(file_url, link_text, folder_path)  # Download the file to the folder
