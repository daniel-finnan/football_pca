import os
from dotenv import load_dotenv
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
import helpers
import logging
from data_references import seasons

# -------------------------------------------------------------
# Config - setting some options for the browser
options = Options()
options.headless = False
# Create the browser driver passing in the options
driver = webdriver.Firefox(options=options)
# -------------------------------------------------------------
# Log all messages
logging.basicConfig(level=logging.INFO)
load_dotenv()

url = os.getenv('TABLE_URL')
driver.get(url)
helpers.random_sleep()
try: 
    logging.info('Clicking cookie modal')
    cookie_modal = WebDriverWait(driver, 30).until(
        EC.presence_of_element_located((By.ID, "onetrust-accept-btn-handler"))
    )
    cookie_modal.click()
except:
    logging.critical('Cookie modal')
    quit()
logging.info('Cookie modal clicked')
helpers.random_sleep()

# Click the advert, but is not always present
try:
    driver.find_element(By.ID, "advertClose").click()
except:
    logging.info('No advert modal')

helpers.random_sleep()

logging.info('Waiting for the season dropdown')
try: 
    statsTable = WebDriverWait(driver, 30).until(
        EC.presence_of_element_located((By.CSS_SELECTOR, ".dropDown:nth-child(3) > .current"))
    )
except:
    logging.critical('Waiting for season dropdown')
    quit()

for season in seasons:
    logging.info(f"Working on season: {season['year']}")
    logging.info('Clicking season dropdown')
    # Plus one to indexes we pulled out
    dropdown_index = season['dropdown_index'] + 1
    # Select the season we're interested in
    driver.find_element(By.CSS_SELECTOR, ".dropDown:nth-child(3) > .current").click()
    driver.find_element(By.CSS_SELECTOR, f".open li:nth-child({dropdown_index})").click()
    helpers.random_sleep()
    logging.info('Waiting for table to be visible')
    try: 
        statsTable = WebDriverWait(driver, 30).until(
            EC.presence_of_element_located((By.CLASS_NAME, "league-table__tbody"))
        )
    except:
        logging.critical('Table')
        quit()
    logging.info('Table ready')
    helpers.random_sleep()
    page = driver.page_source
    logging.info(f"Writing out HTML page {season['year']}")
    with open(f"scraping/tables_html/{season['year']}.html", 'w', encoding='UTF-8') as output:
        output.write(page)
    logging.info('Finished season')










    