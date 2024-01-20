from dotenv import load_dotenv
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
import helpers
import logging
import os
from data_references import seasons, stats_attributes


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

base_url = os.getenv('STATS_URL')
driver.get(base_url)
helpers.random_sleep()

# Click OK on cookies modal
try: 
    logging.info('Clicking cookie modal')
    cookie_modal = WebDriverWait(driver, 30).until(
        EC.presence_of_element_located((By.ID, "onetrust-accept-btn-handler"))
    )
    cookie_modal.click()
except:
    logging.info("No cookie modal")

# Click the advert
advert_visible = driver.find_element(By.ID, "advertClose").is_displayed()
if advert_visible == True:
    driver.find_element(By.ID, "advertClose").click()

helpers.random_sleep()

# Go through seasons
for season in seasons:
    logging.info(f"Working on season: {season['year']}")
    logging.info('Making directory')
    os.mkdir(f'scraping/stats_html/{season["year"]}')

    # Go through stats we're interested in
    for attribute in stats_attributes:
        logging.info(f'Working on: {attribute}')
        # Select the link for the stat
        logging.info('Clicking on link')
        driver.find_element(By.LINK_TEXT, attribute).click()
        helpers.random_sleep()
        logging.info('Waiting for the season dropdown')
        try: 
            statsTable = WebDriverWait(driver, 30).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, ".mobile > .current"))
            )
        except:
            logging.critical("Season dropdown")
            quit()
        logging.info('Clicking season dropdown')
        # Need to select the season we're interested in
        driver.find_element(By.CSS_SELECTOR, ".mobile > .current").click()
        
        driver.find_element(By.CSS_SELECTOR, f'.mobile li:nth-child({season["dropdown_child"]})').click()
        logging.info('Waiting for stats table to be visible')
        try: 
            statsTable = WebDriverWait(driver, 30).until(
                EC.presence_of_element_located((By.CLASS_NAME, "statsTableContainer"))
            )
        except:
            logging.critical("Stats table")
            quit()

        helpers.random_sleep()

        page = driver.page_source
        logging.info('Writing out first HTML page')
        # Write 1st page of stats text file
        with open(f'scraping/stats_html/{season["year"]}/{attribute}_1.html', 'w', encoding='UTF-8') as output:
            output.write(page)
        logging.info('Checking for pagination')
        # Check to see if need to paginate
        try:        
            driver.find_element(By.CSS_SELECTOR, ".paginationNextContainer svg").click()
            logging.info('Clicked pagination')
            try: 
                statsTable = WebDriverWait(driver, 30).until(
                    EC.presence_of_element_located((By.CLASS_NAME, "statsTableContainer"))
                )
            except:
                logging.critical("Stats table")
                quit()
            helpers.random_sleep()
            page = driver.page_source
            logging.info('Writing out second HTML page')
            with open(f'scraping/stats_html/{season["year"]}/{attribute}_2.html', 'w', encoding='UTF-8') as output:
                output.write(page)
        except:
            logging.info('Only 1 page of stats')
        logging.info('Finished collecting statistic')




                    
                    
                    
            