import random
import time
import logging

logging.basicConfig(level=logging.INFO)
# -------------------------------------------------------------
# Function for pausing execution for a random period of seconds
def random_sleep():
    time_to_sleep = random.randint(5,10)
    logging.info(f"I'm going to sleep for {time_to_sleep}")
    time.sleep(time_to_sleep)
    logging.info("I've finished sleeping")
# -------------------------------------------------------------

def remove_amp(x):
    if x.find('&') == -1:
        return x
    else:
        return x.replace('&', 'and')

def remove_comma(x):
    if x.find(',') == -1:
        return x
    else:
        return x.replace(',', '')