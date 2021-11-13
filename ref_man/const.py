from typing import Dict

ACCEPT: str = "text/html,application/xhtml+xml,application/xml;" +\
    "q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8," +\
    "application/signed-exchange;v=b3;q=0.9"
USER_AGENT: str = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)" +\
    "Chrome/87.0.4280.66 Safari/537.36)"
default_headers: Dict[str, str] = {"accept": ACCEPT,
                                   "accept-encoding": "gzip, deflate, br",
                                   "accept-language": "en-GB,en-US;q=0.9,en;q=0.8",
                                   "cache-control": "no-cache",
                                   "user-agent": USER_AGENT}
