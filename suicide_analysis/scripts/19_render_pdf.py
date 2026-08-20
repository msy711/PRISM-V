from playwright.sync_api import sync_playwright
import os

SRC = "/home/user/PRISM-V/suicide_analysis/report.html"
OUT = "/home/user/PRISM-V/suicide_analysis/report.pdf"
CHROME = "/opt/pw-browsers/chromium-1194/chrome-linux/chrome"

with sync_playwright() as p:
    browser = p.chromium.launch(executable_path=CHROME, args=["--no-sandbox"])
    page = browser.new_page()
    page.goto(f"file://{SRC}")
    page.wait_for_timeout(400)
    page.pdf(
        path=OUT,
        format="A4",
        print_background=True,
        margin={"top": "14mm", "bottom": "16mm", "left": "12mm", "right": "12mm"},
    )
    browser.close()

print("Wrote", OUT, os.path.getsize(OUT) / 1024, "KB")
