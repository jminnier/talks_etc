# convert html to pdf for uploading and printing (gifs don't work)

# via https://github.com/yihui/xaringan/wiki/Export-Slides-to-PDF

system("decktape remark 2018_02_Shiny_CSP/minnier_csp2018.html 2018_02_Shiny_CSP/minnier_csp2018.pdf")

system("decktape remark 2018_07_JSM_InteractiveLearning/minnier_jsm2018.html 2018_07_JSM_InteractiveLearning/minnier_jsm2018_2.pdf")
webshot(here::here("2018_07_JSM_InteractiveLearning","minnier_jsm2018.html"),
          file = here::here("2018_07_JSM_InteractiveLearning","minnier_jsm2018.pdf"))
# notes, did not like the `` text, images or plots that were cut off on the html slides did not show up on pdf, neither with Chrome print to pdf