library(mailR)


rmarkdown::render("report.Rmd", 
                  encoding = "utf-8")


sender <- "XXX"

recipients <- c("bar@example.com",
                "foo@example.com")


send.mail(from = sender,
          to = recipients,
          
          subject = "Wochenreport",
          
          body = "Hallo,\nhier der wöchentliche Report.",
          
          attach.files = "report.nb.html",
          
          authenticate = T,
          
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 465, 
                      user.name = "XXX", 
                      passwd = "XXX", 
                      ssl = T))
