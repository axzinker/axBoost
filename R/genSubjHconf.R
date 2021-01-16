#' Generate subject hour confirmation PDFs for psychology students and optionally send them via email
#'
#' The function generates a PDF with information about subject hours, study title,
#' subjects email address, name of the organisator of the study, digital
#' signature of the organisator, timestamp of the encryption key, and a code
#' conatining the encrypted infomation. This information can be used to
#' validate the data and data-integrity of the PDF against manipulation.
#' The code can be verified with the function \link{axDecode}.
#'
#' @param studyTitle String of the title of the study
#' @param organizer String of the organizer of the study
#' @param mailTo String with email addresses of study participants. The subject
#' hour confirmation is locked to the email address. Optionally, the email
#' is used to sent the confirmation to the participants.
#' @param testSubjectHours String with the number of test subject hours to be
#' assigned, (e.g., "1.25 h", "30min").
#' @param signatureFileName String with the name of the signature file. Allowed
#' file formats is png! Default file is "dummysig.png".
#' @param workingDirectory String of the path of the working directory in
#' which the supplemental files (e.g., scanned signature) and output files
#' (e.g., encryption key, subject hour confirmation PDFs) are stored. Path
#' should end in a slash (/).
#' @param newKey Logical value if a new encryption key for the code should be
#' generated. You have to use the same key for encryption and decryption. The
#' number code at the end of the key contains a timestamp converted to be
#' saveable on different operating systems. Don't rename the key, because the
#' timestamp is needed for decryption. In addition, it helps to differentiate
#' between several keys.
#' @param useKey String with the name of the key which should be used for
#' encryption of the code. Default is
#' "dummykey_313937302d30312d30312030303a30303a3031.rda".
#' @param fontscale Integer value to scale the size of the fonts. Default is 1.
#' Values > 1 enlarge the fontsize, values < 1 reduce the fontsize.
#' @param sendMail Logical value if mails should be send or not. Be careful with
#' this option and test it in advance. Default is false
#' @param mailFrom String to set the email sender (your email address)
#' @param mailCc String to set the Cc of the mail (e.g., to receive a copy of
#' the mails)
#' @param mailSubject String to set the subject of the mail
#' @param mailBody String to set the body of the mail (mailtext)
#' @param SMTPserver String with the name of your mailserver. SSL transport
#' encryption (port 465) is used.
#' @param SMTPlogin String with the login of your mailserver
#' @param SMTPpassword String with the password of your mailserver
#' @return Returns PDFs with information about subject hours, study title,
#' subjects email address, name of the organisator of the study, digital
#' signature of the organisator, timestamp of the encryption key, and a code
#' with the encrypted infomation. The encrypted information can be used to
#' validate the information and the integrity of the PDF against manipulation.
#' The code can be verified with the function \link{axDecode}.
#'
#' @author Axel Zinkernagel \email{zinkernagel@uni-landau.de}
#'
#' @examples
#' \dontrun{
#' # 1. Generate a dummy PDF in the working directory
#' genSubjHconf(workingDirectory = "/my/working/directory/")
#'
#' # 2. Generate PDFs with a new encryption key, but dont sent them per email
#' emails <- c("dummy1@dummy.local", "dummy2@dummy.local", "dummy3@dummy.local")
#'
#' genSubjHconf(studyTitle = "My Study", organizer = "My name", mailTo = emails,
#'             testSubjectHours = "30min",
#'             signatureFileName = "My_signature_scan.png",
#'             workingDirectory = "/my/working/directory/",newKey = TRUE)
#'
#' # Show the filename of the generated key (the key is saved in the working
#' # directory).
#' whichKey("2021−01−15 16:40:22")
#'
#' # Decode the code printed on the pdf (code must be in one line!)
#' load("/my/working/directory/key_323032312d30312d31352031363a34303a3232.rda")
#' axDecode("oHtDit5N34y/s+uhOviVKWq/7QGRyQ/M4dP+txAnxpWRcoS...",key)
#'
#' # 3. Generate PDFs with an existing key and sent the PDFs via email
#' # Be careful with the mail option, you are able to send mass mails.
#' genSubjHconf(studyTitle = "My Study", organizer = "My name", mailTo = emails,
#'           testSubjectHours = "30min",
#'           signatureFileName = "My_signature_scan.png",
#'           workingDirectory = "/my/working/directory/",
#'           useKey = "key_32303231....rda",
#'           sendMail = TRUE, mailFrom = "my@email.address",
#'           mailCc = "my@addressFor.copies", mailSubject = "My mail subject",
#'           mailBody = "This is my mailbody",
#'           SMTPserver = "my.smtpserver.com",
#'           SMTPlogin = "myLogin",
#'           SMTPpassword = "myPassword")
#'
#' }
#' @import magrittr grDevices
#' @export
genSubjHconf <- function(studyTitle = "Study title", organizer = "Organizer",
                         mailTo = "dummy@dummy.local", testSubjectHours = "1 h",
                         signatureFileName = "dummysig.png",
                         workingDirectory = "", newKey = FALSE,
                         useKey = "dummykey_313937302d30312d30312030303a30303a3031.rda",
                         fontscale = 1,
                         sendMail = FALSE, mailFrom = "", mailCc = "",
                         mailSubject = "", mailBody = "", SMTPserver = "",
                         SMTPlogin = "", SMTPpassword = "") {

  # first checks of entered data
  if (workingDirectory == "") {
    stop("You must specify a working directory where the encryption key and the pdfs are stored!")
  }

  if (newKey == FALSE && useKey == "") {
    stop("Encryption error: either generate a new key or use an exiting one!")
  }

  setwd(workingDirectory)

  if (newKey) {
    # 256*4, otherwise the key gets to long to be printed on the confirmation
    key <- openssl::rsa_keygen(1024)
    timestamp <- Sys.time()
    timestamp <- as.character(timestamp)
    timestamp4save <- charToRaw(timestamp)
    timestamp4save <- as.character(timestamp4save)
    timestamp4save <- paste(timestamp4save, collapse = "")
    save(key, file = paste0(workingDirectory,"key_",timestamp4save,".rda"))
  } else {
    if (useKey == "dummykey_313937302d30312d30312030303a30303a3031.rda") {
      warning("The default key is for demo purposes only. Don't use it in real settings, because the private key is published within this package!")
      load(system.file("extdata", useKey, package = "axBoost"))
    } else {
      load(paste0(workingDirectory,useKey))
    }
    timestamp <- strsplit(strsplit(useKey,"_", fixed = TRUE)[[1]][2],".", fixed = TRUE)[[1]][1]
    timestamp <- (substring(timestamp, seq(1, nchar(timestamp) - 1, 2), seq(2, nchar(timestamp), 2)))
    timestamp <- base::as.hexmode(timestamp)
    timestamp <- base::as.raw(timestamp)
    timestamp <- rawToChar(timestamp)
  }

  pubkey <- key$pubkey

  for (i in 1:length(mailTo)) {
    if (nchar(paste(mailTo[i], testSubjectHours, studyTitle, organizer, timestamp, sep = ", ")) > 116) {
      stop(paste0("String ",paste(mailTo[i], testSubjectHours, studyTitle, organizer, timestamp, sep = ", "),
                  " is to long for encryption with key length 1024. Please shorten studyTitle or organizer, for example."))
    }
  }

  axEncode <- function(code, pubkey = pubkey) {
    return(openssl::base64_encode(openssl::rsa_encrypt(charToRaw(code), pubkey)))
  }

  # read signature image
  if (signatureFileName == "dummysig.png") {
    signPic <- png::readPNG(system.file("extdata", signatureFileName, package = "axBoost"))
  } else {
    signPic <- png::readPNG(paste0(workingDirectory,signatureFileName))
  }

  for (i in 1:length(mailTo)) { # generate pdfs
    token <- paste(mailTo[i], testSubjectHours, studyTitle, organizer, timestamp, sep = ", ")
    #print(token)
    token <- axEncode(token, pubkey)
    #print(token)
    token1 <- substr(token, 1, (nchar(token)/2))
    token2 <- substr(token, ((nchar(token)/2) + 1), nchar(token))
    # token == paste0(token1,token2)# muss TRUE ergeben

    # graphics computed in inch; if @ is not allowed in filenames, substitute it in the following line
    grDevices::pdf(file = paste0(workingDirectory, gsub("@", "@", mailTo[i], fixed = TRUE), ".pdf"), width = 18, height = 2)
    par(mar = c(0,0,0,0))
    plot(NULL, xlim = c(0, 18), ylim = c(0, 2), xaxt = 'n', yaxt = 'n', type = "n", axes = FALSE)
    rect(xleft = 0, xright = 18, ybottom = 0, ytop = 2, border = "black", lwd = 2)
    segments(x0 = 2.5, y0 = 0, x1 = 2.5, y1 = 2, col = "black", lwd = 2)
    segments(x0 = 13, y0 = 0, x1 = 13, y1 = 2, col = "black", lwd = 2)
    cexLarge <- c((4 * fontscale))
    cexNormal <- c((3 * fontscale))
    cexSmall <- c((2 * fontscale))
    cexSmaller <- c((1 * fontscale))
    text(x = 1.25, y = 1, testSubjectHours, cex = cexLarge, adj = 0.5, col = "black")
    text(x = 3.0, y = 1.5, studyTitle, cex = cexNormal, adj = 0, col = "black")
    text(x = 3.0, y = 1.0, paste0(mailTo[i],", ",timestamp), cex = cexSmall, adj = 0, col = "black")
    text(x = 3.0, y = 0.5, token1, cex = cexSmaller, adj = 0, col = "black")
    text(x = 3.0, y = 0.2, token2, cex = cexSmaller, adj = 0, col = "black")
    signPix_x <- dim(signPic)[2] # x-pixels
    signPix_y <- dim(signPic)[1] # y-pixels
    # 17.9-13.1 = 4.8 width of pdf signature
    signHeight <- ((4.8 * signPix_y) / signPix_x) # height of pdf signature
    signBottomOffset = .3

    if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
      if (names(dev.cur()) == "windows") {
        # windows device doesn't support semi-transparency so we'll need
        # to flatten the image
        transparent <- signPic[,,4] == 0
        signPic <- as.raster(signPic[,,1:3])
        signPic[transparent] <- NA
        # interpolate must be FALSE on Windows, otherwise R will
        # try to interpolate transparency and fail
        graphics::rasterImage(signPic, 13.1, signBottomOffset, 17.9, (signBottomOffset + signHeight), interpolate = FALSE)
      } else {
        # any reasonable device will be fine using alpha
        graphics::rasterImage(signPic, 13.1, signBottomOffset, 17.9, (signBottomOffset + signHeight))
      }
      text(x = 13.1, y = 0.2, paste0("(",organizer,")"), cex = cexSmall, adj = 0, col = "black")
    }

    grDevices::dev.off()

    if (sendMail) { # send mails if TRUE
      email <- emayili::envelope(to = mailTo[i], from = mailFrom, cc =  mailCc, subject = mailSubject, text = mailBody)
      email <- email %>% emayili::attachment(paste0(workingDirectory, mailTo[i], ".pdf"))
      #print(email, details = TRUE)

      smtp <- emayili::server(host = SMTPserver, port = 465, username = SMTPlogin, password = SMTPpassword)
      smtp(email, verbose = FALSE)
    }
  }
}

