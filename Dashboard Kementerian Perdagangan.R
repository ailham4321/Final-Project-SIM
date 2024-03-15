getwd()
setwd("data sim")
getwd()

### BEBERAPA LIBRARY MUNGKIN PERLU DIINSTALL

library(shiny)
library(bs4Dash)
library(thematic)
library(readxl)
library(DT)
library(fmsb)
library(scales)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(designer)
library(remotes)
library(tibble)
library(htmltools)
library(fresh)
library(sf)
library(plotly)
library(leaflet)




mydateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", minviewmode="months",
                        width = NULL) {
  
  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    tags$div(id = inputId,
             class = "shiny-date-input form-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
             
             controlLabel(inputId, label),
             tags$input(type = "text",
                        # datepicker class necessary for dropdown to display correctly
                        class = "form-control datepicker",
                        `data-date-language` = language,
                        `data-date-weekstart` = weekstart,
                        `data-date-format` = format,
                        `data-date-start-view` = startview,
                        `data-date-min-view-mode` = minviewmode,
                        `data-min-date` = min,
                        `data-max-date` = max,
                        `data-initial-date` = value
             )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (identical(!is.null(x), !is.na(x)))
    if (identical(!is.null(y), !is.na(y)))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.0.2", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css")


dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode="months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (identical(!is.null(x), !is.na(x)))
    if (identical(!is.null(y), !is.na(y)))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
   (function() {
   var datepicker = $.fn.datepicker.noConflict();
   $.fn.bsDatepicker = datepicker;
   })();
   </script>")






inflasi = read.csv("Data - Inflasi.csv")

kode_bulan = c(4,3,2,1,rep(12:1, 3))
choicesinflasi = c("Makanan, Minuman, dan Tembakau",	"Pakaian dan Alas Kaki",	"Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga",	"Perlengkapan, Peralatan, dan Pemeliharaan Rutin Rumah Tangga",	"Kesehatan",	"Transportasi",	"Informasi, Komunikasi, dan Jasa Keuangan",	"Rekreasi, Olahraga, dan Budaya",	"Penyedia Makanan dan Minuman/Restoran",	"Perawatan Pribadi dan Jasa Lainnya",	"Umum")
inflasi = cbind(inflasi, paste0(inflasi$Tahun, "-", kode_bulan))
colnames(inflasi) = c("Tahun", "Bulan", "Makanan, Minuman, dan Tembakau",	"Pakaian dan Alas Kaki",	"Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga",	"Perlengkapan, Peralatan, dan Pemeliharaan Rutin Rumah Tangga",	"Kesehatan",	"Transportasi",	"Informasi, Komunikasi, dan Jasa Keuangan",	"Rekreasi, Olahraga, dan Budaya",	"Penyedia Makanan dan Minuman/Restoran",	"Perawatan Pribadi dan Jasa Lainnya",	"Umum", "Kode Tanggal")
inflasi$`Kode Tanggal` = as.Date(paste(inflasi$`Kode Tanggal`, "-01", sep=""))

choicesjenisektor = c("Sektor Primer", "Sektor Sekunder","Sektor Tersier")
choiceswilayah = c("Bali dan Nusa Tenggara", "Sumatera", "Jawa", "Kalimantan", "Sulawesi", "Papua", "Maluku")
choicesbenua = c("Asia","Timur Tengah","Eropa","Eropa Timur","Amerika","Australia","Afrika")
choicestahun1 = c("2017","2018","2019","2020","2021","2022")
choicestahun2 = c("2018","2019","2020","2021","2022")
choicesgol1 = c("Industri Pengolahan","Pertanian","Pertambangan","Lainnya")
choicesgol2 = c("Bahan Baku Penolong", "Barang Konsumsi", "Barang Modal")


investasiPMAsektor = read.csv("Data - Investasi PMA Sektor.csv")
investasiPMAnegara = read.csv("Data - Investasi PMA Negara.csv")
investasiPMDNsektor = read.csv("Data - Investasi PMDN Sektor.csv")
investasiPMDNprovinsi = read.csv("Data - Investasi PMDN Provinsi.csv")

eksporkomoditas = read.csv("Data - Ekspor Komoditas.csv")
ekspornegara = read.csv("Data - Ekspor Negara Tujuan.csv")
eksporgolonganbarang = read.csv("Data - Ekspor Golongan Barang.csv")

imporkomoditas = read.csv("Data - Impor Komoditi.csv")
impornegara =read.csv("Data - Impor Negara Asal.csv")
imporgolongan =read.csv("Data - Impor Berdasarkan Golongan Barang.csv")
mappma = read.csv("Data - PMA Map.csv")
mappma$latitude = as.numeric(sub(",", ".", mappma$latitude, fixed = TRUE))
mappma$longitude = as.numeric(sub(",", ".", mappma$longitude, fixed = TRUE))
colnames(mappma) = c("ID", "Negara", "Latitude", "Longitude", "2017", "2018", "2019", "2020", "2021", "2022")

ekspornegara$`X2018` = as.numeric(sub(",", "", ekspornegara$`X2018`, fixed = TRUE))
ekspornegara$`X2019` = as.numeric(sub(",", "", ekspornegara$`X2019`, fixed = TRUE))
ekspornegara$`X2020` = as.numeric(sub(",", "", ekspornegara$`X2020`, fixed = TRUE))
ekspornegara$`X2021` = as.numeric(sub(",", "", ekspornegara$`X2021`, fixed = TRUE))
ekspornegara$`X2022` = as.numeric(sub(",", "", ekspornegara$`X2022`, fixed = TRUE))






header = dashboardHeader(title = "Menu", titleWidth = 200)

colnames(investasiPMAsektor ) = c("Jenis Sektor", "Uraian", "2017", "2018", "2019", "2020", "2021", "2022")
colnames(investasiPMAnegara ) = c("Benua", "Negara", "2017", "2018", "2019", "2020", "2021", "2022")


colnames(investasiPMDNsektor) = c("Jenis Sektor", "Uraian", "2017", "2018", "2019", "2020", "2021", "2022")  
colnames(investasiPMDNprovinsi) = c("Rumpun Wilayah", "Provinsi", "2017", "2018", "2019", "2020", "2021", "2022") 

colnames(imporkomoditas) = c("Kode HS", "Uraian", "2018", "2019", "2020", "2021", "2022")
colnames(imporgolongan) = c("Golongan", "Uraian", "2018", "2019", "2020", "2021", "2022")
colnames(impornegara) = c("Negara", "2018", "2019", "2020", "2021", "2022")
colnames(eksporgolonganbarang) = c("Golongan", "Uraian", "2018", "2019", "2020", "2021", "2022", "HS")
colnames(eksporkomoditas) = c("Kode HS", "Uraian", "2018", "2019", "2020", "2021", "2022")
colnames(ekspornegara) = c("Negara", "2018", "2019", "2020", "2021", "2022")


eksporgolonganbarang$`2021` = as.numeric(eksporgolonganbarang$`2021`)
eksporgolonganbarang$`2022` = as.numeric(eksporgolonganbarang$`2022`)

impornegara = impornegara %>% dplyr::filter(Negara!="")
imporgolongan = imporgolongan%>%dplyr::filter(Uraian!="BAHAN BAKU PENOLONG")



shp1 = sf::read_sf("SHP/IDN_adm1.shp")


mapekspor = read.csv("Data - Map Ekspor.csv")
mapimpor = read.csv("Data - Map impor.csv")

colnames(mapekspor) = c("ID", "Negara", "Latitude", "Longitude", "2018", "2019", "2020", "2021", "2022")
colnames(mapimpor) = c("ID", "Negara", "Latitude", "Longitude", "2018", "2019", "2020", "2021", "2022")

mapekspor$Latitude = as.numeric(sub(",", ".", mapekspor$Latitude, fixed = TRUE))
mapekspor$Longitude = as.numeric(sub(",", ".", mapekspor$Longitude, fixed = TRUE))

mapimpor$Latitude = as.numeric(sub(",", ".", mapimpor$Latitude, fixed = TRUE))
mapimpor$Longitude = as.numeric(sub(",", ".", mapimpor$Longitude, fixed = TRUE))


sidebar = bs4DashSidebar(vertical = FALSE,
                         width=200,
                         sidebarMenu(
                           menuItem("Halaman Utama",
                                    tabName = "satu",
                                    icon=icon("home")),
                           menuItem("Inflasi", 
                                    tabName = "duaharusnya", 
                                    icon=icon("signal", lib = "glyphicon")),
                           menuItem("Investasi",
                                    tabName = "dua",
                                    icon=icon("usd", lib = "glyphicon"),
                                    menuSubItem("Investasi PMDN", tabName = "duapmdn"),
                                    menuSubItem("Investasi PMA", tabName = "duapma")),
                           menuItem("Ekspor",
                                    tabName = "tiga",
                                    icon=icon("export", lib = "glyphicon"),
                                    menuSubItem("Berdasarkan Komoditas", tabName = "tigakomoditas"),
                                    menuSubItem("Berdasarkan Negara", tabName = "tiganegara"),
                                    menuSubItem("Berdasarkan Golongan", tabName = "tigagolongan")),
                           menuItem("Impor",
                                    tabName = "empat",
                                    icon=icon("import", lib = "glyphicon"),
                                    menuSubItem("Berdasarkan Komoditas", tabName = "empatkomoditas"),
                                    menuSubItem("Berdasarkan Negara", tabName = "empatnegara"),
                                    menuSubItem("Berdasarkan Golongan", tabName = "empatgolongan")),
                           menuItem("Datasets",
                                    tabName = "lima",
                                    icon=icon("file", lib = "glyphicon"),
                                    menuSubItem("Inflasi", tabName = "limainflasi"),
                                    menuSubItem("Investasi", tabName = "limainvestasi"),
                                    menuSubItem("Ekspor", tabName = "limaekspor"),
                                    menuSubItem("Impor", tabName = "limaimpor")
                           )
                         ))

body = bs4DashBody(
  tabItems(
    tabItem(tabName = "satu",
            fluidRow(h1(strong("Selamat Datang Di Dashboard Kementerian Perdagangan Republik Indonesia"),
                        style="text-align: center"),
                     height=150, width = 12),
            fluidRow(box(imageOutput("pakzul"),title = "Menteri Perdagangan RI", style="text-align:center;",height = 200, width = 4),
                     box(title = "Salam Pembuka", p("Selamat datang di Dashboard Kementerian Perdagangan Republik Indonesia! Kami menyajikan platform ini sebagai sumber informasi terkini tentang perdagangan dan ekonomi. Dashboard ini dirancang untuk memberikan akses cepat dan mudah kepada Anda untuk mengakses data inflasi, investasi PMDN dan PMA, ekspor, dan impor.

Di dalam dashboard ini, Anda dapat menelusuri data inflasi yang menjadi indikator penting dalam menjaga stabilitas ekonomi. Selain itu, kami juga menyediakan informasi tentang investasi PMDN dan PMA, yang akan memberikan gambaran sektor dan provinsi yang menarik minat investor. Anda juga dapat memantau perkembangan ekspor dan impor, termasuk informasi komoditas, negara tujuan, dan golongan barang yang diekspor dan diimpor. 
                                                    Kami berharap bahwa melalui Dashboard Kementerian Perdagangan ini, Anda dapat memperoleh pemahaman yang lebih baik mengenai tren perdagangan dan kondisi ekonomi saat ini. Kami berkomitmen untuk terus memperbarui dan meningkatkan dashboard ini agar dapat memberikan informasi yang relevan dan akurat bagi Anda.

Terima kasih atas kunjungan Anda ke Dashboard Kementerian Perdagangan Republik Indonesia. Kami berharap informasi yang disajikan di sini dapat mendukung keputusan dan strategi perdagangan yang cerdas bagi Anda. Tetaplah memantau dashboard ini untuk memperoleh pembaruan terbaru yang dapat mendukung kegiatan perdagangan dan ekonomi negara kita."),
                         style="text-align: justify",
                         width = 8)),
            fluidRow(h1(strong("Author"),
                        style="text-align: center"),
                     height=150, width = 12),
            fluidRow(
              box(title = "Abdillah Ilham (5003211069)", imageOutput("ilham", fill = T), style="text-align:center;", width = 6, collapsible = T, height = 400),
              box(title = "Bagas Mujaddid A (5003211048)",imageOutput("bagas"), style="text-align:center;",width = 6, collapsible = T, height = 400)
            )
            ),
    
    tabItem(tabName = "duaharusnya",
            fluidRow(box(h1(strong("Inflasi Atas Dasar Harga Pokok 2020")), width = 12, style="text-align: center")),
            fluidRow(valueBoxOutput("inflasiumum", width = 4), 
                     box(title = strong("Deskripsi Umum"), width = 8,
                         p(
                           paste0(
                             "Harga Barang dan Jasa di Indonesia secara keseluruhan naik dengan tingkat sebesar ",
                             inflasi$Umum[40], " pada bulan April 2023. Selanjutnya kami menyediakan beberapa visualisasi ",
                             "untuk mengetahui perkembangan nilai inflasi untuk setiap sektor serta sektor apa yang memiliki nilai inflasi yang tinggi"
                           ), style="text-align: justify"
                         )),),
            fluidRow(box(dateRangeMonthsInput("tanggalinflasi", label = "Tahun dan Bulan", 
                                              start = min(inflasi$`Kode Tanggal`), end = max(inflasi$`Kode Tanggal`), 
                                              min = min(inflasi$`Kode Tanggal`), max = max(inflasi$`Kode Tanggal`)),width = 6),
                     box(selectizeInput("sektorinflasi", label = "Sektor", choices = choicesinflasi, selected = "Umum", multiple = T, options = list(maxItems = 3)),width= 6),
                     box(plotlyOutput("lineinflasi"),width = 12)),
            fluidRow(box(mydateInput("tanggalinflasi2", value = "2023-04", startview = "month", format = "yyyy-mm", label = "Tahun dan Bulan", min = min(inflasi$`Kode Tanggal`), max = max(inflasi$`Kode Tanggal`)),
                         plotlyOutput("barinflasi"),
                         width = 12
            )
            )
    ),
    tabItem(tabName = "duapmdn",
            fluidRow(
              tabBox( title = "", width = 12,
                      tabPanel("Investasi PMDN Berdasarkan Sektor",
                               fluidRow(
                                 box(h1(strong("Investasi PMDN Berdasarkan Sektor")),width = 12, style="text-align: center")
                               ),
                               fluidRow(
                                 valueBox(
                                   tags$p(sum(investasiPMDNsektor$`2022`)/1000, style = "font-size: 300%;"),
                                   "Total Investasi PMDN",
                                   icon = icon("money",lib='glyphicon'),
                                   color = "success",
                                   width = 4),
                                 box(title = strong("Deskripsi Umum"), width = 8,
                                     p(
                                       paste(
                                         "Nilai Investasi PMDN di Indonesia secara keseluruhan dari semua sektor pada tahun 2022 adalah ",
                                         sum(investasiPMDNsektor$`2022`)/1000, " Triliun Rupiah. Untuk mempermudah, kami ",
                                         "menyediakan visualisasi sektor dengan kontribusi terbesar serta perkembangan nilai investasi untuk semua sektor"
                                       ), style="text-align: justify"
                                     ))
                               ),
                               fluidRow(
                                 box(selectInput("filterbarpmdn", label = "Tahun", choices = c(2017:2022), selected = "2022" ),
                                     plotlyOutput("barpmdn")
                                     , width = 12)
                               ),
                               fluidRow(
                                 box(selectInput("jenissektor1", label = "Jenis Sektor", choices = choicesjenisektor, selected = "Jenis Primer"),width=6),
                                 box(selectizeInput("jenisuraian1", label = "Sektor", choices = NULL, multiple = T, options = list(maxItems=3)),width=6),
                                 box(plotlyOutput("lineinvestasipmdnsektor"), width=12)
                               )
                      ),
                      tabPanel("Investasi PMDN Berdasarkan Provinsi",
                               fluidRow(
                                 box(h1(strong("Investasi PMDN Berdasarkan Provinsi")),width = 12, style="text-align: center")
                               ),
                               fluidRow(
                                 box( selectInput("jeniswilayah1", label = "Jenis Wilayah", choices = choiceswilayah, selected = "Jawa"), width=6),
                                 box( selectizeInput("jenisprovinsi", label = "Provinsi", choices= NULL, multiple=T, options = list(maxItems=3)),width=6)
                               ),
                               fluidRow(
                                 box(plotlyOutput("lineinvestasipmdnprovinsi"), width=12)
                               ),
                               fluidRow(
                                 box(
                                   selectInput("jenistahun1", label = "Tahun", choices = choicestahun1),
                                   plotlyOutput("barpmdnprovinsi"),
                                   width = 12
                                 )
                               ),
                               fluidRow(
                                 box(plotlyOutput("mappmdnprovinsi"), width=12)
                               )
                      )))),
    tabItem(tabName = "duapma",
            fluidRow(
              tabBox(  title = "", width = 12,
                       tabPanel("Investasi PMA Berdasarkan Sektor",
                                fluidRow(
                                  box(h1(strong("Investasi PMA Berdasarkan Sektor")),width = 12, style="text-align: center")
                                ),
                                fluidRow(
                                  valueBox(
                                    tags$p(sum(investasiPMAsektor$`2022`)/1000, style = "font-size: 300%;"),
                                    "Total Investasi PMA",
                                    icon = icon("money",lib='glyphicon'),
                                    color = "success",
                                    width = 4),
                                  box(title = strong("Deskripsi Umum"), width = 8,
                                      p(
                                        paste(
                                          "Nilai Investasi Penanaman Modal Asing (PMA) di Indonesia secara keseluruhan dari semua sektor pada tahun 2022 adalah ",
                                          sum(investasiPMDNsektor$`2022`)/1000, " Miliar Dollar Amerika. Untuk mempermudah, kami ",
                                          "menyediakan visualisasi sektor dengan kontribusi terbesar serta perkembangan nilai investasi untuk semua sektor"
                                        ), style="text-align: justify"
                                      ))
                                ),
                                fluidRow(
                                  box(
                                    selectInput("jenistahunpma1", label = "Tahun", choices = choicestahun1, selected = "2022"),
                                    plotlyOutput("barpma1"),
                                    width = 12
                                  )
                                ),
                                fluidRow(
                                  box(
                                    selectInput("jenissektorpma1", label = "Jenis Sektor", choices = unique(investasiPMAsektor$`Jenis Sektor`), selected = "Sektor Primer"),
                                    width = 6
                                  ),
                                  box(
                                    selectizeInput("jenissektorpma2", label = "Uraian", choices = NULL, multiple = T, options = list(maxItems = 3)),
                                    width = 6
                                  ),
                                  box(
                                    plotlyOutput("linepmasektor"), width = 12
                                  )
                                )
                       ),
                       tabPanel("Investasi PMA Berdasarkan Negara",
                                fluidRow(
                                  box(h1(strong("Investasi PMA Berdasarkan Negara")),width = 12, style="text-align: center")
                                ),
                                fluidRow(
                                  box(
                                    selectInput("jenisbenua1", label = "Benua", choices = choicesbenua, selected = "Asia"),
                                    width = 6
                                  ),
                                  box(
                                    selectizeInput("jenisnegara", label = "Negara", choices=NULL, multiple=T, options = list(maxItems = 3)),
                                    width = 6
                                  )
                                ),
                                fluidRow(
                                  box(
                                    plotlyOutput("linepmanegara"),
                                    width = 12
                                  )
                                ),
                                fluidRow(
                                  box(
                                    selectInput("jenistahun2", label = "Tahun", choices = choicestahun1),
                                    plotlyOutput("barpmanegara"),
                                    leafletOutput("mappmanegara"),
                                    width = 12
                                  )
                                )
                                
                       )),
            )),
    tabItem(tabName = "tigakomoditas",
            fluidRow(box(h1(strong("Ekspor Barang berdasarkan Komoditas")),width = 12, style="text-align: center")),
            fluidRow(box(width =12, 
                         selectizeInput("eksporkomoditas1", label = "Sektor", choices = unique(eksporkomoditas$Uraian), selected = "BESI DAN BAJA",multiple = T, options = list(maxItems = 3)), 
                         plotlyOutput("lineeksporkomoditas"))),
            fluidRow(box(width = 12, 
                         selectInput("eksporkomoditas2", label = "Tahun", choices = choicestahun2, selected = "2018"),
                         plotlyOutput("bareksporkomoditas")
            )
            )
    ),
    tabItem(tabName = "tiganegara",
            fluidRow(
              box(h1(strong("Ekspor Barang berdasarkan Negara Tujuan")),width = 12, style="text-align: center")
            ),
            fluidRow(
              box(
                selectizeInput("filterekspornegara1", label = "Negara", choices = unique(ekspornegara$Negara), multiple = T, options = list(maxItems = 3)),
                plotlyOutput("lineekspornegara"),
                width = 12
              )
            ),
            fluidRow(
              box(
                selectInput("tahunekspornegara", label = "Tahun", choices = choicestahun2),
                plotlyOutput("barekspornegara"),
                width = 12
              )
            ),
            fluidRow(
              box(leafletOutput("mapekspornegara"), width = 12)
            )
            
    ),
    tabItem(tabName = "tigagolongan",
            fluidRow(
              box(h1(strong("Ekspor Barang Berdasarkan Jenis Golongan")), width = 12, style = "text-align: center")
            ),
            fluidRow(
              box(
                selectInput("tahuneksporgolongan", label = "Pilih Tahun", choices = choicestahun2),
                width = 12
              )
            ),
            fluidRow(
              valueBoxOutput("valueeksporgolongan1", width = 3),
              valueBoxOutput("valueeksporgolongan2", width = 3),
              valueBoxOutput("valueeksporgolongan3", width = 3),
              valueBoxOutput("valueeksporgolongan4", width = 3)
            ),
            fluidRow(
              box(selectInput("filtersektorgolongan1", label = "Uraian", choices = unique(eksporgolonganbarang$Uraian) , multiple = T),
                  plotlyOutput("lineeksporgolongan"),
                  width=12)
            )
    ),
    tabItem(tabName = "empatkomoditas",
            fluidRow(
              box(h1(strong("Impor Barang Berdasarkan Jenis Komoditas")), width = 12, style = "text-align: center")
            ),
            fluidRow(box(width =12, 
                         selectizeInput("imporkomoditas1", label = "Sektor", choices = unique(imporkomoditas$Uraian), selected = "BESI DAN BAJA",multiple = T, options = list(maxItems = 3)), 
                         plotlyOutput("lineimporkomoditas"))),
            fluidRow(box(width = 12, 
                         selectInput("imporkomoditas2", label = "Tahun", choices = choicestahun2, selected = "2018"),
                         plotlyOutput("barimporkomoditas")
            )
            )
            
    ),
    tabItem(tabName = "empatnegara",
            fluidRow(
              box(h1(strong("Impor Barang Berdasarkan Negara Pengimpor")),width = 12, style="text-align: center")
            ),
            fluidRow(
              box(
                selectizeInput("filterimpornegara1", label = "Negara", choices = unique(impornegara$Negara), multiple = T, options = list(maxItems = 3)),
                plotlyOutput("lineimpornegara"),
                width = 12
              )
            ),
            fluidRow(
              box(
                selectInput("tahunimpornegara", label = "Tahun", choices = choicestahun2),
                plotlyOutput("barimpornegara"),
                width = 12
              )
            ),
            fluidRow(
              box(leafletOutput("mapimpornegara"), width = 12)
            )
            
    ),
    tabItem(tabName = "empatgolongan",
            fluidRow(
              box(h1(strong("Impor Barang Berdasarkan Jenis Golongan")), width = 12, style = "text-align: center")
            ),
            fluidRow(
              box(
                selectInput("tahunimporgolongan", label = "Pilih Tahun", choices = choicestahun2),
                width = 12
              )
            ),
            fluidRow(
              valueBoxOutput("valimporgol1"),
              valueBoxOutput("valueimporgolongan2"),
              valueBoxOutput("valueimporgolongan3")
            ),
            fluidRow(
              box(selectInput("filtersektorgolongan2", label = "Uraian", choices = unique(imporgolongan$Uraian) , multiple = T),
                  plotlyOutput("lineimporgolongan"),
                  width=12)
            )
    ),
    tabItem(tabName = "limainflasi",
            DTOutput("inflasi"),
            fluidRow(
              box(h1(strong("Metadata"),style="text-align: center"),
                  p("Berikut Variabel yang terdapat pada data Inflasi yang menampilkan skala inflasi pada sektor tersebut adalah : "),
                  p("1. Makanan, Minuman, dan Tembakau"),
                  p("2. Pakaian dan Alas Kaki"),
                  p("3. Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga"),
                  p("4. Perlengkapan, Peralatan, dan Pemeliharaan Rutin Rumah Tangga"),
                  p("5. Kesehatan	"),
                  p("6. Transportasi"),
                  p("7. Informasi, Komunikasi, dan Jasa Keuangan"),
                  p("8. Rekreasi, Olahraga, dan Budaya"),
                  p("9. Pendidikan"),
                  p("10. Penyedia Makanan dan Minuman/Restoran"),
                  p("11. Perawatan Pribadi dan Jasa Lainnya"),
                  p("12. Umum"),
                  width = 12),
            )),
    tabItem(tabName = "limainvestasi",
            fluidRow(
              tabBox( title = "Investasi", width = 12,
                      tabPanel("Investasi PMA Sektor", DTOutput("investasiPMAsektor"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data investasi PMA yang menampilkan skala penanaman modal pada sektor tersebut adalah : "),
                                   p("1. Jenis Sektor"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)),
                      tabPanel("Investasi PMA Negara", DTOutput("investasiPMAnegara"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data investasi PMA yang menampilkan skala penanaman modal pada sektor tersebut adalah : "),
                                   p("1. Benua"),
                                   p("2. Negara"),
                                   p("3. Tahun"),
                                   width = 12)),
                      tabPanel("Investasi PMDN Sektor", DTOutput("investasiPMDNsektor"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data investasi PMDN yang menampilkan skala penanaman modal pada sektor tersebut adalah : "),
                                   p("1. Jenis Sektor"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)),
                      tabPanel("Investasi PMDN Provinsi", DTOutput("investasiPMDNnegara"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data investasi PMA yang menampilkan skala penanaman modal pada sektor tersebut adalah : "),
                                   p("1. Rumpun WIlayah"),
                                   p("2. Provinsi"),
                                   p("3. Tahun"),
                                   width = 12))
              )
            )),
    tabItem(tabName = "limaekspor",
            fluidRow(
              tabBox( title = "Ekspor", width=12,
                      tabPanel("Komoditas Ekspor", DTOutput("eksporkomoditas"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Ekspor komoditas yang menampilkan skala ekspor pada sektor tersebut adalah : "),
                                   p("1. Kode HS"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)),
                      tabPanel("Negara Tujuan Ekspor", DTOutput("ekspornegara"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Ekspor negara yang menampilkan skala ekspor pada sektor tersebut adalah : "),
                                   p("1. Negara"),
                                   p("2. Tahun"),
                                   width = 12)),
                      tabPanel("Golongan/Sektor Barang Ekspor", DTOutput("eksporgolonganbarang"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Ekspor golongan barang yang menampilkan skala ekspor pada sektor tersebut adalah : "),
                                   p("1. Golongan"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)))
            )),
    tabItem(tabName = "limaimpor",
            fluidRow(
              tabBox( title = "Impor", width=12,
                      tabPanel("Komoditas Impor", DTOutput("imporkomoditas"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Impor komoditas yang menampilkan skala impor pada sektor tersebut adalah : "),
                                   p("1. Kode HS"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)),
                      tabPanel("Negara Asal Impor", DTOutput("impornegara"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Impor negara yang menampilkan skala impor pada sektor tersebut adalah : "),
                                   p("1. Negara"),
                                   p("2. Tahun"),
                                   width = 12)),
                      tabPanel("Golongan/Sektor Barang Impor", DTOutput("imporgolonganbarang"),
                               box(h1(strong("Metadata"),style="text-align: center"),
                                   p("Berikut Variabel yang terdapat pada data Impor golongan barang yang menampilkan skala impor pada sektor tersebut adalah : "),
                                   p("1. Golongan"),
                                   p("2. Uraian"),
                                   p("3. Tahun"),
                                   width = 12)))
            ))
    
  )
)

controlbar = dashboardControlbar(
  collapsed = T,
  div(class = "p-3", skinSelector()),
  pinned = F
)





ui = bs4DashPage(header = header,
                 sidebar = sidebar,
                 body = body, 
                 controlbar = controlbar)

server <- function(input, output, session){
  output$ilham = renderImage({
    list(src = "ilham.png", width = 200, height = 270)
  }, deleteFile = F)
  output$bagas = renderImage({
    list(src = "bagas.png", width = 200, height = 250)
  }, deleteFile = F)
  output$pakzul = renderImage({
    list(src="Pak Zul.jpg",
         height="auto",
         width="100%")
  }, deleteFile = F)
  
  output$inflasi <- renderDT(inflasi)
  output$investasiPMAsektor <- renderDT(investasiPMAsektor)
  output$investasiPMAnegara <- renderDT(investasiPMAnegara)
  output$investasiPMDNsektor <- renderDT(investasiPMDNsektor)
  output$investasiPMDNnegara <- renderDT(investasiPMDNprovinsi)
  
  output$eksporkomoditas <- renderDT(eksporkomoditas)
  output$ekspornegara <- renderDT(ekspornegara)
  output$eksporgolonganbarang <- renderDT(eksporgolonganbarang)
  
  output$imporkomoditas = renderDT(imporkomoditas)
  output$impornegara = renderDT(impornegara)
  output$imporgolonganbarang = renderDT(imporgolongan)
  
  output$valimporgol1 = renderValueBox({
    dval1 = imporgolongan%>%
      select(c(1, input$tahunimporgolongan))%>%
      filter(Golongan=="Bahan Baku Penolong")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval1), style = "font-size: 300%;"),
      "Nilai Impor Bahan Baku Penolong",
      icon = icon("list",lib='glyphicon'),
      color = "primary")
  })
  output$valueimporgolongan2 = renderValueBox({
    dval2 = imporgolongan%>%
      select(c(1, input$tahunimporgolongan))%>%
      filter(Golongan=="Barang Konsumsi")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval2), style = "font-size: 300%;"),
      "Nilai Impor Barang Konsumsi",
      icon = icon("cutlery",lib='glyphicon'),
      color = "purple")
  })
  output$valueimporgolongan3 = renderValueBox({
    dval3 = imporgolongan%>%
      select(c(1, input$tahunimporgolongan))%>%
      filter(Golongan=="Barang Modal")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval3), style = "font-size: 300%;"),
      "Nilai Impor Barang Modal",
      icon = icon("usd",lib='glyphicon'),
      color = "orange")
  })
  
  output$valueeksporgolongan1 = renderValueBox({
    dval4 = eksporgolonganbarang%>%
      select(c(1, input$tahuneksporgolongan))%>%
      filter(Golongan=="Industri Pengolahan")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval4), style = "font-size: 300%;"),
      "Nilai Ekspor Industri Pengolahan",
      icon = icon("list",lib='glyphicon'),
      color = "purple")
  })
  
  output$valueeksporgolongan2 = renderValueBox({
    dval5 = eksporgolonganbarang%>%
      select(c(1, input$tahuneksporgolongan))%>%
      filter(Golongan=="Pertanian")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval5), style = "font-size: 300%;"),
      "Nilai Ekspor Golongan Pertanian",
      icon = icon("tag",lib='glyphicon'),
      color = "warning")
  })
  output$valueeksporgolongan3 = renderValueBox({
    dval6 = eksporgolonganbarang%>%
      select(c(1, input$tahuneksporgolongan))%>%
      filter(Golongan=="Pertambangan")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval6), style = "font-size: 300%;"),
      "Nilai Ekspor Pertambangan",
      icon = icon("road",lib='glyphicon'),
      color = "primary")
  })
  output$valueeksporgolongan4 = renderValueBox({
    dval7 = eksporgolonganbarang%>%
      select(c(1, input$tahuneksporgolongan))%>%
      filter(Golongan=="Lainnya")%>%
      select(c(2))
    
    valueBox(
      tags$p(sum(dval7), style = "font-size: 300%;"),
      "Nilai Ekspor Golongan Lainnya",
      icon = icon("lock",lib='glyphicon'),
      color = "orange")
  })
  
  
  
  output$lineinflasi = renderPlotly({
    validate(
      need(length(input$sektorinflasi)>0, "Masukkan minimal satu sektor")
    )
    
    inflasi%>%
      select(-c(1,2))%>%
      rename(Tanggal = "Kode Tanggal")%>%
      filter(Tanggal>= input$tanggalinflasi[1] & Tanggal<=input$tanggalinflasi[2])%>%
      select("Tanggal", input$sektorinflasi)%>%
      gather("Sektor", value = "Nilai", input$sektorinflasi)%>%
      plot_ly(
        x = ~Tanggal,
        y = ~Nilai,
        split = ~Sektor,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Inflasi",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Sektor, "<br>", "Inflasi : ", Nilai
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F,
        yaxis = list(title = "Inflasi")
      )
  })
  
  output$barinflasi = renderPlotly({
    binflasi = inflasi %>%
      filter(`Kode Tanggal` == input$tanggalinflasi2)%>%
      select(3:13)%>%
      pivot_longer(
        cols = c("Makanan, Minuman, dan Tembakau", "Pakaian dan Alas Kaki", "Perumahan, Air, Listrik, dan Bahan Bakar Rumah Tangga", "Perlengkapan, Peralatan, dan Pemeliharaan Rutin Rumah Tangga", "Kesehatan", "Transportasi", "Informasi, Komunikasi, dan Jasa Keuangan", "Rekreasi, Olahraga, dan Budaya", "Penyedia Makanan dan Minuman/Restoran", "Perawatan Pribadi dan Jasa Lainnya", "Umum"),
        names_to = "Sektor",
        values_to = "Nilai"
      )%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Sektor,
        x = ~Nilai,
        type = "bar",
        name = "Sektor dengan Inflasi Terbesar",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Sektor, "<br>", "Nilai Inflasi : ", Nilai
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
    
  })
  
  d311= reactive({
    linvestasipmdnsektor = NULL
    linvestasipmdnsektor = subset(investasiPMDNsektor, investasiPMDNsektor$`Jenis Sektor`== input$jenissektor1)
    return(linvestasipmdnsektor)
  })
  observeEvent(d311(), {updateSelectizeInput(session, "jenisuraian1", choices = d311()$Uraian)})
  
  d312 = reactive({
    linvestasipmndprovinsi = NULL
    linvestasipmndprovinsi = subset(investasiPMDNprovinsi, investasiPMDNprovinsi$`Rumpun Wilayah`==input$jeniswilayah1)
    return(linvestasipmndprovinsi)
  })
  observeEvent(d312(), {updateSelectizeInput(session, "jenisprovinsi", choices = d312()$Provinsi)})
  
  d321 = reactive({
    linvestasipmasektor = NULL
    linvestasipmasektor = subset(investasiPMAsektor, investasiPMAsektor$`Jenis Sektor`==input$jenissektorpma1)
    return(linvestasipmasektor)
  })
  observeEvent(d321(), {updateSelectizeInput(session, "jenissektorpma2", choices = d321()$Uraian)})
  d322 = reactive({
    linvestasipmanegara = NULL
    linvestasipmanegara = subset(investasiPMAnegara, investasiPMAnegara$Benua==input$jenisbenua1)
    return(linvestasipmanegara)
  })
  observeEvent(d322(), {updateSelectizeInput(session, "jenisnegara", choices = d322()$Negara)})
  
  
  
  output$inflasiumum = renderValueBox({
    valueBox(
      tags$p(inflasi$Umum[40], style = "font-size: 300%;"),
      "Nilai Inflasi Umum",
      icon = icon("money",lib='glyphicon'),
      color = "success")
  })
  
  output$barpmdn = renderPlotly({
    investasiPMDNsektor %>%
      select(Uraian, input$filterbarpmdn)%>%
      rename(Nilai = input$filterbarpmdn)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Uraian,
        x = ~Nilai,
        type = "bar",
        name = "Investasi Dalam Negeri",
        hoverinfo = "text",
        hovertext = ~paste(
          "Jenis Sektor : ", Uraian, "<br>", "Nilai PMDN : ", round(Nilai/1000, 2), " Triliun Rupiah"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
    
    
    
  })
  
  
  
  output$lineinvestasipmdnsektor = renderPlotly({
    validate(
      need(length(input$jenisuraian1)>0, "Masukkan minimal satu sektor")
    )
    
    investasiPMDNsektor%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:7)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun", input$jenisuraian1))%>%
      gather(key = "Sektor", value = "Nilai", input$jenisuraian1)%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Sektor,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Investasi PMDN Berdasarkan Sektor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Sektor, "<br>", "PMDN : ", round(Nilai/1000, 2), " Triliun Rupiah"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
    
  })
  
  output$lineinvestasipmdnprovinsi = renderPlotly({
    validate(
      need(length(input$jenisprovinsi)>0, "Masukkan minimal satu provinsi")
    )
    investasiPMDNprovinsi%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:7)%>%
      spread(key = "Provinsi", value = "Nilai")%>%
      select(c("Tahun",input$jenisprovinsi))%>%
      gather(key = "Provinsi",value = "Nilai", c(input$jenisprovinsi))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Provinsi,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Investasi PMDN Berdasarkan Provinsi",
        hoverinfo = "text",
        hovertext = ~paste(
          "Provinsi : ", Provinsi, "<br>", "PMDN : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$barpmdnprovinsi = renderPlotly({
    
    investasiPMDNprovinsi %>%
      select(Provinsi, input$jenistahun1)%>%
      rename(Nilai = input$jenistahun1)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Provinsi,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Provinsi dalam Kontribusi PMDN",
        hoverinfo = "text",
        hovertext = ~paste(
          "Provinsi : ", Provinsi, "<br>", "Nilai PMDN : ", round(Nilai/1000000, digits = 2), " Triliun Rupiah" 
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$mappmdnprovinsi = renderPlotly({
    shppmdn = shp1 %>%
      left_join(investasiPMDNprovinsi, by = c("NAME_1"="Provinsi"))%>%
      rename(Provinsi=NAME_1)%>%
      dplyr::select(c(1:11), input$jenistahun1)%>%
      rename(PMDN = input$jenistahun1)
    
    plot_ly(
      shppmdn,
      color = ~PMDN,
      split = ~Provinsi,
      alpha = 1,
      showlegend = F,
      colors = "Reds",
      stroke = I("black"),
      text = ~paste('</br>', Provinsi, '</br>', PMDN),
      hoverinfo = "text")
    
  })
  
  output$barpma1 = renderPlotly({
    investasiPMAsektor %>%
      select(Uraian, input$jenistahunpma1)%>%
      rename(Nilai = input$jenistahunpma1)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Uraian,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Sektor dalam Kontribusi PMA",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Uraian, "<br>", "Nilai PMA : ", round(Nilai/1000, digits = 2), " Miliar $US"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$linepmasektor = renderPlotly({
    validate(
      need(length(input$jenissektorpma2)>0, "Masukkan minimal satu sektor")
    )
    investasiPMAsektor%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:7)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun",input$jenissektorpma2))%>%
      gather(key = "Uraian",value = "Nilai", c(input$jenissektorpma2))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Uraian,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Investasi PMA Berdasarkan Sektor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Uraian, "<br>", "PMA : ", round(Nilai/1000, 2), " Triliun Rupiah"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$linepmanegara = renderPlotly({
    validate(
      need(length(input$jenisnegara)>0, "Masukkan minimal satu negara")
    )
    
    investasiPMAnegara%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:7)%>%
      spread(key = "Negara", value = "Nilai")%>%
      select(c("Tahun",input$jenisnegara))%>%
      gather(key = "Negara",value = "Nilai", c(input$jenisnegara))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Negara,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Investasi PMA Berdasarkan Negara",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "PMA : ", round(Nilai/1000, 2), " Triliun Rupiah"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$mappmanegara= renderLeaflet({
    mappmaplot = mappma%>%
      dplyr::select(c(1:4, input$jenistahun2))%>%
      dplyr::rename(PMA = input$jenistahun2)%>%
      dplyr::filter(PMA!="#N/A")
    
    
    mappmaplot
    leaflet(mappmaplot)%>%
      addMarkers(~Longitude, 
                 ~Latitude, 
                 label = ~Negara, 
                 popup = ~paste(
                   "<b>", Negara, "</b>",
                   "<br>", PMA
                 ))%>%
      addTiles()
    
    
  })
  
  
  
  output$barpmanegara = renderPlotly({
    investasiPMAnegara %>%
      select(Negara, input$jenistahun2)%>%
      rename(Nilai = input$jenistahun2)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Negara,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Negara dalam Kontribusi PMA",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "Nilai PMA : ", round(Nilai/1000000, digits = 2), " Miliar $US" 
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$lineeksporkomoditas = renderPlotly({
    validate(
      need(length(input$eksporkomoditas1)>0, "Masukkan minimal satu jenis komoditas")
    )
    
    eksporkomoditas%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun",input$eksporkomoditas1))%>%
      gather(key = "Uraian",value = "Nilai", c(input$eksporkomoditas1))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Uraian,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Ekspor berdasarkan Komoditas",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Uraian, "<br>", "Ekspor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$bareksporkomoditas = renderPlotly({
    eksporkomoditas %>%
      select(Uraian, input$eksporkomoditas2)%>%
      rename(Nilai = input$eksporkomoditas2)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Uraian,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Sektor dalam Ekspor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Uraian, "<br>", "Nilai Ekspor : ", round(Nilai/1000, digits = 2), " Miliar $US"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$lineekspornegara = renderPlotly({
    validate(
      need(length(input$filterekspornegara1)>0, "Masukkan minimal satu negara")
    )
    ekspornegara%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Negara", value = "Nilai")%>%
      select(c("Tahun",input$filterekspornegara1))%>%
      gather(key = "Negara",value = "Nilai", c(input$filterekspornegara1))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Negara,
        type = "scatter", 
        mode = "lines",
        name = "Ekspor Berdasarkan Negara Tujuan",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "Ekspor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$barekspornegara = renderPlotly({
    ekspornegara %>%
      select(Negara, input$tahunekspornegara)%>%
      rename(Nilai = input$tahunekspornegara)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Negara,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Negara Tujuan Ekspor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "Nilai Ekspor : ", round(Nilai/1000, digits = 2), " Miliar $US"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$lineeksporgolongan = renderPlotly({
    validate(
      need(length(input$filtersektorgolongan1)>0,"Masukan minimal 1 uraian")
    )
    eksporgolonganbarang%>%
      select(-c(Golongan, HS))%>%
      dplyr::distinct(Uraian, .keep_all = T)%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun",input$filtersektorgolongan1))%>%
      gather(key = "Uraian", value = "Nilai", c(input$filtersektorgolongan1))%>%
      plot_ly(
        x= ~Tahun,
        y= ~Nilai,
        split = ~Uraian,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Ekspor berdasarkan Golongan Barang",
        hoverinfo = "text",
        hovertext = ~paste(
          "Uraian : ", Uraian, "<br>", "Ekspor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$valueimporgolongan1 = renderValueBox({
    valueBox(
      format(eksporgolonganbarang$Golongan, format="f", big.mark = ","
             , paste("Golongan Industri Pengolahan", eksporgolonganbarang$'2018')
             , icon = icon("database"), color = "#17a2b8")
    )
  })
  
  output$lineimporkomoditas = renderPlotly({
    validate(
      need(length(input$imporkomoditas1)>0, "Masukkan minimal satu jenis komoditas")
    )
    
    imporkomoditas%>%
      select(-c(1))%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun",input$imporkomoditas1))%>%
      gather(key = "Uraian",value = "Nilai", c(input$imporkomoditas1))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Uraian,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Impor berdasarkan Komoditas",
        hoverinfo = "text",
        hovertext = ~paste(
          "Uraian : ", Uraian, "<br>", "Impor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$barimporkomoditas = renderPlotly({
    imporkomoditas %>%
      select(Uraian, input$imporkomoditas2)%>%
      rename(Nilai = input$imporkomoditas2)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Uraian,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Sektor dalam Kontribusi Impor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Sektor : ", Uraian, "<br>", "Nilai Impor : ", round(Nilai/1000, digits = 2), " Miliar $US"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$lineimpornegara = renderPlotly({
    validate(
      need(length(input$filterimpornegara1)>0, "Masukkan minimal satu negara")
    )
    impornegara%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Negara", value = "Nilai")%>%
      select(c("Tahun",input$filterimpornegara1))%>%
      gather(key = "Negara",value = "Nilai", c(input$filterimpornegara1))%>%
      plot_ly(
        x = ~Tahun,
        y = ~Nilai,
        split = ~Negara,
        type = "scatter", 
        mode = "lines",
        name = "Impor Berdasarkan Negara Tujuan",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "Impor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  output$barimpornegara = renderPlotly({
    impornegara %>%
      select(Negara, input$tahunimpornegara)%>%
      rename(Nilai = input$tahunimpornegara)%>%
      dplyr::arrange(desc(Nilai))%>%
      slice_head(n=7)%>%
      plot_ly(
        y = ~Negara,
        x = ~Nilai,
        type = "bar",
        name = "Top 7 Negara Pengimpor",
        hoverinfo = "text",
        hovertext = ~paste(
          "Negara : ", Negara, "<br>", "Nilai Impor : ", round(Nilai/1000, digits = 2), " Miliar $US"
        )
      )%>%
      layout(yaxis = list(categoryorder = "total ascending", title = ""),
             hoverlabel = list(align = "left"))
  })
  
  output$mapekspornegara= renderLeaflet({
    mapeksporplot = mapekspor%>%
      dplyr::select(c(1:4, input$tahunekspornegara))%>%
      dplyr::rename(Ekspor = input$tahunekspornegara)%>%
      dplyr::filter(Ekspor!=0)
    
    
    mapeksporplot
    leaflet(mapeksporplot)%>%
      addMarkers(~Longitude, 
                 ~Latitude, 
                 label = ~Negara, 
                 popup = ~paste(
                   "<b>", Negara, "</b>",
                   "<br>", Ekspor, " Juta $US"
                 ))%>%
      addTiles()
  })
  
  output$mapimpornegara= renderLeaflet({
    mapimporplot = mapimpor%>%
      dplyr::select(c(1:4, input$tahunimpornegara))%>%
      dplyr::rename(Impor = input$tahunimpornegara)%>%
      dplyr::filter(Impor!=0)
    
    
    mapimporplot
    leaflet(mapimporplot)%>%
      addMarkers(~Longitude, 
                 ~Latitude, 
                 label = ~Negara, 
                 popup = ~paste(
                   "<b>", Negara, "</b>",
                   "<br>", Impor, " Juta $US"
                 ))%>%
      addTiles()
  })
  
  output$lineimporgolongan = renderPlotly({
    validate(
      need(length(input$filtersektorgolongan2)>0,"Masukan minimal 1 uraian")
    )
    imporgolongan%>%
      select(-c(Golongan))%>%
      dplyr::distinct(Uraian, .keep_all = T)%>%
      gather(key = "Tahun", value = "Nilai", 2:6)%>%
      spread(key = "Uraian", value = "Nilai")%>%
      select(c("Tahun",input$filtersektorgolongan2))%>%
      gather(key = "Uraian", value = "Nilai", c(input$filtersektorgolongan2))%>%
      plot_ly(
        x= ~Tahun,
        y= ~Nilai,
        split = ~Uraian,
        type = "scatter", 
        mode = "lines",
        name = "Perkembangan Impor berdasarkan Golongan Barang",
        hoverinfo = "text",
        hovertext = ~paste(
          "Uraian : ", Uraian, "<br>", "Impor : ", round(Nilai/1000, 2), " Miliar $US"
        )
      )%>%
      layout(
        hoverlabel = list(align = "left"),
        showlegend = F
      )
  })
  
  
  
  
  
  
  
}


shinyApp(ui = ui, server = server)