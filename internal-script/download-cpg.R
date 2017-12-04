
# download code page tables from microsoft page
# requires internet connection

urls <- paste("http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/",
              c("CP874.TXT", "CP932.TXT", "CP936.TXT",
                "CP949.TXT", "CP950.TXT", "CP1250.TXT",
                "CP1251.TXT", "CP1252.TXT", "CP1253.TXT",
                "CP1254.TXT", "CP1255.TXT", "CP1256.TXT",
                "CP1257.TXT", "CP1258.TXT"),
              sep = "")

savedir <- "data-raw"
if (!dir.exists(savedir)) dir.create(savedir)

for (url in urls)
{
  download.file(url, file.path(savedir, basename(url)))
}




# download code page table for MAC
urls <- paste("http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/MAC/",
              c("CYRILLIC.TXT", "GREEK.TXT", "ICELAND.TXT",
                "LATIN2.TXT", "ROMAN.TXT", "TURKISH.TXT"),
              sep = "")

savedir <- "data-raw"
if (!dir.exists(savedir)) dir.create(savedir)

for (url in urls)
{
  download.file(url, file.path(savedir, paste("mac-", basename(url), sep="")))
}
