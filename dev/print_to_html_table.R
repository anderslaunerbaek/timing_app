## print table

tmp <- km3[,c("Placering","Navn", "Nummer","Tid")]
#tmp <- km5[,c("Placering","Navn", "Nummer","Tid")]
#tmp <- km10[,c("Placering","Navn", "Nummer","Tid")]
#tmp <- km21[,c("Placering","Navn", "Nummer","Tid")]


tmp_str <- ""
for (ii in 1:dim(tmp)[1]) {
    tmp_str <- paste0(tmp_str,"<tr>")
    for (jj in 1:dim(tmp)[2]) {
        tmp_str <- paste0(tmp_str, paste0("<td>",tmp[ii, jj],"</td>"))
    }
    tmp_str <- paste0(tmp_str,print("</tr>"))
}
print(tmp_str)
