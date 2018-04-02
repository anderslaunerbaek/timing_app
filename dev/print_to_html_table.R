#
#' print_to_html_table
#'
#' @param tmp data.frame tmp[,c("Placering","Navn", "Nummer","Tid")].
#'
#' @return str of a html table
#'
print_to_html_table <- function(tmp) {
    #
    tmp_str <- ""
    for (ii in 1:dim(tmp)[1]) {
        tmp_str <- paste0(tmp_str,"<tr>")
        for (jj in 1:dim(tmp)[2]) {
            tmp_str <- paste0(tmp_str, paste0("<td>",tmp[ii, jj],"</td>"))
        }
        tmp_str <- paste0(tmp_str,print("</tr>"))
    }
    print(tmp_str)
}
