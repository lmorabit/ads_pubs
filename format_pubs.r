#!/usr/bin/env R
#
# format_pubs.r
# 
# This was designed to nicely format at sort publications from an ADS export
# and format them for LaTeX as a file to input (i.e., in another LaTeX document
# with a preamble and begin/end document, use \input{mypubs.tex}
#
# How to use:
# 1) conduct your author search on https://ui.adsabs.harvard.edu/
# 2) Select 'Export' > 'Other Formats'
# 3) Select 'Custom format'
# 4) Use the following custom format:
#   %ZEncoding:latex %5.3A XXXtextit{%T}. %q, %V, %p-%P. XXXtextbf{%Y}, cit. %c %D  \n
#       NB: do not change this format line or it will break the code
# 5) Click 'Download to File' and save the file somewhere.
#       NB: File must exist in directory you run the script, or you should give an absolute path
# 6) Change the 'myname' value to your last name, and change 'myfile' if you did not use the default downloaded file name. Select if you want to sort on year, citation or on chronological only.
# 7) Run from the command line as:
#       > Rscript format_pubs.r
# NOTE: you will probably need to add/update the journal names/journal abbreviation lists as necessary. Just make sure the index of the name and abbreviation are the same in each vector. 

####### CHECK THESE VARIABLES AND CHANGE AS NECESSARY

myfile <- 'export-custom.txt'
myname <- 'Morabito'
do_chron <- FALSE  ## if set to true, sort on chronology only. if false: Year, then number of citations

############### Helper functions

check_jrnl <- function( string_to_check ){
    ## check if it's in a journal (if not, it's a conference proceeding), replace full name with abbreviation.
    ## adjust these lists as necessary
    journal_names <- c( 'Monthly Notices of the Royal Astronomical Society', 'The Astrophysical Journal', 'Astronomy and Astrophysics', 'ArXiv e-prints', 'SF2A-2016: Proceedings of the Annual meeting of the French Society of Astronomy and Astrophysics', 'Advancing Astrophysics with the Square Kilometre Array (AASKA14)' )
    journal_abbrv <- c( 'MNRAS', 'ApJ', 'A\\\\&A', 'ArXiv e-prints', 'SF2A-2016: Proceedings of the Annual meeting of the French Society of Astronomy and Astrophysics', 'Advancing Astrophysics with the Square Kilometre Array (AASKA14)' )

    ## search the string
    tmp1 <- strsplit( string_to_check, '}.', fixed=TRUE )[[1]][2]
    tmp2 <- trimws(strsplit( tmp1, ',', fixed=TRUE )[[1]][1])
    jrnl <- trimws( tmp2[length(tmp2)] )        
    chk_val <- 0
    if ( jrnl %in% journal_names ){
        jrnl_idx <- which( journal_names == jrnl )
        string_to_check <- gsub( jrnl, journal_abbrv[jrnl_idx], string_to_check )
        chk_val <- 1
    }

    return( list( chk_val=chk_val, pub=string_to_check ) )
}

format_lists <- function( my_list, chronological=FALSE ){
    # format a list of citations by either chronology or year then citations 
   
    final_list <- c()

    n_cite <- c() 
    pub_year <- c()
    pub_month <- c() 
    for ( ii in seq(1,length(my_list)) ){
        ## add \item to beginning
        my_list[ii] <- paste( '\\item', my_list[ii] )
        tmp <- strsplit( my_list[ii], ',' )[[1]]
        n_cite <- c( n_cite, as.numeric( strsplit( tmp[length(tmp)], ' ' )[[1]][3] ) )
        tmpd <- strsplit( tmp[length(tmp)], ' ' )[[1]][4]
        pub_year <- c( pub_year, as.numeric( strsplit( tmpd, '/' )[[1]][2] ) ) 
        pub_month <- c( pub_month, as.numeric( strsplit( tmpd, '/' )[[1]][1] ) )
        ## drop the date from the end
        tmp <- strsplit( my_list[ii], ' ' )[[1]]
        my_list[ii] <- paste( tmp[seq(1,length(tmp)-1)], collapse=' ' )
    }

    ## loop through years
    years <- sort( unique( pub_year ), decreasing=TRUE )
    for ( year in years ){
        yr_idx <- which( pub_year == year )
        yr_list <- my_list[yr_idx]
        n_c <- n_cite[yr_idx]
        mo_list <- pub_month[yr_idx]
        if ( chronological ){
            ## order by month
            mo_idx <- order( mo_list, decreasing=TRUE )
            yr_list <- yr_list[mo_idx]
        } else {
            ## order by citations
            cite_idx <- order( n_c, decreasing=TRUE )
            yr_list <- yr_list[cite_idx]
        }
        final_list <- c( final_list, yr_list )
    }
    
    for ( ii in seq(1,length(final_list)) ){
        my_item <- final_list[ii]
        list_number <- paste( c( 'item[(', as.character(ii), ')]' ), collapse='' )
        final_list[ii] <- gsub( 'item', list_number, my_item )
    }
    
    return( list( pubs=final_list, ncite=sum(n_cite) ) )

}

# Read the file
A <- readLines(con=myfile)

## join text together per entry
bibitems <- c()
i <- 1
bibit <- ''
while ( i <= length(A) ){

    tmp <- A[i]
    if ( tmp == "" ){
        bibitems <- c( bibitems, trimws(bibit) )
        bibit <- ''
    } else {
        bibit <- paste( bibit, A[i], sep=' ' )   
    }
    i <- i + 1
}

## get rid of empty entries
valid_idx <- which( grepl( " ", bibitems ) )
bibitems <- bibitems[valid_idx]
## get rid of catalogues
valid_idx <- which( ! grepl( 'VizieR', bibitems ) )
bibitems <- bibitems[valid_idx]
n_catalogues <- length(valid_idx)
## and proposals
valid_idx <- which( ! grepl( 'ATNF Proposal', bibitems ) )
bibitems <- bibitems[valid_idx] 
## remove PhD thesis
valid_idx <- which( ! grepl( 'Ph.D. Thesis', bibitems ) )
bibitems <- bibitems[valid_idx]
## remove corrigendum and erratum
valid_idx <- intersect( which( ! grepl( 'Corrigendum', bibitems ) ), which( ! grepl( 'Erratum', bibitems ) ) ) 
bibitems <- bibitems[valid_idx]
## Add other things here

## format italics / bold
bibitems <- gsub( 'XXX', '\\\\', bibitems )
## and the number symbol
bibitems <- gsub( '[#]', '\\\\#', bibitems )
## less than signs
bibitems <- gsub( '\\&lt;', '$<$', bibitems, fixed=TRUE )

## sort into first author, co-author 
first_author <- c()
second_author <- c()
co_author <- c()
conf_proceed <- c()

for ( bibit in bibitems ){

    tmp <- strsplit( bibit, ',' )[[1]]
    if ( tmp[1] == myname ){
        ## first author, bold name
        bold_author <- paste( c('\\textbf{', tmp[1], ',', tmp[2], '} '), collapse='' )
        ## check if it's a 2-author paper
        if ( grepl( 'and', bold_author ) ){
            ## move the trailing }
            bold_author <- gsub( '}', '', bold_author )
            bold_author <- gsub( ' and', '} and', bold_author )   
        }
        pub_item <- paste( c( bold_author, tmp[seq(3,length(tmp))]), collapse=',' )
        ## check if journal
        result <- check_jrnl( pub_item )
        if ( result$chk_val == 0 ){
            ## not in a journal, must be a conference proceeeding
            conf_proceed <- c( conf_proceed, pub_item )
        } else {
            ## add it to first author list
            first_author <- c( first_author, result$pub )
        }
    } else {
        ## check second author
        if ( trimws(tmp[3]) == myname ){
            bold_author <- paste( c('\\textbf{', tmp[3], ',', tmp[4], '} '), collapse='' )
            pub_item <- paste( c( tmp[c(1,2)], bold_author, tmp[seq(5,length(tmp))]) , collapse=',' )
            ## check if journal 
            result <- check_jrnl( pub_item )
            if ( result$chk_val == 0 ){
                ## conf proceeding
                conf_proceed <- c( conf_proceed, pub_item )
            } else {
                ## second author
                second_author <- c( second_author, result$pub )
            } ## endelse
        } else {
            ## coauthor, check for name
            if ( length(grep( myname, bibit )) == 1 ){
                ## my name is there, make it bold
                print( 'filler!' )
            } else {
                ## check journal or coauthor
                result <- check_jrnl( bibit )
                if ( result$chk_val == 0 ){
                    ## conference proceeding
                    conf_proceed <- c( conf_proceed, bibit )
                } else {
                    co_author <- c( co_author, result$pub )
                } ## endelse
            } ## endelse
        } ## endelse
    } ## endelse
} ## endfor


## loop through lists to number and add items

first_formatted <- format_lists( first_author, chronological=do_chron )
second_formatted <- format_lists( second_author, chronological=do_chron )
co_formatted <- format_lists( co_author, chronological=do_chron )
conf_formatted <- format_lists( conf_proceed, chronological=do_chron )

## collect citation info
first_cite <- first_formatted$ncite
second_cite <- second_formatted$ncite
co_cite <- co_formatted$ncite
conf_cite <- conf_formatted$ncite

total_cite <- first_cite + second_cite + co_cite + conf_cite

str1 <- '\\mysec{Publication Record}\n'
str2 <- paste( c( length(first_author), 'first-author,', length(second_author), 'second-author, and', length(co_author),'co-author peer-reviewed publications; also', length(conf_proceed), 'conference proceedings. Total of', total_cite, 'citations as of',format(Sys.Date(),"%d %b %Y"),'.\\\\[-6pt] \n' ), collapse=' ' )
str3 <- '\\textbf{First Author}\\\\[-18pt] \n' 
str4 <- '\\textbf{Second Author}\\\\[-18pt] \n' 
str5 <- '\\textbf{Co-Author}\\\\[-18pt] \n' 
str6 <- '\\textbf{Conference proceedings}\\\\[-18pt] \n' 
strl1 <- '\\blip'
strl2 <- '\\eli \n'  

mylines <- c( str1, str2, str3, strl1, first_formatted$pubs, strl2, str4, strl1, second_formatted$pubs, strl2, str5, strl1, co_formatted$pubs, strl2, str6, strl1, conf_formatted$pubs, strl2 )

writeLines( mylines, con='mypubs.tex' )
print( 'Done, mypubs.tex written.' )




