#create an wiki entry - create page and corresponding link on homepage
wiki.create.entry <- function(wiki, home, title, page){
	outfile <- paste(wiki, page, ".md", sep="")
	wiki.index <- readLines(paste(wiki, main, sep=""), warn = FALSE)
	if (all(wiki.index != paste("[[", title, " | ", page, "]]", sep=""))){ #only add it not existent yet
		cat("\n\n", file = paste(wiki, main, sep=""), append=TRUE)
		cat(paste("[[", title, " | ", page, "]]", sep = ""), file = paste(wiki, main, sep=""), append = TRUE) #create link to new page (with 2 newline character preceeding)
		cat(file = outfile) #create new wiki page
		cat(paste("#", title), file = outfile, append=TRUE) #create title
		wiki.newline(wiki, page)
		# system(paste("git add", page)) #add to git wiki-repo
	} else {
		print("entry exists!")
	}
}

#remove all the content form a wiki page - history will be git-tracked
wiki.wipe.entry <- function(wiki, page, title){
	outfile <- paste(wiki, page, ".md", sep="")
	title <- readLines(outfile, warn = FALSE)[1] #extract title
	cat(title, file = outfile) #create new wiki page
	wiki.newline(wiki, page)
}

#create an internal link on the wiki
wiki.internal.link <- function(wiki, page, link, caption=NULL){
	outfile <- paste(wiki, page, ".md", sep="")
	if (length(caption) > 0){
		cat(paste("[[", caption, " | ", link, "]]", sep=""), file = outfile, append=TRUE)		
	} else {
		cat(paste("[[", link, "]]", sep=""), file = outfile, append=TRUE)		
	}
}

#create an external link on the wiki
wiki.external.link <- function(wiki, page, link, caption=NULL){
	outfile <- paste(wiki, page, ".md", sep="")
	if (length(caption) > 0){
		cat(paste("[[", caption, " | ", link, "]]", sep=""), file = outfile, append=TRUE)
	} else {
		cat(link, file = outfile, append=TRUE)
	}
}

#add a pmid link to wiki (base on wiki.external.link)
wiki.pmid <- function(wiki, page, pmid){
	wiki.external.link(wiki=wiki, page=page, link=paste("http://www.ncbi.nlm.nih.gov/pubmed/", pmid, sep=""), caption=pmid)
}

#append text to wiki
wiki.text <- function(wiki, page, text){
	outfile <- paste(wiki, page, ".md", sep="")
	cat(text, file = outfile, append=TRUE)
}

#create new paragraph in wiki
wiki.newline <- function(wiki, page){
	outfile <- paste(wiki, page, ".md", sep="")
	cat("\n\n", file = outfile, append=TRUE)
}

#format text according to wiki markdown rules
wiki.text.format <- function(wiki, page, text, format=c("bold", "italic", "underlined", "monospaced")){
	outfile <- paste(wiki, page, "/index.md", sep="")
	bold <- "**"
	italic <- "//"
	underlined <- "__"
	monospaced <- "''"
	for (i in 1:length(format)){
		text <- paste(get(format[i]), text, get(format[i]), sep="")		
	}
	cat(text, file = outfile, append=TRUE)
}
	

#create header of section in wiki
wiki.section <- function(wiki, page, subtitle, level){
	outfile <- paste(wiki, page, ".md", sep="")
	if (level == 1) title <- paste("# ", subtitle)
	if (level == 2) title <- paste("## ", subtitle)
	if (level == 3) title <- paste("### ", subtitle)
	cat(paste(subtitle, "\n", sep=""), file = outfile, append = TRUE)
}


#draw line for wiki
wiki.line <- function(wiki, page){
	outfile <- paste(wiki, page, "/index.md", sep="")
	cat("----\n", file = outfile, append=TRUE)
}


#create footnote for wiki
wiki.footnote <- function(wiki, page, text){
	outfile <- paste(wiki, page, "/index.md", sep="")
	text <- paste("((", text, "))")
	cat(paste(text, "\n", sep=""), file = outfile, append=TRUE)	
}

#create list for wiki
wiki.list <- function(wiki, page, text, levels, style = c("ordered", "unordered")){
	outfile <- paste(wiki, page, "/index.md", sep="")
	if (style == "ordered"){
		for(i in 1:length(text)){
			indent <- paste(rep("	", levels[i]), collapse="")
			level <- paste(indent, "- ", sep="") 
			cat(paste(level, text[i], "\n", sep=""), file = outfile, append=TRUE)
		}
	}
	if (style == "unordered"){
		for(i in 1:length(text)){
			indent <- paste(rep("	", levels[i]), collapse="")
			level <- paste(indent, "* ", sep="") 
			cat(paste(level, text[i], "\n", sep=""), file = outfile, append=TRUE)
		}
	}
}

#create plot for wiki
wiki.plot <- function(wiki, page, plot, caption=NULL, width=NULL, height=NULL, alignment="left"){
	outfile <- paste(wiki, page, ".md", sep="")
	if (length(width) > 0){ #set width in pixels
		plot <- paste(plot, " | ", "width=", width, "px", sep="")
	}
	if (length(height) > 0){ #set height in pixels
		plot <- paste(plot, " | ", "height=", height, "px", sep="")
	}
	if (length(caption) > 0){ #set height in pixels
		plot <- paste(plot, " | ", "frame | alt=", caption, sep="")
	}
	#define alignment
	plot <- paste(plot, " | ", "align=", alignment, sep="")
	cat(paste("[[", plot, "]]", "\n", sep=""), file = outfile, append=TRUE)
}

#create table for wiki
wiki.table <- function(wiki, page, table){
	outfile <- paste(wiki, page, ".md", sep="")
	cat("<table> \n", file = outfile, append=TRUE) #start of the table
	#add column names
	headings <- colnames(table)
	if (length(headings) > 0){
		cat("<tr>\n", file = outfile, append=TRUE)
		headings <- c(paste("<th>", headings, "</th>", sep=""), "\n") #"<th>" for headers
		cat(paste(headings, collapse=""), file = outfile, append=TRUE) 
		cat("</tr>\n", file = outfile, append=TRUE) 
	}
	#add entries row by row
	table.md <- paste("<tr>\n", apply(table, 1, function(x){ paste(paste("<td>", x, "</td>", sep=""), collapse="") }), "\n</tr>\n",sep="")
	cat(paste(table.md, collapse=""), file = outfile, append=TRUE)
	cat("</table>\n", file = outfile, append=TRUE) #end of the table
}