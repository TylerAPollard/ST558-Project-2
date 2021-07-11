## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/TylerAPollard/ST558-Project-2/edit/main/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/TylerAPollard/ST558-Project-2/settings/pages). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.

# Markdown Outputs of 7 Analyses
The analysis for [Monday is available here](mondayAnalysis.md)\
The analysis for [Tuesday is available here](tuesdayAnalysis.md)\
The analysis for [Wednesday is available here](wednesdayAnalysis.md)\
The analysis for [Thursday is available here](thursdayAnalysis.md)\
The analysis for [Friday is available here](fridayAnalysis.md)\
The analysis for [Saturday is available here](saturdayAnalysis.md)\
The analysis for [Sunday is available here](sundayAnalysis.md)\

# Code Used to Create Analyses
`weekday <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
output_file <- paste0(weekday, "Analysis.md")
params = lapply(weekday, 
                FUN = function(x){
                  list(weekday = x)
                })
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN = function(x){
        rmarkdown::render(input = "Project2_Tyler_Lucy.Rmd", output_file = x[[1]], params = x[[2]])
      })`
