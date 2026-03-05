This repository is a collection of functions to analyse stats19 data and produce reports for local areas. Currently it is possible to generate reports for Local Authorities and Combined Authorities.

To quickly generate a pre defined report for a Local Authority region, clone the repository and run the following functions:

One of the easiest ways to do this is to use the github command line interface (CLI):

in github CLI (not R), run:
```
gh repo create username/new_repo_name --template BlaiseKelly/stats19_stats --public --clone
```
This will create a new repository "new_repo_name"

Make sure quarto is installed https://quarto.org/docs/get-started/

or in cmd prompt (windows)
```
curl -LO https://github.com/quarto-dev/quarto-cli/releases/latest/download/quarto-1.6.40-win.msi
msiexec /i quarto-1.6.40-win.msi /quiet
```
navigate to the repo
e.g.
```
cd C:/User/github/new_repo_name
```
and run
```
quarto create-project --type website
```
push these files to the repo
```
git add _quarto.yml .gitignore
git commit -m "website files"
git push
```
Then tell github to look in `docs` folder instead of `master` 
```
gh api repos/username/new_repo_name/pages --method POST -H "Accept: application/vnd.github+json" -f "source[branch]=master" -f "source[path]=/docs"
gh api repos/username/new_repo_name/pages --method PUT -H "Accept: application/vnd.github+json" -f "source[branch]=master" -f "source[path]=/docs"
```
Once these steps are done pages can be easily pushed to the website by changing the _quarto.yml file

to generate the plots, and dataframes for the report type the LA e.g. Bristol
```
run_report_inputs(authority = "Bristol")
```
and it will output plots in the outputs/authority/ folder some of which are used to create the report: LA_report.html

To generate the site files (htmls can be manually viewed in the "docs/" folder):

```
run_site(authority = "Bristol")

```
and push the docs folder to github using the github CLI

```
git add docs
git commit -m "site pages"
git push
```
navigate to the github repo and it should indicate the site is rendering

you can also directly visit it at username.github.io/new_repo_name

and should look something like this https://blaisekelly.github.io/stats19_stats/

